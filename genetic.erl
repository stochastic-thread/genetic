-module(genetic).
-author("Mateusz Lenik").
-export([start/7, debug/6]).

-define(INSTANCE_COUNT, 125).
-define(VARIABLE_COUNT, 3).

start(File, BestFile, Base, Islands, Time, Mutability, Meeting) ->
  random:seed(now()),
  Instances = lists:zip(read_best(BestFile), read_instances(File)),
  Run = fun({Best, I}) ->
      new_world(I, Base, Islands, Time, Mutability, Meeting, Best)
  end,
  io:format("| Found | Best  | Time  |~n"),
  lists:foreach(Run, Instances).

% Function for performance testing
debug(FileName, Base, Islands, Time, Mutability, Meeting) ->
  Instances = read_instances(FileName),
  new_world(hd(Instances), Base, Islands, Time, Mutability, Meeting, 0).

% Creates new world and starts genetic algorithm
new_world(Instance, Base, Islands, Time, Mutability, Meeting, Best) ->
  Populations = spawn_populations(Instance, Base, Islands),
  {BestSolution, TimeLeft} = evolve(Populations, Time, Mutability, Meeting, Best),
  io:format("|~p\t|~p\t|~p\t|~n",
    [inverse_fitness(BestSolution), Best, Time - TimeLeft]),
  Best.

% Function reading input files
read_instances(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  parse_instances(Bin).

read_best(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  parse_string(Bin).

% Function parses input data
parse_instances(Bin) ->
  Data = parse_string(Bin),
  InstanceSize = length(Data) div (?INSTANCE_COUNT * ?VARIABLE_COUNT),
  parse_instances(InstanceSize, Data, ?INSTANCE_COUNT, []).

parse_instances(_, _, 0, Acc) -> lists:reverse(Acc);
parse_instances(InstanceSize, Data, N, Acc) ->
  {Instance, Rest} = lists:split(?VARIABLE_COUNT*InstanceSize, Data),
  {Pj, Other} = lists:split(InstanceSize, Instance),
  {Wj, Dj} = lists:split(InstanceSize, Other),
  parse_instances(InstanceSize, Rest, N - 1, [lists:zip3(Pj,Wj,Dj)|Acc]).

% Parses binary string to list of integers
parse_string(Bin) when is_binary(Bin) -> parse_string(binary_to_list(Bin));
parse_string(Str) when is_list(Str) ->
  [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")].

% Computes the value of target function
inverse_fitness(Permutation) ->
  {_, Result} = lists:foldl(fun compute_inverse_fitness/2, {0, 0}, Permutation),
  Result.

compute_inverse_fitness({Pj, Wj, Dj}, {Time, Acc}) ->
  {Time + Pj, Wj*max(0, Time + Pj - Dj) + Acc}.


% Sorts the list by inverse_fitness
sort_by_fitness(Population) ->
  lists:sort(fun sorting_function/2, Population).

% Sorting procedure for sorting by fitness
sorting_function(A, B) ->
  inverse_fitness(A) =< inverse_fitness(B).

% Mutation procedure
% Implemented using sequence swap
mutate(Permutation, P) ->
  case probability(P) of
    true  -> mutate(Permutation);
    false -> Permutation
  end.

mutate(Permutation) ->
  S1 = random:uniform(length(Permutation)),
  S2 = random:uniform(length(Permutation)),
  case S1 > S2 of
    true  -> mutate(Permutation, S2, S1);
    false -> mutate(Permutation, S1, S2)
  end.

mutate(Permutation, S1, S2) ->
  {Head, Tail} = lists:split(S1, Permutation),
  {Middle, End} = lists:split(S2 - S1, Tail),
  Head ++ lists:reverse(Middle) ++ End.

% Breeding algorithm
% Implemented using PMX crossover
breed(Parents = {P1, P2}, ProbabilityOfMutation) ->
  S1 = random:uniform(length(P1)),
  S2 = random:uniform(length(P2)),
  case S1 > S2 of
    true  -> breed(Parents, S2, S1, ProbabilityOfMutation);
    false -> breed(Parents, S1, S2, ProbabilityOfMutation)
  end.

breed(Parents = {P1, P2}, S1, S2, P) ->
  V = breed_vector(Parents, S1, S2),
  C1 = lists:map(fun(X) -> lists:foldl(fun breed_swap/2, X, V) end, P1),
  C2 = lists:map(fun(X) -> lists:foldl(fun breed_swap/2, X, V) end, P2),
  {mutate(C1, P), mutate(C2, P)}.

% Gene swapping function used in PMX crossover
breed_swap({Gene, NewGene}, Gene) -> NewGene;
breed_swap({Gene, NewGene}, NewGene) -> Gene;
breed_swap({_, _}, Gene) -> Gene.

% Computes swapping vector for breeding
breed_vector({Parent1, Parent2}, S1, S2) ->
  L1 = lists:sublist(Parent1, S1, S2 - S1),
  L2 = lists:sublist(Parent2, S1, S2 - S1),
  lists:zip(L1, L2).

% Function returning true with probability of 1/2^N
probability(0) -> true;
probability(N) when N >= 1 ->
  R1 = random:uniform(),
  R2 = random:uniform(),
  case R1 =< R2 of
    true  -> probability(N - 1);
    false -> false
  end.

% Function generates M base populations
spawn_populations(Tasks, N, M) -> spawn_populations(Tasks, N, M, []).
spawn_populations(_, _, 0, Acc) -> Acc;
spawn_populations(Tasks, N, M, Acc) ->
  spawn_populations(Tasks, N, M - 1, [spawn_population(Tasks, N)|Acc]).

% Function generating base population
spawn_population(Tasks, N) -> spawn_population(Tasks, N, []).
spawn_population(_, 0, Acc) -> Acc;
spawn_population(Tasks, N, Acc) ->
  Permutation = lists:keysort(2, [{X, random:uniform()} || X <- Tasks]),
  New = [X || {X,_} <- Permutation],
  spawn_population(Tasks, N - 1, [New|Acc]).

% Genetic algorithm itself
evolve(Populations, TimeLeft, Pmutation, Pmeeting, Best) ->
  Sorted = lists:map(fun(P) -> sort_by_fitness(P) end, Populations),
  evolve(Sorted, TimeLeft, Pmutation, Pmeeting, [], Best).

evolve(_, 0, _, _, BestSolution, _) -> {BestSolution, 0};
evolve(Populations, TimeLeft, Pmutation, Pmeeting, _, Best) ->
  NewPopulations = meet(Populations, Pmeeting),
  Sorted = lists:map(fun(Population) ->
        Length = length(Population) div 3,
        {Good, Bad} = lists:split(Length, Population),
        NewGood = reproduce(Good, Pmutation),
        sort_by_fitness(NewGood ++ Good ++ lists:sublist(Bad, Length))
    end, NewPopulations),
  {FoundBest, BestSolution} = lists:foldl(fun(Population, {Found, B}) ->
        BestS = hd(Population),
        case Found of
          true ->  {true, B};
          false -> {inverse_fitness(BestS) =< Best, BestS}
        end
    end, {false, hd(hd(Sorted))}, Sorted),
  % io:format("~p~n", [length(BestSolution) == 0]),
  case FoundBest of
    false -> evolve(Sorted, TimeLeft - 1, Pmutation, Pmeeting, BestSolution, Best);
    true  -> {BestSolution, TimeLeft}
  end.

meet(Populations, Pmeeting) ->
  case probability(Pmeeting) of
    true  -> meet(Populations);
    false -> Populations
  end.

meet(Populations) ->
  _dupa = length(Populations),
  Populations.

% Function defining reproduction cycle
reproduce(Generation, P) -> reproduce(Generation, [], P).
reproduce([], NewGeneration, _) -> NewGeneration;
reproduce([P1, P2|Rest], NewGeneration, P) ->
  {C1, C2} = breed({P1, P2}, P),
  reproduce(Rest, [C1, C2|NewGeneration], P);
reproduce([Last], NewGeneration, P) ->
  reproduce([], [Last|NewGeneration], P).

