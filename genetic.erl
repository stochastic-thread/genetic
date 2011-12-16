-module(genetic).
-author("Mateusz Lenik").
-import(lists, [keysort/2, split/2, map/2, reverse/1, sublist/3, sublist/2, foldl/3]).
-export([main/1, main/0]).

-define(INSTANCE_COUNT, 125).
-define(VARIABLE_COUNT, 3).

main() ->
  io:put_chars("Args: FILENAME [BASE_SIZE [TIME_LEFT [MUTABILITY]]]\n"),
  erlang:halt(0).
main([FileName]) -> main([FileName, 50]);
main([FileName, Base]) -> main([FileName, Base, 1000]);
main([FileName, Base, Time]) -> main([FileName, Base, Time, 5]);
main([FileName, BaseS, TimeS, MutabilityS]) ->
  random:seed(now()),
  {ok, Bin} = file:read_file(FileName),
  Instances = parse_instances(Bin),
  Base = parse_number(BaseS),
  Time = parse_number(TimeS),
  Mutability = parse_number(MutabilityS),
  lists:foreach(fun(I) -> new_world(I, Base, Time, Mutability) end, Instances),
  erlang:halt(0).

% Creates new world and starts genetic algorithm
new_world(Instance, Base, Time, Mutability) ->
  Population = spawn_population(Instance, Base),
  Best = evolve(Population, Time, Mutability),
  io:format("Best result is ~p.~n", [inverse_fitness(Best)]).

% Function parses input data
parse_instances(Bin) ->
  Data = parse_string(Bin),
  InstanceSize = length(Data) div (?INSTANCE_COUNT * ?VARIABLE_COUNT),
  parse_instances(InstanceSize, Data, ?INSTANCE_COUNT, []).

parse_instances(_, _, 0, Acc) -> reverse(Acc);
parse_instances(InstanceSize, Data, N, Acc) ->
  {Instance, Rest} = split(?VARIABLE_COUNT*InstanceSize, Data),
  {Pj, Other} = split(InstanceSize, Instance),
  {Wj, Dj} = split(InstanceSize, Other),
  parse_instances(InstanceSize, Rest, N - 1, [lists:zip3(Pj,Wj,Dj)|Acc]).

% Parses binary string to list of integers
parse_string(Bin) when is_binary(Bin) -> parse_string(binary_to_list(Bin));
parse_string(Str) when is_list(Str) ->
  [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")].

% Argument parsing function
parse_number(Int) when is_integer(Int) -> Int;
parse_number(Str) when is_list(Str) -> parse_number(list_to_integer(Str));
parse_number(Bin) when is_binary(Bin) -> parse_number(binary_to_list(Bin)).

% Computes the value of target function
% {TaskLen, TaskWeight, TaskDueDate}
inverse_fitness(Permutation) ->
  inverse_fitness(Permutation, 0, 0).
inverse_fitness([], _, Acc) -> Acc;
inverse_fitness([{Pj, Wj, Dj}|Rest], Time, Acc) ->
  inverse_fitness(Rest, Time + Pj, Wj*max(0, Time + Pj - Dj) + Acc).

% Sorts the list by inverse_fitness
sort_by_fitness(Population) ->
  Sorted = keysort(2, [{X, inverse_fitness(X)} || X <- Population]),
  [X || {X,_} <- Sorted].

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
  {Head, Tail} = split(S1, Permutation),
  {Middle, End} = split(S2 - S1, Tail),
  Head ++ reverse(Middle) ++ End.

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
  C1 = map(fun(X) -> foldl(fun breed_swap/2, X, V) end, P1),
  C2 = map(fun(X) -> foldl(fun breed_swap/2, X, V) end, P2),
  {mutate(C1, P), mutate(C2, P)}.

% Gene swapping function used in PMX crossover
breed_swap({Gene, NewGene}, Gene) -> NewGene;
breed_swap({Gene, NewGene}, NewGene) -> Gene;
breed_swap({_, _}, Gene) -> Gene.

% Computes swapping vector for breeding
breed_vector({Parent1, Parent2}, S1, S2) ->
  L1 = sublist(Parent1, S1, S2 - S1),
  L2 = sublist(Parent2, S1, S2 - S1),
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

% Function generating base population
spawn_population(Tasks, N) -> spawn_population(Tasks, N, []).
spawn_population(_, 0, Acc) -> Acc;
spawn_population(Tasks, N, Acc) ->
  Permutation = keysort(2, [{X, random:uniform()} || X <- Tasks]),
  New = [X || {X,_} <- Permutation],
  spawn_population(Tasks, N - 1, [New|Acc]).

% Genetic algorithm itself
evolve(Population, TimeLeft, Pmutation) ->
  Sorted = sort_by_fitness(Population),
  evolve(Sorted, TimeLeft, Pmutation, hd(Sorted)).

evolve(_, 0, _, Best) -> Best;
evolve(Population, TimeLeft, Pmutation, _) ->
  Length = length(Population) div 3,
  {Good, Bad} = split(Length, Population),
  NewGood = reproduce(Good, Pmutation),
  Sorted = sort_by_fitness(NewGood ++ Good ++ sublist(Bad, Length)),
  evolve(Sorted, TimeLeft - 1, Pmutation, hd(Sorted)).

% Function defining reproduction cycle
reproduce(Generation, P) -> reproduce(Generation, [], P).
reproduce([], NewGeneration, _) -> NewGeneration;
reproduce([P1, P2|Rest], NewGeneration, P) ->
  {C1, C2} = breed({P1, P2}, P),
  reproduce(Rest, [C1, C2|NewGeneration], P);
reproduce([Last], NewGeneration, P) ->
  reproduce([], [Last|NewGeneration], P).

