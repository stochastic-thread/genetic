-module(genetic).
-author("Mateusz Lenik").
-import(lists, [keysort/2, split/2, map/2, reverse/1, sublist/3, foldl/3, zip/2]).
-export([inverse_fitness/1, mutate/1, breed/2]).
-export([breed/4, probability/1]).

% main() ->
% % Read data in
% Data =

% % Spawn initial population
% Population = spawn_population(Data, 100),
% Best = evolve(Population, 100000, 5),
% io:format("Best solution is ~p~n", [Best]).

% Computes the value of target function
% {TaskNo, TaskLen, TaskWeight, TaskDueDate}
%
inverse_fitness(Permutation) -> inverse_fitness(Permutation, 0, 0).
inverse_fitness([], _, Acc) -> Acc;
inverse_fitness([{_, Pj, Wj, Dj}|Rest], Time, Acc) ->
  inverse_fitness(Rest, Time + Pj, Wj*max(0, Time + Pj - Dj) + Acc).

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
  {mutate(C1, P), mutate(C2)}.

% Gene swapping used in PMX crossover
breed_swap({Gene, NewGene}, Gene) -> NewGene;
breed_swap({Gene, NewGene}, NewGene) -> Gene;
breed_swap({_, _}, Gene) -> Gene.

% computes swapping vector for breeding
breed_vector({Parent1, Parent2}, S1, S2) ->
  L1 = sublist(Parent1, S1, S2 - S1),
  L2 = sublist(Parent2, S1, S2 - S1),
  zip(L1, L2).

% Function returning true with probability of 1/2^N
probability(1) -> true;
probability(N) when N > 1 ->
  R1 = random:uniform(),
  R2 = random:uniform(),
  case R1 =< R2 of
    true  -> probability(N - 1);
    false -> false
  end.

% evolve(Population = [H|_], TimeLeft, ProbabilityOfMutation) ->
% evolve(Population, TimeLeft, ProbabilityOfMutation, H).

% evolve(_, 0, _, Best) -> Best;
% evolve(Population, TimeLeft, ProbabilityOfMutation, Best) ->
% List = keysort(2, [{X, inverse_fitness(X)} || X <- Population]),
% {Good, Bad} = split(

