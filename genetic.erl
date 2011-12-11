-module(genetic).
-author("Mateusz Lenik").
-import(lists, [keysort/2, split/2]).
-export([inverse_fitness/1]).

% main() ->
  % % Read data in
  % Data =

  % % Spawn initial population
  % Population = spawn_population(Data, 100),
  % Best = evolve(Population, 100000, 5),
  % io:format("Best solution is ~p~n", [Best]).

% Computes the value of target function
inverse_fitness(Permutation) -> inverse_fitness(Permutation, 0, 0).
inverse_fitness([], _, Acc) -> Acc;
inverse_fitness([{Pj, Wj, Dj}|Rest], Time, Acc) ->
  inverse_fitness(Rest, Time + Pj, Wj*max(0, Time + Pj - Dj) + Acc).

% breedPermutation

% evolve(Population = [H|_], TimeLeft, ProbabilityOfMutation) ->
  % evolve(Population, TimeLeft, ProbabilityOfMutation, H).

% evolve(_, 0, _, Best) -> Best;
% evolve(Population, TimeLeft, ProbabilityOfMutation, Best) ->
  % List = keysort(2, [{X, inverse_fitness(X)} || X <- Population]),
  % {Good, Bad} = split(

