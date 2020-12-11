-module(day10).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?ints(Input)), part2(?ints(Input))}.

part1(Ints) ->
    #{1 := Ones, 3 := Threes} = counter:count(connect(connectors(Ints))),
    (Ones + 1) * (Threes + 1).

connectors(Ints) ->
    lists:sort(Ints).

connect([H|T]) ->
    connect(H, T).

connect(_, [])    -> [];
connect(A, [B|T]) -> [B-A|connect(B,T)].

part2(_Ints) ->
    unsolved.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L1 = [0,
          1,
          4,
          5,
          6,
          7,
          10,
          11,
          12,
          15,
          16,
          19,
          22
          ],
    L2 = [28,
         33,
         18,
         42,
         31,
         14,
         46,
         20,
         48,
         47,
         24,
         23,
         49,
         45,
         19,
         38,
         39,
         11,
         1,
         32,
         25,
         35,
         8,
         17,
         7,
         9,
         4,
         2,
         34,
         10,
         3],
   [ ?_assertEqual(220, part1(L2))
   , ?_assertEqual(8, part2(L1))
   , ?_assertEqual({2346, unsolved}, ?solve())
   ].
