-module(day1).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?int_lines(Input)), part2(?int_lines(Input))}.

part1(L) ->
    hd([X*Y || X <- L, Y <- L, X+Y =:= 2020]).


part2(L) ->
    hd([X*Y*Z || X <- L, Y <- L, Z <- L, X+Y+Z =:= 2020]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [ 1721
        , 979
        , 366
        , 299
        , 675
        , 1456
        ],
    [ ?_assertEqual(514579, part1(L))
    , ?_assertEqual(241861950, part2(L))
    , ?_assertEqual({224436,303394260}, ?solve())
    ].
