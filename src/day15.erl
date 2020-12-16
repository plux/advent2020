-module(day15).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(Input), part2(Input)}.

part1(Lines) ->
    {Curr, I} = seed(parse(Lines)),
    solve(Curr, I, 2020).

part2(Lines) ->
    {Curr, I} = seed(parse(Lines)),
    solve(Curr, I, 30000000).

parse(Line) ->
    [?int(N) || N <- ?split(Line, ",")].

solve(Curr, Limit, Limit) ->
    Curr;
solve(Curr, I, Limit) ->
    Next = case put(Curr, I) of
               undefined -> 0;
               Prev      -> I - Prev
           end,
    solve(Next, I+1, Limit).

seed(L) ->
    erase(),
    seed(L, 1).

seed([Curr], I) ->
    {Curr, I};
seed([Curr|T], I) ->
    put(Curr, I),
    seed(T, I+1).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(436,  part1("0,3,6"))
    , ?_assertEqual(1,    part1("1,3,2"))
    , ?_assertEqual(2578, part2("1,3,2"))
    , ?_assertEqual(10,   part1("2,1,3"))
    , ?_assertEqual(27,   part1("1,2,3"))
    , ?_assertEqual(78,   part1("2,3,1"))
    , ?_assertEqual(438,  part1("3,2,1"))
    , ?_assertEqual(1836, part1("3,1,2"))
    , ?_assertEqual(758,  part1("2,20,0,4,1,17"))
    , ?_assertEqual({758, 814}, ?solve())
    ].
