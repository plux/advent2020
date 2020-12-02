-module(day2).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    length(lists:filter(fun is_valid_pass/1, Lines)).

is_valid_pass(Line) ->
    [Min, Max, Char, Pass] = parse(Line),
    N = length([C || C <- Pass, C =:= Char]),
    Min =< N andalso N =< Max.

part2(Lines) ->
    length(lists:filter(fun is_valid_pass_p2/1, Lines)).

is_valid_pass_p2(Line) ->
    [Min, Max, Char, Pass] = parse(Line),
    1 == length([I || I <- [Min, Max], Char == lists:nth(I, Pass)]).

parse(Line) ->
    [Min, Max, [Char], Pass] = string:lexemes(Line, " : -"),
    [?int(Min), ?int(Max), Char, Pass].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [ "1-3 a: abcde"
        , "1-3 b: cdefg"
        , "2-9 c: ccccccccc"
        ],
    [ ?_assertEqual(2, part1(L))
    , ?_assertEqual(1, part2(L))
    , ?_assertEqual([1, 3, $b, "cdefg"], parse("1-3 b: cdefg"))
    , ?_assertEqual(false, is_valid_pass("1-3 b: cdefg"))
    , ?_assertEqual(true, is_valid_pass(["1-3 a: abcde"]))
    , ?_assertEqual(true, is_valid_pass_p2(["1-3 a: abcde"]))
    , ?_assertEqual(false, is_valid_pass_p2("1-3 b: cdefg"))
    , ?_assertEqual({515,711}, ?solve())
    ].
