-module(day6).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?split(Input, "\n\n")), part2(?split(Input, "\n\n"))}.

part1(Groups) ->
    lists:sum([length(lists:usort(Group) -- "\n") || Group <- Groups]).

part2(Groups) ->
    lists:sum([length(intersect(?lines(Group))) || Group <- Groups]).

intersect(Lines) ->
    ordsets:intersection([ordsets:from_list(Line) || Line <- Lines]).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = "abc\n"
        "\n"
        "a\n"
        "b\n"
        "c\n"
        "\n"
        "ab\n"
        "ac\n"
        "\n"
        "a\n"
        "a\n"
        "a\n"
        "a\n"
        "\n"
        "b\n",
    [ ?_assertEqual(11, part1(?split(L, "\n\n")))
    , ?_assertEqual({6763, 3512}, ?solve())
    ].
