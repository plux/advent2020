-module(day5).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    lists:max([seat_id(Line) || Line <- Lines]).

part2(Lines) ->
    my_seat(lists:sort([seat_id(Line) || Line <- Lines])).

my_seat([X,Y|_]) when X+2 == Y ->
    X+1;
my_seat([_|Rest]) ->
    my_seat(Rest).

seat_id(Line) ->
    list_to_integer(lists:map(fun($B) -> $1;
                                 ($R) -> $1;
                                 (_)  -> $0
                              end, Line), 2).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    [ ?_assertEqual(567, seat_id("BFFFBBFRRR"))
    , ?_assertEqual({944, 554}, ?solve())
    ].
