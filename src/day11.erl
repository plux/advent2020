-module(day11).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    fixpoint(?grid(Lines), rules(fun count_adjacent_occupied/2, 4)).

part2(Lines) ->
    fixpoint(?grid(Lines), rules(fun count_seen_occupied/2, 5)).

rules(Counter, Limit) ->
    fun(Grid) ->
            fun(_Pos, $.) ->
                    $.;
               (Pos, V) ->
                    case {V, Counter(Pos, Grid)} of
                        {$L, 0}                         -> $#;
                        {$#, Count} when Count >= Limit -> $L;
                        _                               -> V
                    end
            end
    end.

fixpoint(Grid, Rules) ->
    case maps:map(Rules(Grid), Grid) of
        Grid    -> maps:get($#, counter:count(maps:values(Grid)));
        NewGrid -> fixpoint(NewGrid, Rules)
    end.

count_adjacent_occupied(Pos, Grid) ->
    length([Dir || Dir <- directions(), is_occupied(Dir, Pos, Grid)]).

count_seen_occupied(Pos, Grid) ->
    length([Dir || Dir <- directions(), sees_occupied(Dir, Pos, Grid)]).

is_occupied(Dir, Pos, Grid) ->
    {ok, $#} =:= maps:find(add(Dir, Pos), Grid).

sees_occupied(Dir, Pos, Grid) ->
    case maps:find(add(Dir, Pos), Grid) of
        {ok, $.} -> sees_occupied(Dir, add(Dir, Pos), Grid);
        {ok, $#} -> true;
        {ok, $L} -> false;
        error    -> false
    end.

directions() ->
    [ ?nw, ?north, ?ne
    , ?west,       ?east
    , ?sw, ?south, ?se
    ].

add({X1, Y1}, {X2, Y2}) ->
    {X1+X2, Y1+Y2}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = ["#.##.##.##",
         "#######.##",
         "#.#.#..#..",
         "####.##.##",
         "#.##.##.##",
         "#.#####.##",
         "..#.#.....",
         "##########",
         "#.######.#",
         "#.#####.##"
        ],
    [ ?_assertEqual(37, part1(L))
    , ?_assertEqual(26, part2(L))
    , ?_assertEqual({2296, 2089}, ?solve())
    ].
