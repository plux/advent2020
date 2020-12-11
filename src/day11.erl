-module(day11).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    fixpoint(grid(Lines), rules_p1()).

part2(Lines) ->
    fixpoint(grid(Lines), rules_p2()).

rules_p1() ->
    fun(Grid) ->
            fun(_Pos, $.) ->
                    $.;
               (Pos, V) ->
                    case {V, count_occupied(Pos, Grid)} of
                        {$L, 0}                     -> $#;
                        {$#, Count} when Count >= 4 -> $L;
                        _                           -> V
                    end
            end
    end.

rules_p2() ->
    fun(Grid) ->
            fun(_Pos, $.) ->
                    $.;
               (Pos, V) ->
                    case {V, count_occupied_p2(Pos, Grid)} of
                        {$L, 0}                     -> $#;
                        {$#, Count} when Count >= 5 -> $L;
                        _                           -> V
                    end
            end
    end.

fixpoint(Grid, Rules) ->
    case maps:map(Rules(Grid), Grid) of
        Grid    -> maps:get($#, counter:count(maps:values(Grid)));
        NewGrid -> fixpoint(NewGrid, Rules)
    end.

count_occupied(Pos, Map) ->
    length([1 || $# <- adjacent(Pos, Map)]).

count_occupied_p2(Pos, Grid) ->
    length([Dir || Dir <- directions(), sees_occupied(Dir, Pos, Grid)]).


adjacent(Pos, Map) ->
    [Value || Coord <- directions(),
              {ok, Value} <- [maps:find(add(Pos, Coord), Map)]].

sees_occupied(Dir, Pos, Grid) ->
    case maps:find(add(Dir, Pos), Grid) of
        {ok, $.} -> sees_occupied(Dir, add(Dir, Pos), Grid);
        {ok, $#} -> true;
        {ok, $L} -> false;
        error    -> false
    end.

directions() ->
    [ {-1,-1}
    , {-1, 0}
    , {-1, 1}
    , {0, -1}
    , {0, 1}
    , {1, -1}
    , {1, 0}
    , {1, 1}
    ].

add({X1, Y1}, {X2, Y2}) ->
    {X1+X2, Y1+Y2}.

grid(L) ->
    maps:from_list([{{X,Y},C} || {Y, Line} <- enumerate(L),
                                 {X, C} <- enumerate(Line)]).

enumerate(L) ->
    lists:zip(lists:seq(0, length(L)-1), L).

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
    L2 = [".......#.",
          "...#.....",
          ".#.......",
          ".........",
          "..#L....#",
          "....#....",
          ".........",
          "#........",
          "...#....."
         ],
    [ ?_assertEqual(37, part1(L))
    , ?_assertEqual(26, part2(L))
    , ?_assertEqual({2296, 2089}, ?solve())
    ].
