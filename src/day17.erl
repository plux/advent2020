-module(day17).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    maps:size(step(6, grid(Lines, [0]))).

part2(Lines) ->
    maps:size(step(6, grid(Lines, [0, 0]))).

grid(L, Padding) ->
    maps:from_list([{[X, Y] ++ Padding, $#} || {X, Line} <- aoc:enumerate(L),
                                               {Y, $#} <- aoc:enumerate(Line)]).

step(0, Grid) -> Grid;
step(N, Grid) ->
    Neighs = lists:usort([Neigh || Pos <- maps:keys(Grid), Neigh <- neighbors(Pos)]),
    step(N-1, maps:from_list([{Pos, $#} || Pos <- Neighs, activate(Pos, Grid)])).

activate(Pos, Grid) ->
    NumNeighs = length([N || N <- neighbors(Pos), maps:is_key(N, Grid)]),
    NumNeighs =:= 3 orelse (maps:is_key(Pos, Grid) andalso NumNeighs =:= 2).

neighbors(Pos) ->
    [Neigh || Neigh <- aoc:cartesian([[N-1, N, N+1] || N <- Pos]), Neigh =/= Pos].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = [ ".#."
            , "..#"
            , "###"
            ],
     [ ?_assertEqual(112, part1(Input))
     , ?_assertEqual(848, part2(Input))
     , ?_assertEqual({293, 1816}, ?solve())
     ].
