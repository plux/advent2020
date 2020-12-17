-module(day17).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Grid = lists:foldl(fun(N, Acc) ->
                               step(N, Acc)
                       end, grid(Lines), lists:seq(1,6)),
    length([1 || $# <- maps:values(Grid)]).

grid(L) ->
    maps:from_list([{{X, Y, 0}, C} || {X, Line} <- aoc:enumerate(L),
                                      {Y, C} <- aoc:enumerate(Line)]).

step(N, Grid) ->
    Dims = lists:seq(-N, N+6),
    maps:from_list([{{X,Y,Z}, cube({X,Y,Z}, Grid)} || X <- Dims,
                                                      Y <- Dims,
                                                      Z <- Dims]).

cube(Pos, Grid) ->
    case {maps:get(Pos, Grid, $.), neighbors(Pos, Grid)} of
        {$#, Ns} when Ns =:= 2; Ns =:= 3 -> $#;
        {$., 3}                          -> $#;
        _                                -> $.
    end.

neighbors({X,Y,Z}, Grid) ->
    length([1 || PX <- lists:seq(X-1,X+1),
                 PY <- lists:seq(Y-1,Y+1),
                 PZ <- lists:seq(Z-1,Z+1),
                 {PX,PY,PZ} =/= {X, Y, Z},
                 maps:get({PX,PY,PZ}, Grid, $.) =:= $#]).

part2(Lines) ->
    Grid = lists:foldl(fun(N, Acc) ->
                               step_p2(N, Acc)
                       end, grid_p2(Lines), lists:seq(1,6)),
    length([1 || $# <- maps:values(Grid)]).

grid_p2(L) ->
    maps:from_list([{{X, Y, 0, 0}, C} || {X, Line} <- aoc:enumerate(L),
                                         {Y, C} <- aoc:enumerate(Line)]).
step_p2(N, Grid) ->
    Dims = lists:seq(-N, N+6),
    maps:from_list([{{X,Y,Z,W}, cube_p2({X,Y,Z,W}, Grid)} || X <- Dims,
                                                             Y <- Dims,
                                                             Z <- Dims,
                                                             W <- Dims]).
cube_p2(Pos, Grid) ->
    case {maps:get(Pos, Grid, $.), neighbors_p2(Pos, Grid)} of
        {$#, Ns} when Ns =:= 2; Ns =:= 3 -> $#;
        {$., 3}                          -> $#;
        _                                -> $.
    end.


neighbors_p2({X,Y,Z,W}, Grid) ->
    length([1 || PX <- lists:seq(X-1,X+1),
                 PY <- lists:seq(Y-1,Y+1),
                 PZ <- lists:seq(Z-1,Z+1),
                 PW <- lists:seq(W-1,W+1),
                 {PX,PY,PZ,PW} =/= {X, Y, Z, W},
                 maps:get({PX,PY,PZ,PW}, Grid, $.) =:= $#]).

print_grid(N, Grid) ->
    Dims = lists:seq(-N, N),
    lists:foreach(
      fun(Z) ->
              io:format("\nz=~p\n", [Z]),
              lists:foreach(
                fun(X) ->
                        lists:foreach(
                          fun(Y) ->
                                  io:format("~s", [[maps:get({X,Y,Z}, Grid, $.)]])
                          end, Dims),
                        io:format("\n")
                end, Dims)
      end, Dims).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    Input = [ ".#."
            , "..#"
            , "###"
            ],
     [ ?_assertEqual(112, part1(Input))
       %% timeout , ?_assertEqual(848, part2(Input))
       %% timeout , ?_assertEqual(ok, part2(Input))
       %% timeout , ?_assertEqual({293, ok}, ?solve())
     ].
