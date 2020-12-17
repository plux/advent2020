-module(day17).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Grid = lists:foldl(fun(_, Acc) ->
                               step(Acc)
                       end, grid(Lines), lists:seq(1,6)),
    length([1 || $# <- maps:values(Grid)]).

grid(L) ->
    maps:from_list([{{X, Y, 0}, $#} || {X, Line} <- aoc:enumerate(L),
                                       {Y, $#} <- aoc:enumerate(Line)]).

step(Grid) ->
    [Xs, Ys, Zs] = unzipn(maps:keys(Grid)),
    maps:from_list([{{X,Y,Z}, $#} ||
                       X <- lists:seq(lists:min(Xs)-1, lists:max(Xs)+1),
                       Y <- lists:seq(lists:min(Ys)-1, lists:max(Ys)+1),
                       Z <- lists:seq(lists:min(Zs)-1, lists:max(Zs)+1),
                       activate({X,Y,Z}, Grid)]).

activate(Pos, Grid) ->
    case {maps:is_key(Pos, Grid), neighbors(Pos, Grid)} of
        {true, Ns} when Ns =:= 2; Ns =:= 3 -> true;
        {false, 3}                         -> true;
        _                                  -> false
    end.

neighbors({X,Y,Z}, Grid) ->
    length([1 || PX <- lists:seq(X-1,X+1),
                 PY <- lists:seq(Y-1,Y+1),
                 PZ <- lists:seq(Z-1,Z+1),
                 maps:is_key({PX,PY,PZ}, Grid),
                 {PX,PY,PZ} =/= {X, Y, Z}]).

part2(Lines) ->
    Grid = lists:foldl(fun(_, Acc) ->
                               step_p2(Acc)
                       end, grid_p2(Lines), lists:seq(1,6)),
    length([1 || $# <- maps:values(Grid)]).

grid_p2(L) ->
    maps:from_list([{{X, Y, 0, 0}, $#} || {X, Line} <- aoc:enumerate(L),
                                          {Y, $#} <- aoc:enumerate(Line)]).
step_p2(Grid) ->
    [Xs, Ys, Zs, Ws] = unzipn(maps:keys(Grid)),
    maps:from_list([{{X,Y,Z,W}, $#} ||
                       X <- lists:seq(lists:min(Xs)-1, lists:max(Xs)+1),
                       Y <- lists:seq(lists:min(Ys)-1, lists:max(Ys)+1),
                       Z <- lists:seq(lists:min(Zs)-1, lists:max(Zs)+1),
                       W <- lists:seq(lists:min(Ws)-1, lists:max(Ws)+1),
                       activate_p2({X,Y,Z,W}, Grid)]).

activate_p2(Pos, Grid) ->
    case {maps:is_key(Pos, Grid), neighbors_p2(Pos, Grid)} of
        {true, Ns} when Ns =:= 2; Ns =:= 3 -> true;
        {false, 3}                         -> true;
        _                                  -> false
    end.


neighbors_p2({X,Y,Z,W}, Grid) ->
    length([1 || PX <- [X-1, X, X+1],
                 PY <- [Y-1, Y, Y+1],
                 PZ <- [Z-1, Z, Z+1],
                 PW <- [W-1, W, W+1],
                 maps:is_key({PX,PY,PZ,PW}, Grid),
                 {PX,PY,PZ,PW} =/= {X, Y, Z, W}]).

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

unzipn(L) ->
    [[element(N, Tuple) || Tuple <- L] || N <- lists:seq(1, tuple_size(hd(L)))].

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
