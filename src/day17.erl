-module(day17).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Grid = lists:foldl(fun(_, Acc) ->
                               step(Acc)
                       end, grid(Lines), lists:seq(1,6)),
    maps:size(Grid).

grid(L) ->
    maps:from_list([{[X, Y, 0], $#} || {X, Line} <- aoc:enumerate(L),
                                       {Y, $#} <- aoc:enumerate(Line)]).


cartesian([H|T]) -> [[A|B] || A <- H, B <- cartesian(T)];
cartesian([]) -> [[]].

part2(Lines) ->
    Grid = lists:foldl(fun(_, Acc) ->
                               step(Acc)
                       end, grid_p2(Lines), lists:seq(1,6)),
    maps:size(Grid).

grid_p2(L) ->
    maps:from_list([{[X, Y, 0, 0], $#} || {X, Line} <- aoc:enumerate(L),
                                          {Y, $#} <- aoc:enumerate(Line)]).
step(Grid) ->
    Dims = [lists:seq(lists:min(L)-1, lists:max(L)+1) || L <- unzipn(maps:keys(Grid))],
    maps:from_list([{Pos, $#} || Pos <- cartesian(Dims), activate(Pos, Grid)]).

activate(Pos, Grid) ->
    case {maps:is_key(Pos, Grid), neighbors(Pos, Grid)} of
        {true, Ns} when Ns =:= 2; Ns =:= 3 -> true;
        {false, 3}                         -> true;
        _                                  -> false
    end.

neighbors(Pos, Grid) ->
    Dims = [[N-1, N, N+1] || N <- Pos],
    length([1 || Neigh <- cartesian(Dims), maps:is_key(Neigh, Grid), Neigh =/= Pos]).

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
    [[lists:nth(N, Tuple) || Tuple <- L] || N <- lists:seq(1, length(hd(L)))].

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
