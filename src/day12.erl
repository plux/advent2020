-module(day12).
-compile([export_all]).
-include("aoc.hrl").

%%      N              {0,-1}
%%   NW | NE      {-1,-1}|{1,-1}
%% W ---+--- E {-1,0}----+----{1,0}
%%   SW | SE       {-1,1}|{1,1}
%%      S              {0,1}

-define(west, {-1, 0}).
-define(east, {1, 0}).
-define(north, {0, -1}).
-define(south, {0, 1}).

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    navigate(parse(Lines), fun rules_p1/2, {{0, 0}, ?east}).

part2(Lines) ->
    navigate(parse(Lines), fun rules_p2/2, {{0, 0}, {10, -1}}).

navigate([], _Fun, {{X, Y}, _Dir}) -> abs(X) + abs(Y);
navigate([Action|Rest], Fun, Ship) -> navigate(Rest, Fun, Fun(Action, Ship)).

rules_p1({$F, N}, {Pos, Dir}) -> {move(Pos, Dir, N), Dir};
rules_p1({$L, N}, {Pos, Dir}) -> {Pos, rot(Dir, 360-N)};
rules_p1({$R, N}, {Pos, Dir}) -> {Pos, rot(Dir, N)};
rules_p1({X,  N}, {Pos, Dir}) -> {move(Pos, dir(X), N), Dir}.

rules_p2({$F, N}, {Ship, WP}) -> {move(Ship, WP, N), WP};
rules_p2({$L, N}, {Ship, WP}) -> {Ship, rot(WP, 360-N)};
rules_p2({$R, N}, {Ship, WP}) -> {Ship, rot(WP, N)};
rules_p2({X,  N}, {Ship, WP}) -> {Ship, move(WP, dir(X), N)}.

dir($N) -> ?north;
dir($S) -> ?south;
dir($W) -> ?west;
dir($E) -> ?east.

rot({X, Y}, 0) -> {X, Y};
rot({X, Y}, N) -> rot({-Y, X}, N-90).

parse(Lines) ->
    [{C, ?int(Rest)} || [C|Rest] <- Lines].

move({X1, Y1}, {X2, Y2}, N) ->
    {X1 + N*X2, Y1 + N*Y2}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = ["F10",
         "N3",
         "F7",
         "R90",
         "F11"
        ],
    [ ?_assertEqual(25, part1(L))
    , ?_assertEqual({10,-1}, rot({10,-1}, 360))
    , ?_assertEqual(286, part2(L))
    , ?_assertEqual({1838, 89936}, ?solve())
    ].
