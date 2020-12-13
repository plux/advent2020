-module(day12).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    navigate(actions(Lines), fun rules_p1/2, {{0, 0}, ?east}).

part2(Lines) ->
    navigate(actions(Lines), fun rules_p2/2, {{0, 0}, {10, -1}}).

actions(Lines) ->
    [{C, ?int(Rest)} || [C|Rest] <- Lines].

navigate([], _Fun, {{X, Y}, _Dir}) -> ?manhattan(X, Y);
navigate([Action|Rest], Fun, Ship) -> navigate(Rest, Fun, Fun(Action, Ship)).

rules_p1({$F, N}, {Pos, Dir}) -> {move(Pos, Dir, N), Dir};
rules_p1({$L, N}, {Pos, Dir}) -> {Pos, rotate(Dir, 360-N)};
rules_p1({$R, N}, {Pos, Dir}) -> {Pos, rotate(Dir, N)};
rules_p1({X,  N}, {Pos, Dir}) -> {move(Pos, direction(X), N), Dir}.

rules_p2({$F, N}, {Ship, WP}) -> {move(Ship, WP, N), WP};
rules_p2({$L, N}, {Ship, WP}) -> {Ship, rotate(WP, 360-N)};
rules_p2({$R, N}, {Ship, WP}) -> {Ship, rotate(WP, N)};
rules_p2({X,  N}, {Ship, WP}) -> {Ship, move(WP, direction(X), N)}.

direction($N) -> ?north;
direction($S) -> ?south;
direction($W) -> ?west;
direction($E) -> ?east.

rotate({X, Y}, 0)      -> {X, Y};
rotate({X, Y}, Degree) -> rotate({-Y, X}, Degree - 90).

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
    , ?_assertEqual({10,-1}, rotate({10,-1}, 360))
    , ?_assertEqual(286, part2(L))
    , ?_assertEqual({1838, 89936}, ?solve())
    ].
