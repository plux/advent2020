-module(day12).
-compile([export_all]).
-include("aoc.hrl").

-define(west, {-1, 0}).
-define(east, {1, 0}).
-define(north, {0, -1}).
-define(south, {0, 1}).

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Actions = parse(Lines),
    {X,Y} = apply_actions(Actions, {{0, 0}, ?east}),
    abs(X)+abs(Y).

apply_actions([], {Pos, _Dir}) -> Pos;
apply_actions([Action|Rest], Ship) ->
    apply_actions(Rest, apply_action(Action, Ship)).

apply_action({$F, N}, {Pos, Dir}) -> {move(Pos, Dir, N), Dir};
apply_action({$N, N}, {Pos, Dir}) -> {move(Pos, ?north, N), Dir};
apply_action({$S, N}, {Pos, Dir}) -> {move(Pos, ?south, N), Dir};
apply_action({$E, N}, {Pos, Dir}) -> {move(Pos, ?east, N), Dir};
apply_action({$W, N}, {Pos, Dir}) -> {move(Pos, ?west, N), Dir};
apply_action({$L, N}, {Pos, Dir}) -> {Pos, rot_l(Dir, N)};
apply_action({$R, N}, {Pos, Dir}) -> {Pos, rot_r(Dir, N)}.

%% TODO: This could be made smarter
rot_r(Dir, 0)    -> Dir;
rot_r(?east, N)  -> rot_r(?south, N-90);
rot_r(?south, N) -> rot_r(?west, N-90);
rot_r(?west, N)  -> rot_r(?north, N-90);
rot_r(?north, N) -> rot_r(?east, N-90).

rot_l(Dir, 0)    -> Dir;
rot_l(?east, N)  -> rot_l(?north, N-90);
rot_l(?south, N) -> rot_l(?east, N-90);
rot_l(?west, N)  -> rot_l(?south, N-90);
rot_l(?north, N) -> rot_l(?west, N-90).

parse(Lines) ->
    [{C, ?int(Rest)} || [C|Rest] <- Lines].

part2(_Lines) ->
    unsolved.

move(Pos, {X,Y}, N) ->
    add(Pos, {X*N, Y*N}).

add({X1, Y1}, {X2, Y2}) ->
    {X1+X2, Y1+Y2}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = ["F10",
         "N3",
         "F7",
         "R90",
         "F11"
        ],
    [ ?_assertEqual(25, part1(L))
    , ?_assertEqual(286, part2(L))
    , ?_assertEqual({1838, unsolved}, ?solve())
    ].
