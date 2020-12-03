-module(day3).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    solve(Lines, 3, 1).

solve(Lines, StepX, StepY) ->
    solve(Lines, 0, 0, StepX, StepY).

solve(Lines, _X, Y, _StepX, _StepY) when Y >= length(Lines) ->
    0;
solve(Lines, X, Y, StepX, StepY) ->
    Line = lists:nth(Y + 1, Lines),
    case lists:nth((X rem length(Line)) + 1, Line) of
        $# -> 1 + solve(Lines, X + StepX, Y + StepY, StepX, StepY);
        _  -> 0 + solve(Lines, X + StepX, Y + StepY, StepX, StepY)
    end.

part2(Lines) ->
    solve(Lines, 1, 1) *
        solve(Lines, 3, 1) *
        solve(Lines, 5, 1) *
        solve(Lines, 7, 1) *
        solve(Lines, 1, 2).

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [ "..##.........##.........##.........##.........##.........##.......",
          "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
          ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
          "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
          ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
          "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
          ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
          ".#........#.#........#.#........#.#........#.#........#.#........#",
          "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
          "#...##....##...##....##...##....##...##....##...##....##...##....#",
          ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"
        ],
    [ ?_assertEqual(7, part1(L))
    , ?_assertEqual(336, part2(L))
    , ?_assertEqual({167, 736527114}, ?solve())
    ].
