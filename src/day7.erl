-module(day7).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    BagsMap = parse(Lines),
    length([B || B <- maps:keys(BagsMap), contains_shiny(B, BagsMap)]).

contains_shiny(Bag, BagsMap) ->
    {Bags, _} = lists:unzip(maps:get(Bag, BagsMap)),
    lists:member("shiny gold", Bags) orelse
        lists:any(fun(B) -> contains_shiny(B, BagsMap) end, Bags).

part2(Lines) ->
    count({"shiny gold", 1}, parse(Lines)) - 1.

count({Bag, Amount}, BagsMap) ->
    Amount + Amount*lists:sum([count(BA, BagsMap) || BA <- maps:get(Bag, BagsMap)]).

parse(Lines) ->
    maps:from_list([parse_line(Line) || Line <- Lines]).

parse_line(Line) ->
    [Bag, Rest] = ?split(Line, " bags contain "),
    {Bag, [parse_bag(?words(B)) || B <- ?split(Rest, ","), B =/= "no other bags."]}.

parse_bag([Amount, Adjective, Color, _]) ->
    {Adjective ++ " " ++ Color, ?int(Amount)}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = ["light red bags contain 1 bright white bag, 2 muted yellow bags.",
         "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
         "bright white bags contain 1 shiny gold bag.",
         "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
         "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
         "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
         "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
         "faded blue bags contain no other bags.",
         "dotted black bags contain no other bags."
        ],
    [ ?_assertEqual(4, part1(L))
    , ?_assertEqual(32, part2(L))
    , ?_assertEqual( {"light red", [{"bright white", 1}, {"muted yellow", 2}]}
                   , parse_line(hd(L)))
    , ?_assertEqual({121, 3805}, ?solve())
    ].
