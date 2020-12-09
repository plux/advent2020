-module(day9).
-compile([export_all]).
-include("aoc.hrl").


solve(Input) ->
    {part1(?ints(Input)), part2(?ints(Input))}.

part1(Ints) ->
    first_valid(25, Ints).

first_valid(Size, Ints) ->
    {Preamble, [N|_]} = lists:split(Size, Ints),
    Sums = [X + Y || X <- Preamble, Y <- Preamble],
    case lists:member(N, Sums) of
        false -> N;
        true  -> first_valid(Size, tl(Ints))
    end.

part2(Ints) ->
    Valid = first_valid(25, Ints),
    find_weakness(array:from_list(Ints), Valid, 0, 0, [], 0).

find_weakness(Nums, Num, Start, I, Set0, Sum0) ->
    Elem = array:get(I, Nums),
    Sum = Elem + Sum0,
    Set = [Elem|Set0],
    if Sum == Num -> lists:min(Set) + lists:max(Set);
       Sum >  Num -> find_weakness(Nums, Num, Start+1, Start+1, [], 0);
       Sum <  Num -> find_weakness(Nums, Num, Start, I+1, Set, Sum)
    end.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [35,
         20,
         15,
         25,
         47,
         40,
         62,
         55,
         65,
         95,
         102,
         117,
         150,
         182,
         127,
         219,
         299,
         277,
         309,
         576],
    [ ?_assertEqual(127, first_valid(5, L))
    , ?_assertEqual({18272118, 2186361}, ?solve())
    ].
