-module(day14).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    Ops = [parse(Line) || Line <- Lines],
    solve(Ops, pad(""), #{}).

part2(Lines) ->
    Ops = [parse(Line) || Line <- Lines],
    solve_p2(Ops, pad(""), #{}).

solve([], _, Mem) ->
    lists:sum([list_to_integer(Value, 2) || Value <- maps:values(Mem)]);
solve([{mask, Mask}|Rest], _, Mem) ->
    solve(Rest, Mask, Mem);
solve([{mem, Address, Value}|Rest], Mask, Mem) ->
    solve(Rest, Mask, Mem#{Address => apply_mask(Mask, Value)}).

solve_p2([], _, Mem) ->
    lists:sum([list_to_integer(Value, 2) || Value <- maps:values(Mem)]);
solve_p2([{mask, Mask}|Rest], _, Mem) ->
    solve_p2(Rest, Mask, Mem);
solve_p2([{mem, Address, Value}|Rest], Mask, Mem0) ->
    Addresses = iter(apply_mask_p2(Mask, Address), [[]]),
    Mem = lists:foldl(fun(Adr, Acc) -> Acc#{Adr => Value} end, Mem0, Addresses),
    solve_p2(Rest, Mask, Mem).

iter([], Acc)     -> [lists:reverse(A) || A <- Acc];
iter([$X|T], Acc) -> iter(T, [[$1|A] || A <- Acc] ++ [[$0|A] || A <- Acc]);
iter([C|T], Acc)  -> iter(T, [[C|A] || A <- Acc]).

pad(Str) ->
    lists:flatten(string:pad(Str, 36, leading, $0)).

parse("mask = " ++ Mask) ->
    {mask, Mask};
parse("mem" ++ Str) ->
    [Address, Value] = string:lexemes(Str, "[] ="),
    {mem, pad(integer_to_list(?int(Address), 2)), pad(integer_to_list(?int(Value), 2))}.

apply_mask_p2([], [])          -> [];
apply_mask_p2([$0|T1], [C|T2]) -> [C|apply_mask_p2(T1, T2)];
apply_mask_p2([$1|T1], [_|T2]) -> [$1|apply_mask_p2(T1, T2)];
apply_mask_p2([$X|T1], [_|T2]) -> [$X|apply_mask_p2(T1, T2)].

apply_mask([], [])          -> [];
apply_mask([$0|T1], [_|T2]) -> [$0|apply_mask(T1, T2)];
apply_mask([$1|T1], [_|T2]) -> [$1|apply_mask(T1, T2)];
apply_mask([$X|T1], [C|T2]) -> [C|apply_mask(T1, T2)].

to_bin(0) -> [];
to_bin(Int) ->
    to_bin(Int bsr 1) ++ [(Int band 1) + $0].

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L1 = [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
         , "mem[8] = 11"
         , "mem[7] = 101"
         , "mem[8] = 0"
         ],
    L2 = [ "mask = 000000000000000000000000000000X1001X"
         , "mem[42] = 100"
         , "mask = 00000000000000000000000000000000X0XX"
         , "mem[26] = 1"
         ],
    [ ?_assertEqual(165, part1(L1))
    , ?_assertEqual(208, part2(L2))
    , ?_assertEqual({2346881602152,3885232834169}, ?solve())
    ].
