-module(day8).
-compile([export_all]).
-include("aoc.hrl").

-record(cpu, { ops = []
             , pc = 0
             , visited = []
             , acc = 0
             , num_ops = 0
             }).

solve(Input) ->
    {part1(?lines(Input)), part2(?lines(Input))}.

part1(Lines) ->
    {cycle, #cpu{acc = Acc}} = run(cpu(parse_ops(Lines))),
    Acc.

part2(Lines) ->
    {ok, #cpu{acc = Acc}} = switch_ops(0, cpu(parse_ops(Lines))),
    Acc.

switch_ops(N, CPU0) ->
    case run(switch_op(N, CPU0)) of
        {ok, CPU}  -> {ok, CPU};
        {cycle, _} -> switch_ops(N+1, CPU0)
    end.

switch_op(N, #cpu{ops = Ops} = CPU) ->
    CPU#cpu{ops = array:set(N, switch_op(array:get(N, Ops)), Ops)}.

switch_op({nop, N}) -> {jmp, N};
switch_op({jmp, N}) -> {nop, N};
switch_op(Op)       -> Op.

cpu(Ops) ->
    #cpu{ops = array:from_list(Ops), num_ops = length(Ops)}.

run(#cpu{pc = PC, num_ops = NumOps} = CPU) when PC >= NumOps ->
    {ok, CPU};
run(#cpu{pc = PC, visited = Visited, ops = Ops} = CPU) ->
    case lists:member(PC, Visited) of
        true  -> {cycle, CPU};
        false -> run(exec(array:get(PC, Ops), CPU#cpu{visited = [PC|Visited]}))
    end.

exec({nop, _}, #cpu{pc = PC} = CPU) ->
    CPU#cpu{pc = PC+1};
exec({acc, N}, #cpu{pc = PC, acc = Acc} = CPU) ->
    CPU#cpu{pc = PC+1, acc = Acc+N};
exec({jmp, N}, #cpu{pc = PC} = CPU) ->
    CPU#cpu{pc = PC+N}.

parse_ops(Lines) ->
    [parse_op(?words(Line)) || Line <- Lines].

parse_op([Op, N]) ->
    {list_to_atom(Op), ?int(N)}.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = [ "nop +0"
        , "acc +1"
        , "jmp +4"
        , "acc +3"
        , "jmp -3"
        , "acc -99"
        , "acc +1"
        , "jmp -4"
        , "acc +6"
        ],
    [ ?_assertEqual(5, part1(L))
    , ?_assertEqual(8, part2(L))
    , ?_assertEqual({1744, 1174}, ?solve())
    ].
