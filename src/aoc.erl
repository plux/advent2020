-module(aoc).

-compile([export_all]).

-type str() :: list().

%% Integers ------------------------------------------------------------
-spec int(str()) -> integer().
int(N) when is_integer(N) -> N;
int(L)                    -> list_to_integer(string:trim(L)).

-spec ints(str()) -> [integer()].
ints(L) ->
    [int(X) || X <- words(L), is_int(X)].

-spec is_int(_) -> boolean().
is_int(X) ->
    is_integer(catch aoc:int(X)).

%% Floats --------------------------------------------------------------
float(L) ->
    list_to_float(string:trim(L)).

-spec floats(str()) -> [float()].
floats(L) ->
    [aoc:float(X) || X <- words(L), aoc:is_float(X)].

-spec is_float(_) -> boolean().
is_float(X) ->
    erlang:is_float(catch aoc:float(X)).

%% Splitting -----------------------------------------------------------
-spec words(str()) -> [str()].
words(L) ->
    string:lexemes(L, " \t\n").

-spec lines(str()) -> [str()].
lines(L) ->
    string:lexemes(L, "\n").

-spec split(str(), str()) -> [str()].
split(Input, Sep) ->
    case string:split(Input, Sep) of
        [Input]       -> [Input];
        [Match, Rest] -> [Match|split(Rest, Sep)]
    end.

%% Grid
-spec grid([str()]) -> map().
grid(L) ->
    maps:from_list([{{X, Y}, C} || {Y, Line} <- enumerate(L),
                                   {X, C} <- enumerate(Line)]).

-spec enumerate([X]) -> [{integer(), X}].
enumerate(L) ->
    lists:zip(lists:seq(0, length(L)-1), L).


cartesian([H|T]) -> [[A|B] || A <- H, B <- cartesian(T)];
cartesian([])    -> [[]].
%% Solve ---------------------------------------------------------------
solve(Mod) when is_atom(Mod) ->
    {ok, Input} = file:read_file("input/" ++ atom_to_list(Mod) ++ ".txt"),
    {T, Answer} = timer:tc(fun() -> Mod:solve(string:chomp(binary_to_list(Input))) end),
    io:format("~s: ~p (~p ms)\n",
              [Mod, Answer, trunc(math:ceil(T / 1000))]),
    Answer;
solve(Files) ->
    lists:map(
      fun(F) ->
              solve(list_to_atom(hd(string:tokens(F, "."))))
      end, lists:sort(Files)).

solve(Mod, File) ->
    {ok, Input} = file:read_file(File),
    Mod:solve(string:chomp(binary_to_list(Input))).


%% Tests ---------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
aoc_test_() ->
    [ ?_assertEqual(123,            aoc:int("123"))
    , ?_assertEqual([123, 456],     aoc:ints("123 bapa \n456 apa"))
    , ?_assertEqual(false,          aoc:is_int("1.0"))
    , ?_assertEqual(true,           aoc:is_int("1"))
    , ?_assertEqual(["foo", "bar"], aoc:words("foo \n    bar"))
    , ?_assertEqual(["foo", "bar"], aoc:lines("foo\nbar"))
    , ?_assertEqual(1.0,            aoc:float("1.0"))
    , ?_assertEqual([1.5, 8.9],     aoc:floats("1.5 \n6 8.9 apa"))
    , ?_assertEqual(true,           aoc:is_float("1.0"))
    , ?_assertEqual(false,          aoc:is_float("1"))
    ].
