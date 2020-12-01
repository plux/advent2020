-module(aoc).

-export([int/1]).
-export([ints/1]).
-export([words/1]).
-export([lines/1]).
-export([solve/1]).
-export([solve/2]).

-spec int(list()) -> integer().
int(L) ->
    list_to_integer(L).

-spec words(list()) -> [list()].
words(L) ->
    string:lexemes(L, " \t").

-spec ints(list()) -> [integer()].
ints(L) ->
    [int(X) || X <- words(L)].

-spec lines(list()) -> [list()].
lines(L) ->
    string:lexemes(L, "\n").

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

-ifndef(TEST).
-include_lib("eunit/include/eunit.hrl").
aoc_test_() ->
    [ ?_assertEqual(123, int("123"))
    , ?_assertEqual([123, 456], ints("123 456"))
    , ?_assertEqual(["foo", "bar"], words("foo bar"))
    , ?_assertEqual(["foo", "bar"], lines("foo\nbar"))
    ].
-endif.
