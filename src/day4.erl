-module(day4).
-compile([export_all]).
-include("aoc.hrl").

solve(Input) ->
    {part1(passports(Input)), part2(passports(Input))}.

part1(Passports) ->
    length([P || P <- Passports, 8 =:= maps:size(P)]).

part2(Passports) ->
    length([P || P <- Passports, is_valid(P)]).

passports(Input) ->
    [passport(["cid:ignore"|?words(Chunk)]) || Chunk <- ?split(Input, "\n\n")].

passport(Words) ->
    maps:from_list([list_to_tuple(?split(W, ":")) || W <- Words]).

is_valid(Passport) ->
    8 =:= maps:size(maps:filter(fun is_valid/2, Passport)).

%% byr (Birth Year) - four digits; at least 1920 and at most 2002.
is_valid("byr", Value) -> in_range(Value, 1920, 2002);
%% iyr (Issue Year) - four digits; at least 2010 and at most 2020.
is_valid("iyr", Value) -> in_range(Value, 2010, 2020);
%% eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
is_valid("eyr", Value) -> in_range(Value, 2020, 2030);
%% hgt (Height) - a number followed by either cm or in:
%%     If cm, the number must be at least 150 and at most 193.
%%     If in, the number must be at least 59 and at most 76.
is_valid("hgt", [X, Y, Z, $c, $m]) -> in_range([X, Y, Z], 150, 193);
is_valid("hgt", [X, Y, $i, $n])    -> in_range([X, Y], 59, 76);
%% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
is_valid("hcl", [$#|Value]) when length(Value) =:= 6 ->
    is_integer(catch list_to_integer(Value, 16));
%% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
is_valid("ecl", Value) ->
    lists:member(Value, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]);
%% pid (Passport ID) - a nine-digit number, including leading zeroes.
is_valid("pid", Value) when length(Value) =:= 9 ->
    lists:all(fun(C) -> in_range(C, $0, $9) end, Value);
%% cid (Country ID) - ignored, missing or not.
is_valid("cid", _) -> true;
is_valid(_, _) -> false.

in_range(Value, X, Y) ->
    X =< ?int(Value) andalso ?int(Value) =< Y.

-include_lib("eunit/include/eunit.hrl").
solve_test_() ->
    L = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n"
        "byr:1937 iyr:2017 cid:147 hgt:183cm\n"
        "\n"
        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n"
        "hcl:#cfa07d byr:1929\n"
        "\n"
        "hcl:#ae17e1 iyr:2013\n"
        "eyr:2024\n"
        "ecl:brn pid:760753108 byr:1931\n"
        "hgt:179cm\n"
        "\n"
        "hcl:#cfa07d eyr:2025 pid:166559648\n"
        "iyr:2011 ecl:brn hgt:59in",
    [ ?_assertEqual(2, part1(passports(L)))
    , ?_assertEqual({216, 150}, ?solve())
    ].
