-define(lines(X), aoc:lines(X)).
-define(words(X), aoc:words(X)).
-define(int(X), aoc:int(X)).
-define(ints(X), aoc:ints(X)).
-define(solve(), aoc:solve(?MODULE)).
-define(words_lines(X), [?words(Line) || Line <- ?lines(X)]).
-define(ints_lines(X), [?ints(Line) || Line <- ?lines(X)]).
-define(int_lines(X), [?int(Line) || Line <- ?lines(X)]).