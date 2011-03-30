-module(specs).

-export([foo/1, bar/1, baz/0]).

-spec foo(0) -> 1; (1) -> 2; (2) -> 3; (3) -> 4.

foo(X) -> X + 1.

bar(X) ->
    foo(X).

baz() ->
    3 = bar(0).
