-module(clauses1).

-export([foo/1, bar/0]).

foo(a) -> 1;
foo(b) -> 2.

bar() ->
    2 = foo(a).
