-module(clauses2).

-export([foo/1, bar/0]).

foo(a) -> b;
foo(X) -> X.

bar() ->
    a = foo(a).
