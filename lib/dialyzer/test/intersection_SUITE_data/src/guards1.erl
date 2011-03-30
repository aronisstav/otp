-module(guards1).

-export([foo/1, test/0]).

foo(X) when X=:= a -> 1;
foo(X) when X=:= b -> 2.

test() ->
    2 = foo(a).
