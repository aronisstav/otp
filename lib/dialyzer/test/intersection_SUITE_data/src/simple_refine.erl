-module(simple_refine).

-export([test1/0, test2/0]).

test1() ->
    a = foo(b).

test2() ->
    b = foo(a).

foo(X) -> X.
