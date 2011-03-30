-module(failing_calls).

-export([test1/0, test2/0, test3/0, foo/2, foo/3, bar/2]).

test1() ->
    foo(a, c).

test2() ->
    foo(a, c, e).

test3() ->
    bar(c, c).

foo(a, d) ->
    5;
foo(b, c) ->
    10.

foo(a, d, e) ->
    5;
foo(b, c, e) ->
    5;
foo(a, c, f) ->
    5.

bar(a, c) ->
    5;
bar(b, c) ->
    10.
