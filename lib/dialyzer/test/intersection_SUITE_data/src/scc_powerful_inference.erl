-module(scc_powerful_inference).

-export([test/0, foo/1]).

-define(BASE, 1).
-define(LIMIT, 16).

test() ->
    foo(16) == bar(16),
    foo(1) == foo(16),
    bar(4) == bar(15).

foo(1) ->
    foo;
foo(N) when N > ?BASE, N < ?LIMIT ->
    bar(N-1).

bar(?BASE) ->
    bar;
bar(N) when N > ?BASE, N < ?LIMIT ->
    foo(N-1).
