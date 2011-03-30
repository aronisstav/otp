-module(self_recursive_2).

-export([test/0]).

test() ->
    12 = test(4).

test(0) ->
    0;
test(N) when N < 26 ->
    2 + test(N-1).
