-module(self_recursive).

-export([test/1, test0/0, test1/0]).

test0() ->
    [_] = test([]).

test1() ->
    [] = test([a]).

test(X) ->
    test(X, []).

test([], []) ->
    [];
test([], A) ->
    A;
test([H|T], A) ->
    test(T, [H|A]).
