-module(cases1).

-export([foo/1, test/0]).

foo(X) ->
    case X of
	a -> 1;
	b -> 2
    end.

test() ->
    2 = foo(a).
