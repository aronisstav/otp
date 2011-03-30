-module(opaque_infinite_2).

-export([f4/0]).

f3(O) ->
    case random() of
	1 -> opaque_infinite_1:f1(O);
	2 -> ok
    end.

f4() ->
    f3(opaque_infinite_1:f2()).

random() ->
    get(42).
