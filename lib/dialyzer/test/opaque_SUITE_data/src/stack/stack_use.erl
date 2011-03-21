-module(stack_use).

-export([wrong1/0, wrong2/0]).

wrong1() ->
    length(stack:new()).

wrong2() ->
    case stack:new() of
	X when is_list(X) -> break;
	X when not is_list(X) -> break;
	X when length(X) > 0 -> break;
	X when not length(X) > 0 -> break
    end.
