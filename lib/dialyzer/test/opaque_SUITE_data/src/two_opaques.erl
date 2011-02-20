-module(two_opaques).

-export([foo/2, bar/2]).

-opaque one() :: integer().
-opaque two() :: float().

foo(X, Y) when is_integer(Y)->
    bar(X, Y).

-spec bar(integer, one()) -> ok;
	 (  float, two()) -> ok.

bar(_, _) -> ok.
    
