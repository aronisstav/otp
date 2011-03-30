-module(opaque_infinite_1).

-export([f1/1, f2/0]).

-opaque o() :: tuple().

-spec f1(o()) -> ok.
f1(_) -> ok.

f2() -> list_to_tuple(get(foo)).
