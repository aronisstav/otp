-module(bif_bug).

-export([test/1]).

-opaque my_int() :: integer().

test(X) -> foo(41).

-spec foo(my_int()) -> my_int().

foo(X) -> X + 1.
