-module(opaque_tuple).

-export([foo/1]).

-opaque t() :: tuple().

foo({_} = X) -> bar(X).

-spec bar(t()) -> 42.

bar({_}) -> 42.

