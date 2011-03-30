-module(remote_opaque).

-export([test/0]).

-opaque my_opaque() :: {erl_types:erl_type()}.

-spec test() -> my_opaque().
test() -> foo().

-spec foo() -> my_opaque().
foo() -> {erl_types:t_atom(foo)}.
