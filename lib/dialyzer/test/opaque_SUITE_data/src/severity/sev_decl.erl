-module(sev_decl).

-export([new/0, check/1]).

-opaque t() :: {atom(), atom()}.

-spec new() -> t().
new() -> {foo, bar}.

-spec check(t()) -> ok.
check({foo,bar}) -> ok.
