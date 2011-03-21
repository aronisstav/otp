-module(sev_use).

-export([test/0]).

test() ->
    sev_decl:check({foo,bar}),
    ok.
