-module(remote1).

-export([foo/1, bar/1, test/0]).

-export_type([foo_ret/0, bar_ret/0]).

-type foo_ret() :: 'a'|'b'.
-type bar_ret() :: 'a'|'b'|'c'.
-type generic() :: atom()|integer()|string()|float().

-opaque opaque_generic() :: atom()|integer()|string()|float().

-spec foo(atom()) -> foo_ret().
foo(a) -> a;
foo(b) -> b.

-spec bar(atom()) -> bar_ret().
bar(X) -> foo(X).

test() ->
    gen1(),
    gen2(),
    gen3(),
    ogen1(),
    ogen2(),
    ogen3(),
    ok.

-spec gen1() -> generic().
gen1() -> foo.

-spec gen2() -> generic().
gen2() -> 42.

-spec gen3() -> generic().
gen3() -> "Hello world".

-spec ogen1() -> opaque_generic().
ogen1() -> foo.

-spec ogen2() -> opaque_generic().
ogen2() -> 42.

-spec ogen3() -> opaque_generic().
ogen3() -> "Hello world".
