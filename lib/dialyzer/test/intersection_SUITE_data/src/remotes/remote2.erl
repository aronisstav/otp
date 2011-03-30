-module(remote2).

-export([foo/0, bar/0]).

-spec foo() -> remote1:foo_ret().
foo() -> remote1:foo(a).

-spec bar() -> remote1:bar_ret().
bar() -> remote1:bar(a).
