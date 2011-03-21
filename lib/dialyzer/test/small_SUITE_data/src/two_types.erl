-module(two_types).

-export([foo/0]).

-type bar(X) :: list(X).
-type bar()  :: bar(any()).

foo() -> 42.
