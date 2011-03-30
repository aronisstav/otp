-module(types).

-export([out/0]).

-export_type([my_unit/0]).

-type my_unit() :: no_return().

-spec out() -> my_unit().
out() -> exit(1).
