%%--------------------------------------------------------------------
%% Program which produced a warning stating that the specification of
%% function add/2 is invalid. The problem appears related to recursive
%% opaque types. Perhaps surprisingly, the warning is not generated if
%% the spec contains types of the form `invalid_spec_bug:foo()'
%% instead of `foo()'.
%%--------------------------------------------------------------------
-module(invalid_spec_bug).

-export([add/2]).

-export_type([foo/0]).

-opaque foo() :: {'foo', [bar()]}.

-type fi()  :: foo() | integer().
-type bar() :: {'bar', fi()}.

-spec add(fi(), foo()) -> foo().
add(Value, {'foo', Props}) ->
    {'foo', [{'bar', Value} | Props]}.
