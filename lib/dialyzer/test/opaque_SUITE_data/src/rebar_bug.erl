%%---------------------------------------------------------------------
%% A stripped down version of a rebar module for which the analysis
%% gives an erroneous warning. Function erl_scan:tokens/3 takes either
%% a erl_scan:cont() :: erl_scan:return_cont() | [] in its first arg,
%% where erl_scan:return_cont() is opaque. The current analysis thinks
%% that one cannot call a function expecting some union of an opaque
%% and a structured term with an opaque term.
%%
%% The original version of the test appears commented out so that the
%% test case is smaller -- should change to its original version once
%% the bug is fixed.
%%---------------------------------------------------------------------
-module(rebar_bug).
%%-export([consult/1]).
-export([consult/3]).

%%consult(Str) when is_list(Str) ->
%%    consult([], Str, []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
%%                {ok, Tokens, _} ->
%%                    {ok, Term} = erl_parse:parse_term(Tokens),
%%                    consult([], Remaining, [Term | Acc]);
                {eof, _Other} ->
                    lists:reverse(Acc)
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.

