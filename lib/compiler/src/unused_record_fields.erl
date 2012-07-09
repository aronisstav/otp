%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose : 
%%

-module(unused_record_fields).

-export([module/2, format_error/1]).

-record(state, {
	  warnings       = []            :: list(),
	  module                         :: atom(),
	  records        = dict:new()    :: dict(),
	  function                       :: [{atom(), integer()}],
	  parameter_info = single_clause :: 'single_clause' | list(),
	  var_dict       = dict:new()    :: dict
	 }).

-record(var, {
	  first     = []    :: list(),
	  escapes   = false :: boolean(),
	  as_record = none  :: none | {one, atom(), [atom()]} | multiple
	 }).

%% -define(DEBUG, on).
-ifdef(DEBUG).
-define(debug(A, B), io:format(A, B)).
-else.
-define(debug(A, B), ok).
-endif.
-define(debug(A), ?debug(A, [])).

module(Code,File) ->
  {ok,Code,[{File,analyze_abstract(Code)}]}.

analyze_abstract(Abstract) ->
  analyze_abstract(Abstract, #state{}).

analyze_abstract([], #state{warnings = Warnings}) ->
  Warnings;
analyze_abstract([Form|Rest], State) ->
  case erl_syntax:type(Form) of
    function ->
      _Line    = erl_syntax:get_pos(Form),
      Name    = erl_syntax:concrete(erl_syntax:function_name(Form)),
      Arity   = erl_syntax:function_arity(Form),
      Clauses = erl_syntax:function_clauses(Form),
      ?debug("\n\nL~w: ~w/~w\n",[_Line, Name, Arity]),
      NewState = analyze_function_clauses(Name, Arity, Clauses, State),
      analyze_abstract(Rest, NewState);
    attribute ->
      case erl_syntax:concrete(erl_syntax:attribute_name(Form)) of
	module ->
	  [Module] = [erl_syntax:concrete(Arg) ||
		       Arg <- erl_syntax:attribute_arguments(Form)],
	  ?debug("Module: ~w\n",[Module]),
	  analyze_abstract(Rest, State#state{module = Module});
	record ->
	  [TName, TFields] = erl_syntax:attribute_arguments(Form),
	  Name = erl_syntax:data(TName),
	  Fields =
	    [erl_syntax:concrete(FName) ||
	      FName <-
		[erl_syntax:record_field_name(TTField) ||
		  TTField <- erl_syntax:tuple_elements(TFields)
		]
	    ],
	  ?debug("Record: ~p, ~p\n",[Name, Fields]),
	  FieldsSet = ordsets:from_list(Fields),
	  NewRecords = dict:store(Name, FieldsSet, State#state.records),
	  analyze_abstract(Rest, State#state{records = NewRecords});
	_Other ->
	  ?debug("Ignore attribute ~w: ~w\n",[_Other, Form]),
	  analyze_abstract(Rest, State)
      end;
    _Other ->
      ?debug("Ignore ~w: ~w\n",[_Other, Form]),
      analyze_abstract(Rest, State)
  end.

analyze_function_clauses(Name, Arity, [Clause], State) ->
  OldFunction = State#state.function,
  NewState =
    State#state{function = [{Name,Arity}|OldFunction],
		parameter_info = single_clause},
  NewState1 = analyze_clause(Clause, NewState, true),
  NewState1#state{function = OldFunction};
analyze_function_clauses(Name, Arity, Clauses, State) ->
  OldFunction = State#state.function,
  ParameterInfo = lists:duplicate(Arity, []),
  NewState =
    State#state{function = [{Name,Arity}|OldFunction],
		parameter_info = ParameterInfo},
  NewState1 = analyze_clauses(Clauses, NewState),
  NewState1#state{function = OldFunction}.

analyze_clauses([], #state{var_dict = _VarDict} = State) ->
  ?debug("Clauses done: ~p\n",[dict:to_list(_VarDict)]),
  State;
analyze_clauses([Clause|Rest], State) ->
  ?debug("Clause:\n"),
  NewState = analyze_clause(Clause, State, false),
  ?debug("Clause done: ~p\n",[dict:to_list(NewState#state.var_dict)]),
  analyze_clauses(Rest, NewState).
  
analyze_clause(Clause, State = #state{parameter_info = single_clause},
	       CheckScope) ->
  Patterns = erl_syntax:clause_patterns(Clause),
  Guard    = erl_syntax:clause_guard(Clause),
  Body     = erl_syntax:clause_body(Clause),
  ?debug("\nPatterns:\n~p\n",[Patterns]),
  ?debug("Guard:\n~p\n",[Guard]),
  ?debug("Map: ~p\n",[dict:to_list(State#state.var_dict)]),
  case is_simple_clause(Patterns) of
    true ->
      OldVarDict = State#state.var_dict,
      UnusedVarDict = mark_unused(OldVarDict),
      NewState0 =
	traverse(Patterns, State#state{var_dict = UnusedVarDict}, false),
      NewState1 = 
	case Guard of
	  none -> NewState0;
	  _Other -> traverse([Guard], NewState0, true)
	end,
      NewState2 = traverse(Body, NewState1, body),
      NewState3 =
	case CheckScope of
	  true -> check_scope(NewState2);
	  false -> NewState2
	end,
      NewVarDict = NewState3#state.var_dict,
      UpdatedVarDict = update_defs(OldVarDict, NewVarDict),
      ?debug("Updated: ~p\n",[dict:to_list(UpdatedVarDict)]),
      NewState3#state{var_dict = UpdatedVarDict};
    false ->
      State
  end;
analyze_clause(_Clause, State, _CheckScope) ->
  State.

is_simple_clause(Patterns) ->
  Pred =
    fun(Term) ->
	erl_syntax:is_leaf(Term) orelse erl_syntax:is_literal(Term)
    end,
  {_LeafOrLiteral, Others} = lists:partition(Pred, Patterns),
  Others =:= [].

mark_unused(Dict) ->
  Map = fun(_Key, Val) -> #var{first = Val#var.first} end,
  dict:map(Map, Dict).

traverse([_Stmt] = Single, State, body) ->
  ?debug("Next statement is last in list and will be considered escaping!\n"),
  traverse(Single, State, true);
traverse([Stmt|Rest], #state{var_dict = VarDict} = State, Escaping) ->
  ?debug("\nStmt:\n~p\n",[Stmt]),
  ?debug("Escaping: ~p\n", [Escaping]),
  ?debug("Map: ~p\n",[dict:to_list(VarDict)]),
  Pos = erl_syntax:get_pos(Stmt),
  NewState =
    case erl_syntax:type(Stmt) of
      application ->
	Args = erl_syntax:application_arguments(Stmt),
	traverse(Args, State, true);
      case_expr ->
	Arg = erl_syntax:case_expr_argument(Stmt),
	Clauses = erl_syntax:case_expr_clauses(Stmt),
	?debug("Arg: ~p\nClauses: ~p\n", [Arg, Clauses]),
	NewState0 = traverse([Arg], State, true),
	analyze_clauses(Clauses, NewState0);
      conjunction ->
	Body = erl_syntax:conjunction_body(Stmt),
	traverse(Body, State, true);
      disjunction ->
	Body = erl_syntax:disjunction_body(Stmt),
	traverse(Body, State, true);
      fun_expr ->
	Clauses = erl_syntax:fun_expr_clauses(Stmt),
	Arity = erl_syntax:fun_expr_arity(Stmt),
	analyze_function_clauses({anon, Pos}, Arity, Clauses, State);
      if_expr ->
	Clauses = erl_syntax:if_expr_clauses(Stmt),
	analyze_clauses(Clauses, State);
      infix_expr ->
	Left = erl_syntax:infix_expr_left(Stmt),
	Right = erl_syntax:infix_expr_right(Stmt),
	traverse([Left, Right], State, true);
      list ->
	Head = erl_syntax:list_head(Stmt),
	Tail = erl_syntax:list_tail(Stmt),
	traverse([Head, Tail], State, true);
      match_expr ->
	Pattern = erl_syntax:match_expr_pattern(Stmt),
	Body = erl_syntax:match_expr_body(Stmt),
	?debug("Pattern: ~p\nBody: ~p\n",[Pattern, Body]),
	NewState0 = traverse([Pattern], State, true),
	traverse([Body], NewState0, body);
      record_access ->
	Arg = erl_syntax:variable_name(erl_syntax:record_access_argument(Stmt)),
	Field = erl_syntax:concrete(erl_syntax:record_access_field(Stmt)),
	Record = erl_syntax:data(erl_syntax:record_access_type(Stmt)),
	?debug("Arg: ~p\nField: ~p\nType: ~p\n",
	       [Arg, Field, Record]),
	NewAsRecord = {Record, [Field]},
	NewVar =
	  case dict:find(Arg, VarDict) of
	    {ok, Value} ->
	      ?debug("Lookup value: ~p\n", [Value]),
	      MergedAsRecord =
		merge_as_record(Value#var.as_record, NewAsRecord),
	      Value#var{as_record = MergedAsRecord};
	    error ->
	      ?debug("Lookup error\n"),
	      #var{as_record = NewAsRecord}
	  end,
	State#state{var_dict = dict:store(Arg, NewVar, VarDict)};
      record_expr ->
	Arg = erl_syntax:record_expr_argument(Stmt),
	_Type = erl_syntax:record_expr_type(Stmt),
	Fields = erl_syntax:record_expr_fields(Stmt),
	?debug("Arg: ~p\nType: ~p\nFields: ~p\n",[Arg, _Type, Fields]),
	NewState0 =
	  case Arg of
	    none -> State;
	    Other -> traverse([Other], State, true)
	  end,
	traverse(Fields, NewState0, true);
      record_field ->
	Value = erl_syntax:record_field_value(Stmt),
	traverse([Value], State, true);
      tuple ->
	Elements = erl_syntax:tuple_elements(Stmt),
	traverse(Elements, State, true);
      variable ->
	Name = erl_syntax:variable_name(Stmt),
	?debug("Name: ~p ",[Name]),
	NewValue0 =
	  case dict:find(Name, VarDict) of
	    {ok, Value} -> Value;
	    error -> #var{first = [Pos]}
	  end,
	NewValue =
	  case Escaping of
	    true -> NewValue0#var{escapes = true};
	    _Other -> NewValue0
	  end,
	NewVarDict = dict:store(Name, NewValue, State#state.var_dict),
	State#state{var_dict = NewVarDict};
      _Other ->
	?debug("Type: ~p\n",[_Other]),
	State
    end,
  traverse(Rest, NewState, Escaping);
traverse([], State, _Escaping) ->
  State.

merge_as_record(none, Value2) ->
  Value2;
merge_as_record(Value1, none) ->
  Value1;
merge_as_record({Record, Fields1}, {Record, Fields2}) ->
  {Record, ordsets:union(Fields1, Fields2)};
merge_as_record(_Value1, _Value2) ->
  multiple.

update_defs(OldVarDict, NewVarDict) ->
  Map =
    fun(Key, Value) ->
	NewValue = dict:fetch(Key, NewVarDict),
	OldAsRecord = Value#var.as_record,
	NewAsRecord = NewValue#var.as_record,
	?debug("MERGE:\n~p\n~p\n",[OldAsRecord, NewAsRecord]),
	Value#var{as_record = merge_as_record(OldAsRecord, NewAsRecord)}
    end,
  dict:map(Map, OldVarDict).

check_scope(#state{var_dict = VarDict, records = Records,
		    function = [Fun|_]} = State) ->
  ?debug("End of scope!\n"),
  Fold =
    fun(Key, Value, Acc) ->
	?debug("Var: ~p\nValue: ~p\n",[Key, Value]),
	case Value#var.as_record of
	  none -> Acc;
	  multiple -> Acc;
	  {Record, Fields} ->
	    case Value#var.escapes of
	      true -> Acc;
	      false ->
		AllFields = dict:fetch(Record, Records),
		case ordsets:subtract(AllFields, Fields) of
		  [] -> Acc;
		  Missing ->
		    ?debug("MISSING!\nVar: ~p\nFields: ~p\n",[Value, Missing]),
		    [Line] = Value#var.first,
		    [{Line, ?MODULE, {Fun, Key, Record, Missing}}|Acc]
		end
	    end
	end
    end,
  Warns = dict:fold(Fold, [], VarDict),
  State#state{warnings = Warns ++ State#state.warnings}.
		  
format_error({Fun, Var, Record, Missing}) ->
  {Name, Arity} = Fun,
  Id =
    case Name of
      {anon, Line} ->
	io_lib:format("an anonymous function defined at line ~p",[Line]);
      Other        ->
	io_lib:format("~p/~p",[Other, Arity])
    end,
  Pattern =
    "Within the scope of ~s, the variable ~p is being used only as"
    " a value of record #~p but the fields ~w are never being used.",
  io_lib:format(Pattern, [Id, Var, Record, Missing]).
  
