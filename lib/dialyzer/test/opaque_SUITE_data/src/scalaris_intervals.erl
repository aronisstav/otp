%%----------------------------------------------------------------------------
%% Stripped down version of src/intervals.erl from the Scalaris code base.
%% In OTP's 'dev' version (of R14B02), it crashes as follows:
%%
%%   Analysis failed with error:
%%     {function_clause,[{erl_types,t_list_elements,1},
%%     {erl_bif_types,'-type/4-anonymous-114-',1},
%%     {dialyzer_typesig,solve_one_c,3},
%%     ...
%%   Last messages in the log cache:
%%     Typesig analysis for SCC: [{intervals,union,2}]
%%     ...
%%
%% The crash is related to the definition of interval() as opaque.
%% (With Stavros' fixes on 'hipe-fixes' it does not crash but it gives
%%  bogus warnings.)
%%----------------------------------------------------------------------------

-module(scalaris_intervals).

-vsn('$Id: scalaris_intervals.erl,v 1.1 2011/02/07 13:30:51 kostis Exp $').

-define(RT, rt_chord).
-define(MINUS_INFINITY, 0).
-define(PLUS_INFINITY, 16#100000000000000000000000000000000).

-export([empty/0, new/1, new/4, all/0,
         is_empty/1, is_all/1, is_subset/2, is_continuous/1,
         is_adjacent/2, in/2, is_left_of/2, is_right_of/2,
         intersection/2, union/2, minus/2
        ]).

-export_type([interval/0, key/0, left_bracket/0, right_bracket/0]).

-type left_bracket() :: '(' | '['.
-type right_bracket() :: ')' | ']'.
-type key() :: ?RT:key() | ?MINUS_INFINITY | ?PLUS_INFINITY.
-type simple_interval() :: {element, key()} | {interval, left_bracket(), key(), key(), right_bracket()} | all.
-opaque interval() :: [simple_interval()].

%% @doc Creates an empty interval.
-spec empty() -> interval().
empty() -> [].

%% @doc Creates an interval covering the whole key space.
-spec all() -> interval().
all() -> [all].

%% @doc Creates an interval covering a single element.
-spec new(key()) -> interval().
new(X) -> [{element, X}].

%% @doc Creates a new interval depending on the given brackets, i.e.:
%%      - closed interval [A, B],
%%      - half-open interval (A, B], aka ]A, B]
%%      - half-open interval [A, B), aka [A, B[
%%      - open interval (A, B), aka ]A, B[
%%      The new interval may wrap around, e.g. if A > B.
%%      If '[A,A]' is given, an interval with the element A is created.
%%      The special cases '(A,A)', '[A,A)', '(A,A]' and
%%      '(?PLUS_INFINITY,?MINUS_INFINITY,)' translate to an empty interval.
%%      '[?MINUS_INFINITY,?PLUS_INFINITY]' translates to 'all'.
-spec new(LeftBr::left_bracket(), A::key(), B::key(), RightBr::right_bracket()) -> interval().
new(LeftBr, Begin, End, RightBr) ->
    normalize_simple({interval, LeftBr, Begin, End, RightBr}).

%% @doc Checks whether the given interval is empty.
-spec is_empty(interval()) -> boolean().
is_empty([]) -> true;
is_empty(_) ->  false.

%% @doc Checks whether the given interval is covering everything.
-spec is_all(interval()) -> boolean().
is_all([all]) -> true;
is_all(_) ->  false.

%% @doc Creates the intersection of two intervals.
%%      Precondition: is_well_formed(A) andalso is_well_formed(B).
-spec intersection(A::interval(), B::interval()) -> interval().
intersection([all], B)  -> B;
intersection(A, [all])  -> A;
intersection([] = A, _) -> A;
intersection(_, [] = B) -> B;
intersection([{element, _}] = A, B) -> intersection_element(A, B);
intersection(A, [{element, _}] = B) -> intersection_element(B, A);
intersection(A, A) -> A;
intersection(A, B) ->
    normalize_internal([intersection_simple(IA, IB)
                          || IA <- A, IB <- B,
                             intersection_simple(IA, IB) =/= []]).

%% @doc Intersection between an element and an interval (removes unnecessary
%%      code duplication from intersection/2).
-spec intersection_element(A::[{element, key()}], B::interval()) -> interval().
intersection_element([{element, X}] = A, B) ->
    case in(X, B) of
        true  -> A;
        false -> empty()
    end.

%% @doc Creates the intersection of two simple intervals or empty lists.
-spec intersection_simple(A::simple_interval() | [], B::simple_interval() | []) -> simple_interval() | [].
intersection_simple(all, B)    -> B;
intersection_simple(A, all)    -> A;
intersection_simple([] = A, _) -> A;
intersection_simple(_, [] = B) -> B;
intersection_simple(A, A)      -> A;
intersection_simple({element, _} = A, B) -> intersection_simple_element(A, B);
intersection_simple(A, {element, _} = B) -> intersection_simple_element(B, A);
intersection_simple({interval, A0Br, A0, A1, A1Br},
                    {interval, B0Br, B0, B1, B1Br}) ->
    B0_in_A = is_between(A0Br, A0, B0, A1, A1Br),
    B1_in_A = is_between(A0Br, A0, B1, A1, A1Br),
    A0_in_B = is_between(B0Br, B0, A0, B1, B1Br),
    A1_in_B = is_between(B0Br, B0, A1, B1, B1Br),
    if
        B0_in_A orelse B1_in_A orelse A0_in_B orelse A1_in_B ->
            {NewLeft, NewLeftBr} =
                case A0 =:= B0 of
                    true when A0Br =:= '(' -> {A0, '('};
                    true -> {A0, B0Br};
                    false ->
                        case util_max(A0, B0) =:= A0 of
                            true  -> {A0, A0Br};
                            false -> {B0, B0Br}
                        end
                end,
            {NewRight, NewRightBr} =
                case A1 =:= B1 of
                    true when A1Br =:= ')' -> {A1, ')'};
                    true -> {A1, B1Br};
                    false ->
                        case util_min(A1, B1) =:= A1 of
                            true  -> {A1, A1Br};
                            false -> {B1, B1Br}
                        end
                end,
            % note: if left and right are the same in a closed interval, this
            % means 'single element' here, in (half) open intervals, the result
            % is an empty interval
            case NewLeft =:= NewRight of
                true when (NewLeftBr =:= '[') andalso (NewRightBr =:= ']') ->
                    {element, NewLeft};
                true -> [];
                false -> {interval, NewLeftBr, NewLeft, NewRight, NewRightBr}
            end;
        true -> []
    end.

%% @doc Intersection between an element and a simple interval (removes
%%      unnecessary code duplication from intersection_simple/2).
-spec intersection_simple_element(A::{element, key()}, B::simple_interval()) -> {element, key()} | [].
intersection_simple_element({element, X} = A, B) ->
    case in_simple(X, B) of
        true  -> A;
        false -> []
    end.

%% @doc Returns true if A is a subset of B, i.e. the intersection of both is A.
%%      Precondition: is_well_formed(A) andalso is_well_formed(B).
-spec(is_subset(A::interval(), B::interval()) -> boolean()).
is_subset(A, B) -> A =:= intersection(A, B).

%% @doc X \in I. Precondition: is_well_formed(I).
-spec in_simple(X::key(), I::simple_interval()) -> boolean().
in_simple(X, {interval, FirstBr, First, Last, LastBr}) ->
    is_between(FirstBr, First, X, Last, LastBr);
in_simple(_, all)          -> true;
in_simple(X, {element, X}) -> true;
in_simple(_, {element, _}) -> false.

%% @doc X \in I. Precondition: is_well_formed(I).
-spec in(X::key(), I::interval()) -> boolean().
in(_, [])         -> false;
in(X, [I | Rest]) -> in_simple(X, I) orelse in(X, Rest).

-spec normalize_internal([simple_interval()]) -> interval(). % for dialyzer
normalize_internal(List) ->
    NormalizedList1 =
        lists:flatmap(fun(I) -> normalize_simple(I) end, List),
    merge_adjacent(lists:usort(fun interval_sort/2, NormalizedList1), []).

%% @doc Normalizes simple intervals (see normalize/1).
-spec normalize_simple(simple_interval()) -> [simple_interval()].
normalize_simple(all) -> [all];
normalize_simple({element, _A} = I) -> [I];
normalize_simple({interval, '(', X, X, _RightBr}) -> [];
normalize_simple({interval, _LeftBr, X, X, ')'}) -> [];
normalize_simple({interval, '[', X, X, ']'}) -> [{element, X}];
normalize_simple({interval, '[', ?MINUS_INFINITY, ?PLUS_INFINITY, ']'}) ->
    [all];
normalize_simple({interval, LeftBr, ?MINUS_INFINITY, ?PLUS_INFINITY, RightBr}) ->
    [{interval, LeftBr, ?MINUS_INFINITY, ?PLUS_INFINITY, RightBr}];
normalize_simple({interval, '(', ?PLUS_INFINITY, ?MINUS_INFINITY, ')'}) ->
    [];
normalize_simple({interval, '(', ?PLUS_INFINITY, ?MINUS_INFINITY, ']'}) ->
    [{element, ?MINUS_INFINITY}];
normalize_simple({interval, '[', ?PLUS_INFINITY, ?MINUS_INFINITY, ')'}) ->
    [{element, ?PLUS_INFINITY}];
normalize_simple({interval, '[', ?PLUS_INFINITY, ?MINUS_INFINITY, ']'}) ->
    [{element, ?MINUS_INFINITY}, {element, ?PLUS_INFINITY}];
normalize_simple({interval, '(', ?PLUS_INFINITY, X, RightBr}) ->
    [{interval, '[', ?MINUS_INFINITY, X, RightBr}];
normalize_simple({interval, '[', ?PLUS_INFINITY, X, RightBr}) ->
    [{interval, '[', ?MINUS_INFINITY, X, RightBr}, {element, ?PLUS_INFINITY}];
normalize_simple({interval, LeftBr, X, ?MINUS_INFINITY, ')'}) ->
    [{interval, LeftBr, X, ?PLUS_INFINITY, ']'}];
normalize_simple({interval, LeftBr, X, ?MINUS_INFINITY, ']'}) ->
    [{element, ?MINUS_INFINITY}, {interval, LeftBr, X, ?PLUS_INFINITY, ']'}];
normalize_simple({interval, LeftBr, Begin, End, RightBr} = I) ->
    case wraps_around(LeftBr, Begin, End, RightBr) of
        true ->  [{interval, '[', ?MINUS_INFINITY, End, RightBr},
                  {interval, LeftBr, Begin, ?PLUS_INFINITY, ']'}];
        false -> [I]
    end.

%% @doc Specifies an order over simple intervals (returns true if I1 &lt;= I2).
%%      The order is based on the intervals' first components
%%      and in case of elements based on their value. 'all' is the first and
%%      elements are sorted before intervals with the same values, if two
%%      intervals' first components compare equal the one with '[' is smaller
%%      (to ease merge_adjacent/2), otherwise normal &lt;= from erlang is used.
-spec interval_sort(I1::simple_interval(), I2::simple_interval()) -> boolean().
interval_sort(all, _Interval2) ->
    true;
interval_sort({element, A}, {element, B}) ->
    greater_equals_than(B, A);
interval_sort({element, A}, {interval, _B0Br, B0, _B1, _B1Br}) ->
    greater_equals_than(B0, A);
interval_sort({interval, _A0Br, A0, _A1, _A1Br}, {element, B}) ->
    greater_than(B, A0);
interval_sort({interval, A0Br, A0, _A1, _A1Br} = A, {interval, B0Br, B0, _B1, _B1Br} = B) ->
    % beware of not accidentally making two intervals equal, which is defined
    % as A==B <=> interval_sort(A, B) andalso interval_sort(B, A)
    greater_than(B0, A0) orelse
        (A0 =:= B0 andalso A0Br =:= '[' andalso B0Br =:= '(') orelse
        (A0 =:= B0 andalso (not (A0Br =:= '(' andalso B0Br =:= '[')) andalso A =< B);
interval_sort(_Interval1, _Interval2) ->
    false.

%% @doc Merges adjacent intervals in a sorted list of simple intervals using
%%      union_simple/2.
-spec merge_adjacent([simple_interval()], [simple_interval()]) -> [simple_interval()].
merge_adjacent([], Results) ->
    lists:reverse(Results);
merge_adjacent([all | _T], _Results) ->
    [all];
merge_adjacent([H | T], []) ->
    merge_adjacent(T, [H]);
merge_adjacent([HI | TI], [HR | TR]) ->
    merge_adjacent(TI, union_simple(HI, HR) ++ TR).

%% @doc Creates the union of two intervals.
-spec union(A::interval(), B::interval()) -> interval().
union([all] = A, _B) -> A;
union(_A, [all] = B) -> B;
union([], B)         -> B;
union(A, [])         -> A;
union(A, A)          -> A;
union(A, B)          -> normalize_internal(lists:append(A, B)).

%% @doc Creates the union of two simple intervals or empty lists.
-spec union_simple(A::simple_interval(), B::simple_interval()) -> [simple_interval()].
union_simple(all, _B) -> [all];
union_simple(_A, all) -> [all];
union_simple({element, B0}, {interval, _B0Br, B0, B1, B1Br}) ->
    new('[', B0, B1, B1Br);
union_simple({element, B1}, {interval, B0Br, B0, B1, _B1Br}) ->
    new(B0Br, B0, B1, ']');
union_simple({interval, A0Br, A0, A1, _A1Br}, {element, A1}) ->
    new(A0Br, A0, A1, ']');
union_simple({interval, _A0Br, A0, A1, A1Br}, {element, A0}) ->
    new('[', A0, A1, A1Br);
union_simple({element, A_Value} = A, B) ->
    case in_simple(A_Value, B) of
        true  -> [B];
        false -> [A, B]
    end;
union_simple(A, {element, B_Value} = B) ->
    case in_simple(B_Value, A) of
        true  -> [A];
        false -> [A, B]
    end;
union_simple({interval, A0Br, A0, A1, ']'}, {interval, _B0Br, A1, B1, B1Br}) ->
    new(A0Br, A0, B1, B1Br);
union_simple({interval, A0Br, A0, A1, _A1Br}, {interval, '[', A1, B1, B1Br}) ->
    new(A0Br, A0, B1, B1Br);
union_simple({interval, '[', A0, A1, A1Br}, {interval, B0Br, B0, A0, _B1Br}) ->
    new(B0Br, B0, A1, A1Br);
union_simple({interval, _A0Br, A0, A1, A1Br}, {interval, B0Br, B0, A0, ']'}) ->
    new(B0Br, B0, A1, A1Br);
union_simple({interval, A0Br, A0, A1, A1Br} = A, {interval, B0Br, B0, B1, B1Br} = B) ->
    B0_in_A = is_between(A0Br, A0, B0, A1, A1Br),
    B1_in_A = is_between(A0Br, A0, B1, A1, A1Br),
    A0_in_B = is_between(B0Br, B0, A0, B1, B1Br),
    A1_in_B = is_between(B0Br, B0, A1, B1, B1Br),
    if
        B0_in_A orelse B1_in_A orelse A0_in_B orelse A1_in_B ->
            {NewLeft, NewLeftBr} =
                case A0 =:= B0 of
                    true when A0Br =:= '[' -> {A0, '['};
                    true -> {A0, B0Br};
                    false ->
                        case util_min(A0, B0) =:= A0 of
                            true  -> {A0, A0Br};
                            false -> {B0, B0Br}
                        end
                end,
            {NewRight, NewRightBr} =
                case A1 =:= B1 of
                    true when A1Br =:= ']' -> {A1, ']'};
                    true -> {A1, B1Br};
                    false ->
                        case util_max(A1, B1) =:= A1 of
                            true  -> {A1, A1Br};
                            false -> {B1, B1Br}
                        end
                end,
            case NewLeft =:= NewRight of
                true when (NewLeftBr =:= '[') andalso (NewRightBr =:= ']') ->
                    new(NewLeft);
                true -> empty();
                false -> new(NewLeftBr, NewLeft, NewRight, NewRightBr)
            end;
        true -> [A, B]
    end.

-spec util_max(?PLUS_INFINITY, any()) -> ?PLUS_INFINITY;
              (any(), ?PLUS_INFINITY) -> ?PLUS_INFINITY;
              (T | ?MINUS_INFINITY, T | ?MINUS_INFINITY) -> T.
util_max(?PLUS_INFINITY, _) -> ?PLUS_INFINITY;
util_max(_, ?PLUS_INFINITY) -> ?PLUS_INFINITY;
util_max(?MINUS_INFINITY, X) -> X;
util_max(X, ?MINUS_INFINITY) -> X;
util_max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

-spec util_min(?MINUS_INFINITY, any()) -> ?MINUS_INFINITY;
              (any(), ?MINUS_INFINITY) -> ?MINUS_INFINITY;
              (T | ?PLUS_INFINITY, T | ?PLUS_INFINITY) -> T.
util_min(?MINUS_INFINITY, _) -> ?MINUS_INFINITY;
util_min(_, ?MINUS_INFINITY) -> ?MINUS_INFINITY;
util_min(?PLUS_INFINITY, X) -> X;
util_min(X, ?PLUS_INFINITY) -> X;
util_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

%% @doc Checks whether the given interval is a continuous interval, i.e. simple
%%      intervals are always continuous, complex intervals are continuous if
%%      they contain 2 simple intervals which are adjacent and wrap around.
%%      Note: empty intervals are not continuous!
-spec is_continuous(interval()) -> boolean().
is_continuous([all]) -> true;
is_continuous([{element, _Key}]) -> true;
is_continuous([{interval, _LBr, _L, _R, _RBr}]) -> true;
% complex intervals have adjacent intervals merged except for those wrapping around
% -> if it contains only two simple intervals which are adjacent, it is continuous!
is_continuous([{interval, '[', ?MINUS_INFINITY, _B1, _B1Br},
               {interval, _A0Br, _A0, ?PLUS_INFINITY, ']'}]) -> true;
is_continuous([{element, ?MINUS_INFINITY},
               {interval, _A0Br, _A0, ?PLUS_INFINITY, ']'}]) -> true;
is_continuous([{interval, '[', ?MINUS_INFINITY, _B1, _B1Br},
               {element, ?PLUS_INFINITY}]) -> true;
is_continuous([{element, ?MINUS_INFINITY},
               {element, ?PLUS_INFINITY}]) -> true;
is_continuous(_) -> false.

%% @doc Gets the bounds of a given continuous (!) interval including their
%%      brackets. Note that here
%%      'all' transfers to {'[', ?MINUS_INFINITY, ?PLUS_INFINITY, ']'},
%%      {element, Key} to {'[', Key, Key, ']'} and
%%      [{interval,'[',?MINUS_INFINITY,Key,')'},{interval,'(',Key,?PLUS_INFINITY,']'}] to {'(', Key, Key, ')'}.
%%      Other normalized intervals that wrap around (as well as the first two)
%%      are returned the same way they can be constructed with new/4.
%%      Note: this method will only work on continuous non-empty intervals
%%      and will throw an exception otherwise!
-spec get_bounds(interval()) -> {left_bracket(), key(), key(), right_bracket()}.
get_bounds([all]) -> {'[', ?MINUS_INFINITY, ?PLUS_INFINITY, ']'};
get_bounds([{element, Key}]) -> {'[', Key, Key, ']'};
get_bounds([{interval, LBr, L, R, RBr}]) -> {LBr, L, R, RBr};
get_bounds([{interval, '[', ?MINUS_INFINITY, B1, B1Br},
            {interval, A0Br, A0, ?PLUS_INFINITY, ']'}]) -> {A0Br, A0, B1, B1Br};
get_bounds([{element, ?MINUS_INFINITY},
            {interval, A0Br, A0, ?PLUS_INFINITY, ']'}]) -> {A0Br, A0, ?MINUS_INFINITY, ']'};
get_bounds([{interval, '[', ?MINUS_INFINITY, B1, B1Br},
            {element, ?PLUS_INFINITY}]) -> {'[', ?PLUS_INFINITY, B1, B1Br};
get_bounds([{element, ?MINUS_INFINITY}, {element, ?PLUS_INFINITY}]) -> {'[', ?PLUS_INFINITY, ?MINUS_INFINITY, ']'};
get_bounds([]) -> erlang:throw('no bounds in empty interval').

%% @doc Checks whether two intervals are adjacent, i.e. the intervals are both
%%      continuous, their union is continuous and their intersection is empty,
%%      e.g. ('(A,B]', '(B,C)') with A=/=B and B=/=C.
%%      Note: intervals like (A,B), (B,C) are not considered adjacent because
%%      the element b would be between these two.
-spec is_adjacent(interval(), interval()) -> boolean().
is_adjacent(A, B) ->
    is_continuous(A) andalso is_continuous(B) andalso
        is_empty(intersection(A, B)) andalso is_continuous(union(A, B)).

%% @doc Subtracts the second from the first simple interval.
-spec minus_simple(simple_interval(), simple_interval()) -> interval().
minus_simple(A, A)   -> empty();
minus_simple(_, all) -> empty();
minus_simple(all, {element, B0}) ->
    % hack: use [?MINUS_INFINITY, ?PLUS_INFINITY] as 'all' and [B0, B0] as element - minus_simple2 can handle this though
    minus_simple2({interval, '[', ?MINUS_INFINITY, ?PLUS_INFINITY, ']'},
                  {interval, '[', B0, B0, ']'});
minus_simple(all, B = {interval, _B0Br, _B0, _B1, _B1Br}) ->
    % hack: use [?MINUS_INFINITY, ?PLUS_INFINITY] as 'all' and [B0, B0] as element - minus_simple2 can handle this though
    minus_simple2({interval, '[', ?MINUS_INFINITY, ?PLUS_INFINITY, ']'}, B);
minus_simple(A = {element, _}, {element, _}) -> [A];
minus_simple(A = {element, A0}, B = {interval, _B0Br, _B0, _B1, _B1Br}) ->
    case in_simple(A0, B) of
        true -> empty();
        _    -> [A]
    end;
minus_simple({interval, '[', A0, A1, A1Br}, {element, A0}) ->
    new('(', A0, A1, A1Br);
minus_simple({interval, A0Br, A0, A1, ']'}, {element, A1}) ->
    new(A0Br, A0, A1, ')');
minus_simple(A = {interval, A0Br, A0, A1, A1Br}, {element, B0}) ->
    case in_simple(B0, A) of
        false -> [A];
        true  -> union(new(A0Br, A0, B0, ')'), new('(', B0, A1, A1Br))
    end;
minus_simple(A = {interval, _A0Br, _A0, _A1, _A1Br}, B = {interval, _B0Br, _B0, _B1, _B1Br}) ->
    B_ = intersection_simple(A, B),
    case B_ of
        []                     -> [A];
        A                      -> empty();
        {interval, _, _, _, _} -> minus_simple2(A, B_);
        _                      -> minus_simple(A, B_)
    end.

%% @doc Subtracts the second from the first simple interval (no elements, no
%%      'all', no empty interval). The second interval must be a subset of the
%%      first interval!
-spec minus_simple2({interval, left_bracket(), key(), key(), right_bracket()}, {interval, left_bracket(), key(), key(), right_bracket()}) -> interval().
minus_simple2({interval, A0Br, A0, A1, A1Br}, {interval, B0Br, B0, B1, B1Br}) ->
    First = case B0Br of
                '(' when B0 =:= A0 andalso A0Br =:= '[' ->
                    new(A0);
                '(' when B0 =:= A0 -> empty();
                '('                -> new(A0Br, A0, B0, ']');
                '[' when B0 =:= A0 -> empty();
                '['                -> new(A0Br, A0, B0, ')')
               end,
    Second = case B1Br of
                 ')' when B1 =:= A1 andalso A1Br =:= ']' ->
                     new(A1);
                 ')' when B1 =:= A1 -> empty();
                 ')'                -> new('[', B1, A1, A1Br);
                 ']' when B1 =:= A1 -> empty();
                 ']'                -> new('(', B1, A1, A1Br)
               end,
    union(First, Second).

%% @doc Subtracts the second from the first interval.
-spec minus(interval(), interval()) -> interval().
minus(_A, [all]) -> empty();
minus(A, [])     -> A;
minus(A, A)      -> empty();
minus(A, B) ->
    % from every simple interval in A, remove all simple intervals in B
    % note: we cannot use minus_simple in foldl since the result may be a list again
    normalize_internal(lists:flatten([minus2(IA, B) || IA <- A])).

-spec minus2(A::simple_interval(), B::[simple_interval()]) -> [simple_interval()].
minus2(A, []) -> A;
minus2(A, [HB | TB]) ->
    minus(minus_simple(A, HB), TB).

%% @private
%% @doc Determines whether an interval with the given borders wraps around,
%%      i.e. the interval would cover the (non-existing) gap between
%%      ?PLUS_INFINITY and ?MINUS_INFINITY.
-spec wraps_around(left_bracket(), key(), key(), right_bracket()) -> boolean().
wraps_around(_LeftBr, X, X, _RightBr) ->
    false;
wraps_around(_LeftBr, ?MINUS_INFINITY, _, _RightBr) ->
    false;
wraps_around(_LeftBr, _, ?PLUS_INFINITY, _RightBr) ->
    false;
wraps_around(_LeftBr, _, ?MINUS_INFINITY, ')') ->
    % same as [A, ?PLUS_INFINITY] or (A, ?PLUS_INFINITY]
    false;
wraps_around(_LeftBr, _, ?MINUS_INFINITY, _RightBr) ->
    true;
wraps_around('(', ?PLUS_INFINITY, _, _RightBr) ->
    % same as [?MINUS_INFINITY, A] or [?MINUS_INFINITY, A)
    false;
wraps_around(_LeftBr, ?PLUS_INFINITY, _, _RightBr) ->
    true;
wraps_around(_LeftBr, First, Last, _RightBr) when First > Last ->
    true;
wraps_around(_LeftBr, _First, _Last, _RightBr) ->
    false.

% @doc Begin &lt;= X &lt;= End
% precondition Begin &lt;= End
-spec is_between(BeginBr::left_bracket(), Begin::key(), X::key(), End::key(), EndBr::right_bracket()) -> boolean().
is_between('[', Begin, X, End, ']') ->
    greater_equals_than(X, Begin) andalso greater_equals_than(End, X);
is_between('[', Begin, X, End, ')') ->
    greater_equals_than(X, Begin) andalso greater_than(End, X);
is_between('(', Begin, X, End, ']') ->
    greater_than(X, Begin) andalso greater_equals_than(End, X);
is_between('(', Begin, X, End, ')') ->
    greater_than(X, Begin) andalso greater_than(End, X).

%% @doc A &gt; B
-spec greater_than(A::key(), B::key()) -> boolean().
greater_than(X, X)              -> false;
greater_than(?MINUS_INFINITY, _) -> false;
greater_than(?PLUS_INFINITY, _)  -> true;
greater_than(_, ?PLUS_INFINITY)  -> false;
greater_than(_, ?MINUS_INFINITY) -> true;
greater_than(X, Y)              -> X > Y.

%% @doc A &gt;= B
-spec greater_equals_than(A::key(), B::key()) -> boolean().
greater_equals_than(A, B) -> (A =:= B) orelse greater_than(A, B).

%% @doc X and Y are adjacent and Y follows X
-spec is_left_of(interval(), interval()) -> boolean().
is_left_of(X, Y) ->
    case is_adjacent(X, Y) of
        true ->
            {_, _A,  B, _} = get_bounds(X),
            {_,  C, _D, _} = get_bounds(Y),
            % in(B, X) =/= in(B, Y) implied by is_adjacent
            (B =:= C andalso (in(B, X) orelse in(B, Y)))
                orelse
            ({B, C} =:= {?PLUS_INFINITY, ?MINUS_INFINITY} andalso
             (in(?PLUS_INFINITY, X) orelse in(?MINUS_INFINITY, Y)));
        false ->
            false
    end.

%% @doc X and Y are adjacent and X follows Y
-spec is_right_of(interval(), interval()) -> boolean().
is_right_of(X, Y) ->
    is_left_of(Y, X).

