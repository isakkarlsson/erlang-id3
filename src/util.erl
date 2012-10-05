-module(util).
-compile(export_all).


%% Reduce list L of two tuples to a single value determined by  {_, V1}, {_, V2} F(V1, V2)
%% Input:
%%   - F: Function taking two values returning one of them
%%   - L: List of two tuples
%% Output:
%%   - The value last returned from F
reduce_tuple(F, L) ->
    lists:foldl(fun ({K1, V1}, {K2, V2}) ->
			case F(V1, V2) of
			    V1 ->
				{K1, V1};
			    V2 ->
				{K2, V2}
			end
		end, hd(L), tl(L)).

%% Return the smallest two tuple {K, Value} when minimizing Value
%% Input:
%%   - L: list of two tuples {K, V} where V is to be minimzed
%% Output:
%%   - The minimized tuple
min(L) ->
    reduce_tuple(fun erlang:min/2, L).

%% Return the largest two tuple {K, Value} when maximizing Value
%% Input:
%%   - L: list of two tuples {K, V} where V is to be maximized
%% Output:
%%   - The maximized tuple
max(L) ->
    reduce_tuple(fun erlang:max/2, L).

-spec gb_fold(fun((term(), term(), term()) -> term()), term(), gb_tree()) ->
		  term().

gb_fold(F, A, {_, T}) when is_function(F, 3) ->
    gb_fold_1(F, A, T).

gb_fold_1(F, Acc0, {Key, Value, Small, Big}) ->
    Acc1 = gb_fold_1(F, Acc0, Small),
    Acc = F(Key, Value, Acc1),
    gb_fold_1(F, Acc, Big);
gb_fold_1(_, Acc, _) ->
    Acc.
