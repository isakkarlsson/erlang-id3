-module(util).
-compile(export_all).


pos(List, Ele) ->
     pos(List, Ele, 1).
pos([Ele | _], Ele, Pos) ->
     Pos;
pos([_ | Tail], Ele, Pos) ->
     pos(Tail, Ele, Pos+1);
pos([], _Ele, _) ->
     0.

%% Determine if a string is a number,
%% returns {true, int()|float()} or false
is_numeric(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    case is_number(Float) of
	true ->
	    {true, Float};
	false ->
	    case is_number(Int) of
		true ->
		    {true, Int};
		false ->
		    false
	    end
    end.

%% Remove the N:th number
remove_nth(List, N) ->
  {L1, [_|L2]} = lists:split(N-1, List),
  L1 ++ L2.


shuffle(List) ->
    Random_list = [{random:uniform(), X} || X <- List],
    [X || {_,X} <- lists:sort(Random_list)].

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

split(List,Splits, Len) ->
  split(lists:reverse(List),[],0,Len div Splits).
split([],Acc,_,_) -> Acc;
split([H|T],Acc,Pos,Max) when Pos==Max ->
    split(T,[[H] | Acc],1,Max);
split([H|T],[HAcc | TAcc],Pos,Max) ->
    split(T,[[H | HAcc] | TAcc],Pos+1,Max);
split([H|T],[],Pos,Max) ->
    split(T,[[H]],Pos+1,Max).
