% Representing data as a list of gb_trees
-module(rinst).

-include("nodes.hrl").
-export([features/1,  get/2, load/1, golfing/0, extract_branches/2, 
	 occurences/2, is_pure/2, feature_ratio/2, feature_gain/2, 
	 gain_ratio/2, majority/2, classify/2, stop_induce/2]).

load(File) ->
    {_, Data} = file:consult(File),
    load(Data, []).

load([I|R], Acc) ->
    load(R, [lists:foldl(fun ({K, V}, Tree) ->
				 gb_trees:enter(K, V, Tree)
			 end, gb_trees:empty(), I)|Acc]);
load([], Acc) ->
    Acc.



% O(m)
features([I|_]) ->
    gb_trees:keys(gb_trees:delete(class, I));
features([]) ->
    [].

% Don't use
% O(n log m)
get(Feature, Instances) ->
    lists:map(fun (Instance) ->
		      gb_trees:get(Feature, Instance)
	      end, Instances).

extract_branches(F, I) ->
    gb_trees:to_list(extract_branches(F, I, gb_trees:empty())).

extract_branches(_, [], Acc) ->
    Acc;
extract_branches(F, [I|R], Acc) ->
    Value = gb_trees:get(F, I),
    case gb_trees:lookup(Value, Acc) of
	{value, List} ->
	    extract_branches(F, R, gb_trees:enter(Value, [gb_trees:delete(F, I)|List], Acc));
	none ->
	    extract_branches(F, R, gb_trees:enter(Value, [gb_trees:delete(F, I)], Acc))
    end.

% O(n log m)
occurences(F, I) ->
    occurences(F, I, gb_trees:empty()).
occurences(_, [], Acc) ->
    gb_trees:to_list(Acc);
occurences(F, [I|R], Acc) ->
    Value = gb_trees:get(F, I),
    case gb_trees:lookup(Value, Acc) of
	{value, C} ->
	    occurences(F, R, gb_trees:enter(Value, C + 1, Acc));
	none ->
	    occurences(F, R, gb_trees:enter(Value, 1, Acc))
    end.

% O(n log m)
is_pure(F, [I|R]) ->
    Value = gb_trees:get(F, I),
    is_pure(Value, F, R).

is_pure(V, F, [I|R]) ->
    Value = gb_trees:get(F, I),
    if 
	Value == V ->
	    is_pure(V, F, R);
	true ->
	    false
    end;
is_pure(V, _, []) ->
    {true, V}.


feature_ratio(F, I) ->
    gb_trees:to_list(gb_trees:map(fun (_, V) ->
					  gb_trees:to_list(V)
				  end, feature_ratio(F, I, gb_trees:empty()))).

feature_ratio(F, [I|R], Acc) ->
    Value = gb_trees:get(F, I),
    Class = gb_trees:get(class, I),
    case gb_trees:lookup(Value, Acc) of
	{value, CDict} ->
	    case gb_trees:lookup(Class, CDict) of
		{value, Count} ->
		    feature_ratio(F, R, gb_trees:enter(Value, gb_trees:enter(Class, Count + 1, CDict), Acc));
		none ->
		    feature_ratio(F, R, gb_trees:enter(Value, gb_trees:enter(Class, 1, CDict), Acc))
	    end;
	none ->
	    feature_ratio(F, R, gb_trees:enter(Value, gb_trees:enter(Class, 1, gb_trees:empty()), Acc))
    end;
feature_ratio(_, [], Acc) ->
    Acc.

gain_ratio(F, I) ->
    gain_ratio(F, I, []).
gain_ratio([], _, Acc) ->
    Acc;
gain_ratio([F|R], I, Acc) ->
    gain_ratio(R, I, [{F, feature_gain(F, I)}|Acc]).


feature_gain(F, I) ->
    Ratios = feature_ratio(F, I),
    N = lists:sum([lists:sum([C || {_, C} <- CDist]) || {_,CDist} <- Ratios]),
    G = stat:gain(Ratios, N),
    Gi = stat:split_info(Ratios, N),
    G / (Gi + 0.000000000001).

majority(F, I) ->
    {Ret, _} = util:max(occurences(F, I)),
    Ret.

%% Determine wheter we should stop the induction of the tree
%% Input:
%%   - I: The instance set
%% Output
%%   - {majority, MajorityClass} or dont_stop
stop_induce(I, []) ->
    {majority, majority(class, I)};
stop_induce(Instances, _) ->
    Count = occurences(class, Instances),
    N = lists:sum([V || {_, V} <- Count]),
    case lists:filter(fun ({_, C}) -> C / N == 1 end, Count) of
	[] -> dont_stop;
	[{X,_}|_] -> {majority, X}
    end.


%% TODO: Support rinst instances
%%
%% Classify Instance according to Model
%% Input:
%%   - Instance: An instance
%%   - Model: A model
%% Output:
%%   - The class label as predicted by Model
classify(_, #node{type=classify, value=#classify{as=Class}}) ->
    Class;
classify(Instance, #node{type=compare, value=#compare{type=nominal, feature=F, branches=B}}) ->
    case gb_trees:lookup(F, Instance) of
	{value, [V|_]} ->
	    Branch = find_branch(V, B),
	    classify(Instance, Branch);
	none ->
	    classify(Instance, {error, instance_dont_have_feature, F})
    end;
classify(_, {error, R, M}) ->
    {error, R, M}.


find_branch(V, []) ->
    {error, branch_not_found, V};
find_branch(V, [{V,B}|_]) ->
    B;
find_branch(V, [{_,_}|Br]) ->
    find_branch(V, Br).


golfing() ->
    Data = [[{class, dont_play}, {f0, sunny}, {f1, false}],
	    [{class, dont_play}, {f0, sunny}, {f1, true}],
	    [{class, play}, {f0, overcast}, {f1, false}],
	    [{class, play},  {f0, rain}, {f1, false}],
	    [{class, play}, {f0, rain}, {f1, false}]  ,
            [{class, dont_play}, {f0, rain},{f1, true} ],
	    [{class, play}, {f0, overcast}, {f1, true}],
	    [{class, dont_play}, {f0, sunny},  {f1, false}],
	    [{class, play}, {f0, sunny}, {f1, false}],
	    [{class, play}, {f0, rain}, {f1, false}],
	    [{class, play}, {f0, sunny}, {f1, true}],
	    [{class, play}, {f0, overcast}, {f1, true}],
	    [{class, play}, {f0, overcast}, {f1, false} ],
	    [{class, dont_play}, {f0, rain}, {f1, true}]],
	lists:map(fun (List) ->
			  lists:foldl(fun ({K, V}, Dict) ->
					      gb_trees:enter(K, V, Dict)
				      end, gb_trees:empty(), List)
		  end, Data).

		       
		       

