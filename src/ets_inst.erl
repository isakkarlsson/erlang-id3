-module(ets_inst).
-compile(export_all).

new() ->
    ets:new(no_name, [set, {read_concurrency, true}]).

load(File) ->
    {ok, Data} = file:consult(File),
    load(Data, 0, new()).

load([I|R], N, Acc) ->
    ets:insert(Acc, {N, lists:foldl(fun ({K, V}, Tree) ->
					    gb_trees:enter(K, V, Tree)
				    end, gb_trees:empty(), I)}),
    load(R, N + 1, Acc);
load([], _,  Acc) ->
    Acc.

lookup(Set, Id) ->
    [{Id, Out}|_] = ets:lookup(Set, Id),
    Out.
lookup1(Set, Id) ->
    [{Id, Out}|_] = ets:lookup(Set, Id),
    {Id, Out}.

features(Set) ->
    I = lookup(Set, ets:first(Set)),
    gb_trees:keys(gb_trees:delete(class, I)).

split(F, Set) ->
    gb_trees:to_list(split(F, Set, ets:first(Set), gb_trees:empty())).

split(_, _, '$end_of_table', Acc) ->
    Acc;
split(F, Set, Id, Acc) ->
    {Id, Tree0} = lookup1(Set, Id),
    Value = gb_trees:get(F, Tree0),
    Tree = gb_trees:delete(F, Tree0),
    Next =  ets:next(Set, Id),
    case gb_trees:lookup(Value, Acc) of
	{value, Ets} ->
	    ets:insert(Ets, {Id, Tree}),
	    split(F, Set, Next, gb_trees:enter(Value, Ets, Acc));
	none ->
	    Ets = new(),
	    ets:insert(Ets, {Id, Tree}),
	    split(F, Set, Next, gb_trees:enter(Value, Ets, Acc))
    end.

occurences(F, Set) ->
    occurences(F, Set, ets:first(Set), gb_trees:empty()).
occurences(_, _, '$end_of_table', Acc) ->
    gb_trees:to_list(Acc);
occurences(F, Set, Id, Acc) ->
    {Id, Tree} = lookup1(Set, Id),
    Value = gb_trees:get(F, Tree),
    Next = ets:next(Set, Id),
    case gb_trees:lookup(Value, Acc) of
	{value, Count} ->
	    occurences(F, Set, Next, gb_trees:enter(Value, Count + 1, Acc));
	none ->
	    occurences(F, Set, Next, gb_trees:enter(Value, 1, Acc))
    end.
    
feature_ratio(F, Set) ->    
    gb_trees:to_list(gb_trees:map(fun (_, V) ->
					  gb_trees:to_list(V)
				  end, feature_ratio(F, Set, ets:first(Set), gb_trees:empty()))).

feature_ratio(_, _, '$end_of_table', Acc) ->
    Acc;
feature_ratio(F, Set, Id, Acc) ->
    {Id, Tree} = lookup1(Set, Id),
    Value = gb_trees:get(F, Tree),
    Class = gb_trees:get(class, Tree),
    Next = ets:next(Set, Id),
    case gb_trees:lookup(Value, Acc) of
	{value, CDict} ->
	    case gb_trees:lookup(Class, CDict) of
		{value, Count} ->
		    feature_ratio(F, Set, Next, gb_trees:enter(Value, gb_trees:enter(Class, Count + 1, CDict), Acc));
		none ->
		    feature_ratio(F, Set, Next, gb_trees:enter(Value, gb_trees:enter(Class, 1, CDict), Acc))
	    end;
	none ->
	    feature_ratio(F, Set, Next, gb_trees:enter(Value, gb_trees:enter(Class, 1, gb_trees:empty()), Acc))
    end.

async_gain(F, Set, N) ->
    LenF = length(F),
    Sc = erlang:system_info(schedulers),
    Fs = util:split(F, case Sc > LenF of 
			   true -> LenF; 
			   false -> Sc end, LenF),
    Me = self(),
    Pids = [spawn_link(?MODULE, async_feature_gain, [Me, Fi, Set, N]) || Fi <- Fs],
    collect_gain(Me, Pids, []).

collect_gain(_, [], Acc) ->
    Acc;
collect_gain(Me, Pids, Acc) ->
    receive
	{Me, Pid, L} ->
	    collect_gain(Me, lists:delete(Pid, Pids), L ++ Acc)
    end.
		    
async_feature_gain(Me, F, I, N) ->
    Me ! {Me, self(), gain_ratio(F, I, N)}.

gain(async, F, Set, N) ->
    async_gain(F, Set, N);
gain(sync, F, Set, N) ->
    gain_ratio(F, Set, N).


gain_ratio(F, Set, N) ->
    gain_ratio(F, Set, N, []).

gain_ratio([], _, _, Acc) ->
    Acc;
gain_ratio([F|R], Set, N, Acc) ->
    gain_ratio(R, Set, N, [{F, feature_gain(F, Set, N)}|Acc]).

feature_gain(F, Set, N) ->
    Ratios = feature_ratio(F, Set),
    G = stat:gain(Ratios, N),
    Gi = stat:split_info(Ratios, N),
    G / (Gi + 0.000000000001).

majority(F, Set) ->
    {Ret, _} = util:max(occurences(F, Set)),
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
	[] -> {dont_stop, N};
	[{X,_}|_] -> {majority, X}
    end.
