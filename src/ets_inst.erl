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
    
    


    
    

