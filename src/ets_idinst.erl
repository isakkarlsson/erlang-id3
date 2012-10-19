-module(ets_idinst).
-compile(export_all).


%
% Examples: {Class, NoClasses, [InstId|InstIds]}
% Types:    [{type, idx}|...]
%
%
%
%
load(File) ->
    {ok, Data} = file:consult(File),
    Types = lists:map(fun({_, V}) ->
			      V
		      end, lists:keydelete('class', 1, hd(Data))),
   {lists:zip(lists:seq(1, length(Types)), Types), load(Data, 0, gb_trees:empty())}.


load([], _, Examples) ->
    lists:map(fun ({C, {N, L}}) -> {C, N, L} end, gb_trees:to_list(Examples));
load([Inst|Rest], N, Examples) ->
    {value, {_, Class}, Tmp} = lists:keytake('class', 1, Inst),
    ets:insert(examples, {N, list_to_tuple(lists:map(fun({_, V}) -> V end, Tmp))}),
    load(Rest, N + 1, case gb_trees:lookup(Class, Examples) of
			  {value, {Num, Ids}} ->
			      gb_trees:enter(Class, {Num + 1, [N|Ids]}, Examples);
			  none ->
			      gb_trees:enter(Class, {1, [N]}, Examples)
		      end).

occurences(class, Examples) ->
    lists:map(fun ({C, N, _}) ->
		      {C, N}
	      end, Examples).

majority(Examples) ->
    {C, _, _} = lists:foldl(fun ({C, N, _}, {C1, N1, _}) ->
				    case erlang:max(N, N1) of
					N ->
					    {C, N, []};
					N1 ->
					    {C1, N1, []}
				    end
			    end, hd(Examples), tl(Examples)),
    C.
			

split(AttrId, Examples) ->
    split(AttrId, Examples, gb_trees:empty()).

split(AttrId, Examples, Acc) ->
    ok.
    
