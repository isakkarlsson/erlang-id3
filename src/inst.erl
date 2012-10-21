-module(inst).
-compile(export_all).

load_cvs(File) ->
    Data = cvs:parse(file, File),
    [Types0|Data0] = Data,
    [Attr0|Examples0] = Data0,
    
    % [{nominal, pos}, numeric, class]
    {ClassIdx, Types} = case lists:keytake(class, 1, load_types(Types0, 1)) of
			    {value, {_, Idx}, Types} ->
				{Class, Types};
			    false ->
				throw({error, no_class})
			end,
    Examples = load_examples_cvs(Examples0, 0, ClassIdx, gb_trees:empty()),
    
    % [{nominal, pos}, ....]
    % but inserts attributes to ets:dict with key pos
    Attributes = load_attributes_cvs(Attr0, Types, Examples),
    {Attributes, Examples}.


load_types(Types, N) ->
    load_types(Types, N, Acc).

load_types([], _, Acc) ->
    lists:reverse(Acc);
load_types([Type|Types], N, Acc) when Type == class or 
				      Type == nominal or
				      Type == numeric ->
    load_types(Types, N + 1, [{Type, N}|Acc]);
load_types(_, _, _) ->
    throw({error, invalid_type}).




%
% Examples: {Class, NoClasses, [InstId|InstIds]}
% Types:    [{type, idx}|...]
%
%
%
%
load(File) ->
    {ok, Data} = file:consult(File),
    Attrs0 = lists:map(fun({K, _}) ->
			      K
		      end, lists:keydelete('class', 1, hd(Data))),
    Attrs = load_attributes(Attrs0, 1),
   {Attrs, load_examples(Data, 0, gb_trees:empty())}.

load_attributes(Types, N) ->
    case Types of
	[] ->
	    [];
	[H|R] ->
	    ets:insert(attributes, {N, H}),
	    [N | load_attributes(R, N + 1)]
    end.

load_examples([], _, Examples) ->
    lists:map(fun ({C, {N, L}}) -> 
		      {C, N, L} 
	      end, gb_trees:to_list(Examples));
load_examples([Inst|Rest], N, Examples) ->
    {value, {_, Class}, Tmp} = lists:keytake('class', 1, Inst),
    ets:insert(examples, {N, list_to_tuple(lists:map(fun({_, V}) -> V end, Tmp))}),
    load_examples(Rest, N + 1, case gb_trees:lookup(Class, Examples) of
			  {value, {Num, Ids}} ->
			      gb_trees:enter(Class, {Num + 1, [N|Ids]}, Examples);
			  none ->
			      gb_trees:enter(Class, {1, [N]}, Examples)
		      end).

lookup(Set, Id) ->
    [{Id, Out}|_] = ets:lookup(Set, Id),
    Out.

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

split(_, [], Acc) ->
    gb_trees:to_list(Acc);
split(AttrId, [{Class, _, ExampleIds}|Examples], Acc) ->
    split(AttrId, Examples, split_class(AttrId, Class, ExampleIds, Acc)).

split_class(_, _, [], Acc) ->
    Acc;
split_class(AttrId, Class, [Ex|Examples], Acc) ->
    Value = element(AttrId, lookup(examples, Ex)),
    split_class(AttrId, Class, Examples, 
		case gb_trees:lookup(Value, Acc) of
		    {value, Classes} ->
			case lists:keytake(Class, 1, Classes) of
			    {value, {Class, Num, ExList}, ClassRest} ->
				gb_trees:enter(Value, [{Class, Num + 1, [Ex|ExList]}|ClassRest], Acc);
			    false ->
				gb_trees:enter(Value, [{Class, 1, [Ex]}|Classes], Acc)
			end;
		    none ->
			gb_trees:enter(Value, [{Class, 1, [Ex]}], Acc)
		end).
	    
feature_ratio(AttrId, Examples) ->
    feature_ratio(AttrId, Examples, gb_trees:empty()).

feature_ratio(_, [], Acc) ->
    gb_trees:to_list(Acc);
feature_ratio(AttrId, [{Class, _, ExIds}|Examples], Acc) ->
    feature_ratio(AttrId, Examples, feature_ratio_class(AttrId, Class, ExIds, Acc)).

feature_ratio_class(_, _, [], Acc) ->
    Acc;
feature_ratio_class(AttrId, Class, [ExId|Rest], Acc) ->
    Value = element(AttrId, lookup(examples, ExId)),
    feature_ratio_class(AttrId, Class, Rest,
			case gb_trees:lookup(Value, Acc) of
			    {value, Classes} ->
				case lists:keytake(Class, 1, Classes) of
				    {value, {Class, Count}, List} ->
					gb_trees:enter(Value, [{Class, Count + 1}|List], Acc);
				    false ->
					gb_trees:enter(Value, [{Class, 1}|Classes], Acc)
				end;
			    none ->
				gb_trees:enter(Value, [{Class, 1}], Acc)
			end).

async_gain(AttrId, Examples, N) ->
    AttrLen = length(AttrId),
    Cores = erlang:system_info(schedulers),
    AttrSplits = util:split(AttrId, if Cores > AttrLen ->
					    AttrLen;
				       true ->
					    Cores
				    end, AttrLen),
    Me = self(),
    [spawn(?MODULE, async_feature_gain, [Me, AttrSplit, Examples, N]) ||
	AttrSplit <- AttrSplits],
    collect_gain(Me, length(AttrSplits), []).


async_feature_gain(Me, AttrIds, Examples, N) ->
    Me ! {Me, self(), gain_ratio(AttrIds, Examples, N)}.

collect_gain(_, 0, Acc) ->
    Acc;
collect_gain(Me, N, Acc) ->
    receive
	{Me, _, L} ->
	    collect_gain(Me, N - 1, L ++ Acc)
    end.

gain(async, AttrId, Examples, Count) ->
    async_gain(AttrId, Examples, Count);
gain(sync, AttrId, Examples, Count) ->
    gain_ratio(AttrId, Examples, Count).


gain_ratio(AttrIds, Examples, Count) ->
    gain_ratio(AttrIds, Examples, Count, []).

gain_ratio([], _, _, Acc) ->
    Acc;
gain_ratio([AttrId|AttrIds], Examples, Count, Acc) ->
    gain_ratio(AttrIds, Examples, Count,
	       [{AttrId, feature_gain(AttrId, Examples, Count)}|Acc]).

feature_gain(AttrId, Examples, Count) ->
    Ratios = feature_ratio(AttrId, Examples),
    G = stat:gain(Ratios, Count),
    Gi = stat:split_info(Ratios, Count),
    G / (Gi + 0.000000000001).

%% Determine wheter we should stop the induction of the tree
%% Input:
%%   - I: The instance set
%% Output
%%   - {majority, MajorityClass} or dont_stop
stop_induce([], Examples) ->
    {majority, majority(Examples)};
stop_induce(_, Examples) ->
    Count = [V || {_, V, _} <- Examples],
    N = lists:sum(Count),
    case lists:filter(fun ({_, C}) -> C / N == 1 end, [{Cl, Nc} || {Cl, Nc,_} <- Examples]) of
	[] -> {dont_stop, N};
	[{X,_}|_] -> {majority, X}
    end.


test() ->
    ets:new(examples, [named_table, set, {read_concurrency, true}]),
    ets:new(attributes, [named_table, set, {read_concurrency, true}]),
    {Types, Examples} = load("../data/connect-4_erl.data"),
    {Time, _} = timer:tc(?MODULE, split, [2, Examples]),
    io:format("Types: ~p~n", [Types]),
    io:format("Split: ~p~n", [Time]),

    {Time0, Ratio} = timer:tc(?MODULE, feature_ratio, [2, Examples]),
    io:format("Ratio: ~p ~p~n", [Time0, Ratio]),

    {Time1, Gains} = timer:tc(?MODULE, gain, 
			      [sync, Types, Examples,
			       lists:sum([C || {_, C, _} <- Examples])]),
    io:format("Gains: ~p ~p ~n", [Time1, Gains]),


    {Time2, Gains0} = timer:tc(?MODULE, gain, 
			      [async, Types, Examples,
			       lists:sum([C || {_, C, _} <- Examples])]),
    io:format("AGain: ~p ~p ~n", [Time2, Gains0]),

    



    ets:delete(attributes),
    ets:delete(examples).
    
