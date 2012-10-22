-module(inst).
-compile(export_all).

test(File) ->
    ets:new(examples, [named_table, set]),
    ets:new(attributes, [named_table, set]),
    {Attr, Data} = load(File),

    io:format("Example: ~p ~n", [feature_ratio(hd(Attr), Data)]),
    ets:delete(examples),
    ets:delete(attributes),
    Data.
    

load(File) ->
    Data = cvs:parse(file, File),
    [Types0|Data0] = Data,
    [Attr0|Examples0] = Data0,
    
    {ClassIdx, Types} = case lists:keytake(class, 1,
					   load_types(lists:map(
							fun (Type) -> 
								list_to_atom(Type) 
							end, Types0), 1)) of
			    {value, {_, Idx}, T} ->
				{Idx, T};
			    false ->
				throw({error, no_class})
			end,

    Examples = load_examples(Examples0, 0, ClassIdx, Types, gb_trees:empty()),

    % [{nominal, pos}, ....]
    % but inserts attributes to ets:dict with key pos
    Attributes = load_attributes(Attr0, Types, Examples, []),
    {Attributes, Examples}.

load_attributes([], _, _, Acc) ->
    lists:reverse(Acc);
load_attributes([A|Attrs], [{categoric, Id}|Types], Examples, Acc) ->
    ets:insert(attributes, {Id, A}),
    load_attributes(Attrs, Types, Examples, [{categoric, Id}|Acc]);
load_attributes([A|Attrs], [{numeric, Id}|Types], Examples, Acc) ->
    Sorted = sort_examples_by(Id, Examples),
    ets:insert(attributes, {Id, A, Sorted}),
    load_attributes(Attrs, Types, Examples, [{numeric, Id}|Acc]);
load_attributes(Attrs, Types, Examples, Acc) ->
    load_attributes(tl(Attrs), Types, Examples, Acc).

sort_examples_by(_, []) ->
    [];
sort_examples_by(Id, Examples) ->
    All = take_examples_with(Id, Examples),
    lists:keysort(1, All).

take_examples_with(_, []) ->
    [];
take_examples_with(Id, Examples) ->
    lists:foldl(fun ({Class, _, ExIds}, List) ->
			lists:foldl(fun (ExId, Out) ->
					    [{element(Id, lookup(examples, ExId)), ExId}|Out]
				    end, List, ExIds)
		end, [], Examples).


load_examples([], _, _, _, Examples) ->
    lists:map(fun ({C, {N, L}}) -> 
		      {C, N, L} % Class, NumerOfOccurences, Ids
	      end, gb_trees:to_list(Examples));
load_examples([Inst|Rest], N, ClassIdx, Types, Examples) ->
    Class = list_to_atom(lists:nth(ClassIdx, Inst)), % NOTE: not optimal
    Tmp = remove_nth(ClassIdx, Inst),
    ets:insert(examples, {N, format_attributes(Types, Tmp, [])}),
    load_examples(Rest, N + 1, ClassIdx, Types, 
		      case gb_trees:lookup(Class, Examples) of
			  {value, {Num, Ids}} ->
			      gb_trees:enter(Class, {Num + 1, [N|Ids]}, Examples);
			  none ->
			      gb_trees:enter(Class, {1, [N]}, Examples)
		      end).

format_attributes([], [], Acc) ->
    list_to_tuple(lists:reverse(Acc));
format_attributes([{numeric, _}|Types], [A|Attrs], Acc) ->
    Number = case is_numeric(A) of
		 {true, N} ->
		     N;
		 false ->
		     list_to_atom(A)
	     end,
    format_attributes(Types, Attrs, [Number|Acc]);
format_attributes([_|Types], ["?"|Attrs], Acc) ->
    format_attributes(Types, Attrs, ['?'|Acc]);
format_attributes([{categoric,_}|Types], [A|Attrs], Acc) ->
    format_attributes(Types, Attrs, [list_to_atom(A)|Acc]).


    
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

remove_nth(N, List) ->
  {L1, [_|L2]} = lists:split(N-1, List),
  L1 ++ L2.

load_types(Types, N) ->
    load_types(Types, N, []).

load_types([], _, Acc) ->
    lists:reverse(Acc);
load_types([Type|Types], N, Acc) when Type =:= class; 
				      Type =:= categoric;
				      Type =:= numeric ->
    load_types(Types, N + 1, [{Type, N}|Acc]);
load_types([T|_], _, _) ->
    throw({error, invalid_type, T}).


lookup(examples, Id) ->
    [{Id, Out}|_] = ets:lookup(examples, Id),
    Out;
lookup(attributes, Id) ->
    case ets:lookup(attributes, Id) of
	[{Id, Name, Sorted}|_] ->
	    {Name, Sorted};
	[{Id, Name}|_] ->
	    Name
    end.


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
			

split(Attr, Examples) ->
    case Attr of
	{{numeric, _}, _} ->
	    split_numeric(Attr, Examples, [{'<', []}, {'>=', []}]);
	{categoric, _} ->
	    split_categoric(Attr, Examples, gb_trees:empty());
	_ -> throw({error, invalid_attribute, Attr})
    end.

split_categoric(_, [], Acc) ->
    gb_trees:to_list(Acc);
split_categoric(AttrId, [{Class, _, ExampleIds}|Examples], Acc) ->
    split_categoric(AttrId, Examples, split_class(AttrId, Class, ExampleIds, Acc)).

split_numeric(_, [], Acc) ->
    Acc;
split_numeric(AttrId, [{Class, _, ExampleIds}|Examples], Acc) ->
    split_numeric(AttrId, Examples, split_class(AttrId, Class, ExampleIds, Acc)).

split_class(_, _, [], Acc) ->
    Acc;
split_class({categoric, AttrId} = Attr, Class, [Ex|Examples], Acc) ->
    Value = element(AttrId, lookup(examples, Ex)),
    split_class(Attr, Class, Examples, 
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
		end);
split_class({{numeric, AttrId}, Treshold} = Attr, Class, [Ex|Examples], Acc) ->
    Value = element(AttrId, lookup(examples, Ex)),
    split_class(Attr, Class, Examples, case Value < Treshold of
					   true ->
					       [{Lt, Left}, Right] = Acc,
					       case lists:keytake(Class, 1, Left) of
						   {value, {Class, Num, ExList}, ClassRest} ->
						       [{Lt, [{Class, Num + 1, [Ex|ExList]}|ClassRest]}, Right];
						   false ->
						       [{Lt, [{Class, 1, [Ex]}|Left]}, Right]
					       end;
					   false ->
					       [Left, {Gt, Right}] = Acc,
					       case lists:keytake(Class, 1, Right) of
						   {value, {Class, Num, ExList}, ClassRest} ->
						       [Left, {Gt, [{Class, Num + 1, [Ex|ExList]}|ClassRest]}];
						   false ->
						       [Left, {Gt, [{Class, 1, [Ex]}|Right]}]
					       end
				       end).
					       
					       

	    
feature_ratio(Attr, Examples) ->
    feature_ratio_categoric(Attr, Examples, gb_trees:empty()).

feature_ratio_categoric(_, [], Acc) ->
    gb_trees:to_list(Acc);
feature_ratio_categoric(AttrId, [{Class, _, ExIds}|Examples], Acc) ->
    feature_ratio_categoric(AttrId, Examples, feature_ratio_class(AttrId, Class, ExIds, Acc)).

feature_ratio_class(_, _, [], Acc) ->
    Acc;
feature_ratio_class({categoric, AttrId} = Attr, Class, [ExId|Rest], Acc) ->
    Value = element(AttrId, lookup(examples, ExId)),
    feature_ratio_class(Attr, Class, Rest,
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
			end);
feature_ratio_class({{numeric, AttrId}, Treshold} = Attr, Class, [Ex|Examples], Acc) ->
    Value = element(AttrId, lookup(examples, Ex)),
    feature_ratio_class(Attr, Class, Examples, case Value < Treshold of
					   true ->
					       [{Lt, Left}, Right] = Acc,
					       case lists:keytake(Class, 1, Left) of
						   {value, {Class, Num}, ClassRest} ->
						       [{Lt, [[{Class, Num + 1}|ClassRest]|Left]}, Right];
						   false ->
						       [{Lt, [[{Class, 1}]|Left]}, Right]
					       end;
					   false ->
					       [Left, {Gt, Right}] = Acc,
					       case lists:keytake(Class, 1, Right) of
						   {value, {Class, Num}, ClassRest} ->
						       [Left, {Gt, [[{Class, Num + 1}|ClassRest]|Right]}];
						   false ->
						       [Left, {Gt, [[{Class, 1}]|Right]}]
					       end
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
	       [feature_gain(AttrId, Examples, Count)|Acc]).

feature_gain({categoric, AttrId} = Attr, Examples, Count) ->
    Ratios = split(Attr, Examples),
    G = stat:gain(Ratios, Count),
    Gi = stat:split_info(Ratios, Count),
    {G / (Gi + 0.000000000001), AttrId, Ratios};
feature_gain({numeric, AttrId}, Examples, Count) ->
    {_, Sorted} = lookup(attributes, AttrId),
    evaluate_numeric_split(AttrId, Examples, Count).


evaluate_numeric_split(AttrId, Examples, Count) ->
    [First|ExampleIds] = 
	lists:keysort(1, lists:foldl(fun({Class, _, ExIds}, NewExIds) ->
					     lists:foldl(fun(ExId, NewExIds1) ->
								 [{element(AttrId, lookup(examples, ExId)), Class}|NewExIds1]
							 end, NewExIds, ExIds)
				     end, [], Examples)),
    evaluate_number_split(ExampleIds, First, AttrId, Examples, [], 0, 1, Count).

evaluate_number_split([], _, _, AttrId, Split, Threshold, Gain, _) ->
    {Gain, Threshold, AttrId, Split};
evaluate_number_split([{Value, Class}|Rest], {PrevValue, PrevClass}, AttrId, Examples, OldSplit, OldThreshold, OldGain, Count) ->
    case Class == PrevClass of
	true ->
	    evaluate_number_split(Rest, {Value, Class}, AttrId, Examples, OldSplit, OldThreshold, OldGain, Count);
	false ->
	    Threshold = (Value + PrevValue) / 2,
	    Ratios = split({{numeric, AttrId}, Threshold}, Examples),
	    G = stat:gain(Ratios, Count),
	    Gi = stat:split_info(Ratios, Count),
	    Gain = (G / (Gi + 0.000000000001)),
	    {NewThreshold, NewGain, NewSplit} = case OldGain <  Gain of
						    true ->
							{Threshold, Gain, Ratios};
						    false ->
							{OldThreshold, OldGain, OldSplit}
				     end,
	    evaluate_number_split(Rest, {Value, Class}, AttrId, Examples, NewSplit, NewThreshold, NewGain, Count)
    end.
		    
	    
    

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
    {Types, Examples} = load("../data/iris.txt"),

    {Time1, Gains} = timer:tc(?MODULE, gain, 
			      [sync, Types, Examples,
			       lists:sum([C || {_, C, _} <- Examples])]),
    io:format("Gains: ~p ~p ~n", [Time1, []]),


    {Time2, Gains0} = timer:tc(?MODULE, gain, 
			      [async, Types, Examples,
			       lists:sum([C || {_, C, _} <- Examples])]),
    io:format("AGain: ~p ~p ~n", [Time2, []]),

    



    ets:delete(attributes),
    ets:delete(examples).
    


%%
%% attributes = [{AttrId, Name, SortedListOfEx={Value, ExId}}, ...]
%% 
%%
