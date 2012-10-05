% Representing data as a gb_trees with feature vectors
%

-module(inst).
-compile(export_all).
%% Return the features from Instances
%% Input:
%%   - Instances: Instance set
%% Output:
%%   - List of features
features(Instances) ->
    gb_trees:keys(gb_trees:delete(class, Instances)).

get(Feature, Instances) ->
    gb_trees:get(Feature, Instances).

%% Extract new instance sets where feature has a value in Values
%% Input:
%%   - Instances: The instance set
%%   - Feature: The feature to split
%%   - Values: A list of possible values
%% Output:
%%   - A list of tuples {Value, NewInstanceSet}
branches(Instances, Feature, Values) ->
    lists:map(fun (Value) ->
		      {Value, extract_features(Instances, Feature, Value)}
	      end, Values).


%% Find the possible split points
%% Input:
%%   - I: The instance set
%%   - F: The feature to split
%% Output:
%%   - the possible values to split on
possible_splits(I, F) ->
    lists:map(fun ({Value, _}) ->
		      Value
	      end, stat:occurences(dict:fetch(F, I))).

extract_branches(I, F) ->
    FF = gb_trees:get(F, I),
    gb_trees:to_list(extract_branches(gb_trees:delete(F, I), FF, gb_trees:empty())).

extract_branches(I, [F|Fr], Dict) ->
    NewI = util:gb_fold(fun (Key, Val, DDict) ->
			     gb_trees:insert(Key, tl(Val), DDict)
		     end, gb_trees:empty(), I),
    case gb_trees:lookup(F, Dict) of
	{value, DDict} ->
	    extract_branches(NewI, Fr, gb_trees:enter(F, 
						  util:gb_fold(fun (K, V, D) ->
								    gb_trees:enter(K, [hd(V) | case gb_trees:lookup(K, D) of
												 {value, Found} ->
												     Found;
												 none -> []
											     end], D)
							    end, DDict, I), Dict));
	none -> extract_branches(NewI, Fr, gb_trees:enter(F, 
						  util:gb_fold(fun (K, V, D) ->
								    gb_trees:enter(K, [hd(V) | case gb_trees:lookup(K, D) of
												 {value, Found} ->
												     Found;
												 none -> []
											     end], D)
							    end, gb_trees:empty(), I), Dict))
    end;
extract_branches(_, [], Acc) ->
    gb_trees:map(fun (_, D) ->
		     gb_trees:map(fun (_, L) ->
					  lists:reverse(L)
				  end, D)
		 end, Acc).

						   


%% Input:
%%   - I: The instance set
%%   - F: The feature
%%   - V: The value
%% Output:
%%   - A new instance set with only those instances that has value v for feature vector F - F
extract_features(I, F, V) ->
    FF = dict:fetch(F, I),
    extract_features(dict:erase(F, I), FF, V, dict:new()).
extract_features(I, [F|Fr], V, Acc) ->
    NewI = dict:fold(fun (Key, Val, Dict) ->
			     dict:store(Key, tl(Val), Dict)
		     end, dict:new(), I),
    case F == V of
	true ->
	    extract_features(NewI, Fr, V, 
			     dict:fold(fun (Key, Val, Dict) ->
					       dict:store(Key, [hd(Val)|
								case dict:find(Key, Dict) of
								    {ok, Found} ->
									Found;
								    error -> []
								end], Dict)
				       end, Acc, I));
	false ->
	    extract_features(NewI, Fr, V, Acc)
    end;
extract_features(_, [], _, Acc) -> 
    dict:map(fun (_, V) -> lists:reverse(V) end, Acc).


%% Return an instance set for the golfing case
golfing() ->
    Classes = [dont_play, dont_play, play, play, play, 
	       dont_play, play, dont_play, play, play, 
	       play, play, play, dont_play],
    F0 = [sunny, sunny, overcast, rain, rain, 
	  rain, overcast, sunny, sunny, rain, 
	  sunny, overcast, overcast, rain],
    F1 = [false, true, false, false, false, true, 
	  true, false, false, false, true, true, 
	  false, true],
    Dict0 = gb_trees:empty(),
    gb_trees:enter(windy, F1, gb_trees:enter(outlook, F0, gb_trees:enter(class, Classes, Dict0))).

%% Return an instance set for the stock case
stock() ->
    Classes = [down, down, down, down, down, up, up, up, up, up],
    F0 = [old, old, old, mid, mid, mid, mid, new, new, new],
    F1 = [yes, no, no, yes, yes, no, no, yes, no, no],
    F2 = [swr, swr, hwr, swr, hwr, hwr, swr, swr, hwr, swr],
    gb_trees:enter(age, lists:flatten([F0 || _ <- lists:seq(1, 1)]),
	      gb_trees:enter(competition, lists:flatten([F1 || _ <- lists:seq(1, 1)]), 
			 gb_trees:enter(type, lists:flatten([F2 || _ <- lists:seq(1, 1)]),
				    gb_trees:enter(class, lists:flatten([Classes || _ <- lists:seq(1, 1)]), dict:new())))).


%% Determine the most frequent occurence in A
%% Input:
%%   - A: List of term()
%% Output:
%%   - the most frequent term()
majority(A) ->
    {Ret, _} = util:max(occurences(A)),
    Ret.


%% Return the number of occurences in Attr vector
%% Input:
%%   - Vector with attributes
%% Output:
%%   - A list of tuples {Attr, Count}
occurences(Attr) ->
    occurences(Attr, gb_trees:empty()).
occurences([], Acc) ->
    gb_trees:to_list(Acc);
occurences([F|Ff], Acc) ->
    case gb_trees:lookup(F, Acc) of
	{value, V} ->
	    occurences(Ff, gb_trees:enter(F, V + 1, Acc));
	none ->
	    occurences(Ff, gb_trees:enter(F, 1, Acc))
    end.
is_pure(Attr) ->
    is_pure(hd(Attr), tl(Attr)).

is_pure(X, []) ->
    {true, X};
is_pure(H, [F|R]) when H == F->
    is_pure(F, R);
is_pure(_, [_|_]) ->
    false.



%% Determine the class ratio of feature vector F with regard to
%% class vector C
%% Input:
%%   - F: A feature vector
%% Output:
%%   - C: A Class vector
feature_ratio(F, C) ->
    gb_trees:to_list(gb_trees:map(fun (_, V) ->
				  gb_trees:to_list(V)
			  end, feature_ratio(F, C, gb_trees:empty()))).
feature_ratio([F|FRest], [C|CRest], Acc) ->
    case gb_trees:lookup(C, Acc) of
	{value, FDict} ->
	    case gb_trees:lookup(F, FDict) of
		{value, V} ->
		    feature_ratio(FRest, CRest, gb_trees:enter(C, gb_trees:enter(F, V + 1, FDict), Acc));
		none ->
		    feature_ratio(FRest, CRest, gb_trees:enter(C, gb_trees:enter(F, 1, FDict), Acc))
	    end;
	none ->
	    feature_ratio(FRest, CRest, gb_trees:enter(C, gb_trees:enter(F, 1, gb_trees:empty()), Acc))
    end;
feature_ratio([], [], Acc) ->
    Acc.

%% Calculate the gain for all feature vectors in F
%% Input:
%%   - I: Instance dictionary
%%   - F: List of feature vectors
%% Output:
%%   - A list of tuples with {feature, Gain}
gain_ratio(I, F) ->
    gain_ratio(I, F, []).
gain_ratio(_, [], Acc) ->
    Acc;
gain_ratio(I, [F|R], Acc) ->
    gain_ratio(I, R, case gb_trees:lookup(F, I) of
		   {value, Features} ->
		       Class = gb_trees:get(class, I),
		       [{F, feature_gain(Features, Class)}|Acc];
		   none ->
		       exit(using_non_existent_feature)
	       end).
%% Calculate the gain for a feature vector
%% Input:
%%  - F: Feature vector
%%  - C: Class vector
%% Output:
%%  - The gain for feature vector F with regard to class vector C
feature_gain(FV, CV) when length(FV) == length(CV) ->
    Ratios = feature_ratio(CV, FV),
    N = length(FV),
    io:format("Ratios: ~p, N=~p ~n", [Ratios, N]),
    G = stat:gain(Ratios, N),
    GI = stat:split_info(Ratios, N),
    G / (GI+0.00000000000001).
    

%%
%%
%%
%%
load(File) ->
    {_, Data} = file:consult(File),
    load(Data, gb_trees:empty()).
load([], Dict) ->
    Dict;
load([{K,V}|R], Dict) ->
    load(R, gb_trees:enter(K, V, Dict)).

