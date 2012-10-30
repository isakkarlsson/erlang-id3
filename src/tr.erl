%% Asynchronous decision tree inducer
%%
%% TODO: 
%%      * Fix $default_branches - change the classification algorithm
%%      * Improve handling of missing values (not default to 0 for numeric, or 
%%        create a separate branch for categoric)
%%      * ?
%% 
%% Author: Isak Karlsson (isak-kar@dsv.su.se)
%%
-module(tr).
-export([induce/5, induce_branch/7, start/0, run_experiment/6]).

-include("nodes.hrl").

-define(DATE, "2012-10-27").
-define(MAJOR_VERSION, 0).
-define(MINOR_VERSION, 1).
-define(REVISION, 1).

-define(INST, inst). % intended to be used for future changes to inst 
-define(MAX_DEPTH, 5). % max branch depth to parallelize
-define(GAIN, async).

%% Induce decision tree from Instances
%% Input:
%%   - From: Pid to process
%%   - Instances: Dictionary with instances
%% Output:
%%   - Decision tree
induce(Attributes, Examples, M, GainAsync, Early) ->
    Paralell = if M > 0 -> ?GAIN; true -> sync end,
    case ?INST:stop_induce(Paralell, Early, GainAsync, Attributes, Examples) of
	{majority, Class} -> % NOTE: Examples are pure (of one class only)
	    #node{type=classify, value=#classify{as=Class}};
	{induce, {categoric, {_, Attr, Splitted}}} ->
	    Branches = induce_branches(Paralell, Attributes -- [Attr], Splitted, M - 1, GainAsync, Early), % NOTE: Categoric values are depleted when splitted..
	    categoric_branch(Attr, Branches, most_popular_split(Splitted));
	{induce, {numeric, {_, Attr, Threshold, Splitted}}} ->
	    NewAttributes = if length(Splitted) == 1 -> % NOTE: but numeric are only if they result in one region (e.g. when all values are the same)
				    Attributes -- [Attr]; 
				true -> Attributes
			    end,
	    Branches = induce_branches(Paralell, NewAttributes, Splitted, M - 1, GainAsync, Early),
	    numeric_branch(Attr, Threshold, Branches, most_popular_split(Splitted))
    end.


categoric_branch(Attr, Branches, Popular) ->
     #node{type=compare, value=#compare{type=categoric, 
					feature=Attr,
					most_popular=Popular,
					branches=Branches}}.
numeric_branch(Attr, Threshold, Branches, Popular) ->
    #node{type=compare, value=#compare{type=numeric, 
				       feature={Attr, Threshold},
				       most_popular=Popular,
				       branches=Branches}}.

%% Finds the most popular branch, finding the branch that maximizes
%% the number of 
most_popular_split(S) ->
    most_popular_split(S, {'?', 0}).
most_popular_split([], {Max, _}) ->
    Max;
most_popular_split([{Pop, [{_, Num, _}|_]}|Rest], {OldPop, OldNum}) ->
    case Num > OldNum of
	true -> most_popular_split(Rest, {Pop, Num});
	false -> most_popular_split(Rest, {OldPop, OldNum})
    end.



%% Input:
%%    - Features: The features left
%%    - Splits: The Instance set splitted ad Feature
%% Output:
%%    - A list of Two tuples {SplittedValue, Branch}
induce_branches(Paralell, Features, Splits, M, GainAsync, Early) ->
    case Paralell of
	async ->
	    Me = self(),
	    Pids = [spawn_link(?MODULE, induce_branch, 
			       [Me, Sn, Value, Features, M, GainAsync, Early]) || {Value, Sn} <- Splits, Sn /= []],
	    
	    collect_branches(Me, Pids, []);
	sync ->
	    [{Value, induce(Features, Sn, 0, GainAsync, Early)} || {Value, Sn} <- Splits]
    end.

%% Induce a brance using Instances
%% Input:
%%    - From: Return result to me
%%    - Instances: The instance set
%%    - Value: The splitted value
%%    - Features: the features to split
%% Output:
%%    From ! {From, self(), {Value, NewBranch}}
induce_branch(From, Examples, Value, Attributes, M, GainAsync, Early) ->
    From ! {From, self(), {Value, induce(Attributes, Examples, M, GainAsync, Early)}}.

%% Collect branches induced by Pids
%% Input:
%%    - From: The one how created Pid
%%    - Pids: A list of Pids inducing branches
%%    - Acc: The accumulator
%% Output:
%%    - A list of two tuples {SplittedValue, Branch}
collect_branches(_, [], Acc) ->
    Acc;
collect_branches(From, Pids, Acc) ->
    receive
	{From, Pid, Node} ->
	    collect_branches(From, lists:delete(Pid, Pids), [Node|Acc])    
    end.

start() ->
    case init:get_argument(h) of
	{ok, _} ->
	    show_help(),
	    halt();
	_ ->
	    true
    end,
    ExamplesFile = case init:get_argument(i) of
	{ok, Files} ->
	    case Files of
		[[File]] ->
		    File;
		[_] ->
		    stdillegal("i"),
		    halt()
	    end;
	_ ->
	    stdwarn("Traning examples are required"),
	    halt()
    end,
    SplitSize = case init:get_argument(y) of
		    {ok, ArgSplit} ->
			case ArgSplit of
			    [[Split]] ->
				list_to_float(Split);
			    [_] ->
				stdillegal("y"),
				halt()
			end;
		    _ -> 
			0.66
    end,
    DepthSize = case init:get_argument(d) of
	{ok, ArgDepth} ->
	    case ArgDepth of
		[[Depth]] ->
		    list_to_integer(Depth);
		[_] ->
		    stdillegal("d"),
		    halt()
	    end;
	_ -> 
	    ?MAX_DEPTH
    end,
    GainAsync = case init:get_argument(ag) of
	{ok, ArgGainAsync} ->
	    case ArgGainAsync of
		[[]] ->
		    true;
		[_] ->
		    stdillegal("ag"),
		    halt()
	    end;
	_ -> 
	    false
    end,
    Early = case init:get_argument(p) of
	{ok, ArgStop} ->
	    case ArgStop of
		[[Stop]] ->
		    list_to_integer(Stop);
		[_] ->
		    stdillegal("p"),
		    halt()
	    end;
	_ -> 
	    ?MAX_DEPTH
    end,
    Output = case init:get_argument(o) of
	{ok, ArgOutput} ->
	    case ArgOutput of
		[[]] ->
		    true;
		[_] ->
		    stdillegal("o"),
		    halt()
	    end;
	_ -> 
	    false
    end,
    run_experiment(ExamplesFile, SplitSize, DepthSize, GainAsync, Early, Output),
    halt().

run_experiment(ExamplesFile, SplitRatio, Depth, GainAsync, Early, Output) ->
    ets:new(examples, [named_table, set]),
    ets:new(attributes, [named_table, set]),

    {Attr, Examples} = ?INST:load(ExamplesFile),
    {Test, Train} = ?INST:split_ds(Examples, SplitRatio),

    io:format(standard_error, "Running:
 * File: ~p
 * Split: ~p
 * Attributes: ~p
 * Examples: ~p (train), ~p (test)\n", [ExamplesFile, SplitRatio, length(Attr), 
					lists:sum([N || {_, N, _} <- Train]),
					lists:sum([N || {_, N, _} <- Test])]),

    
    Then = now(),
    Tree = induce(Attr, Train, Depth, GainAsync, Early),
    io:format(standard_error, "Took: ~p ~n", [timer:now_diff(erlang:now(), Then)/1000000]),
    if Output == true ->
	    io:format("~p\n", [Tree]);
       true -> ok
    end,
    
    Total = lists:foldl(fun({Actual, _, Ids}, Acc) ->
				lists:foldl(fun(Id, A) ->
						    Abut = ?INST:lookup(examples, Id),
						    Predict = ?INST:classify(Abut, Tree),
						    [Actual == Predict|A]
					    end, Acc, Ids)
			end, [], Test),
    True = [T || T <- Total, T == true],
    False = [F || F <- Total, F == false],
    io:format(standard_error, "Accuracy: ~p ~n", [length(True)/(length(True)+length(False))]),
    ets:delete(examples),
    ets:delete(attributes),
    ok.
    

stdwarn(Out) ->
    io:format(standard_error, " **** ~s **** ~n", [Out]),
    io:format(standard_error, "See tr -h for options ~n", []).

stdillegal(Arg) ->
    stdwarn(io_lib:format("Error: Missing argument to -~s", [Arg])).

show_help() ->
    io:format(standard_error, "~s", [show_information() ++
"

Example: tr -i <EXAMPLES> -y 0.66 -d 5 
         tr -i <EXAMPLES> -y 1 -d 0 

-y  [Percent training data] 
     A percentage of data that is used for
     training the model. The rest is used for validation.
     Default: 0.66
-d  [Maximum depth to paralellize]
     The depth in which the model is induced in paralell.
     Default: 5
-ag []
     Calculate gain in parallel even thought the MAX_DEPTH
     is reached (-d).
-o  []
     Output the tree model
-p  [N instances required to partition]
     Stop inducing a branch (and take the majority) when
     N is reached (for a given branch)
     Default: 10
"]).

show_information() ->
    io_lib:format("Decision tree inducer
Version (of ~s): ~p.~p.~p
All rights reserved Isak Karlsson 2012+",
		  [?DATE, ?MAJOR_VERSION, ?MINOR_VERSION, ?REVISION]).
		  
    
