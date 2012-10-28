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
-export([induce/2, induce_branch/5, start/0]).

-include("nodes.hrl").

-define(DATA, "2012-10-27").
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
induce(Attributes, Examples) ->
    induce(Attributes, Examples, ?MAX_DEPTH).
induce(Attributes, Examples, M) ->
    Paralell = if M > 0 -> ?GAIN; true -> sync end,
    case ?INST:stop_induce(Paralell, Attributes, Examples) of
	{majority, Class} -> % NOTE: Examples are pure (of one class only)
	    #node{type=classify, value=#classify{as=Class}};
	{induce, {categoric, {_, Attr, Splitted}}} ->
	    Branches = induce_branches(Paralell, Attributes -- [Attr], Splitted, M - 1), % NOTE: Categoric values are depleted when splitted..
	    
	    categoric_branch(Attr, Branches, most_popular_split(Splitted));
	{induce, {numeric, {_, Attr, Threshold, Splitted}}} ->
	    NewAttributes = if length(Splitted) == 1 -> % NOTE: but numeric are only if they result in one region (e.g. when all values are the same)
				    Attributes -- [Attr]; 
				true -> Attributes
			    end,
	    Branches = induce_branches(Paralell, NewAttributes, Splitted, M - 1),
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

find(Value, []) ->
    throw({error, cant_find, Value});
find(Value, [{Value, Branch}|_]) ->
    Branch;
find(Value, [{_, _}|Rest]) ->
    find(Value, Rest).


%% Finds the most popular branch, finding the branch that maximizes
%% the number of occurences
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
induce_branches(Paralell, Features, Splits, M) ->
    case Paralell of
	async ->
	    Me = self(),
	    Pids = [spawn_link(?MODULE, induce_branch, 
			       [Me, Sn, Value, Features, M]) || {Value, Sn} <- Splits, Sn /= []],
	    
	    collect_branches(Me, Pids, []);
	sync ->
	    [{Value, induce(Features, Sn, 0)} || {Value, Sn} <- Splits]
    end.

%% Induce a brance using Instances
%% Input:
%%    - From: Return result to me
%%    - Instances: The instance set
%%    - Value: The splitted value
%%    - Features: the features to split
%% Output:
%%    From ! {From, self(), {Value, NewBranch}}
induce_branch(From, Examples, Value, Attributes, M) ->
    From ! {From, self(), {Value, induce(Attributes, Examples, M)}}.

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
    case init:get_argument(i) of
	{ok, Files} ->
	    case Files of
		[[File]] ->
		    ExampleFile = File,
		    true;
		
		[[]] ->
		    stdillegal("i"),
		    halt()
	    end;
	_ ->
	    true
    end,
    case init:get_argument(v) of
	{ok, ArgSplit} ->
	    case ArgSplit of
		[[Split]] ->
		    SplitSize = Split;
		[[]] ->
		    stdillegal("v"),
		    halt()
	    end
    end,
    halt().

stdwarn(Out) ->
    io:format(standard_error, " **** ~s **** ~n", [Out]),
    io:format(standard_error, "See tr -h for options ~n", []).

stdillegal(Arg) ->
    stdwarn(io_lib:format("Error: Missing argument to -~s", [Arg])).

show_help() ->
    io:format("Ioooooo").
