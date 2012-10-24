%% Asynchronous decision tree inducer based on ID3
%%
%% Todo:
%%    - allow for numerical attributes (base on c4.5)
%%    - etc.
-module(pid3).
-export([induce/2, induce_branch/5, run/3, test/2, test/3, load/2]).

-include("nodes.hrl").

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
	{induce, {numeric, {_, _, _, [{_, Split}]}}} ->
	    #node{type=classify, value=#classify{as=inst:majority(Split)}};
	{induce, {categoric, {_, _, [{_, Split}]}}} ->
	    #node{type=classify, value=#classify{as=inst:majority(Split)}};
	{induce, {categoric, {Gain, Attr, Splitted}}} ->
	    Branches = case Paralell of
			   async -> induce_branches(Attributes -- [Attr], Splitted, M - 1); % NOTE: Categoric values are depleted when splitted
			   sync -> [{Value, induce(Attributes -- [Attr], Sn, 0)} || {Value, Sn} <- Splitted]
		       end,
	    #node{type=compare, value=#compare{type=categoric, 
					       feature=Attr, 
					       branches=Branches ++ [{'$default_branch', 
								      #node{type=classify, 
									    value=#classify{as=majority_split(Splitted)}}}]}}; % NOTE: ? should be majority
	{induce, {numeric, {Gain, Attr, Threshold, Splitted}}} ->
	    Branches = case Paralell of
			   async -> induce_branches(Attributes, Splitted, M - 1); % NOTE: but numeric are not
			   sync -> [{Value, induce(Attributes, Sn, 0)} || {Value, Sn} <- Splitted]
		       end,
	    #node{type=compare, value=#compare{type=numeric, 
					       feature={Attr, Threshold}, 
					       branches=Branches}} % NOTE: ? should be majority
	
    end.

majority_split(S) ->
    majority_split(S, {'?', 0, []}).
majority_split([], {Max, _, _}) ->
    Max;
majority_split([{_, [{Class, Num, _}|_]}|Rest], {OldClass, OldNum, _}) ->
    case Num > OldNum of
	true -> majority_split(Rest, {Class, Num, []});
	false -> majority_split(Rest, {OldClass, OldNum, []})
    end.



%% Induce brances for Split
%% Input:
%%    - Features: The features left
%%    - Splits: The Instance set splitted ad Feature
%% Output:
%%    - A list of Two tuples {SplittedValue, Branch}
induce_branches(Features, Splits, M) ->
    Me = self(),
    Pids = [spawn_link(?MODULE, induce_branch, 
		       [Me, Sn, Value, Features, M]) || {Value, Sn} <- Splits],
   
    collect_branches(Me, Pids, []).

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

run(file, File, N) ->
    Data = ?INST:load(File),
    run(data, Data, N);
run(data, {Attr, Examples}, N) ->
    {Test, Train} =  lists:split(round(length(Examples) * N), Examples),

    {Time, Model} = timer:tc(?MODULE, induce, [Attr, Train]),
    Result = lists:foldl(fun (Inst, Acc) -> 
				 [?INST:classify(Inst, Model) == gb_trees:get(class, Inst)|Acc] 
			 end, [], Test),
    Correct = length(lists:filter(fun (X) -> X end, Result)),
    Incorrect = length(lists:filter(fun (X) -> X == false end, Result)),
    {Time, Correct, Incorrect, Correct / (Incorrect + Correct), Result}.

test(data, Attributes, Examples) ->
    {Time, Tree} = timer:tc(?MODULE, induce, [Attributes, Examples]),
    {Time, Tree}.

load(file, File) ->
    ets:new(examples, [named_table, set, {read_concurrency, true}]),
    ets:new(attributes, [named_table, set, {read_concurrency, true}]),
    ?INST:load(File).

test(file, File) ->
    {Attributes, Examples} = load(file, File),
    {Time, Tree} = timer:tc(?MODULE, induce, [Attributes, Examples]),
    ets:delete(examples),
    ets:delete(attributes),
    {Time, Tree}.
