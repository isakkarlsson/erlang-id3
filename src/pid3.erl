%% Asynchronous decision tree inducer based on ID3
%%
%% Todo:
%%    - allow for numerical attributes (base on c4.5)
%%    - etc.
-module(pid3).
-export([induce/1, induce_branch/4]).

-include("nodes.hrl").

-define(INST, rinst). % intended to be used for future changes to inst 

%% Induce decision tree from Instances
%% Input:
%%   - From: Pid to process
%%   - Instances: Dictionary with instances
%% Output:
%%   - Decision tree
induce(Instances) ->
    induce(Instances, ?INST:features(Instances)).
induce(Instances, Features) ->
    case ?INST:stop_induce(Instances, Features) of
	{majority, Class} ->
	    #node{type=classify, value=#classify{as=Class}};
	dont_stop ->
	    {F, _} = util:min(?INST:gain_ratio(Features, Instances)),
	    S = ?INST:extract_branches(F, Instances),
	    Branches = induce_branches(Features, F, S),
	    #node{type=compare, value=#compare{type=nominal, feature=F, branches=Branches}}
    end.

%% Induce brances for Split
%% Input:
%%    - Features: The features left
%%    - Splits: The Instance set splitted ad Feature
%% Output:
%%    - A list of Two tuples {SplittedValue, Branch}
induce_branches(Features, Feature, Splits) ->
    Me = self(),
    Pids = [spawn_link(?MODULE, induce_branch, 
		       [Me, Sn, Value, lists:delete(Feature, Features)]) || {Value, Sn} <- Splits],
   
    collect_branches(Me, Pids, []).

%% Induce a brance using Instances
%% Input:
%%    - From: Return result to me
%%    - Instances: The instance set
%%    - Value: The splitted value
%%    - Features: the features to split
%% Output:
%%    From ! {From, self(), {Value, NewBranch}}
induce_branch(From, Instances, Value, Features) ->
    From ! {From, self(), {Value, induce(Instances, Features)}}.

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
