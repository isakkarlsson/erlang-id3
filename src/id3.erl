%% Non asynchronous decision tree inducer based on ID3
%%
-module(id3).
-export([induce/1]).

-include("nodes.hrl").
-define(INST, rinst).

%% Induce decision tree from Instances
%% Input:
%%   - Instances: Dictionary with instances
%% Output:
%%   - Decision tree model
induce(Instances) ->
    induce(Instances, ?INST:features(Instances)).
induce(Instances, Features) ->
    case ?INST:stop_induce(Instances, Features) of
	{majority, Class} ->
	    #node{type=classify, value=#classify{as=Class}};
	dont_stop ->
	    {F, _} = util:min(?INST:gain_ratio(Features, Instances)),
	    S = ?INST:extract_branches(F, Instances),
	    Branches = [{Value, induce(Sn, Features -- [F])} || {Value, Sn} <- S],
	    #node{type=compare, value=#compare{type=nominal, feature=F, branches=Branches}}
    end.

