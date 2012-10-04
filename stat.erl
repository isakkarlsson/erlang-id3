-module(stat).
-compile(export_all).


split_info(Ratios, N) ->
    -1 * lists:sum([(Si/N) * (math:log(Si/N)/math:log(2)) || Si <- [lists:sum([C || {_, C} <- CDist]) || 
								     {_, CDist} <- Ratios]]).
gain(Ratios, N) ->
    lists:sum([(lists:sum([C || {_, C} <- CDist])/N) * entropy(CDist) || {_,CDist} <- Ratios]).
    
    
%% Calculate the entropy of V
%% Input:
%%   - V: List of tuples {type, Count}
%% Output:
%%   - The entropy for vector V
entropy([]) ->
    0;
entropy(Classes) ->
    CCount = [X || {_, X} <- Classes],
    Total = lists:sum(CCount),
    -1 * lists:sum([case C of 
		   0 -> 0;
		   X -> X / Total * (math:log(X / Total)/math:log(2))
	       end || C <- CCount]).    

