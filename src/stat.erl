-module(stat).
-compile(export_all).

log2(V) ->
    math:log(V)/math:log(2).

split_info(Ratios, N) ->
    -1 * lists:sum([(Si/N) * (math:log(Si/N)/math:log(2)) 
		    || Si <- [lists:sum([C || {_, C} <- CDist]) ||  {_, CDist} <- Ratios]]).
gain(Ratios, N) ->
    lists:sum([(lists:sum([C || {_, C} <- CDist])/N) * entropy(CDist, N) || {_,CDist} <- Ratios]).
    
    
%% Calculate the entropy of V
%% Input:
%%   - V: List of tuples {type, Count}
%% Output:
%%   - The entropy for vector V
entropy([], _) ->
    0;
entropy(Classes, N) ->
    CCount = [X || {_, X} <- Classes],
    -1 * lists:sum([case C of 
		   0 -> 0;
		   X -> X / N * (math:log(X / N)/math:log(2))
	       end || C <- CCount]).    

