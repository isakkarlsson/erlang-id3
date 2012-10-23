-module(stat).
-compile(export_all).

log2(V) ->
    math:log(V)/math:log(2).

split_info(Ratios, N) ->
    -1 * lists:sum([(Si/N) * (math:log(Si/N)/math:log(2))
		    || Si <- [lists:sum([C || {_, C, _} <- CDist]) ||  {_, CDist} <- Ratios], Si > 0]).
gain(Ratios, N) ->
    ToSum = [lists:sum([C || {_, C, _} <- CDist])/N * entropy(CDist) || {_, CDist} <- Ratios],
    lists:sum(ToSum).
    
   
%% Calculate the entropy of V
%% Input:
%%   - V: List of tuples {type, Count}
%% Output:
%%   - The entropy for vector V
entropy([]) ->
    0;
entropy(Classes) ->
    CCount = count(Classes),
    N = lists:sum(CCount),
    entropy(CCount, N).

entropy(CCount, N) ->
    -1 * lists:sum([if C < 0 -> io:format("CCount: ~p ~n", [CCount]), throw({error});
		       C == 0 -> 0;
		       true -> C / N * log2(C / N)
		    end || C <- CCount]).

count(Dist) ->
    [X || {_, X, _} <- Dist].
