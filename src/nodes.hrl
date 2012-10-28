%% type = leaf | numeric | nominal
%% value = classify | compare
-record(node, {type, value}).
-record(classify, {as}).
-record(compare, {type, feature, most_popular, branches}).
