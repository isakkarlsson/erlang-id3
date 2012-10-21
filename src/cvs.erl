-module(cvs).
-compile(export_all).

parse(file, File) ->
    case file:open(File, [raw,read]) of
	{ok, IO} ->
	    parse(IO, []);
	_ ->
	    throw({error, file_dont_exisit})
    end;
parse(Io, Acc) ->
    case file:read_line(Io) of
	{ok, Line} ->
	    parse(Io, [parse_line(Line, [])| Acc]);
	eof ->
	    Acc;
	{error, Reason} ->
	    throw({error, Reason})
    end.

parse_line(Line, Acc) ->
    lists:reverse(parse_line(Line, [], Acc)).
parse_line([End], Str, Acc) ->
    Str0 = case End of
	       $\n ->
		   Str;
	       _ ->
		   [End|Str]
	   end,		   
    [lists:reverse(Str0)|Acc];
parse_line([$,|R], Str, Acc) ->
    parse_line(R, [], [lists:reverse(Str)|Acc]);
parse_line([I|R], Str, Acc) ->
    parse_line(R, [I|Str], Acc).

    
    
    
	    
	   
