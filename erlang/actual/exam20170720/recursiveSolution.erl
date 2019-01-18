-module(exam20170720).
-compile(export_all).




create_dlist(0) -> [];
create_dlist(N) ->
	spawn(?MODULE, dlist, [N-1,none]).

dlist(0,none) ->
	dlist(0,finish);

dlist(N,none) ->
	Succ = spawn(?MODULE, dlist, [N-1, none]),
	dlist(0, Succ);
	
dlist(N, finish) ->
	receive
		{P, getvalues} -> 
			P ! {self(),[N | []]},
			dlist(N,finish);
		{P, mapfunc, Fun} ->
			P ! {self(),ok},
			dlist(Fun(N),finish)
	end;
	
dlist(Value, Next) -> 
	receive
		{P, getvalues} -> 
			Next ! {self(), getvalues},
			receive
				{Next,[V|VS]} ->
					P ! {self(),[Value | [V|VS]]]}
			end,
			dlist(Value, Next);
		{P, mapfunc, Fun} ->
			Next ! {self(), mapfunc, Fun},
			receive
				{Next, ok} ->
					P ! {self(),ok}
			end,
			dlist(Fun(Value),Next)
	end.
	

dlist_to_list(D) ->	
	D ! {self(),getvalues},
	receive
		{D, [V|VS]} -> [V|VS]
	end.

dmap(D, Fun) ->
	D ! {self(), mapfunc, Fun},
	receive
		{D, ok} -> ok
	end.
	
launch_dlist_to_list(D) ->
	spawn(?MODULE, dlist_to_list, [D]).
	
launch_dmap(D, Fun) ->
	spawn(?MODULE, dmap, [D, Fun]).
