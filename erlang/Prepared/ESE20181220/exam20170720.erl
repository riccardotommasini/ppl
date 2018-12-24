-module(exam20170720).
-compile(export_all).

% We want to define a “dynamic list” data structure, where 
% each element of the list is an actor storing a value. 
% Such value can be read and set in parallel.

% 1) Define create_dlist, which takes a number n and 
%    returns a dynamic list of length n initialized to 0.
% 2) Define the function dlist_to_list, 
%    which takes a dynamic list and returns a list 
%    of the contained values. NBBB ORDER!!!!
% 3) Define a map for dynamic list. 
%    Of course this operation has side effects, 
%    since it changes the content of the list.


create_dlist(0) -> [];
create_dlist(N) ->
	[spawn(?MODULE, dlist, [I,0]) || I <- lists:seq(1,N)].

get(0,[P|_]) -> 
	P ! {get, self()},
	receive
		V -> V
	end;
get(I,[_|Ps]) -> get(I-1,Ps).

set(0,[P|_],V) -> P ! {set,V};
set(I,[_|Ps],V) -> set(I-1,Ps,V).

dlist(I,V) ->
	receive
		{set,V1} -> dlist(I,V1);
		{get, P} -> 
			P ! V, 
			dlist(I,V)
	end.

dlist_to_list([]) -> [];
dlist_to_list(D) -> d2l(D,[]).

d2l([],L) -> L;
d2l([P|Ps],L) ->
	P ! {get, self()},
	receive
		V -> d2l(Ps, L ++ [V])
	end.

dmap([],_) -> ok;
dmap([X|Xs], F) ->
	X ! {get, self()},
	receive
		V -> 
			X ! {set,F(V)},
			dmap(Xs,F)
	end.

dfoldl([],Acc,_) -> Acc;
dfoldl([X|Xs],Acc,F) ->
	X ! {get, self()},
	receive
		V -> dfoldl(Xs,F(Acc,V),F)
	end.

main() ->
	L = create_dlist(10),
	[set(N, L, N) || N <- lists:seq(0,9)],
	[get(N, L) || N <- lists:seq(0,9)],

	L1 = dlist_to_list(L),

	dmap(L, fun(X) -> X+1 end),
	
	L2 = dlist_to_list(L),

	{L1,L2}.



