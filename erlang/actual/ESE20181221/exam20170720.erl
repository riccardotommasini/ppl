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
 	[spawn(?MODULE, dlist, [0]) || _ <- lists:seq(1,N)].


dlist(V) ->
	receive
		{get,P} -> 
			P ! V, 
			dlist(V);
		{set,V} -> dlist(V)
	end.


dlist_to_list(D) -> dlist_to_list1(D,[]).

dlist_to_list1([],L) -> L;
dlist_to_list1([P|Ps],L) ->
	P ! {get,self()},
	receive
		V -> dlist_to_list(Ps, L ++ [V])
	end

dmap([],_) -> ok;
dmap([P|Ps],Fun) ->
	P ! {get,self()},
	receive	
		V -> P ! {set, Fun(V)}, dmap(Ps,Fun)
	end



% --- 

dlinkedlist(V,Next) ->
	receive
		{get,P} -> 
			P ! {V,Next}, 
			dlist(V);
		{set,V} -> dlist(V)
	end.

create_dlinkedlist(N) -> create_dlinkedlist1(N-1,none)/

create_dlinkedlist1(0) -> spawn(?MODULE, dlinkedlist, [0,last]).
create_dlinkedlist1(N,Prev) ->
	P = spawn(?MODULE, dlinkedlist, [0,Prev])
	create_dlinkedlist(N-1,P).

dmap1(D,Fun) ->
	D ! {get, self()},
	receive	
		{V,none} -> P ! {set, Fun(V)}, 
		{V,Next} -> P ! {set, Fun(V)}, 
		dmap1(Next,Fun)
	end


