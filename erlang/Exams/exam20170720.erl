-module(exam20170720).
-compile(export_all).

% We want to define a “dynamic list” data structure, where each element of the list is an actor storing a value. Such value can be of
% course read and set, and each get/set operation on the list can be performed in parallel.
% 1) Define create_dlist, which takes a number n and returns a dynamic list of length n. You can assume that each element store the
% value 0 at start.
% 2) Define the function dlist_to_list, which takes a dynamic list and returns a list of the contained values. NBBB ORDER!!!!
% 2) Define a map for dynamic list. Of course this operation has side effects, since it changes the content of the list.

create_dlist(0) -> [];
create_dlist(N) -> [spawn(fun() -> store(N,N) end) || N <- lists:seq(0,N)].

store(N,V) ->
	receive
		{get, P} -> P! {self(), N, V}, store(N,V);
		{put, V1} -> store(N,V1)
	end.

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

apply1(X, F) ->
	X ! {get, self()},
	receive
		{X, N, V} ->  X ! {put, F(V)}
	end.

%%Retain Order!!!
dmap([], _) -> ok;
dmap([X|Xs], F) ->
	apply1(X, F),
	dmap(Xs,F).

dlist_to_list([]) -> [];
dlist_to_list([X|XS]) -> 
	X ! {get, self()},
	L = dlist_to_list(XS),
	receive
		{X, N, V} -> [V] ++ L
	end.
	
main2() ->
	[X|XS] = create_dlist(1),
	X ! {put, -1},
	dlist_to_list([X|XS]).

main3() ->
	L = create_dlist(10),
	L1 = dlist_to_list(L),
	dmap(L, fun(X) -> X+1 end),
	L2 = dlist_to_list(L),
	{L1, L2}.



