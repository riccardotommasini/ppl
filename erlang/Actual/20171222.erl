-module(class).
-compile(export_all).

% Consider a binary tree stored with tuples, 
% e.g. {branch, 
% 		{branch, {leaf, 1}, 
% 				 {leaf, 2}}, 
% 		{leaf, 3}}.
% Define an activate function, which takes a 
% binary tree and a binary function f and creates a network of actors having the same
% structure of the given tree. 

% Actors corresponding to leaves run a function called leafy, 
% that must be defined, 
% which answers to the message {ask, P} by sending to process P the pair {self(), V}, 
% where V is the value stored in the leaf, then terminate.

% Actors for the branches run a function called branchy, 
% that must be also defined: if they receive an {ask, P} request like that of
% leaves, they ask both their sons; 
% when they receive the answers, they call f on the obtained values, 
% then send the result V to P as {self(), V} and terminate.

%E.g. running the following code:
test() ->
	T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
	Root = activate(T1, fun min/2),
	io:format("Me~p~n",[self()]),
	Root ! {ask, self()},
 	receive
 		{Root, V} -> V
 	end.

activate({leaf, V}, _) -> spawn(fun() -> leafy(V) end);
activate({branch,L,R}, Fun) ->
	La = activate(L, Fun),
	Ra = activate(R, Fun),
	spawn(fun()-> branchy2(La,Ra,Fun, false, self()) end).


branchy2(L, R, F) ->
	receive
		{ask, P} -> 
			L ! {ask, self()},
			branchy2(L, R, fun(Vl, _) -> fun(_, Vr) 
				-> P ! {slef(), F(Vl, Vr)} end end);
		{L,Vl} -> R ! {ask, self()}, branchy2(L, R, F(Vl, none));
		{R,Vr} -> branchy2(L, R, F(none, Vr))
	end.

branchy(L, R, F, Ready, Parent) ->
	io:format("Daddy~p~n",[Parent]),
	receive
		{ask, P} -> 
			L ! {ask, self()},
			R ! {ask, self()},
			io:format("Stalker~p~n",[P]),
			branchy(L,R,F, Ready, P);
		{L,V} -> 
			case Ready of
				true -> Parent ! {self(), F(V, R)};
				false -> branchy(V, R, F, true, Parent)
			end;
		{R,V} -> 
			case Ready of
				true -> Parent ! {self(), F(L, V)};
				false -> branchy(L, V, F, true, Parent)
			end
	end.


leafy(V)->
	receive
		{ask, P} -> P ! {self(), V}
	end.


% We want to define a “dynamic list” data 
% structure, where each element of the 
% list is an actor storing a value. Such value can be of
% course read and set, and each get/set operation on 
%the list can be performed in parallel.
% 1) Define create_dlist, which takes a number 
% n and returns a dynamic list of length n. 
% You can assume that each element store the
%  value 0 at start.
% 2) Define the function dlist_to_list, 
%% which takes a dynamic list and returns a 
% list of the contained values. NBBB ORDER!!!!
% 3) Define a map for dynamic list. 
% Of course this operation has side effects, 
% since it changes the content of the list.


store(N) ->
	receive
		{merry, Santa} -> Santa ! {self(), N}, store(N);
		{gift, V} -> io:format("~p~n",[V]),store(V)
	end.

create_dlist(N) -> [spawn(fun() -> store(0) end) || _ <- lists:seq(0,(N-1))].

%% [<0.85.0>,<0.86.0>,<0.87.0>,<0.88.0>,<0.89.0>]
%% [1,2,3,4,5]
dlist_to_list([]) -> [];
dlist_to_list([GoodKid|Kids]) ->
	GoodKid ! {merry, self()},
	L = dlist_to_list(Kids),
	receive
		{GoodKid, V} -> [V] ++ L
	end.
	
xmas(GoodKid, Gnam) ->
	GoodKid ! {merry, self()},
	receive
		{GoodKid, Cookie} -> GoodKid ! {gift, Gnam(Cookie)}
	end.

dmap([], _) -> ok;
dmap(StantaList, Gnam) -> 
	[spawn(?MODULE, xmas, [GoodKid, Gnam]) || GoodKid <- StantaList].

main() ->
	L = create_dlist(10),
	L1=dlist_to_list(L),
	dmap(L, fun(X) -> X+1 end),
	L2=dlist_to_list(L),
	{L1,L2}.




