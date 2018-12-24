-module(exam20170705).
-compile(export_all).

% Define an **activate** function, which takes as input:

% a binary tree stored with tuples, e.g. {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}}.
% a binary function F 
% then **activate** creates a network of actors having the same structure of the given tree.

% Actors corresponding to leaves run a function called **leafy**, 
% and answer messages {ask, P} where P is a process, with 
% the pair {self(), V}, where V is the value stored in the leaf, then terminate.

% Actors for the branches run a function called **branchy**,
% if a branch actor receives an {ask, P} request it forward it to both its leaves, and 
% when a branch actor receives the answers, they call 
% F on the obtained values, then send the result V to P as
% {self(), V} and terminate.

% E.g. running the following code:
% test() ->
%  T1 = {branch, 
%		{branch, 
%			{leaf, 1}, 
%				{leaf, 2}}, 
%		{leaf, 3}},
%  A1 = activate(T1, fun min/2),
%  A1 ! {ask, self()},
%  receive
%  {A1, V} -> V
%  end.
% should return 1.

activate({leaf,V},_) ->
	io:format("spawn a leaf ~n"),
	spawn(?MODULE, leafy, [V]);
activate({branch,L,R}, Fun) ->
	io:format("activate, branch~n"),
	Tl = activate(L,Fun),
	Tr = activate(R,Fun),	
	spawn(?MODULE, branchy, [Fun,Tl,Tr,false, none]).
leafy(V) ->
	io:format("leafy ~p~n",[self()]),
	receive
		{ask, P} ->
			io:format("sending the leaf value~n"),
			P ! {self(), V}
	end.

branchy(F,L,R,Go,P) ->
	io:format("branchy ~p ~p ~p ~p ~n",[self(), L, R, P]),
	receive
		{ask, P1} ->
			io:format("forwarding to leaves ~p ~p ~n", [L,R]),
			L ! {ask, self()},
			R ! {ask, self()},
			branchy(F,L,R,Go,P1);
		{L,V} ->
			case Go of
				true -> P ! {self(), F(V,R)};
				false -> branchy(F,V,R,true,P)
			end;
		{R,V} ->
			case Go of
				true -> P ! {self(), F(L,V)};
				false -> branchy(F,L,V,true,P)
			end;
		M ->
			io:format("Debug~w~n",[M])
	end.
	

main() ->
	T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
 	A1 = activate(T1, fun(X,Y) -> X+Y end),
 	io:format("asking ~p~n", [A1]),
  	A1 ! {ask, self()},
  	receive
  		{A1, V} -> 
  			io:format("value is ~w~n", [V])
  	end.
