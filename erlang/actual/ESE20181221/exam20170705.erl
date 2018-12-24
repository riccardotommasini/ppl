-module(exam20170705).
-compile(export_all).

% Define an **activate** function, which takes as inputs:

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


leafy(V)->
	receive
		{ask,P} ->
			P ! {self(),V}
	end.

branchy(Fun,L,R,Ready,P) ->
	receive
		{ask,P1} ->
			L ! {ask, self()},
			R ! {ask, self()},
			branchy(Fun,L,R,Ready,P1);
		{L,V} -> 
			case Ready of
				true -> P ! {self(),Fun(V,R)};
				false -> branchy(Fun,V,R,true,P)
			end;
		{R,V} -> 
			case Ready of
				true -> P ! {self(),Fun(L,V)};
				false -> branchy(Fun,L,V,true,P)
			end;
		M -> io:format("Debug ~w~n",[M])
	end.

activate({leaf,V},_) ->
	spawn(?MODULE, leafy, [V]);
activate({branch,L,R}, Fun) ->
	A1 = activate(L,Fun),
	A2 = activate(R,Fun),
	spawn(?MODULE, branchy, [Fun,A1,A2,false,none]).


test() ->
 T1 = {branch, 
		{branch, 
				{leaf, 1}, 
				{leaf, 2}}, 
		{leaf,9}},
 A1 = activate(T1, fun min/2),
 A1 ! {ask, self()},
 receive
	{A1, V} -> V
 end.

