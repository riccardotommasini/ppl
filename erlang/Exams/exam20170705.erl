-module(exam20170705).
-compile(export_all).

% Consider a binary tree stored with tuples, e.g. {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}}.
% Define an activate function, which takes a 
%binary tree and a binary function f and creates a network of actors having the same
% structure of the given tree. Actors corresponding to leaves run a function called leafy, that must be defined, which answer to the
% message {ask, P} by sending to process P the pair {self(), V}, where V is the value stored in the leaf, then terminate.
% Actors for the branches run a function called branchy, that must be also defined: if they receive an {ask, P} request like that of
% leaves, they ask both their sons; when they receive the answers, they call f on the obtained values, then send the result V to P as
% {self(), V} and terminate.
% E.g. running the following code:
% test() ->
%  T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}},
%  A1 = activate(T1, fun min/2),
%  A1 ! {ask, self()},
%  receive
%  {A1, V} -> V
%  end.
% should return 1.


main() ->
	T1 = {branch, {branch, {leaf, 1}, {leaf, 2}}, {leaf, 3}} end.
 	% A1 = activate(T1, fun min/2 end),
  % 	A1 ! {ask, self()},
  % 	receive
  % 		{A1, V} -> V
  % 	end.

leafy(V) ->
	receive
		{ask, P} -> P ! {self(), V}
	end.
	
activate({leaf,v}, F) -> 
	Leaf = spawn(fun() -> leafy(V) end, 
	Leaf;
activate({branch, L, R}, F) -> 
	La = activate(L,F), 
	Ra = activate(R,F), 
	B = spawn(fun() -> branchy(La, Ra, F) end),
	B.




branchy(L, R, F) ->
	receive
		{ask, P} -> L ! {ask, self()}, R ! {ask, self()}, branchy(L, R, fun(X,Y) -> P ! {self(), F(X,Y)} end);
		{L, Vl} -> F(Vl);
		{R, Vr} -> F(Vr)
	end.