-module(basics).
-compile(export_all).

% foldL(_, Acc, []) -> Acc;
% foldL(F, Acc, [H|T]) ->
% 	foldL(F, F(Acc, H) ,T).

% foldR(_, Acc, []) -> Acc;
% foldR(F, Acc, [H|T]) -> F(H,foldR(F, Acc, T)).

% filter(P, L) -> 
% 	lists:reverse(filter(P, L, [])).

% filter(_, [], AL) -> AL;
% filter(P, [H|T], AL) ->
% 	case P(H) of
% 		true -> filter(P, T, [H|AL]);
% 		false -> filter(P,T,AL)
% 	end.

G = fun(X) -> timer:sleep(10), io:format("~p~n", [X]) end.
[spawn(fun()-> G(X) end || X <- lists:seq(1,10))].