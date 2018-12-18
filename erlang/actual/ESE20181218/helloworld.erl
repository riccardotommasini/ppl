-module(helloworld).
-compile(export_all).
-import(math, [pow/2]).


add(A,B) ->
	A + B.

greet(male, Name) ->
	io:format("Hello Mr. ~s~n", [Name]);
greet(female, Name) ->
	io:format("Hello Mrs. ~s~n", [Name]);
greet(_, Name) ->
	io:format("Hello ~s~n", [Name]).


factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).

tf(N) -> tf(N,1).

tf(0,Acc) -> Acc;
tf(N,Acc) when N < 0 -> bad;
tf(N,Acc) when N > 0 -> tf(N-1, Acc*N).


map(_,[]) -> [];
map(F,[H|B]) -> [F(H)|map(F,B)].

filter(_,[]) -> [];
filter(P,[H|B]) ->
	case P(H) of
		 	true -> [H|filter(P,B)];
		 	false -> filter(P,B)
	end.

foldL(_,Start,[]) -> Start;
foldL(F,Start,[H|B]) -> foldL(F, F(H,Start),B).

foldR(_,Start,[]) -> Start;
foldR(F,Start,[H|[]]) -> F(H,Start).
foldR(F,Start,[H|B]) -> F(H,foldR(F,Start,B)).















