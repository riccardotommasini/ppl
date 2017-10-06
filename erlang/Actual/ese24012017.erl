-module(ese24012017).
-compile(export_all).
%-export([add/2, hello/0, sum_n_greet/1, fact/1, bmi/2]).


add(X,Y) -> 
	X + Y.

hello() ->
	io:format("Hello World!~n").

sum_n_greet(X) ->
	add(X,2),
	hello().

greet(male, Name) ->
	io:format("Hello, Mr. ~s!~n", [Name]);
greet(female, Name) ->
	io:format("Hello, Mrs. ~s!~n", [Name]);
greet(_, Name) ->
	io:format("Hello, ~s!~n", [Name]).

caaar([_,_,T|_]) ->
		T.

equifun(X,X) ->
	true;
equifun(_,_) ->
	false.

formula(H,W) ->
	(math:pow(H/W,2)).

bmi(H,W) ->
	K = (W/(H*H)),
	bmi(K).
bmi(K) 
	when K < 18.5 ->
	skinny;
bmi(K) when K >= 18.5, K < 25.5 ->
	normal;
bmi(K) when K >= 25.5, K < 30.0 ->
	fat;
bmi(K) when K >= 30.0 ->
	truppette.


fact(0) -> 1;
fact(N) -> N*fact(N-1).

tfact(N) -> tfact(N,1).
tfact(0,Acc) -> Acc;
tfact(N,Acc) when N > 0 -> tfact(N-1,N*Acc).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
	{Smaller, Larger} = partition(Pivot, Rest, [],[]),
	quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_,[], Smaller, Larger) -> 
		{Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
	if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
	   H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
	end.

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
	lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
	++ [Pivot] ++
	lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).




one() -> 1.
two() -> 2.

addf(Fx, Fy) ->
	Fx() + Fy().


map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

base(A) ->
	B = A + 1,
	F = fun() -> A * B end,
	F().

PrepareAlarm = fun(Room) -> 
		io:format("Alarm Set in ~s.~n", [Room]), 
		fun() -> io:format("Alarm tripped in ~s.~n", [Room])

















