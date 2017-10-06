-module(ese24012017). %this is the name we'll use to call function from other modules as namespace:functioname(params..)
%-export([add/2, hello/0, greet_and_add_two/1, greet/2, car/1, caar/1, cdr/1, same/2, bmi/2, fac/1, tail_fac/1, len/1, tail_len/1, addf/2, one/0, two/0]).
-compile(export_all).
-import(math, [pow/2]).

add(A,B) ->
	A + B.

hello() ->
io:format("Hello, world!~n").

greet_and_add_two(X) ->
	hello(),
	add(X,2).

% function greet(Gender,Name)
% 	if Gender == male then
% 		print("Hello, Mr. %s!", Name)
% 	else if Gender == female then
% 		print("Hello, Mrs. %s!", Name)
% 	else
% 		print("Hello, %s!", Name)
% 	end

greet(male, Name) ->
	io:format("Hello, Mr. ~s ", [Name]);
greet(female, Name) ->
	io:format("Hello, Mrs. ~s ", [Name]); %<- note the semicolon
greet(_, Name) ->
	io:format("Hello, ~s ", [Name]).

%% FUNCTION

car([H|_]) -> H.

caar([_,S|_]) -> S.

cdr([_|T]) -> T.

same(X,X) -> % exploits patter matching to check equality. The first argument is assigned to the first X and
	true;    % then pattern-matched with the second one
same(_,_) ->
	false.

% GUARDS
%% local/imported function are avoided in guards due to side effects
%% a small set of function are permitted isa
% bmi :: (RealFloat a) => a -> a -> String
% bmi weight height
%     | formula < skinny = "Underweight!"
%     | formula <= ok = "Ok!"
%     | formula <= fat = "Fat!"
%     | otherwise = "American!"
%     where skinny = 18.5
%           ok = 25.0
%           fat = 30.0
%           formula = weight / height ^ 2
bmi(H,W) when (H/W)*(H/W) < 18.5
	-> skinny;
bmi(H,W) when (H/W)*(H/W) >= 18.5, (H/W)*(H/W)=< 25
	-> normal;
bmi(H,W) when (H/W)*(H/W) >= 25
	-> fat.

%If Expression, USE GUARDS

formula(H,W) -> pow((H/W),2).

%Case Expression, allows UDF

%% Recursion

fac(0) -> 1;
fac(N) -> N*fac(N-1).

tail_fac(N) -> tail_fac(N,1).
tail_fac(0,Acc) -> Acc; %% we create a local function that we do not expose
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc). %% and that sets a default value for the accumulator=0

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_len(N) -> tail_len(N,0).
tail_len([],Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,Acc+1).

% QuickSort

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
	{Smaller, Larger} = partition(Pivot,Rest,[],[]),
quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_,[], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
	if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
	   H >  Pivot -> partition(Pivot, T, Smaller, [H|Larger])
	end.

%%Using list comprehension <- check shortcomings
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
	lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
	++ [Pivot] ++
	lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).

%%List is traversed twice -> inefficient
%% user lists:sort/1

%%Higher order functions

one() -> 1. 
two() -> 2.
 
addf(X,Y) -> X() + Y().

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].
 
incr(X) -> X + 1.
decr(X) -> X - 1.

%Lambdas exploits the "fun" keyword at the beginning of an hh call
%Don't forget to close (end)

%Closures
%ese24012017:map(fun(X) -> X + 1 end, [1,2,3]).
% PrepareAlarm = fun(Room) -> io:format("Alarm set in ~s.~n", [Room]), %% (1)
%  fun() -> io:format("Alarm tripped in ~s! Call Batman!~n", [Room]) %%(2) end end.

%(1) is called once we set AlarmReady = PrepareAlarm("bathroom"). and returns a function that gets assigned to AlarmReady
%(2) is called once we call AlarmReady(). The value "Room" is local to that function and taken from (1).

%%Functions' Scope

base(A) ->
	B = A + 1,
	F = fun() -> A * B end,
	F().

%Inherited scopes follows anonymous function when are passed


a() ->
	Secret = "pony",
	fun() -> Secret end.
 
b(F) ->
	"a/0's password is " ++ F().

% c() -> % we can's redefine variable in inner scopes
% 	A = 1,
% 	(fun() -> A = 2 end)().

d() -> % we can redefine variable in inner scopes if passed as parameter
	A = 1,
	(fun(A) -> A = 2 end)(2).


%% Named Lambda 
% f(PrepareAlarm), f(AlarmReady). %free variables

% PrepareAlarm = fun(Room) ->
% 	io:format("Alarm set in ~s.~n",[Room]),
%     fun Loop() -> %doesn't remind you anything? (named let)
% 	    io:format("Alarm tripped in ~s! Call Batman!~n",[Room]),
% 	    timer:sleep(500),
% 		Loop()
% 	end
% end.

% Filter

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
	case Pred(H) of
		true  -> filter(Pred, T, [H|Acc]); % I leave T
		false -> filter(Pred, T, Acc) %I remove T
	end.

foldL(_, Start, []) -> Start;
foldL(F, Start, [H|T]) -> foldL(F, F(H,Start), T).

foldR(_, Start, []) -> Start;
foldR(F, Start, [H|[]]) -> F(H,Start);
foldR(F, Start, [H|T]) -> F(H,foldR(F, Start, T)).

reverseL(L) ->
	foldL(fun(X,Acc) -> [X|Acc] end, [], L).

reverseR(L) ->
	foldR(fun(X,Acc) -> Acc ++ [X] end, [], L).

map2(F,L) ->
	lists:reverse(foldL(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

map3(F,L) ->
	(foldR(fun(X,Acc) -> [F(X)|Acc] end, [], L)).
 
