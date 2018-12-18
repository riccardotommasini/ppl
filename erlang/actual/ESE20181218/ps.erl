-module(ps).
-compile(export_all).

main() ->
	Broker = spawn(?MODULE, broker, []),
	register(broker, Broker).

start() ->
	spawn(?MODULE, start_consumers, []).

start_consumers() -> 
	[spawn(?MODULE, consumer, [haskell]) || _ <- lists:seq(1,3)],
	[spawn(?MODULE, consumer, [erlang]) || _ <- lists:seq(1,3)],
	[spawn(?MODULE, consumer, [scheme]) || _ <- lists:seq(1,3)],
	handle_errors().

handle_errors() ->
	process_flag(trap_exit, true),
	receive
		{'EXIT', PID, {Msg,_}} ->
			io:format("Process ~p died with message [~s]~n",[PID, Msg])
	end.

broker() ->
	Topics = #{ erlang => [], haskell => [], scheme => []},
	Messages = #{ erlang => [], haskell => [], scheme => []},
	broker_loop(Topics,Messages).

broker_loop(Topics, Messages) ->
	receive
		{subscribe, T, C} ->
			io:format("New subscription to topic ~w by ~p~n", [T,C]),
			#{T := CS} = Topics,
			broker_loop(Topics#{T := [C|CS]}, Messages);
		{publish, T, M} ->
			io:format("New message about~w~n", [T]),
			#{T := CS} = Topics,
			#{T := Msgs} = Messages,
			[C ! {publish, T, M} || C <- CS],
			broker_loop(Topics,Messages#{T := [M|Msgs]});	
		{help, Topic, C} ->
			#{Topic := [Last|_]} = Messages,
			C ! {repub, Topic, Last},
			broker_loop(Topics,Messages)
	end.

consumer(Topic) ->
	broker ! {subscribe, Topic, self()},
	consumer_loop(Topic).

	
consumer_loop(Topic) ->
	receive
		{publish, Topic, M} ->
			io:format("New message received ~s~n", [M]),
			consumer_loop(Topic);
		{repub, Topic, M} ->
			io:format("Repost ~s~n",[M])
		after 10000 ->
			Chance = rand:uniform(0,100) < 5,
			if 
				Chance -> error("I'm Dying ~p",[self()]);
				true -> broker ! {help, Topic, self()}
			end
	end.

producer(Topic, Message) ->
	broker ! {publish, Topic, Message}.




