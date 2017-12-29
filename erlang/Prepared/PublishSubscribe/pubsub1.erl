-module(pubsub1).
-compile(export_all).

% Publish Subscribe with actors
% in this version several clients communicate with one central server
% they subscribe once spawned about a topic
% and they receive all the messages about that topic
% to create a new message use the server {spread, Topic, Msg} from an external process
% E.g. the console

start() ->
	Pid = spawn(?MODULE, server, []),
	register(server, Pid),
	[spawn(?MODULE, client, [haskell]) || _ <- lists:seq(1,3)],
	[spawn(?MODULE, client, [erlang]) || _ <- lists:seq(1,3)],
	[spawn(?MODULE, client, [scheme]) || _ <- lists:seq(1,3)].

	%spawn(?MODULE, start_clients, []).

server() ->
	Map = #{ erlang => [], haskell => [], scheme => []},
	server_loop(Map, Map).

server_loop(Topics, Messages) ->
	receive
		{subscribe, Topic, Pid} -> % Selective receive only on the Topic T
			io:format("Registering ~w for topic ~w~n", [Pid, Topic]),
			#{Topic := Pids} = Topics,
			server_loop(Topics#{Topic := [Pid|Pids]}, Messages);
		{spread, Topic, Msg} ->
			#{Topic := Pids} = Topics,
			#{Topic := Msgs} = Messages,
			[P ! {publish, Topic, Msg} || P <- Pids ],
			server_loop(Topics, Messages#{Topic := [Msg|Msgs]})
	end.

client(T) -> 
	server ! {subscribe, T, self()},
	client_loop(T).

client_loop(T) ->
	receive
		{publish, T, Msg} ->
			io:format("I received this message ~s about ~w~n",[Msg,T]),
			client_loop(T)
	end.



