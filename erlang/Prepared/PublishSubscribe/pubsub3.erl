-module(pubsub3).
-compile(export_all).

% Publish Subscribe with actors
% in this version several clients communicate with one central server
% they subscribe once spawned about a topic
% and they receive all the messages about that topic
% to create a new message use the server {spread, Topic, Msg} from an external process
% E.g. the console.

% Add the request of update by a client after a fixed amount of time
% since the last message received.
% Add message store and "repost" as an answer to the "update" message

converse() ->
	server ! {sbam, haskell, "So elegant"},
	server ! {sbam, erlang, "So concurrent"},
	server ! {sbam, scheme, "So Macro"}.

kill() ->

start() ->
	Server = spawn(?MODULE, server, []),
	register(server, Server),
	Manager = spawn(?MODULE, manage_clients, []),
	register(manager, Manager).

server() ->
	Map = #{ erlang => [], haskell => [], scheme => []},
	server_loop(Map, Map).

server_loop(Topics, Messages) ->
	receive
		{subscribe, Topic, Pid} ->
			io:format("Registering ~w for topic ~w~n", [Pid, Topic]),
			#{Topic := Pids} = Topics,
			server_loop(Topics#{Topic := [Pid|Pids]},Messages);
		{sbam, Topic, Msg} ->
			#{Topic := Pids} = Topics,
			#{Topic := TopicMessages} = Messages,
			NewMessages = Messages#{Topic := [Msg|TopicMessages]},
			[P ! {publish, Topic, Msg} || P <- Pids ],
			server_loop(Topics,NewMessages);
		{update,Topic,Pid} ->
			#{Topic := TopicMessages} = Messages,
			LaastMsg = last(TopicMessages),
			Pid ! {repost, Topic, LastMsg},
			server_loop(Topics, Messages)
	end.

last([]) -> "No Messages";
last([T|_]) -> T.

client(T) -> 
	server ! {subscribe, T, self()},
	client_loop(T).

client_loop(T) ->
	receive
		{publish, T, Msg} ->
			io:format("I received this message ~s about ~w~n",[Msg,T]),
			client_loop(T);
		{repost, T, Msg} ->
			io:format("I received this old message ~s about ~w~n",[Msg,T]),
			client_loop(T)
		after 10000 ->
			Chance = rand:uniform(9) < 5,
			if
				Chance -> error("I am dying");
				true -> server ! {update, T, self()},
						 client_loop(T)
			end
	end.

manage_clients() ->
	start_clients([{3,haskell}, {3, erlang}, {3, scheme}]),
	handle_errors().

start_clients([]) -> ok;
start_clients([{N,Topic}|L]) -> 
	[spawn_link(?MODULE, client, [Topic]) || _ <- lists:seq(1,N)],
	start_clients(L).

handle_errors() ->
	process_flag(trap_exit, true),
	receive
			{'EXIT', Pid, {Msg,_}} ->
				io:format("Process ~p [~s] died~n",[Pid,Msg]),
				handle_errors()
	end.


% %NOTES:
% 	the protocol is hided behind methods 

% server ! {spread, erlang, "So parallel"}.
% server ! {spread, scheme, "So macro"}.
% server ! {spread, haskell, "So elegant"}.

%possible edits
	% - handle state (received message in clients)
	% - allow multiple topic subscription
	% - define unregistering
	% - define error handling





