-module(pubsub).
-export([setup/0, start/0, start_clients/1, server/0, client/1, manage_clients/0]).

setup() ->
	server ! {spread, haskell, "So elegant"},
	server ! {spread, scheme, "So macro"},
	server ! {spread, erlang, "So concurrent"},
	ok.	

start() ->
	Pid = spawn(?MODULE, server, []),
	register(server, Pid),
	spawn(?MODULE, manage_clients, []).

server() -> 
	Map = #{ erlang => [], haskell => [], scheme => []},
	server_loop(Map, Map).

server_loop(Topics, Messages) ->
	receive
		{subscribe, Topic, Pid} ->
			io:format("Registering ~w for topic ~w~n",[Pid, Topic]),
			#{Topic := Pids} = Topics, %access
			server_loop(Topics#{Topic := [Pid|Pids]}, Messages); % Insert. NB: Maps are immutable
		{spread, Topic, Msg} ->
			#{Topic := Pids} = Topics, %access processes interested in Topic
			#{Topic := TopicMessages} = Messages, %access messages about Topic
			[P ! {publish, Topic, Msg} || P <- Pids],
			server_loop(Topics, Messages#{Topic := [Msg|TopicMessages]});
		{update, Topic, Pid} ->
			#{Topic := TopicMessages} = Messages, %access messages about Topic
			Last = last(TopicMessages),
			io:format("Reposting ~s about ~w~n", [Last,Topic]),
			Pid ! {repost, Topic, Last},
			server_loop(Topics, Messages);
		Test -> %Good practice to avoid message queuing
			io:format("Testing ~s~n", [Test]),
			server_loop(Topics, Messages)
	end.


last([]) -> "No Messages";
last([T|_]) -> T.

manage_clients() ->
	start_clients([{3, erlang}, {3, haskell},{3, scheme}]),
	handle_error().

start_clients([]) -> ok;
start_clients([T|L]) ->
	{N,Topic} = T,
	io:format("~w~n",[Topic]),
	[spawn_link(?MODULE, client, [Topic]) || _ <- lists:seq(1,N)],
	start_clients(L).

handle_error() ->
	process_flag(trap_exit, true),
	receive
		{'EXIT', Pid, {TopicString,_}} ->
			io:format("Process ~p died on ~s~n", [Pid, TopicString]),
			handle_error()
	end.


client(T) ->
	server ! {subscribe, T, self()},
	client_loop(T).

client_loop(T) ->
	receive
		{publish, Topic, Msg} ->
			io:format("~w: I received a message about ~w: ~s~n",[self(), Topic, Msg]),
			client_loop(T);
		{repost, Topic, Msg} ->
			io:format("~w: The last message about ~w: ~s~n",[self(), Topic, Msg]),
			client_loop(T)
		after 10000 ->
			Chance = rand:uniform(10) < 5,
			if 
				Chance ->
					%io:format("~p: I'm dying~n", [self()]),
					error("I'm dying");
				true ->
					io:format("~p; what's up about ~w~n",[self(), T]),
					server ! {update, T, self()},
					client_loop(T)
			end
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

