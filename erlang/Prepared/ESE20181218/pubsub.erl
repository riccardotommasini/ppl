-module(pubsub). 
-compile(export_all).

start() ->
    start_broker(),
    start_manager().

start_broker() ->
    Broker = spawn(?MODULE, broker, []),
    register(broker, Broker).

start_manager() ->
    spawn(?MODULE, start_consumers, []).
    %register(manager, Manager).

start_consumers() ->
    [spawn_link(?MODULE, consumer, [[haskell]]) || _ <- lists:seq(1,3)],
    [spawn_link(?MODULE, consumer, [[erlang]]) || _ <- lists:seq(1,3)],
    [spawn_link(?MODULE, consumer, [[scheme]]) || _ <- lists:seq(1,3)],
    handle_errors().

handle_errors() ->
    process_flag(trap_exit, true),
    receive
            {'EXIT', Pid, {Msg,_}} ->
                io:format("Process ~p died with message [~s]~n",[Pid,Msg]),
                handle_errors()
    end.

broker() ->
    Topics = #{ erlang => [], haskell => [], scheme => []},
    Messages = #{ erlang => [], haskell => [], scheme => []},
    broker_loop(Topics, Messages).

broker_loop(Topics, Messages) ->
    receive  % Selective receive the following messages
        {subscribe, Topic, Pid} -> % this message is sent by a consumer to subscribe to a topic
            io:format("Registering ~p for topic ~w~n", [Pid, Topic]),
            #{Topic := Pids} = Topics,
            broker_loop(Topics#{Topic := [Pid|Pids]}, Messages);
        {produce, Topic, Msg} -> % this message is sent by a producer to write about a topic
            io:format("Spreading ~s for topic ~w~n", [Msg, Topic]),
            #{Topic := Pids} = Topics,
            #{Topic := Msgs} = Messages,
            [P ! {publish, Topic, Msg} || P <- Pids ],
            broker_loop(Topics, Messages#{Topic := [Msg|Msgs]});
        {update, Topic, Pid} ->
            io:format("Requested Repost ~p for topic ~w~n", [Pid, Topic]),
            #{Topic := Msgs} = Messages,
            repost(Topic, Msgs, Pid),
            broker_loop(Topics, Messages)
    end.

repost(_,[],_) -> ok;
repost(Topic,[Msg|_],Pid) ->
    Pid ! {repost, Topic, Msg}.

consumer(T) -> 
    [broker ! {subscribe, X, self()} || X <- T],
    consumer_loop(T).

consumer_loop(Topics) ->
    receive
            {publish, Topic, Message} -> %this message is sent by the broker when a new message arrives
                io:format("Received a new message ~s about ~w~n", [Message, Topic]),
                consumer_loop(Topics);
            {repost, Topic, Message} ->
                io:format("I received this repost ~s about ~w~n",[Message,Topic]),
                consumer_loop(Topics)
            after 10000 ->
                Chance = rand:uniform(100) < 5,
                if
                    Chance -> error("I am dying");
                    true -> [broker ! {update, T, self()} || T <- Topics]
                end,
                consumer_loop(Topics)

    end.

producer(Message, Topic) ->
    broker ! {produce, Topic, Message}.


