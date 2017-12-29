-module(subscriber).
-export([start/1, main/2]).

start([Port,Topic]) ->
    io:format("SERVER Trying to bind to port ~p~n",[Port]),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    io:format("SERVER Listening on port ~p~n",[Port]),
    REST = spawn(fun()-> rest(Listen) end),
    register(rest, REST),

    accept(Listen, Topic).

accept(Listen, Topic) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    respond(Socket, Topic),
    accept(Listen, Topic).

respond(Socket, Topic) ->
    receive
        {tcp, Socket, <<"registered">>} ->
            io:format("Registration request: ~n"),
            gen_tcp:send(Socket, Topic),
            respond(Socket, Topic);
        {tcp, Socket, Bin} ->
            io:format("SERVER Received: ~p~n", [Bin]),
            respond(Socket, Topic);
        {tcp_closed, Socket} ->
            io:format("SERVER: The client closed the connection~n")
    end.

rest(Listen) ->
    receive
        {Handler, register, {Host, Port}} ->
            io:format("POST Request of Registration~n"),
            {ok, RequestSocket} = gen_tcp:connect(Host, Port,[binary, {packet, 0}]),
            ok = gen_tcp:send(RequestSocket, "registered"),
            Handler ! {internal, ok, "registered"},
            P = spawn(fun() -> responder(Handler, RequestSocket) end),
            gen_tcp:controlling_process(RequestSocket, P),
            rest(Listen)
    end

main(Port, User)->
    spawn(client, start, [[Port, User]]).

plain_text_response(Text) ->
    Length = integer_to_list(string:len(Text)),
    "POST /"++Text++" HTTP/1.1\r\nContent-Length: " ++ Length ++ "\r\nContent-Type: text/plain\r\n\r\n" ++ Text.
