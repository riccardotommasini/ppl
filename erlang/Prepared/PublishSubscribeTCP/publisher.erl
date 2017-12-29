-module(publisher).
-export([main/1]).


main(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    Map = #{ erlang => [], haskell => [], scheme => []},
    Gatway = spawn(fun()-> gateway(Listen) end),
    REST = spawn(fun()-> rest(Listen) end),
    Server = spawn(fun()-> server(Map) end),
    register(gateway, Gatway),
    register(rest, REST),
    register(server, Server).


gateway(Listen)->
    io:format("Server Running Listening on Port 2345~n"),
    io:format("New Process Ready to Accept Connections~n",[]),
    {ok, Accept} = gen_tcp:accept(Listen),
    Pid = spawn(fun()-> handler(Accept) end),
    gen_tcp:controlling_process(Accept, Pid),
    gateway(Listen).

handler(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Received by Handler~p~p~n",[self(),Bin]),
            {Method, ReqParams} = parse_response(Bin),
            rest ! {self(), Method, ReqParams},
            handler(Socket);
        {tcp_closed, Socket} ->
            io:format("SERVER closed connection~n"),
            gen_tcp:close(Socket);
%% Internal Protocol Between Processes Can I split this?
        {internal, ok, Msg} ->
            io:format("Internal ok ~p~n",[Msg]),
            Length = integer_to_list(string:len(Msg)),
            Response = "HTTP/1.1 200 OK\r\nContent-Length: " ++ Length ++ "\r\nContent-Type: text/plain\r\n\r\n" ++ Msg,
            ok=gen_tcp:send(Socket, Response),
            handler(Socket);
        {internal, ko, Msg} ->
            io:format("Internal ko ~sp~n",[Msg]),
            Length = integer_to_list(string:len(Msg)),
            Response = "HTTP/1.1 500 500 Internal Server Error\r\nContent-Length: " ++ Length ++ "\r\nContent-Type: text/plain\r\n\r\n" ++ Msg,
            ok=gen_tcp:send(Socket, Response),
            handler(Socket)
    end.

responder(Handler, RequestSocket) ->
    receive
        {forward, Msg} ->
            Handler ! {internal, ok, "forwarded"},
            gen_tcp:send(RequestSocket, Msg),
            responder(Handler,RequestSocket);
        {tcp, RequestSocket, Bin} ->
                T = list_to_atom(binary_to_list(Bin)),
                io:format("Received by Responder ~p~p -- ~p~n",[self(),Bin, T]),
                server ! {add, T, self()},
                responder(Handler,RequestSocket);
        {tcp_closed, RequestSocket} ->
                io:format("SERVER closed connection~n"),
                gen_tcp:close(RequestSocket)
    end.

server(Map) ->
    receive
        {add, Topic, Pid} -> io:format("Added ~p under ~p~n",[Pid,Topic]), #{Topic := Pids}=Map, server(Map#{Topic := [Pid|Pids]});
        {forward, Topic, Msg} -> io:format("Sent ~s to who looks for ~p~n",[Msg,Topic]), #{Topic := Pids}=Map, [P ! {forward,Msg} || P <- Pids], server(Map)
    end.

rest(Listen) ->
    receive
        {Handler, register, {Host, Port}} ->
            io:format("POST Request of Registration~n"),
            {ok, RequestSocket} = gen_tcp:connect(Host, Port,[binary, {packet, 0}]),
            ok = gen_tcp:send(RequestSocket, "registered"),
            Handler ! {internal, ok, "registered"},
            P = spawn(fun() -> responder(Handler, RequestSocket) end),
            gen_tcp:controlling_process(RequestSocket, P);
        {Handler, update, {T, Msg}} ->
            io:format("POST Request of Update~n"),
            server ! {forward, list_to_atom(T), Msg};
        X -> io:format("Received ~p~n",[X])
    end,
    rest(Listen).

to_integer("") -> -1;
to_integer(PortS) -> {Port,_} = string:to_integer(PortS), Port .

plain_text_response(Text) ->
    Length = integer_to_list(string:len(Text)),
    "HTTP/1.1 200 OK\r\nContent-Length: " ++ Length ++ "\r\nContent-Type: text/plain\r\n\r\n" ++ Text.

%% Erlang processes binary streams to optimize
response_method([80,79,83,84|_]) -> register ;
response_method([71,69,84|_]) -> list ;
response_method([80,85,84|_]) -> update ;
response_method([68,69,76,69,84,69|_]) -> delete ;
response_method(_)-> error.

%% we can use pattern match to dispatch the request
get_req_params(register, _, Rest) -> ["host", Host, "port", PortS] = string:tokens(lists:nth(length(Rest), Rest), "&="), {Host, to_integer(PortS)};
get_req_params(update, _, Rest) -> ["topic", T, "msg", Msg] = string:tokens(lists:nth(length(Rest), Rest), "&="), {T,Msg};
get_req_params(_, _, _) -> {}.

parse_response(Bin) ->
    [Text|Rest] = string:tokens(binary_to_list(Bin), "\r\n"), %% First line is the GET request
    Method =response_method(Text),
    ReqParams = get_req_params(Method, Text, Rest),
    {response_method(Text), ReqParams}.