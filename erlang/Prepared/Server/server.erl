-module(server).
-export([start/0,server/0]).

%% This server only stays alive for one connection, but this is OK for demonstration in a modern
%% browser, since HTTP 1.1 keeps the connection open for multiple requests.

start() ->
    Pid = spawn(?MODULE, server, []), %%global naming for processes
    register(server, Pid).

server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    io:format("SERVER Listening on port 2345~n"),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    Map = #{ erlang => [], haskell => [], scheme => []}, %%maps
    loop(Socket, Map, []).

loop(Socket, Subscribers, Users) ->
    receive %%crazy levels of pattern matching
        {register, Pid, Usr} ->
            io:format("Adding User~p~n", [Usr]),
            loop(Socket, Subscribers, [Usr|Users]);
        {tcp, Socket, Bin} ->
            io:format("SERVER Received a request~n"),
            case parse_response(Bin) of
                {get, "all", []} ->
                    io:format("Get all~n"),
                    spawn(fun () -> list(Socket, Users) end),
                    loop(Socket, Subscribers, Users);
                {post, "update", ["topic", TopicS, "msg", Msg]} ->
                    io:format("POST update~n"),
                    Topic = list_to_atom(TopicS), %%atoms vs string
                    #{Topic := Subs} = Subscribers, %%maps
                    [spawn(fun () -> forward(Sub, Msg, Socket) end)|| Sub <- Subs], %%list comprehension
                    loop(Socket, Subscribers, Users);
                {post, "register", ["host",Host,"port",PortS, "topic", TopicS]} ->
                    io:format("POST register~n"),
                    Topic = list_to_atom(TopicS),
                    {Port,_} =string:to_integer(PortS),
                    #{Topic := Subs} = Subscribers,
                    spawn(fun () -> respond(Host, Port, Socket) end),
                    loop(Socket, Subscribers#{Topic := [{Host, Port}|Subs]}, Users)
            end;
        {tcp_closed, Socket} ->
            io:format("SERVER: The client closed the connection~n")
    end.

%%case of recursion
concatMap([]) -> "\n";
concatMap([X|XS]) -> X ++ " " ++ concatMap(XS).

list(ResponseSocket, Users) ->
    L = concatMap(Users),
    io:format("Users ~p~n", [Users]),
    gen_tcp:send(ResponseSocket, plain_text_response(L)).
    
forward({Host,Port}, Msg, ResponseSocket) -> 
        {ok, RequestSocket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
        ok = gen_tcp:send(RequestSocket, Msg),
        io:format("SERVER Sent request to back end~n"),
        gen_tcp:send(ResponseSocket, plain_text_response("Received")).

respond(Host, Port, ResponseSocket) ->
    {ok, RequestSocket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(RequestSocket, "registered"),
    io:format("SERVER Sent request to back end~n"),
    receive
        {tcp, RequestSocket, Bin} ->
            Response = plain_text_response(binary_to_list(Bin)),
            io:format("SERVER Sent HTTP response: ~p~n", [Bin]),
            server ! {register, self(), binary_to_list(Bin)}, %% intra process communication
            gen_tcp:send(ResponseSocket, Response)
    end.


plain_text_response(Text) ->
    Length = integer_to_list(string:len(Text)),
    "HTTP/1.1 200 OK\r\nContent-Length: " ++ Length ++ "\r\nContent-Type: text/plain\r\n\r\n" ++ Text.

%% Erlang processes binary streams to optimize
response_method([80,79,83,84|_]) -> post ;
response_method([71,69,84|_]) -> get ;
response_method([80,85,84|_]) -> put ;
response_method([68,69,76,69,84,69|_]) -> delete ;
response_method(_)-> error.

%% we can use pattern match to dispatch the request
get_req_params(post, _, Rest) -> string:tokens(lists:nth(length(Rest), Rest), "&=");
get_req_params(delete, _, Rest) -> string:tokens(lists:nth(length(Rest), Rest), "&=");
get_req_params(_, _, _) -> [].

%%String difference, pattern matching simplified
get_uri_params(get, Text,_) -> ((Text -- " GET ") -- " HTTP/1.1 ") -- "/";
get_uri_params(post, Text,_) -> ((Text -- " POST ") -- " HTTP/1.1 ") -- "/";
get_uri_params(put, Text,_) -> ((Text -- " PUT ") -- " HTTP/1.1 ") -- "/" ;
get_uri_params(delete, Text,_) -> ((Text -- " DELETE ") -- " HTTP/1.1 ") -- "/" .

parse_response(Bin) ->
    [Text|Rest] = string:tokens(binary_to_list(Bin), "\r\n"), %% First line is the GET request
    Method =response_method(Text),
    UriParams = get_uri_params(Method, Text, Rest),
    ReqParams = get_req_params(Method, Text, Rest),
    {response_method(Text), UriParams, ReqParams} .