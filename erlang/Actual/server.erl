-module(server).
-export([main/0,respond/3,server/0]).

main() ->
    Pid = spawn(?MODULE, server, []),
    register(publisher, Pid).

server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    io:format("Server Running on Port 2345~n"),
    {ok, Accept} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    Map = #{ erlang => [], haskell => [], scheme => []},
    loop(Accept, Map, []).

loop(Socket, Subs, Users) ->
    receive
        {add, Pid, User} ->
            io:format("Adding User ~p~n", [User]),
            loop(Socket, Subs, [User|Users]);
        {tcp, Socket, Bin} ->
            io:format("Received a request ~p~n", [Bin]),
            case parse_response(Bin) of %% {method, URIParams, ReqParams}
                {post, "register", ["host", Host, "port", PortS, "topic", TopicS]} ->
                    io:format("POST Request of Registration~n"),
                    Topic = list_to_atom(TopicS),
                    {Port,_} = string:to_integer(PortS),
                    #{Topic := List} = Subs,
                    spawn(?MODULE, respond, [Host, Port, Socket]),
                    loop(Socket, Subs#{Topic := [{Host, Port}|List]}, Users);
                {post, "update", ["topic", TopicS, "msg", Msg]} ->
                    Topic = list_to_atom(TopicS),
                    #{Topic := List} = Subs,
                    [spawn(?MODULE, forward, [Host, Port, Msg]) || {Host, Port} <- List],
                    loop(Socket, Subs, Users)
            end;
        {tcp_closed, Socket} ->
             io:format("SERVER closed connection~n")
    end.

respond(Host, Port, ResponseSocket) ->
    {ok, RequestSocket} = gen_tcp:connect(Host, Port,[binary, {packet, 0}]),
    ok = gen_tcp:send(RequestSocket, "registered"),
    receive
        {tcp, RequestSocket, Bin} ->
            User = binary_to_list(Bin),
            Response = plain_text_response(User),
            publisher ! {add, self(), User},
            gen_tcp:send(ResponseSocket, Response),
            gen_tcp:close(RequestSocket)
    end.

forward(Host, Port, Msg) ->
    {ok, RequestSocket} = gen_tcp:connect(Host, Port,[binary, {packet, 0}]),
    gen_tcp:send(RequestSocket, Msg),
    gen_tcp:close(RequestSocket).

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
    Resp = {response_method(Text), UriParams, ReqParams},
    Resp.



