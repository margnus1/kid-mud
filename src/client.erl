-module(client).
-export([connect/0, connect/1, remote/3]).

%% @doc Starts a console session to the mud on this node
%% @spec connect() -> ok
connect() ->
    Name = droplast(io:get_line("Name?> ")),
    Writer = spawn(fun writer/0),
    {ok, Server} = player:start_link(Name, Writer),
    loop(Server, Writer).

%% @doc Starts a console session to the mud on the node Node
%% @spec connect(Node::node()) -> ok
connect(Node) ->
    Name = droplast(io:get_line("Name?> ")),
    Writer = spawn(fun writer/0),
    spawn(Node, client, remote, [self(), Name, Writer]),
    Server = receive {server, {ok, Server}} -> Server end,
    loop(Server, Writer).

%% @doc Performs connection at remote node
%% @spec remote(Caller::pid(), Name::string(), Writer::pid()) -> ok
remote(Caller, Name, Writer) ->
    Caller ! {server, player:start_link(Name, Writer)},
    ok.

loop(Server, Writer) ->
    case io:get_line("?> ") of 
        "logout\n" -> 
            Writer ! plz_die,
            player:command(Server, "logout"),
            ok;
        Line ->
            player:command(Server, droplast(Line)),
            loop(Server, Writer)
    end.

writer() ->
    receive
        plz_die ->
            ok;
        {message, Message} -> 
            io:fwrite("~s~n", [Message]),
            writer()
    end.

droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

