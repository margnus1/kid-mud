%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arn\erl�v
%% See the file license.txt for copying permission.

-module(client).
-export([connect/0, connect/1, remote/3]).

%% @doc Starts a console session to the mud on this node
%% @spec connect() -> ok
connect() ->
    Name = droplast(io:get_line("Name?> ")),
    case check_name(Name) of
	{failed, Reason} ->
	    io:fwrite("~s~n",[Reason]),
	    connect();
	ok ->
            Writer = spawn(fun writer/0),
            case playermaster:start_player(Name, Writer) of
                {ok, Server} ->
                    loop(Server, Writer);
                login_failed ->
                    Writer ! plz_die,
                    io:fwrite("You cannot connect with name ~p~n", [Name]),
                    connect()
            end
    end.

%% @doc Starts a console session to the mud on the node Node
%% @spec connect(Node::node()) -> ok
connect(Node) ->
    Name = droplast(io:get_line("Name?> ")),
    case check_name(Name) of
	{failed, Reason} ->
	    io:fwrite("~s~n",[Reason]),
	    connect(Node);
	ok ->
            Writer = spawn(fun writer/0),
            spawn(Node, client, remote, [self(), Name, Writer]),
            receive 
                {server, {ok, Server}} -> 
                    loop(Server, Writer); 
                {server, login_failed} ->
                    Writer ! plz_die,
                    io:fwrite("You cannot connect with name ~p~n", [Name]),
                    connect(Node)
            end
    end.

%% @doc Performs connection at remote node
%% @spec remote(Caller::pid(), Name::string(), Writer::pid()) -> ok
remote(Caller, Name, Writer) ->
    Caller ! {server, playermaster:start_player(Name, Writer)},
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

check_name(Name) ->
    case re:run(Name, "^[^ ]{1,15}$") of
	{match, _} ->
	    ok;
	nomatch ->
	    {failed, "Name is too long and/or contains spaces"}
    end.
