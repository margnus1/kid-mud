%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arn\erlöv
%% See the file license.txt for copying permission.

-module(client).
-export([connect/0, connect/1, remote/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Starts a console session to the mud on this node
%% @spec connect() -> ok
connect() ->
    connect(node()).

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
                    io:fwrite("You cannot connect with name \"~s\"~n", [Name]),
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
            io:fwrite("~s~n", [striptags(Message)]),
            writer()
    end.

droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

striptags(S) ->
    re:replace(S, "<[^>]*>", "", [global]).

check_name(Name) ->
    case re:run(Name, "^.{3,15}$") of
	{match, _} ->
	    case re:run(Name, "^.[^\s]*$") of
		{match, _} ->
		    ok;
		nomatch ->
		    {failed, "Name cannot contain spaces"}
	    end;
	nomatch ->
	    {failed, "Name must be 3-15 characters long"}
    end.



%% EUnit tests

client_test_() ->
    [?_assertEqual("foo", droplast("foo\n")),
     ?_assertEqual("", droplast("\n")),
     ?_assertEqual({failed, "Name must be 3-15 characters long"},
		   check_name("")),
     ?_assertEqual({failed, "Name must be 3-15 characters long"},
		   check_name("Bo")),
     ?_assertEqual(ok,check_name("Bob")),
     ?_assertEqual(ok, check_name("femton_tecken12")),
     ?_assertEqual({failed, "Name must be 3-15 characters long"}, 
		   check_name("sexton_tecken123")),
     ?_assertEqual({failed, "Name cannot contain spaces"}, 
		   check_name("My Key")),
     ?_assertEqual(ok, check_name("MyKey"))].
