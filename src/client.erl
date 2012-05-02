-module(client).
-export([connect/0]).

%% @doc Starts a console session to the mud
connect() ->
    Name = droplast(io:get_line("Name?> ")),
    Writer = spawn(fun writer/0),
    Server = player:login(Name, Writer),
    loop(Server, Writer).

loop(Server, Writer) ->
    case io:get_line("?> ") of 
        eof -> 
            Writer ! plz_die,
            Server ! user_disconnect,
            ok;
        Line ->
            Server ! {command, droplast(Line)},
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

