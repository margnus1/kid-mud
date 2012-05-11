%% Copyright (c) 2012 Magnus LÂng, Mikael Wiberg and Michael Bergroth
%% See the file license.txt for copying permission.

-module(parser).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Parses a user command string into a usable representation
%% @spec parse(String::string() | bitstring()) -> Command | parse_error
%% where
%%       Command = {go, Direction} | look | logout
%%       Direction = north | east | south | west
parse(String) ->
    UnicodeBinary = unicode:characters_to_binary(String, latin1),
    case parser_grammar:parse(UnicodeBinary) of
	{_, _, {{line, _}, {column,_}}} -> parse_error;
	{fail, _} -> parse_error;
	Result -> Result
    end.


parse_test_() ->
    [?_assertEqual(parse_error        , parse("")),
     ?_assertEqual(logout             , parse("logout")),
     ?_assertEqual(stop               , parse("stop")),
     ?_assertEqual(look               , parse("look")),
     ?_assertEqual(look               , parse("l")),
     ?_assertEqual(parse_error        , parse("lo")),
     ?_assertEqual({go, north}        , parse("go north")),
     ?_assertEqual({go, east}         , parse("move e")),
     ?_assertEqual({go, west}         , parse("west")),
     ?_assertEqual({go, south}        , parse("s")),
     ?_assertEqual(exits              , parse("exits")),
     ?_assertEqual({say, "tt"}        , parse("say tt")),
     ?_assertEqual({attack, "Mikael"} , parse("attack Mikael")),
     ?_assertEqual({attack, "Michael"}, parse("kill Michael")),
     ?_assertEqual({say, "Â‰"}        , parse("say Â‰")),
     ?_assertEqual(parse_error        , parse("go southish"))].
