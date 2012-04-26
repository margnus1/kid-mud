-module(parser).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Parses a user command string into a usable representation
parse(String) ->
    case parser_grammar:parse(String) of
        List when is_list(List) ->
            list_to_tuple(List);
        Atom when is_atom(Atom) -> Atom;
        _ErrorMessage -> parse_error
    end.


parse_test_() ->
    [?_assertEqual(parse_error, parse("")),
     ?_assertEqual(logout     , parse("logout")),
     ?_assertEqual(look       , parse("look")),
     ?_assertEqual(look       , parse("l")),
     ?_assertEqual(parse_error, parse("lo")),
     ?_assertEqual({go, north}, parse("go north")),
     ?_assertEqual({go, east} , parse("move e")),
     ?_assertEqual({go, west} , parse("west")),
     ?_assertEqual({go, south}, parse("s")),
     ?_assertEqual(parse_error, parse("go southish"))].
