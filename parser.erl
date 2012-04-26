-module(parser).
-export([parse/1]).

%% @doc Parses a user command string into a usable representation
parse(String) ->
    case parser_grammar:parse(String) of
        List when is_list(List) ->
            list_to_tuple(List);
        Atom when is_atom(Atom) -> Atom;
        _ErrorMessage -> parse_error
    end.
