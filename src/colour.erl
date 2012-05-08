-module(colour).
-include_lib("eunit/include/eunit.hrl").
-export([text/2, text/3]).

-type io_list() :: [integer() | io_list()].
-type colour() :: black | red | green | yellow | blue | magenta | cyan | white.

%% @doc Formats the text Text with foreground colour Foreground
%% @todo Implement it
-spec text(Foreground :: colour(), Text :: io_list()) -> io_list().
text(Foreground, Text) ->
    Text.

%% @doc 
%%      Formats the text Text with foreground colour Foreground
%%      and background colour Background
%% @end
%% @todo Implement it
-spec text(Foreground :: colour(), Background :: colour(), Text :: io_list()) -> io_list().
text(Foreground, Background, Text) ->
    Text.



colour_test_() ->
    [?_assert(colourz =:= true)].
