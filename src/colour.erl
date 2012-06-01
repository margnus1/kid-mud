%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
%% See the file license.txt for copying permission.

-module(colour).
-include_lib("eunit/include/eunit.hrl").
-export([text/2, text/3]).

-type io_list() :: [integer() | io_list()].
-type colour() :: black | red | green | yellow | blue | magenta | cyan | white.

%% @doc Formats the text Text with foreground colour Foreground
%% @todo Implement it
-spec text(_Foreground :: colour(), Text :: io_list()) -> io_list().
text(Foreground, Text) ->
    ["<span style='color:", atom_to_list(Foreground), "'>", Text, "</span>"].
    %% The control characters are escaped by io:fprint so this is currently useless
    %% [27, 91, $9, $0 + get_colour(Foreground), $m, Text, 27, 91, $m].

%% @doc 
%%      Formats the text Text with foreground colour Foreground
%%      and background colour Background
%% @end
%% @todo Implement it
-spec text(_Foreground :: colour(), _Background :: colour(), Text :: io_list()) -> io_list().
text(Foreground, Background, Text) ->
    ["<span style='color:", atom_to_list(Foreground), 
     " ;background-color:", atom_to_list(Background), "'>", Text, "</span>"].
