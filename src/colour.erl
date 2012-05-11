%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arn\erlöv
%% See the file license.txt for copying permission.

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
    %% The control characters are escaped by io:fprint so this is currently useless
    %% [27, $[, 30 + get_colour(Foreground), $m, Text, 27, $[, $m].

%% @doc 
%%      Formats the text Text with foreground colour Foreground
%%      and background colour Background
%% @end
%% @todo Implement it
-spec text(Foreground :: colour(), Background :: colour(), Text :: io_list()) -> io_list().
text(Foreground, Background, Text) ->
    Text.

%% @doc Returns the ANSI colour code for colour Colour
-spec get_colour(Colour::colour()) -> integer().
get_colour(black) -> 0;
get_colour(red) -> 1; 
get_colour(green) -> 2;
get_colour(yellow) -> 3;
get_colour(blue) -> 4;
get_colour(magenta) -> 5;
get_colour(cyan) -> 6;
get_colour(white) -> 7.
