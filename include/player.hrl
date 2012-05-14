%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
%% See the file license.txt for copying permission.

-record(player, {name, location=0, health={erlang:now(), 100.0}}).
-type player() :: #player {name :: string(), location :: integer(), health :: {erlang:timestamp(), integer()}}.
