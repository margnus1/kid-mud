%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arnerl�v
%% See the file license.txt for copying permission.

-record(player, {name, location=0, health={erlang:now(), 100.0}}).
-type player() :: #player {name :: string(), location :: integer(), health :: {erlang:timestamp(), integer()}}.
