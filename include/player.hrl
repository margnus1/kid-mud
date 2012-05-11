%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arnerl�v
%% See the file license.txt for copying permission.

%% @type player() = #player {name=string(), location=integer(), health={time=erlang=timestamp(), hp=integer()}}
-record(player, {name, location=0, health={erlang:now(), 100.0}}).
