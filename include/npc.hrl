%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
%% See the file license.txt for copying permission.
-record(npc, {id, name, disp=neutral, health={erlang:now(), 30.0, 30.0}, damage=3}).
-type npc() :: #npc {id :: integer(), name :: string(), disp :: helpful | neutral | hostile, health :: {erlang:timestamp(), float(), float()} , damage :: integer()}.
