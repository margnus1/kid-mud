%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
%% See the file license.txt for copying permission.

-type exit() :: {Direction :: north | east | south | west, Id :: integer()}.
-record(zone, {id, desc="", npc=[], items=[], exits=[]}).
-type zone() :: #zone {id :: integer(), desc :: string(), npc :: [npc()], items :: [{integer(), integer()}], exits :: [exit()]}.

-record(npc, {id, name, disp=neutral, health={erlang:now(), 100.0}, damage=3}).
-type npc() :: #npc {id :: integer(), name :: string(), disp :: neutral | aggressive, health :: {erlang:timestamp(), integer()} , damage :: integer()}.

-record(item, {id, name}).
-type item() :: #item {id :: integer(), name :: string()}.

%%placeholder to maek compiler happy
%%-type player() :: any().
