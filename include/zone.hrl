%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
%% See the file license.txt for copying permission.

-type exit() :: {Direction :: north | east | south | west, Id :: integer()}.

-record(zone, {id, desc="", npc=[], items=[], exits=[], level_range={1,1}, habitat=forest}).
-type zone() :: #zone {id :: integer(), 
		       desc :: string(), 
		       npc :: [integer() | {dead, erlang:timestamp()}], 
		       items :: [{integer(), integer()}], 
		       exits :: [exit()],
                       level_range :: {integer(), integer()},
                       habitat :: npc:habitat()}.

-record(item, {id, name}).
-type item() :: #item {id :: integer(), 
		       name :: string()}.

%%placeholder to maek compiler happy
%%-type player() :: any().
