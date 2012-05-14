%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
%% See the file license.txt for copying permission.

-type exit() :: {Direction :: north | east | south | west, Id :: integer()}.
-record(zone, {id, desc="", npc=[], items=[], exits=[]}).
-type zone() :: #zone {id :: integer(), desc :: string(), npc :: [npc()], items :: [{integer(), integer()}], exits :: [exit()]}.


-record(item, {id, name}).
-type item() :: #item {id :: integer(), name :: string()}.

%%placeholder to maek compiler happy
%%-type player() :: any().
