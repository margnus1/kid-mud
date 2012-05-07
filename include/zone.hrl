-type exit() :: {Direction :: north | east | south | west, Id :: integer()}.
-record(zone, {id, desc="", npc=[], items=[], exits=[]}).
-type zone() :: #zone {id :: integer(), desc :: string(), npc :: [integer()], items :: [{integer(), integer()}], exits :: [exit()]}.

-record(npc, {id, name}).
-type npc() :: #npc {id :: integer(), name :: string()}.

-record(item, {id, name}).
-type item() :: #item {id :: integer, name :: string()}.

%%placeholder to maek compiler happy
-type player() :: any().
