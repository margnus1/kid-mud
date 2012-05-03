-module(database).
-export([read_player/1, write_player/1, read_zone/1, write_zone/1, start/0]).
-include("zone.hrl").
-include("player.hrl").

%% @doc
%%     Reads the player with the name Name from the database,
%%     giving a default player back if it does not already exist
%% @end
read_player(Name) ->
    Trans = fun() -> mnesia:read(player, Name) end,
    case mnesia:transaction(Trans) of
	{atomic, [Player]} ->
	    Player;
	{atomic, []} ->
	    #player{name=Name}
    end.

%% @doc Writes the player Player to database
write_player(Player) ->
    Trans = fun() -> mnesia:write(player, Player, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

%% @doc Reads the zone with id Id from database
read_zone(Id) ->
    Trans = fun() -> mnesia:read(zone, Id) end,
    case mnesia:transaction(Trans) of
	{atomic, [Zone]} ->
		Zone;
	{atomic, []} -> zone_not_found
    end.

%% @doc Writes the zone Zone to database
write_zone(Zone) ->
    Trans = fun() -> mnesia:write(zone, Zone, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

%% @doc Starts the database
start() ->
    ok = mnesia:start(),
    ok = setup(),
    ok.

setup() ->
    {atomic, ok} = mnesia:create_table(player,
				       [{attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(zone,
				       [{attributes, record_info(fields, zone)}]),
    write_zone(#zone{id=0, desc="You are in a dark room"}).
