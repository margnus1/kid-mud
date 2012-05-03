-module(database).
-export([read_player/1, write_player/1, read_zone/1, write_zone/1, start/0]).
-include("zone.hrl").
-include("player.hrl").

read_player(Name) ->
    ok.

write_player(Player = #player{name=Name}) ->
    %%mnesia:transaction
    ok.

read_zone(Id) ->
    Trans = fun() -> mnesia:read(zone, Id) end,
    case mnesia:transaction(Trans) of
	{atomic, [Zone]} ->
		Zone;
	_ -> zone_not_found
    end.

write_zone(Zone) ->
    Trans = fun() -> mnesia:write(zone, Zone, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

start() ->
    ok = mnesia:start(),
    ok = setup(),
    ok.

setup() ->
    {atomic, ok} = mnesia:create_table(player,
				       [{attributes, record_info(fields, player)}]),
    {atomic, ok} = mnesia:create_table(zone,
				       [{attributes, record_info(fields, zone)}]),
    write_zone(#zone{id=0,
		     exits=[{east, none}, {west, none}, {north, none}, {south, none}],
		     npc=[], desc="You are in a dark room"}).
