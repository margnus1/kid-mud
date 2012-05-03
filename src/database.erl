-module(database).
-export([read_player/1, write_player/1, read_zone/1, write_zone/1, start/0, stop/0, setup/0]).
%% @doc Kid-MUD database interface

-include("zone.hrl").
-include("player.hrl").

%% @doc
%%     Reads the player with the name Name from the database.
%%     A default player is returned if it does not already exist
%% @end
%% @spec read_player(Name::string()) -> player()
read_player(Name) ->
    Trans = fun() -> mnesia:read(player, Name) end,
    case mnesia:transaction(Trans) of
	{atomic, [Player]} ->
	    Player;
	{atomic, []} ->
	    #player{name=Name}
    end.

%% @doc Writes the player Player to database
%% @spec write_player(Player::player()) -> ok
write_player(Player) ->
    Trans = fun() -> mnesia:write(player, Player, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

%% @doc Reads the zone with id Id from database
%% @spec read_zone(Id::integer()) -> zone() | zone_not_found
read_zone(Id) ->
    Trans = fun() -> mnesia:read(zone, Id) end,
    case mnesia:transaction(Trans) of
	{atomic, [Zone]} ->
		Zone;
	{atomic, []} -> zone_not_found
    end.

%% @doc Writes the zone Zone to database
%% @spec write_zone(Zone::zone()) -> ok
write_zone(Zone) ->
    Trans = fun() -> mnesia:write(zone, Zone, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

%% @doc Starts the database
%% @spec start() -> ok | {error, Reason}
start() ->
    mnesia:start().

%% @doc Stops the database
%% @spec stop() -> ok
stop() ->
    stopped = mnesia:stop(),
    ok.

%% @doc Performs first-time initialisation of database.
%%      Requires a schema to be configured for mnesia
%% @end
%% @spec setup() -> ok
setup() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(player,
				       [{attributes, record_info(fields, player)},
                                        {disc_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(zone,
				       [{attributes, record_info(fields, zone)},
                                        {disc_copies, [node()]}]),
    write_zone(#zone{id=0, desc="You are in a dark room"}),
    mnesia:stop(),
    ok.
