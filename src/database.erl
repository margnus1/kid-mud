-module(database).
-export([read_player/1, write_player/1, read_zone/1, write_zone/1, start/0, stop/0, setup/0, create_tables/1]).
-include("zone.hrl").
-include("player.hrl").

-include_lib("eunit/include/eunit.hrl").

%% @doc
%%     Reads the player with the name Name from the database.
%%     A default player is returned if it does not already exist
%% @end
%% @spec read_player(Name::string()) -> player()
%% @type player() = #player {name=string(), location=integer()}
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
%% @type zone() = #zone {id=integer(), exits=[exit()], npc=[npc()], desc=string()}
%% @type exit() = {Direction :: north | east | south | west, Id :: integer() | none}
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
%%      mnesia must not be running
%% @end
%% @spec setup() -> ok
setup() ->
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    create_tables([{disc_copies, [node()]}]),
    write_zone(#zone{id=0, desc="You are in a dark room",exits=[{north,1}]}),
    write_zone(#zone{id=1, desc="You are in a even darker room",exits=[{south,0}]}),
    mnesia:stop(),
    ok.

%% @doc Creates the required tables in the mnesia database.
%%      Uses the options in Options in addition to the individual options.
%% @end
create_tables(Options) ->
    {atomic, ok} = mnesia:create_table(player,
			[{attributes, record_info(fields, player)}] ++ Options),
    {atomic, ok} = mnesia:create_table(zone,
			[{attributes, record_info(fields, zone)}] ++ Options),
    ok.

%% @hidden
test_setup() ->
    mnesia:start().
    %%create_tables([]).

%% @hidden
database_test_() ->
    Korv = #player{name="Korv", location=2},
    Five = #zone{id=5, desc="asd"},
    {setup, fun test_setup/0, 
     [fun() -> write_player(Korv),
               ?assertEqual(Korv, read_player("Korv")) end,
      ?_assertEqual(#player{name="Gustav"}, read_player("Gustav")),
      ?_assertEqual(read_zone(0), zone_not_found),
      fun() -> write_zone(Five),
               ?assertEqual(read_zone(5), Five) end]}.
