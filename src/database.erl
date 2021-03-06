%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg, Michael Bergroth and Eric Arnerl�v
%% See the file license.txt for copying permission.

-module(database).
-export([read_player/1, write_player/1, read_zone/1, write_zone/1, read_npc/1,
	 write_npc/1, find_npc/2, get_npc_names/0, init/0, setup/0, create_tables/1]).
-include("zone.hrl").
-include("player.hrl").
-include("npc.hrl").

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

%% @doc Reads the npc with id Id from database
-spec read_npc(Id :: integer()) -> npc_not_found | npc().
read_npc(Id) ->
    Trans = fun () -> mnesia:read(npc, Id) end,
    case mnesia:transaction(Trans) of
	{atomic, [NPC]} -> NPC;
	{atomic, []} -> npc_not_found
    end.

%% @doc Writes the npc NPC to database
-spec write_npc(npc()) -> ok.
write_npc(NPC) ->
    Trans = fun () -> mnesia:write(npc, NPC, write) end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.		     

%% @doc Finds the id of all npc that match the level and habitat requirement.
%%      The level must be in the range [FromLevel, ToLevel] and the
%%      habitat must be Habitat.
%% @end
-spec find_npc({integer(), integer()}, habitat()) -> integer().
find_npc({FromLevel, ToLevel}, Habitat) ->
    Trans = fun() -> 
		    MatchHead = #npc{id='$1', level='$2', habitat=Habitat, _='_'},
		    LowGuard = {'>', '$2', FromLevel-1},
		    HighGuard = {'<', '$2', ToLevel+1},
		    mnesia:select(npc, [{MatchHead, [LowGuard, HighGuard], ['$1']}])
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

%% @doc Return the list of names on NPCs
-spec get_npc_names() -> [string()].
get_npc_names() ->
    Trans = fun () ->
		    MatchHead = #npc{name='$1', _ = '_'}, 
		    mnesia:select(npc, [{MatchHead, [], ['$1']}])
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.    

%% @doc Starts the database
%% @spec init() -> ok | {error, Reason}
init() ->
    create_tables([{disc_copies, [node()]}]),
    mnesia:wait_for_tables([player, zone], 2000),
    maploader:load(["priv/map", "../priv/map", "/var/kidmud/map"]),
    database:write_npc(#npc{id=1, name="Rabbit",       level=1, habitat=forest, disp=neutral, damage=1, health=20}),
    database:write_npc(#npc{id=2, name="Goblin",       level=2, habitat=forest, disp=neutral, damage=2, health=30}),
    database:write_npc(#npc{id=3, name="Goblin",       level=4, habitat=forest, disp=hostile, damage=4, health=35}),
    database:write_npc(#npc{id=4, name="Goblin",       level=5, habitat=forest, disp=hostile, damage=6, health=55}),
    database:write_npc(#npc{id=5, name="Troll",        level=6, habitat=forest, disp=hostile, damage=7, health=80}),
    database:write_npc(#npc{id=6, name="Frog",         level=1, habitat=road,   disp=neutral, damage=1, health=10}),
    database:write_npc(#npc{id=7, name="Traveler",     level=2, habitat=road,   disp=neutral, damage=2, health=25}),
    database:write_npc(#npc{id=8, name="Traveler",     level=4, habitat=road,   disp=neutral, damage=4, health=40}),
    database:write_npc(#npc{id=9, name="Highwayman",   level=7, habitat=road,   disp=hostile, damage=6, health=70}),
    database:write_npc(#npc{id=10, name="Mudcrabling", level=1, habitat=beach,  disp=neutral, damage=2, health=15}),
    database:write_npc(#npc{id=11, name="Mudcrab",     level=3, habitat=beach,  disp=neutral, damage=4, health=35}),
    database:write_npc(#npc{id=12, name="Mudcrab",     level=5, habitat=beach,  disp=neutral, damage=5, health=60}),
    database:write_npc(#npc{id=13, name="Crablord",    level=7, habitat=beach,  disp=hostile, damage=8, health=80}),
    database:write_npc(#npc{id=14, name="Small Bat",   level=1, habitat=cave,   disp=neutral, damage=2, health=15}),
    database:write_npc(#npc{id=15, name="Bat",         level=1, habitat=cave,   disp=neutral, damage=2, health=15}),
    database:write_npc(#npc{id=16, name="Spider",      level=2, habitat=cave,   disp=neutral, damage=3, health=30}),
    database:write_npc(#npc{id=17, name="Worm",        level=3, habitat=cave,   disp=neutral, damage=4, health=35}),
    database:write_npc(#npc{id=18, name="Bat",         level=4, habitat=cave,   disp=hostile, damage=4, health=50}),
    database:write_npc(#npc{id=19, name="Bat",         level=5, habitat=cave,   disp=hostile, damage=5, health=55}),
    database:write_npc(#npc{id=20, name="Bat",         level=6, habitat=cave,   disp=hostile, damage=6, health=65}),
    database:write_npc(#npc{id=21, name="Lynx",        level=7, habitat=cave,   disp=hostile, damage=7, health=75}),
    database:write_npc(#npc{id=22, name="Bear",        level=8, habitat=cave,   disp=hostile, damage=9, health=90}),
    database:write_npc(#npc{id=23, name="Dragon",      level=10, habitat=cave,  disp=hostile, damage=20, health=150}).
    

%% @doc Performs first-time initialisation of database.
%%      mnesia must not be running
%% @end
%% @spec setup() -> ok
setup() ->
    mnesia:create_schema([node()]).

%% @doc Creates the required tables in the mnesia database.
%%      Uses the options in Options in addition to the individual options.
%% @end
create_tables(Options) ->
    ok = create_table(player, Options, record_info(fields, player)),
    ok = create_table(npc, Options, record_info(fields, npc)),
    ok = create_table(zone, Options, record_info(fields, zone)).

create_table(Table, Options, Attributes) ->
    case mnesia:create_table(Table, [{attributes, Attributes}] ++ Options) of
	{atomic, ok} -> ok;
	{aborted, {already_exists, Table}} ->
	    case mnesia:table_info(Table, attributes) of
		GoodAtts when GoodAtts =:= Attributes -> ok;
		_BadAtts ->
		    {atomic, ok} = mnesia:delete_table(Table),
		    case mnesia:create_table(
			   Table, [{attributes, Attributes}] ++ Options) of
			{atomic, ok} -> ok;
			{aborted, Reason} -> {error, Reason}
		    end
	    end;
	{aborted, OtherReason} -> {error, OtherReason}	       
    end.
		   

%% @hidden
test_setup() ->    
    mnesia:start(),
    create_tables([]),
    mnesia:clear_table(player),
    mnesia:clear_table(zone),
    mnesia:clear_table(npc).

%% @hidden
database_test_() ->
    Korv = #player{name="Korv", location=2},
    Five = #zone{id=5, desc="asd"},
    Korvgubbe = #npc{id=1232, name="Korvgubbe", level=2, habitat=forest},
    Staalmannen = #npc{id=1233, name="Staalmannen", level=5, habitat=forest},
    Crab = #npc{id=1234, name="Crab", level=2, habitat=beach},
    {setup, fun test_setup/0, 
     [fun() -> write_player(Korv),
               ?assertEqual(Korv, read_player("Korv")) end,
      fun () ->
	      #player{health=Health} = G = read_player("Gustav"),
	      ?assertEqual(#player{name="Gustav", health=Health}, G) end,
      ?_assertEqual(zone_not_found, read_zone(0)),
      fun () -> write_zone(Five),
		?assertEqual(Five, read_zone(5)) end,
      fun () -> write_npc(Korvgubbe),
		?assertEqual(Korvgubbe, read_npc(1232)) end,
      fun () -> write_npc(Staalmannen),
		write_npc(Crab),
	        ?assertEqual([1232], find_npc({2,4}, forest)) end,
      ?_assertEqual([1232, 1233], find_npc({2,5}, forest)),
      ?_assertEqual([1234], find_npc({1,2}, beach)),
      ?_assertEqual(["Crab","Korvgubbe","Staalmannen"], get_npc_names())]}.
