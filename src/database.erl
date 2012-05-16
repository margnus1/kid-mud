%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
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
    maploader:load("priv/map").
    

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
