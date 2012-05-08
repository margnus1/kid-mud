-module(zonemaster).
-export([start/0, loop/1, get_zone/1]).
-include_lib("eunit/include/eunit.hrl").
-include("zone.hrl").

%% @doc Starts the zonemaster
start() ->
    ActiveZonesTree = gb_trees:empty(),
    Id = spawn(zonemaster, loop, [ActiveZonesTree]),
    register(zonemaster, Id),
    Id.

%% @doc The main loop of the zonemaster
loop(ActiveZonesTree) ->
    receive 
	%% A get_zone command from a player
	{get_zone, Player, Id} ->
	    %% Checks if the zone is active
	    case gb_trees:lookup(Id, ActiveZonesTree) of
		none ->
		    %% Spawn a new zone
		    ZonePid = zone:start(Id),
		    NewTree = gb_trees:insert(Id, ZonePid, ActiveZonesTree),

		    %% Send the zone info to the player
		    Player ! {zone, ZonePid},
		    loop(NewTree);

	       {value, Pid} ->
		    %% Send the zone info to the player
		    Player ! {zone, Pid},

		    loop(ActiveZonesTree)

	    end;

	%% A zone_inactive message from a zone that is going inactive
	{zone_inactive, Id} ->
	    case gb_trees:lookup(Id,ActiveZonesTree) of
		none ->
		    erlang:error("Trying to inactivate a zone that's not active!");
		{value, _} ->
		    NewTree = gb_trees:delete(Id,ActiveZonesTree),
		    
		    loop(NewTree)
	    end
    end.

%% @doc Gets the pid of the zone in question
get_zone(Id) ->
    zonemaster ! {get_zone, self(), Id},
    receive 
	{zone, Zone} ->
	    Zone
    end.

%% @doc Message that a zone is going inactive
zone_inactive(Id) ->
    zonemaster ! {zone_inactive, Id},
    ok.

test_setup() ->
    mnesia:start(),
    database:create_tables([]),
    database:write_zone(#zone{id=1234}).

zonemaster_test_() ->
    {setup, fun test_setup/0, 
     [fun () ->
	      Id = start(),
	      ?assertEqual(get_zone(1234), get_zone(1234)),
	      TempId = get_zone(1234),
	      Id ! {zone_inactive,1234},
	      ?assert(TempId =/= get_zone(1234))


      end
     ]}.

