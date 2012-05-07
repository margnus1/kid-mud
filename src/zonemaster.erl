-module(zonemaster).
-export([start/0, loop/1, get_zone/1]).

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

