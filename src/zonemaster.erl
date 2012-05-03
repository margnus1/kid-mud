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
	    ActiveZone =  gb_trees:lookup(Id,ActiveZonesTree),
	    if ActiveZone =:= none ->
		    %% Spawn a new zone
		    ZonePid = spawn(zone,start,[Id]),%%får man rätt pid här för zone kör ju spawn också?
		    NewTree = gb_trees:insert(gb_trees:size(ActiveZonesTree),{Id,ZonePid},ActiveZonesTree),

		    %% Send the zone info to the player
		    Player ! {zone,ZonePid},
		    loop(NewTree);

	       true ->
		    {_,{_,Pid}} = ActiveZone,
		    %% Send the zone info to the player
		    Player ! {zone,Pid},

		    loop(ActiveZonesTree)

	    end;

	%% A zone_inactive message from a zone that is going inactive
	{zone_inactive, Id} ->
	    ActiveZone =  gb_trees:lookup(Id,ActiveZonesTree),
	    if ActiveZone =:= none ->
		    erlang:error("Trying to inactivate a zone that's not active!");
	       true ->
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

