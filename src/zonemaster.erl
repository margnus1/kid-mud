-module(zonemaster).
-export([start/0, loop/1, get_zone/1]).

start() ->
    Tree = gb_trees:empty(),
    Id = spawn(zonemaster, loop, [Tree]),
    register(zonemaster, Id),
    Id.


loop(ActiveZonesTree) ->
    receive {get_zone, Player, Id} ->
	    X =  gb_trees:lookup(Id,ActiveZonesTree),
	    if X =:= none ->

		    ZonePid = spawn(zone,start,[Id]),
		    NewTree = gb_trees:insert(gb_trees:size(ActiveZonesTree),{Id,ZonePid},ActiveZonesTree),



		    %% Send {zone,ZonePid} to player.
		    Player ! {zone,ZonePid},
		    loop(NewTree);
	       true ->
		    {_,{_,Pid}} = X,
		    %% Send {zone,Pid} to player.
		    Player ! {zone,Pid},
		    loop(ActiveZonesTree)

	    end;

	    {zone_inactive, Id} ->
	    Y =  gb_trees:lookup(Id,ActiveZonesTree),
	    if Y =:= none ->
		    erlang:error("Trying to inactivate a zone that's not active!");
	       true ->

		    NewTree = gb_trees:delete(Id,ActiveZonesTree),


		    loop(NewTree)
	    end

    end.

get_zone(Id) ->
    zonemaster ! {get_zone, self(), Id},
    receive 
	{zone, Zone} ->
	    Zone
    end.

