-module(masterzone).
-export([start/0]).

start() ->
	X = self(),
	Tree = gb_trees:empty(),
	loop(Tree).


loop(ActiveZonesTree) ->
    receive {get_zone, Player, Id} ->
	    X =  gb_trees:lookup(Id,ActiveZonesTree),
	    if X =:= none ->

		    ZonePid = spawn(zone,start,[Id]),
		    NewTree = gb_trees:insert(gb_trees:size(ActiveZonesTree),{Id,ZonePid},ActiveZonesTree),



		    %% Send {zone,ZonePid} to player.
		    %% Player ! {zone,ZonePid}
		    loop(NewTree);
	       true ->
		    {_,{_,Pid}} = X,
		    %% Send {zone,Pid} to player.
		    %% Player ! {zone,Pid},
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



