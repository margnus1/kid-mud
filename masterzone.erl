-module(masterzone).
-export([start/0,loop/1]).

start() ->
	X = self(),
	io:format("Hej ~p",[X]),
	Tree = gb_trees:empty(),
	loop(Tree).

loop(Tree) ->
	self() ! {zone_inactive, 123},
	receive {get_zone, Player, Id} ->
		X =  gb_trees:lookup(Id,Tree),
		if X == none ->
		
			ZonePid = 4, %% Spawn new process.
			NewTree = gb_trees:insert(gb_trees:size(Tree),{Id,ZonePid},Tree),
			
			
			
			%% Send {zone,ZonePid} to player.
			%% Player ! {zone,ZonePid}
			loop(NewTree);
		true ->
			io:format("Ja"),
			{_,{_,Pid}} = X,
			%% Send {zone,Pid} to player.

			loop(Tree)
			
		end;

		{zone_inactive,Id} ->
			Y =  gb_trees:lookup(Id,Tree),
		if Y == none ->
			%% This shouldn't happen!!
			io:format("Ja"),
			loop(Tree);
			
		true ->
			NewTree = gb_trees:delete(Id,Tree),
			loop(NewTree)
		end

	end.
	


