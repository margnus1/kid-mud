-module(masterzone).
-export([start/0,tempDatabase/2]).

start() ->
	X = self(),
	%%io:format("Hej ~p",[X]),
	Tree = gb_trees:empty(),
	loop(Tree).

tempDatabase(Pid,Num) -> %% TEMPORARY FUNCTION
	if Num =:= 1 ->
	
	Exits = [{north, 1}, {south, 2}, {west,3}, {east,none}],
	Pid ! Exits;
	Num =:= 2 ->
	NPC = [arne],
	Pid ! NPC;
	Num =:= 3 -> 
	Desc = ["här är en text"],
	Pid ! Desc
	end.
	

loop(Tree) ->
	self() ! {zone_inactive, 123},
	receive {get_zone, Player, Id} ->
		X =  gb_trees:lookup(Id,Tree),
		if X =:= none ->
		
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

		{zone_inactive, Id} ->
			Y =  gb_trees:lookup(Id,Tree),
		if Y =:= none ->
			%% This shouldn't happen!!
			io:format("Ja"),
			loop(Tree);
			
		true ->
			NewTree = gb_trees:delete(Id,Tree),
			
			
			loop(NewTree)
		end

	end.
	


