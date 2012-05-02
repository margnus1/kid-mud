-module(zone).
-export([start/0]).



start() ->
	Exits = masterzone:tempDatabase(self(),1), %%Load from DB
	NPC =  masterzone:tempDatabase(self(),2),
	Desc =  masterzone:tempDatabase(self(),3),
	Players = [{1,2},{3,4},{3,4}],
	
	State = [Exits,NPC,Desc,Players],
	loop(State).
	%%spawn(zone, loop, [State]).

messagePlayers(List, Playername) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {enternotification, Playername}~n"), ok end, ok, List).


loop(State) ->
	self() ! {enter, 'Eric', 'Ericigen', north},
	receive {go, Player, Direction} ->
		Exits = lists:nth(1,State),
		NPC = lists:nth(2,State),

		io:format(" ~p~n",[Exits]),
		io:format(" ~p~n",[NPC]),
	
		E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
            	Dir =:= Direction ],
		{_,DirectionID} = hd(E),

		if DirectionID =:= none ->
			io:format("Player ! {go,error,doesntexist}~n");
			%%Player ! {go,error,doesntexist}
		true -> 
			io:format("Player ! {go, DirectionID}~n"),
			%%Player ! {go, DirectionID}

			loop(State)
		end;
		
		{look, Player} -> 
		Description = hd(lists:nth(3,State)),
		io:format("~p~n",[Description]),
		io:format("Player ! {look, Description}~n"),
		%%Player ! {look, Description}
		loop(State);
		
		{enter, Player, Name, Direction} ->
		%%Sends the description to the player
		Description = hd(lists:nth(3,State)),
		io:format("~p~n",[Description]),
		io:format("Player ! {look, Description}~n"),
		%%Player ! {look, Description}

		%%Sends a notification to the other players
		Players = lists:nth(4,State),
		messagePlayers(Players, Name),

		%%Adds the player to the players list
		NewPlayers = Players ++ [{Player, Name}],
		io:format(" ~p~n",[NewPlayers])


		%%loop(State)
		
		%%Player ! {look, Description}
	end.



	%%	Player ! 
	%%exits.
