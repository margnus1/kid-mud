-module(zone).
-export([start/0,loop/1]).



start() ->
    Exits = masterzone:tempDatabase(self(),1), %% Should load from DB
    NPC =  masterzone:tempDatabase(self(),2), %% Should load from DB
    Desc =  masterzone:tempDatabase(self(),3), %% Should load from DB
    Players = [{1,2},{3,4},{5,7}], %% Should load from DB

    State = {Exits,NPC,Desc,Players},
    spawn(zone, loop, [State]).


messagePlayers(List, Playername, Direction, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername, Direction}~n"), ok end, ok, List).


messagePlayers(List, Playername, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername}~n"), ok end, ok, List).


store_db(State) ->
    io:format("Stored this in the DB: ~p~n",[State]).


loop(State = {Exits,NPC,Desc,Players}) ->
    %% self() ! {enter, self(), 'Ericigen', north},
    %% self() ! {logout, 1, 2},
    receive 
	%% A go command from a player
	{go, Player, Name, Direction} ->
	    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
				 Dir =:= Direction ],
	    {_,DirectionID} = hd(E),

	    if DirectionID =:= none ->
		    io:format("Player ! {go,error,doesntexist}~n"),
	       %% Player ! {go,error,doesntexist}
	       loop(State);

	       true -> 
		    io:format("Player ! {go, DirectionID}~n"),
		    %% Player ! {go, DirectionID}

		    %% Remove the player from the player list,

		    UpdatedPlayers = lists:delete({Player,Name}, Players),
		    io:format(" ~p~n",[State]), %% TEST

		    %% Send a notification to the other players
		    messagePlayers(UpdatedPlayers, Name, Direction, exitnotification),

		    loop({Exits,NPC,Desc,UpdatedPlayers})
	    end;

	%% Look command from a player
	{look, Player} -> 
	    io:format("~p~n",[Desc]),
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}
	    loop(State);

	%% A new player enters the zone
	{enter, Player, Name, Direction} ->
	    %% Sends the description to the player
	    io:format("~p~n",[Desc]),
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}

	    %% Sends a notification to the other players
	    messagePlayers(Players, Name, Direction, enternotification),

	    %% Adds the player to the players list
	    NewPlayers = Players ++ [{Player, Name}],
	    io:format(" ~p~n",[NewPlayers]),

	    loop({Exits,NPC,Desc,NewPlayers});

	%% A logout command from a player
	{logout, Player, Name} ->

	    %% Remove the player from the player list,
	    UpdatedPlayers = lists:delete({Player,Name}, Players), 

	    %% Send a notification to the other players
	    messagePlayers(UpdatedPlayers, Name, logoutnotification),

	    loop({Exits,NPC,Desc,UpdatedPlayers})

	%% A inactivate command from the masterzone
	%% {inactivate} -> 
	%%    store_db(State),
	%%    ok
    end.
