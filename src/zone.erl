-module(zone).
-export([start/0,loop/2]).
-include("zone.hrl").


start() ->
    Data = database:read_zone(Id).
    Players = [{1,2},{3,4},{5,7}], %% Should load from DB

    spawn(zone, loop, [Players, Data]).


messagePlayers(List, Playername, Direction, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername, Direction}~n"), ok end, ok, List).


messagePlayers(List, Playername, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername}~n"), ok end, ok, List).


loop(Players, Data = #zone{exits=Exits,npc=NPC,desc=Desc}) ->
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
	       loop(Players, Data);

	       true -> 
		    io:format("Player ! {go, DirectionID}~n"),
		    %% Player ! {go, DirectionID}

		    %% Remove the player from the player list,

		    UpdatedPlayers = lists:delete({Player,Name}, Players),
		    io:format(" ~p~n",[State]), %% TEST

		    %% Send a notification to the other players
		    messagePlayers(UpdatedPlayers, Name, Direction, exitnotification),

		    loop(UpdatedPlayers, Data)
	    end;

	%% Look command from a player
	{look, Player} -> 
	    io:format("~p~n",[Desc]),
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}
	    loop(Players, Data);

	%% A new player enters the zone
	{enter, Player, Name, Direction} ->
	    %% Sends the description to the player
	    io:format("~p~n",[Desc]),
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}

	    %% Sends a notification to the other players
	    messagePlayers(Players, Name, Direction, enternotification),

	    %% Adds the player to the players list
	    UpdatedPlayers = [{Player, Name} | Players],
	    io:format(" ~p~n",[UpdatedPlayers]),

	    loop(UpdatedPlayers, Data);

	%% A logout command from a player
	{logout, Player, Name} ->

	    %% Remove the player from the player list,
	    UpdatedPlayers = lists:delete({Player,Name}, Players), 

	    %% Send a notification to the other players
	    messagePlayers(UpdatedPlayers, Name, logoutnotification),

	    loop(UpdatedPlayers, Data)

	%% A inactivate command from the masterzone
	%% {inactivate} -> 
	%%    database:write_zone(State),
	%%    ok
    end.
