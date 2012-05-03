-module(zone).
-export([start/1,loop/2]).

-include("zone.hrl").

start(Id) ->
    Data = database:read_zone(Id),
    Players = [{1,2},{3,4},{5,7}],
    spawn(zone, loop, [Players, Data]).

messagePlayers(List, Playername, Direction, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername, Direction}~n"), ok end, ok, List).

messagePlayers(List, Playername, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Player ! {Notice, Playername}~n"), ok end, ok, List).

messagePlayer(List, Id, Notice) ->
    lists:foldl(fun ({Player, Name}, _) -> io:format("Id ! {Notice, Name}~n"), ok end, ok, List).

loop(Players, Data = #zone{id=Id, exits=Exits, npc=NPC, desc=Desc}) ->
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
		    io:format(" ~p~n",[Data]), %% TEST

		    %% Send a notification to the other players
		    messagePlayers(UpdatedPlayers, Name, Direction, exit_notification),

		    %% Check if the zone is empty		    
		    if UpdatedPlayers =:= [] ->
			    database:write_zone(Data),
			    masterzone ! {zone_inactive, Id},
			    ok;

		       true ->
			    loop(UpdatedPlayers, Data)

		    end
	    end;

	%% Look command from a player
	{look, Player} -> 
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}
	    loop(Players, Data);

	%% A new player enters the zone
	{enter, Player, Name, Direction} ->
	    %% Sends the description to the player
	    io:format("Player ! {look, Desc}~n"),
	    %% Player ! {look, Desc}

	    %% Sends the player the other players
	    messagePlayer(Players, Player, player_in_zone_notification),

	    %% Sends a notification to the other players
	    messagePlayers(Players, Name, Direction, enter_notification),

	    %% Adds the player to the players list
	    UpdatedPlayers = [{Player, Name} | Players],
	    io:format(" ~p~n",[UpdatedPlayers]),

	    loop(UpdatedPlayers, Data);

	%% A logout command from a player
	{logout, Player, Name} ->

	    %% Remove the player from the player list,
	    UpdatedPlayers = lists:delete({Player,Name}, Players), 

	    %% Send a notification to the other players
	    messagePlayers(UpdatedPlayers, Name, logout_notification),

	    %% Check if the zone is empty
	    if UpdatedPlayers =:= [] ->
		    database:write_zone(Data),
		    masterzone ! {zone_inactive, Id},
		    ok;

	       true ->
		    loop(UpdatedPlayers, Data)

	    end

    end.
