-module(zone).
-export([start/1,loop/2]).

-include("zone.hrl").

%% @doc Starts the zone
start(Id) ->
    Data = database:read_zone(Id),
    Players = [{1,2},{3,4}], %% Should be empty!
    spawn(zone, loop, [Players, Data]).

%% @doc Sends a message to all the players in the zone when a new player enters
messagePlayers(PlayerList, Playername, Direction, Notice) ->
    lists:foldl(fun ({Player, _}, _) -> Player ! {Notice, Playername, Direction}, ok end, ok, PlayerList).

%% @doc Sends a message to all the players in the zone when a player logout
messagePlayers(PlayerList, Playername, Notice) ->
    lists:foldl(fun ({Player, _}, _) -> Player ! {Notice, Playername}, ok end, ok, PlayerList).

%% @doc Sends Notice and a list to the Player of all the names in List if its not empty.
messagePlayer([], _, _) ->
    ok;
messagePlayer(List, Player, Notice) ->
    Names = [ Name || {_, Name} <- List ],
    Player ! {Notice, Names}.

%% @doc The main loop of the zone
loop(Players, Data = #zone{id=Id, exits=Exits, npc=NPCs, desc=Desc}) ->
    receive 
	%% A 'go' command from a player
	{go, Player, Name, Direction} ->
	    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
				 Dir =:= Direction ],
	    {_,DirectionID} = hd(E),

	%%Göra om så att den bara har exits som finns?, annars tomma listan?
	%%I databasen dvs.

	    %% Checks if the exit is valid
	    if DirectionID =:= none ->
		    %% There is no exit in that location
		    Player ! {go, error, doesnt_exist},

		    loop (Players, Data);

	       true -> 
		    %% There is an exit in that location
		    Player ! {go, DirectionID},

		    %% Remove the player from the player list,
		    UpdatedPlayers = lists:delete({Player,Name}, Players),

		    %% Check if the zone is empty		    
		    if UpdatedPlayers =:= [] ->
			    %% Store and close
			    database:write_zone(Data),
			    zonemaster ! {zone_inactive, Id},
			    ok;

		       true ->
			    %% Send a notification to the other players
			    messagePlayers(UpdatedPlayers, Name, Direction, exit_notification),

			    loop(UpdatedPlayers, Data)
		    end
	    end;

	%% A 'look' command from a player
	{look, Player} -> 
	    %% Sends the description to the player
	    Player ! {look, Desc},

	    %% Sends the player the other players as a list of names
	    messagePlayer(Players, Player, players_in_zone),

	    %% Sends the player the NPC's as a list of names
	    %%messagePlayer(NPC, Player, npcs_in_zone),

	    %% Sends the player the Item's as a list of names
	    %%messagePlayer(Items, Player, items_in_zone),

	    %% Sends the player the Exits's as a list of names
	    %%Player ! {exits, exits_in_zone},

	    loop(Players, Data);

	%% A new player enters the zone
	{enter, Player, Name, Direction} ->
	    %% Sends the description to the player
	    Player ! {look, Desc},

	    %% Sends the player the other players as a list of names
	    messagePlayer(Players, Player, players_in_zone),

	    %% Sends the player the NPCs as a list of names
	    %%messagePlayer(NPCs, Player, npcs_in_zone),

	    %% Sends the player the Items as a list of names
	    %%messagePlayer(Items, Player, items_in_zone),

	    %% Sends the player the Exits's as a list of names
	    %%Player ! {exits, exits_in_zone},

	    %% Sends a notification to the other players
	    messagePlayers(Players, Name, Direction, enter_notification),

	    %% Adds the player to the players list
	    UpdatedPlayers = [{Player, Name} | Players],

	    loop(UpdatedPlayers, Data);

	%% A 'logout' command from a player
	{logout, Player, Name} ->
	    %% Remove the player from the player list,
	    UpdatedPlayers = lists:delete({Player,Name}, Players), 

	    %% Check if the zone is empty
	    if UpdatedPlayers =:= [] ->
		    %% Store and close
		    database:write_zone(Data),
		    zonemaster ! {zone_inactive, Id},
		    ok;

	       true ->
		    %% Send a notification to the other players
		    messagePlayers(UpdatedPlayers, Name, logout_notification),

		    loop(UpdatedPlayers, Data)
	    end

	%% A 'exits' command from a player
	%% {exits, Player} -> 
	%%Player ! {exits, exits_in_zone};

	%% A 'drop item' command from a player
	%% A 'take item' command from a player
	%% A 'con target' command from a player (consider)
	%% A 'attack/kill target' command from a player
	%% A 'look target' command from a player
	%% A 'say line' command from a player

    end.
