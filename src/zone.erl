-module(zone).
-export([start/1,loop/2]).

-include("zone.hrl").

%% @doc Starts the zone
start(Id) ->
    Data = database:read_zone(Id),
    Players = [], %% Should be empty!
    spawn(zone, loop, [Players, Data]).

%% @doc Sends a message to all the players in the zone when a new player enters
messagePlayers([{Player, _}|Rest], Playername, Direction, Notice) ->
    Player ! {Notice, Playername, Direction}, 
    messagePlayers(Rest, Playername, Direction, Notice);
messagePlayers([], _, _, _) -> ok.


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
	{go, Player, Direction} ->
	    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
				 Dir =:= Direction ],
	    case E of
		[] ->

		    %% There is no exit in that location
		    Player ! {go, error, doesnt_exist},

		    loop (Players, Data);

		[DirectionID] -> 
		    %% There is an exit in that location
		    Player ! {go, DirectionID},

		    %% Remove the player from the player list,
		    
		    UpdatedPlayers = lists:keydelete(Player, 1, Players),

		    %% Check if the zone is empty		    
		    if UpdatedPlayers =:= [] ->
			    %% Store and close
			    database:write_zone(Data),
			    zonemaster ! {zone_inactive, Id},
			    ok;

		       true ->
			    %% Send a notification to the other players
			    [Name] = [Name || {Player, Name} <- Players],
			    messagePlayers(UpdatedPlayers, Name, Direction, player_leave),

			    loop(UpdatedPlayers, Data)
		    end
	    end;

	%% A 'look' command from a player
	{look, Player} -> 
	    %% Sends the description to the player
	    Player ! {look, look(lists:keydelete(Player, 1, Players), Data)},
	    Player ! {look, exits_message(Exits)},
	    loop(Players, Data);

	%% A new player enters the zone
	{enter, Player, Name, Direction} ->
	    %% Sends the description to the player
	    Player ! {look, look(Players, Data)},
	    Player ! {look, exits_message(Exits)},

	    %% Sends a notification to the other players
	    messagePlayers(Players, Name, Direction, player_enter),

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
		    messagePlayers(UpdatedPlayers, Name, player_logout),

		    loop(UpdatedPlayers, Data)
	    end;

	    %% A 'exits' command from a player
	{exits, Player} -> 
	    Player ! {look, exits_message(Exits)},
	    loop(Players, Data)

	    %% A 'drop item' command from a player
	    %% A 'take item' command from a player
	    %% A 'con target' command from a player (consider)
	    %% A 'attack/kill target' command from a player
	    %% A 'look target' command from a player
	    %% A 'say line' command from a player

    end.

%% @doc Constructs a "look" message
-spec look(Players::[player()], Zone::zone()) -> string().

look(Players, Zone) ->
    lists:flatten(
      io_lib:format(
	"~s~n" ++
	%% "~s~n" ++
	"~s",
	%% "~s",
	[Zone#zone.desc,
	 %% string:join(lists:map(fun(NPC) -> "Here stands " ++ NPC#npc.name end,
	 %% 		       Zone#zone.npc), "~n"),
	 string:join(lists:map(fun({_, Name}) -> "Here stands " ++ Name end,
			       Players), "~n")
	 %% string:join(lists:map(fun({Amount, Item}) -> "Here lies " ++ 
	 %% 						  format_item(Amount, Item) end,
	 %% 		       Zone#zone.items), "~n")
	])).


format_item(Amount, Item) ->
    lists:flatten(
      io_lib:format(
	"~d ~s", [Amount, Item#item.name])).

exits_message(Exits) ->
    "There are exits to " ++ 
	string:join(lists:map(fun ({Dir, _}) -> atom_to_list(Dir) end, Exits), ", ").
