-module(zone).
-export([start/1,loop/2]).

-include("zone.hrl").

%% @doc Starts the zone
start(Id) ->
    Data = database:read_zone(Id),
    Players = [], %% Should be empty!
    spawn(zone, loop, [Players, Data]).

%% @doc Sends a message to all the players in the zone
messagePlayers([{Player, _}|Rest], Playername, Message, Integer, Notice) ->
    Player ! {Notice, Playername, Message, Integer}, 
    messagePlayers(Rest, Playername, Message, Integer, Notice);
messagePlayers([], _, _, _, _) -> ok.

%% @doc Sends a message to all the players in the zone
messagePlayers([{Player, _}|Rest], Playername, Message, Notice) ->
    Player ! {Notice, Playername, Message}, 
    messagePlayers(Rest, Playername, Message, Notice);
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
		%% There is no exit in that location
		[] ->
		    Player ! {go, error, doesnt_exist},

		    loop (Players, Data);

		%% There is an exit in that location
		[{_, DirectionID}] -> 
		    %% Remove the player from the player list,
		    UpdatedPlayers = lists:keydelete(Player, 1, Players), 

		    %% Check if the zone is empty		    
		    if UpdatedPlayers =:= [] ->
			    %% Store and close
			    database:write_zone(Data),
			    zonemaster ! {zone_inactive, Id},

			    Player ! {go, DirectionID},
			    ok;

		       true ->
			    %% Send a notification to the other players
			    {_, Name} = lists:keyfind(Player, 1, Players), 

			    Player ! {go, DirectionID},

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
	    loop(Players, Data);

	%% A 'kick' command from somewhere
	{kick, Name} ->

	    case lists:keyfind(Name, 2, Players) of 

		{Player,_} ->

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

		false ->
		    loop(Players, Data)
	    end;

	%% A 'drop item' command from a player
	%% A 'take item' command from a player
	%% A 'con target' command from a player (consider)
	%% A 'look at target' command from a player

	%% A 'attack/kill target' command from a player

	%% A 'death' message from a player
	%%{death, Player, Killer} ->
	%%{_, Name} = lists:keyfind(Player, 1, Players),

	%%Stop any attacking mobs

	%% Remove the player from the player list,
	%%UpdatedPlayers = lists:keydelete(Player, 1, Players),
	
	%% Check if the zone is empty		    
	%%if UpdatedPlayers =:= [] ->
		%% Store and close
	%%	database:write_zone(Data),
	%%	zonemaster ! {zone_inactive, Id},

	%%	ok;
	%%true ->
	%%	messagePlayers(UpdatedPlayers, Name, Killer, player_died),  %% new message!

	%%	loop(UpdatedPlayers, Data)
	%%end;

	%% Player ! {receive_exp, Amount}

	%% messagePlayers(Players, Name, Mob, Amount, player_damage), %% Mob hits Name for Amount damage.
	%% Player ! {take_damage, Amount}
	%% Player ! {skill, Player, Skill, Target}



%% Player {PID, NAME, STATUS, AS, TOHIT, DAMAGE, DV(defensive value), PV(protection value)}

%% Eller vi bör hämta detta ur databasen.

%%kill message (attack, Attacker, Target, AS, ToHit, Damage, Type),

%%case Status of 
%%	combat -> Player ! already_in_combat
%%		  loop

%%	normal -> 
%%		calculate if hit.
%%		send message to everyone of the outcome
%%		if NPC make him aggro Player and set timer for new attack
%%		if AnotherPlayer send him damage report if any.
%%		set timer for new attack depending on AS
%%		loop

%%	fleeing -> loop
%%	dead -> loop

%%{flee, Player} -> set Player status = fleeing
%%		  message other Players that Player is trying to flee
%%		  set timer for flee attempt
%%		  loop

	%% A 'attack/kill target' command from a player
	%%{attack, Player, Target, Damage, Hit} ->
	%% Calculate if he hits the target

	%% Calculate the damage to the target if he hits

	%% Message the outcome to the player that is being attacked (if any)

	%% Message the outcome to the other players
	%%{_, Name} = lists:keyfind(Player, 1, Players),
	%%messagePlayers(lists:keydelete(Player, 1, Players), Name, Target, Damage, player_attack);

	%% A 'say line' command from a player
	{say, Player, Message} -> 
	    {_, Name} = lists:keyfind(Player, 1, Players),
	    messagePlayers(lists:keydelete(Player, 1, Players), Name, Message, say),
	    loop(Players, Data)
    end.

%% @doc Constructs a "look" message
-spec look(Players::[player()], Zone::zone()) -> string().

look(Players, Zone) ->
    string:join(
      [Zone#zone.desc] ++
	 %% lists:map(fun(NPC) -> "Here stands " ++ NPC#npc.name end,
	 %% 		       Zone#zone.npc) ++
	 lists:map(fun({_, Name}) -> "Here stands " ++ Name end,
			       Players), %% ++
	 %% lists:map(fun({Amount, Item}) -> "Here lies " ++ 
	 %% 		format_item(Amount, Item) end, Zone#zone.items),
      "\n").

format_item(Amount, Item) ->
    lists:flatten(
      io_lib:format(
	"~d ~s", [Amount, Item#item.name])).


exits_message([{Exit,_}]) ->
    "There is an exit to the " ++ atom_to_list(Exit);
exits_message(Exits) ->
    "There are exits to " ++ 
	string:join(lists:map(fun ({Dir, _}) -> atom_to_list(Dir) end, Exits), ", ").

