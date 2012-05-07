-module(player).
-export([login/2]).
-include("player.hrl").

-include_lib("eunit/include/eunit.hrl").

%% @doc Load the player with the name Name from the database and spawn a process for that player
-spec login(Name::string(), Console::pid()) -> pid().
login(Name, Console) ->
    Player = database:read_player(Name),
    ZoneID = zonemaster:get_zone(Player#player.location),
    Pid = spawn(fun() -> loop(Console, ZoneID, Player) end),
    ZoneID ! {enter, Pid, Name, login},
    Pid.

loop(Console, ZonePID, Player) ->
    receive 
	{command, Command} ->
            case parser:parse(Command) of
                {go, Direction} ->
                    ZonePID ! {go, self(), Direction},
		    receive 
			{go, Id} ->
			    Console ! {message, "You successfully moved " ++ atom_to_list(Direction)},

			    NewZonePID = zonemaster:get_zone(Id),
			    NewZonePID ! {enter, self(), Player#player.name, Direction},
			    loop(Console, NewZonePID, Player#player{location=Id});

			{go, error, doesnt_exist} ->
                            Console ! {message, "You cannot go that way"},
			    loop(Console, ZonePID, Player)
                    end;
                logout ->
		    ZonePID ! {logout, Player, Player#player.name},
                    database:write_player(Player),
		    loop(Console, ZonePID, Player);

                look ->
                    ZonePID ! {look, self()},
		    loop(Console, ZonePID, Player);

		parse_error ->
		    Console ! {message, "Command not recognized"},
		    loop(Console, ZonePID, Player)

	    end;

	{player_logout, Name} ->
	    Console ! {message, Name ++ " logged out"},
	    loop(Console, ZonePID, Player);

	{look, Description} ->
	    Console ! {message, Description},
	    loop(Console, ZonePID, Player);

	{player_leave, Name, Direction} ->
	    Console ! {message, Name ++ " went " ++ atom_to_list(Direction)},
	    loop(Console, ZonePID, Player);

	{player_enter, Name, Direction} ->
	    Console ! {message, Name ++ " arrives from " ++ atom_to_list(Direction)},
	    loop(Console, ZonePID, Player)
    end.
