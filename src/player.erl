-module(player).
-export([login/2]).
-include("player.hrl").

-include_lib("eunit/include/eunit.hrl").

%% @doc Load the player with the name Name from the database and spawn a process for that player
%% @spec login(Name::string(), Console:pid() -> pid())
login(Name, Console) ->
    Player = database:read_player(Name),
    spawn(fun() -> loop(Console, zonemaster:get_zone(Player#player.location), Player) end).

loop(Console, ZonePID, Player) ->
    receive 
	{command, Command} ->
            case cmd = parser:parse(Command) of
                {go, Direction} ->
                    ZonePID ! {go, self(), Direction},
		    receive 
			{go, Id} ->
			    Player = #player{location = Id},
			    %% TODO: se till att man får veta vart man gick
			    Console ! {message, "You successfully moved"},
			    loop(Console, zonemaster:get_zone(Id), Player);
			
			{go, error, doesnotexist} ->
                            Console ! {message, "You cannot go that way"}
                    end;
                logout ->
		    ZonePID ! {logout, Player},
                    database:write_player(Player);

                look ->
                    ZonePID ! {look, self()},
                    receive {look, Description} ->
                            Console ! {message, Description}
		    end;
		
		parse_error ->
		    Console ! {message, "Command not recognized"}
	    end,
	    loop(Console, ZonePID, Player);
	
	{player_logout, Name} ->
	    Console ! {message, Name ++ " logged out"},
	    loop(Console, ZonePID, Player);

	{player_enter, Name, Direction} ->
	    Console ! {message, Name ++ " arrives from " ++ Direction},
	    loop(Console, ZonePID, Player)
    end.
