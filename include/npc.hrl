%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg, Michael Bergroth and Eric Arnerl�v
%% See the file license.txt for copying permission.

-record(npc, {id, 
	      name, 
	      disp=neutral, 
	      health=30, 
	      damage=3, 
	      attack_speed=1.0,
	      level=1, 
	      habitat=forest}).

-type habitat() :: beach | forest | cave | road.
-type disposition() :: helpful | neutral | hostile.
-type npc_health() :: {erlang:timestamp(), float(), float()}.

-type npc() :: #npc {id :: integer(), 
		     name :: string(), 
		     disp :: disposition(),
		     health :: npc_health(),
		     damage :: integer(),
                     attack_speed :: float(),
		     level :: integer(),
		     habitat :: habitat()}.
