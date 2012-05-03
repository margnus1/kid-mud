-module(player).
-export([login/2]).
-record(player, {name, client}).

login(Name, Console) ->
	    P = #player {name = Name, client = Console},
	    spawn(fun() -> loop(P) end).
	  
%% Ska ändras till en 2-tupel, sessionsdata-record och databasdatarecord    
loop(P) -> 
	receive {message, Message} ->
	       	  io:format("Message"),
       	       	  loop(P)
       	end.
		