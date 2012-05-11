%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth
%% See the file license.txt for copying permission.

{application, kidmud, 
 [{description, "An Erlang MUD"},
  {vsn, "Ms2"},
  {modules, [client, colour, database, kidmud, maploader, master_supervisor,
	     parser_grammar, parser, player, zone_sup, zone, zonemaster]},
  {registered, [zonemaster, zone_sup, master_supervisor]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {kidmud, []}}
 ]}.
