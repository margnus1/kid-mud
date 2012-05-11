{application, kidmud, 
 [{description, "An Erlang MUD"},
  {vsn, "Ms2"},
  {modules, [client, colour, database, kidmud, maploader, master_supervisor,
	     parser_grammar, parser, player_sup, player, playermaster, zone_sup, zone, zonemaster]},
  {registered, [playermaster, player_sup, zonemaster, zone_sup, master_supervisor]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {kidmud, []}}
 ]}.
