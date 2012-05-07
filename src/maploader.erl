-module(maploader).
-export([load/1]).
-include("zone.hrl").

load(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    Rooms = loop(Device, 0, []),
    Zones = prepare_exits(Rooms, Rooms, []),
    insert_zones(Zones).

insert_zones([]) ->
    ok;
insert_zones([{Id, Desc, Exits}|Rest]) ->
    write_zone(Id, Desc, Exits),
    insert_zones(Rest).

    

prepare_exits(Rooms, [{Id, {X, Y}, Desc}|Rest], Acc) ->
    prepare_exits(Rooms, Rest, [{Id, Desc, gen_exit(Rooms, {X-1,Y}, east) ++
				     gen_exit(Rooms, {X-1,Y}, east) ++
				     gen_exit(Rooms, {X-1,Y}, east) ++
				     gen_exit(Rooms, {X-1,Y}, east)} | Acc]);
prepare_exits(_, [], Acc) ->
    Acc.


gen_exit(Rooms, Coord, Dir) ->
    case lists:keyfind(Coord, 2, Rooms) of
	false ->
	    [];
	{Id,_,_} ->
	    [{Dir, Id}]
    end.


loop(Device, Id, Acc) ->
    case io:get_line(Device, "") of
	eof ->
	    file:close(Device),
	    Acc;
	Data -> 
	    Line = droplast(Data),
	    [Coords, Description] = string:tokens(Line, "\t"),
	    [Xstr, Ystr] = string:tokens(Coords),
	    {X, Y} = {list_to_integer(Xstr), list_to_integer(Ystr)},
	    loop(Device, Id+1, [{Id, {X, Y}, Description}|Acc])
    end.

droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

write_zone(Id, Desc, Exits) ->
    database:write_zone(
      case database:read_zone(Id) of
	  zone_not_found ->
	      #zone{id=Id, desc=Desc, exits=Exits};
	  Zone ->
	      Zone#zone{id=Id, desc=Desc, exits=Exits}
      end).
