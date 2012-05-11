%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arn\erlöv
%% See the file license.txt for copying permission.

-module(maploader).
-export([load/1]).
-include("zone.hrl").
-include_lib("eunit/include/eunit.hrl").

-type exits() :: [{north | south | east | west, integer()}].
-type room1() :: {integer(), {integer(), integer()}, string()}.
-type room2() :: {integer(), string(), exits()}.

%% @doc Loads the mapfile Filename and inserts it into the database
-spec load(file:name()) -> ok.
load(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    Rooms = parse(Device, 0, []),
    Zones = prepare_exits(Rooms, Rooms, []),
    insert_zones(Zones).

%% @doc Insert the zones Zones into the database
-spec insert_zones([room2()]) -> ok.
insert_zones([]) ->
    ok;
insert_zones([{Id, Desc, Exits}|Rest]) ->
    write_zone(Id, Desc, Exits),
    insert_zones(Rest).

    
%% Generate exit lists for the rooms InRooms to the rooms ExitRooms and append it to Acc
-spec prepare_exits([room1()], [room1()], [room2()]) -> [room2()].
prepare_exits(Rooms, [{Id, {X, Y}, Desc}|Rest], Acc) ->
    prepare_exits(Rooms, Rest, [{Id, Desc, gen_exit(Rooms, {X+1,Y}, east) ++
				     gen_exit(Rooms, {X-1,Y}, west) ++
				     gen_exit(Rooms, {X,Y+1}, north) ++
				     gen_exit(Rooms, {X,Y-1}, south)} | Acc]);
prepare_exits(_, [], Acc) ->
    Acc.

%% @doc If there is a room with coordinats Coord in Rooms, returns 
%%      [{Dir, Id}] where Id is that rooms id.
%%      Otherwise returns [].
%% @end
-spec gen_exit([room1()], {integer(), integer()}, north | east | west | south) -> [{north | east | west | south, integer()}].
gen_exit(Rooms, Coord, Dir) ->
    case lists:keyfind(Coord, 2, Rooms) of
	false ->
	    [];
	{Id,_,_} ->
	    [{Dir, Id}]
    end.

%% @doc
%% Reads lines from device on the format X,Y{tab}Desc and parses them into a list.
%% The rooms are numbered from Id, inclusicem and the resulting list is appended to Acc.
%% @end
-spec parse(file:io_device(), integer(), [room1()]) -> [room1()].
parse(Device, Id, Acc) ->
    case io:get_line(Device, "") of
	eof ->
	    file:close(Device),
	    Acc;
	[$%|_] ->
	    parse(Device, Id, Acc);
	"\n" -> 
	    parse(Device, Id, Acc);
	Data -> 
	    Line = droplast(Data),
	    [Coords, Description] = string:tokens(Line, "\t"),
	    [Xstr, Ystr] = string:tokens(Coords, ","),
	    {X, Y} = {list_to_integer(Xstr), list_to_integer(Ystr)},
	    parse(Device, Id+1, [{Id, {X, Y}, Description}|Acc])
    end.

%% @doc Removes the last element from the list L
-spec droplast([any()]) -> [any()].
droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

%% @doc Insert the zones Zones into the database
-spec write_zone(integer(), string(), exits()) -> ok.
write_zone(Id, Desc, Exits) ->
    database:write_zone(
      case database:read_zone(Id) of
	  zone_not_found ->
	      #zone{id=Id, desc=Desc, exits=Exits};
	  Zone ->
	      Zone#zone{id=Id, desc=Desc, exits=Exits}
      end).
 

%% EUnit tests

temp_file_name() ->
    "temp.tmp".


parse_test() ->
    Name = temp_file_name(),
    file:write_file(Name, "1,-1\tTest Room One\n\n" ++ 
			  "-1,1\tTest Room Two\n% This is a comment\n"),
    {ok, Device} = file:open(Name, [read]),
    Result = parse(Device, 0, []),
    ?assertEqual([{1, {-1, 1}, "Test Room Two"},
		  {0, {1, -1}, "Test Room One"}],
		 Result).


prepare_exits_test_() ->
    Arg1 = [{0,{2,2},"A"},{1,{2,3},"B"}, {3, {2,5}, "C"}, {2, {1,5}, "D"}],
    [?_assertEqual([{2, "D", [{east, 3}]},
		    {3, "C", [{west, 2}]},
		    {1, "B", [{south, 0}]},
		    {0, "A", [{north, 1}]}],
		   prepare_exits(Arg1, Arg1, []))].
