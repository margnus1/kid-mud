%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
%% See the file license.txt for copying permission.

-module(maploader).
-export([load/1]).
-include("zone.hrl").
-include_lib("eunit/include/eunit.hrl").

-type exits() :: [{north | south | east | west, integer()}].
-type room1() :: {integer(), {integer(), integer()}, integer(),
		  {integer(), integer()}, npc:habitat(), string()}.
-type room2() :: {integer(), {integer(), integer()}, npc:habitat(),
		  string(), exits()}.

%% @doc Loads the [first of the] specified file[s that exists] and
%% inserts it into the database
-spec load([file:name()] | file:name()) -> ok | {error, posix()}.
load([Filename|More]) when is_list(Filename) or is_binary(Filename) ->
    case file:open(Filename, [read]) of
	{ok, Device} ->
	    Rooms = parse(Device, 0, []),
	    Zones = prepare_exits(Rooms, Rooms, []),
	    insert_zones(Zones);
	E_R ->
	    case More of
		[] -> E_R;
		_ -> load(More)
	    end
    end;
load(Filename) ->
    load([Filename]).

%% @doc Insert the zones Zones into the database
-spec insert_zones([room2()]) -> ok.
insert_zones([]) ->
    ok;
insert_zones([Zone|Rest]) ->
    write_zone(Zone),
    insert_zones(Rest).

    
%% Generate exit lists for the rooms InRooms to the rooms ExitRooms and append it to Acc
-spec prepare_exits([room1()], [room1()], [room2()]) -> [room2()].
prepare_exits(Rooms, [{Id, {X, Y}, MC, LR, Habitat, Desc}|Rest], Acc) ->
    prepare_exits(Rooms, Rest, [{Id, MC, LR, Habitat, Desc,
				 gen_exit(Rooms, {X+1,Y}, east) ++
				     gen_exit(Rooms, {X-1,Y}, west) ++
				     gen_exit(Rooms, {X,Y+1}, north) ++
				     gen_exit(Rooms, {X,Y-1}, south)} | Acc]);
prepare_exits(_, [], Acc) ->
    Acc.

%% @doc If there is a room with coordinats Coord in Rooms, returns 
%%      [{Dir, Id}] where Id is that rooms id.
%%      Otherwise returns [].
%% @end
-spec gen_exit([room1()], {integer(), integer()}, north | east | west | south) -> [exit()].
gen_exit(Rooms, Coord, Dir) ->
    case lists:keyfind(Coord, 2, Rooms) of
	false ->
	    [];
	{Id,_,_,_,_,_} ->
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
	    [Coords, Monsters, Description] = 
		string:tokens(Line, "\t"),
	    [Xstr, Ystr] = string:tokens(Coords, ","),
	    {X, Y} = {list_to_integer(Xstr), list_to_integer(Ystr)},
	    {MonsterCount, Habitat, LR} = 
		case string:tokens(Monsters, ":") of
		    ["0"] -> {0, none, {0, 0}};
		    [Count, Hab, LevelRange] ->
			[LowStr, HiStr] = string:tokens(LevelRange, "-"),
			{Low, Hi} = {list_to_integer(LowStr), list_to_integer(HiStr)},
			{list_to_integer(Count), list_to_atom(Hab), {Low, Hi}} 
		end,
	    parse(Device, Id+1, [{Id, {X, Y}, MonsterCount, LR, Habitat,
				  Description} | Acc])
    end.

%% @doc Removes the last element from the list L
-spec droplast([any()]) -> [any()].
droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

%% @doc Updates the zone Zone with the map data Data
-spec modify_zone(Zone :: zone(), Data :: room2()) -> zone().
modify_zone(Zone, {Id, MC, LR, Habitat, Desc, Exits}) ->
    NPC = case length(Zone#zone.npc) of
	      Smaller when Smaller < MC ->
		  Zone#zone.npc ++ 
		      lists:duplicate(MC - Smaller, {dead, {0,0,0}});
	      MC -> Zone#zone.npc;
	      Larger when Larger > MC ->
		  lists:sublist(Zone#zone.npc, MC)
	  end,
    Zone#zone{id=Id, desc=Desc, npc=NPC, exits=Exits, 
	      level_range=LR, habitat=Habitat}.

%% @doc Insert the zones Zones into the database
-spec write_zone(room2()) -> ok.
write_zone(Data = {Id, _, _, _, _, _}) ->
    database:write_zone(
      modify_zone(
	case database:read_zone(Id) of
	  zone_not_found -> #zone{id=0};
	  Zone           -> Zone
      end, Data)).
 

%% EUnit tests

temp_file_name() ->
    case os:getenv("TMP") of
	false -> "/tmp/";
	Dir -> Dir ++ "\\"
    end ++ 
    "temp" ++ integer_to_list(erlang:phash2(make_ref())) ++ ".tmp".


parse_test() ->
    Name = temp_file_name(),
    file:write_file(Name, "1,-1\t0\tTest Room One\n\n" ++ 
			  "-1,1\t2:road:2-6\tTest Room Two\n" ++
			  "% This is a comment\n"),
    {ok, Device} = file:open(Name, [read]),
    Result = parse(Device, 0, []),
    file:delete(Name),
    ?assertEqual([{1, {-1, 1}, 2, {2, 6}, road, "Test Room Two"},
		  {0, {1, -1}, 0, {0, 0}, none, "Test Room One"}],
		 Result).


prepare_exits_test_() ->
    Arg1 = [{0, {2,2}, 0, {0,0}, none, "A"}, {1, {2,3}, 0, {0,0}, none, "B"}, 
	    {3, {2,5}, 0, {0,0}, none, "C"}, {2, {1,5}, 0, {0,0}, none, "D"}],
    [?_assertEqual([{2, 0, {0,0}, none, "D", [{east, 3}]},
		    {3, 0, {0,0}, none, "C", [{west, 2}]},
		    {1, 0, {0,0}, none, "B", [{south, 0}]},
		    {0, 0, {0,0}, none, "A", [{north, 1}]}],
		   prepare_exits(Arg1, Arg1, []))].
