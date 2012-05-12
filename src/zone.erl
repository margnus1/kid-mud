%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arnerlöv
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Magnus Lang <mala7837@fries.it.uu.se>
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2012 by Magnus Lång <mala7837@fries.it.uu.se>
%%%-------------------------------------------------------------------
-module(zone).
-include("zone.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1, go/3, validate_target/2, look/2, enter/4, logout/2,
	 exits/2,  kick/2, death/2, attack/4, say/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Id :: integer()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).


%%--------------------------------------------------------------------
%% @doc
%% Looks if the direction is an valid exit.
%% If the exit is invalid, tell the player that he cannot move that way.
%% Otherwise return the exits ID and remove the player from the zone. 
%%
%% @end
%%--------------------------------------------------------------------
-spec go(pid(), pid(),
	 north | south | east | west | login) -> ok.
go(Zone, PlayerPID, Direction) ->
    gen_server:call(Zone, {go, PlayerPID, Direction}).

%%--------------------------------------------------------------------
%% @doc
%% Checks if Target is a valid target in the zone.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_target(pid(), string()) -> ok.
validate_target(Zone, Target) ->
    gen_server:call(Zone, {validate_target,  Target}).


%%--------------------------------------------------------------------
%% @doc
%% Return the look information of the zone to the player.
%% 
%% @end
%%--------------------------------------------------------------------
-spec look(pid(), pid()) -> ok.
look(Zone, PlayerPID) ->
    gen_server:cast(Zone, {look, PlayerPID}).


%%--------------------------------------------------------------------
%% @doc
%% Add the player to the zones player list and send the information 
%% of the zone to the player.
%% Inform the other players in the zone of this change.
%% 
%% @end
%%--------------------------------------------------------------------
-spec enter(pid(), pid(), string(), 
	    north | south | east | west | login) -> ok.
enter(Zone, PlayerPID, Name, Direction) ->
    gen_server:cast(Zone, {enter, PlayerPID, Name, Direction}).


%%--------------------------------------------------------------------
%% @doc
%% Remove the player from the zone. 
%% If that was the last player in the zone the zone will shutdown, 
%% otherwise inform the other players in the zone of this change.
%% @end
%%--------------------------------------------------------------------
-spec logout(pid(), pid()) ->  ok.
logout(Zone, PlayerPID) ->
    gen_server:cast(Zone, {logout, PlayerPID}).


%%--------------------------------------------------------------------
%% @doc
%% Return the exits of the zone to the player.
%% 
%% @end
%%--------------------------------------------------------------------
-spec exits(pid(), pid()) -> ok.
exits(Zone, PlayerPID) ->
    gen_server:cast(Zone, {exits, PlayerPID}).


%%--------------------------------------------------------------------
%% @doc
%% Remove the player from the zone and send a kick message to that player.
%% If that was the last player in the zone the zone will shutdown, 
%% otherwise inform the other players in the zone of this change.
%%
%% @end
%%--------------------------------------------------------------------
-spec kick(pid(), string()) -> ok.
kick(Zone, Name) ->
    gen_server:cast(Zone, {kick, Name}).


%%--------------------------------------------------------------------
%% @doc
%% If Target is a valid target make an attack on it, 
%% otherwise tell player to stop attacking that Target.
%% Inform the target of this attack.
%% Inform all the players in the zone of this attack.
%% 
%% @end
%%--------------------------------------------------------------------
-spec attack(pid(), pid(), string(), integer()) -> ok.
attack(Zone, PlayerPID, Target, Damage) ->
	gen_server:cast(Zone, {attack, PlayerPID, Target, Damage}).


%%--------------------------------------------------------------------
%% @doc
%% Remove the player from the zone.
%% If that was the last player in the zone the zone will shutdown, 
%% otherwise inform the other players in the zone of this change.
%% @end
%%--------------------------------------------------------------------
-spec death(pid(), pid()) -> ok.
death(Zone, PlayerPID) ->
    gen_server:cast(Zone, {death, PlayerPID}).


%%--------------------------------------------------------------------
%% @doc
%% Broadcasts the say Message to all the players in the zone.
%% 
%% @end
%%--------------------------------------------------------------------
-spec say(pid(), pid(), string()) -> ok.
say(Zone, PlayerPID, Message) ->
    gen_server:cast(Zone, {say, PlayerPID, Message}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Id]) ->
    Data = database:read_zone(Id),
    Players = [],
    {ok, {Players, Data}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({go, PlayerPID, Direction},
	    _From, {Players, Data = #zone{id=Id, exits=Exits}}) ->
    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
			 Dir =:= Direction ],
    case E of
	[] ->
	    {reply, {error, doesnt_exist}, {Players, Data}};

	[{_, DirectionID}] -> 
	    case lists:keydelete(PlayerPID, 1, Players) of

		[] ->
		    zonemaster:zone_inactive(Id),
		    {reply, {ok, DirectionID}, {[], Data}};

		UpdatedPlayers ->
		    Name = get_name(PlayerPID, Players),

		    [player:message(
		       PlayerPIDs, [Name, " has left ", 
				    atom_to_list(Direction)]) || 
			{PlayerPIDs,_} <- UpdatedPlayers],


		    {reply, {ok, DirectionID}, {UpdatedPlayers, Data}}
	    end
    end;


handle_call({validate_target, Target},
	    _From, {Players, Data}) -> %%= #zone{npc=NPC}}) ->

    %% @todo make help function maybe

    %%case lists:keyfind(Target, 2, NPC) of	
    %%	{_,_,_,_,_} ->
    %%	    {reply, valid_target, {Players, Data}};
    %%	false ->
    %%	    ok
    %%    end,

    case lists:keyfind(Target, 2, Players) of	
	{_, _} ->
	    {reply, valid_target, {Players, Data}};
	false ->
	    {reply, no_target, {Players, Data}}
    end;


handle_call(Request, _From, State) ->
    Reply = ok,
    io:fwrite("Unknown call to zone ~p: ~p~n", [self(), Request]),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({look, PlayerPID}, 
	    State={Players, Data = #zone{exits=Exits}}) ->
    player:message(
      PlayerPID, look_message(lists:keydelete(PlayerPID, 1, Players), Data)),
    player:message(PlayerPID, exits_message(Exits)),

    %%Bara ett test, som inte funkade helt korrekt
    %%NewNPC =  [#npc{id=random:uniform(100), 
    %%		    name="ARNE", disp=aggressive} | NPC],

    {noreply, {Players,Data}}; %%#zone{npc=NewNPC}}};


handle_cast({enter, PlayerPID, Name, Direction}, 
	    {Players, Data = #zone{exits=Exits}}) ->
    player:message(PlayerPID, look_message(Players, Data)),
    player:message(PlayerPID, exits_message(Exits)),

    [player:message(
       PlayerPIDs, [Name, format_arrival(Direction)] ) || 
	{PlayerPIDs,_} <- Players],

    UpdatedPlayers = [{PlayerPID, Name} | Players],

    {noreply, {UpdatedPlayers, Data}};

handle_cast({logout, PlayerPID}, {Players, Data = #zone{id=Id}}) ->
    case lists:keydelete(PlayerPID, 1, Players) of 

	[] ->
	    zonemaster:zone_inactive(Id),
	    {noreply,  {[], Data}};

	UpdatedPlayers ->

	    [player:message(
	       PlayerPIDs, 
	       [get_name(PlayerPID, Players), " has logged out"]) || 
		{PlayerPIDs,_} <- UpdatedPlayers],

	    {noreply, {UpdatedPlayers, Data}}
    end;


handle_cast({exits, PlayerPID}, State={_,#zone{exits=Exits}}) ->
    player:message(PlayerPID, exits_message(Exits)),
    {noreply, State};


handle_cast({kick, Name}, {Players, Data = #zone{id=Id}}) ->
    case lists:keyfind(Name, 2, Players) of 
	{PlayerPID, _} ->
	    player:kick(PlayerPID),

	    case lists:delete({PlayerPID,Name}, Players) of 
		[] ->
		    zonemaster:zone_inactive(Id),
		    {noreply,  {[], Data}};

		UpdatedPlayers ->

		    [player:message(
		       PlayerPIDs, 
		       [Name, " has logged out"]) || 
			{PlayerPIDs,_} <- UpdatedPlayers],

		    {noreply, {UpdatedPlayers, Data}}
	    end;

	false ->
	    {noreply, {Players, Data}}
    end;


handle_cast({attack, PlayerPID, Target, Damage}, 
	    State = {Players, Data}) -> %% = #zone{npc=NPC}}) ->

    Name = get_name(PlayerPID, Players),

    %% @todo Add NPC combat
    %%case lists:keyfind(Target, 3, NPC) of	
    %%	NPCtarget ->
    %%	    player:message(PlayerPID,"You!!!!!!!!!!!!!"),
    %%	    {noreply, State};
    %%	false ->
    %%	    ok
    %%   end,

    %% Player VS Player combat
    case lists:keyfind(Target, 2, Players) of	
	{TargetPID, _} ->
	    OtherPlayers = lists:keydelete(PlayerPID, 1, Players),
	    Rest =  lists:keydelete(TargetPID, 1, OtherPlayers),
	    %% Om man slår sig själv får man 2 meddelanden, fixa?
	    case Damage of 
		miss ->
		    [player:message(
		       PlayerPIDs, 
		       io_lib:format("~s misses ~s",
				     [Name, Target])) || 
			{PlayerPIDs,_} <- Rest],

		    player:message(PlayerPID, 
				   ["You miss your attack on ", Target]),
		    player:message(TargetPID,
				   [Name, " misses his attack on YOU"]),
		    {noreply, State};
		Damage -> 
		    [player:message(
		       PlayerPIDs, 
		       io_lib:format("~s hits ~s for ~p",
				     [Name, Target, Damage])) || 
			{PlayerPIDs,_} <- Rest],

		    player:message(
		      PlayerPID, io_lib:format(
				   "You hit ~s for ~p", [Target, Damage])),

		    player:damage(TargetPID, Damage, Name),
		    {noreply, State}
	    end;

	false ->
	    player:stop_attack(PlayerPID, Target),
	    {noreply, State} 
    end;


handle_cast({death, PlayerPID}, {Players, Data = #zone{id=Id}}) ->
    Name = get_name(PlayerPID, Players), 

    case lists:keydelete(PlayerPID, 1, Players) of
	[] ->
	    zonemaster:zone_inactive(Id),
	    {noreply,  {[], Data}};

	UpdatedPlayers ->
	    playermaster:broadcast([Name, " has been slain!"]),

	    [player:stop_attack(
	       PlayerPIDs, Name) || {PlayerPIDs,_} <- Players],

	    {noreply, {UpdatedPlayers, Data}}
    end;


handle_cast({say, PlayerPID, Message}, State={Players,_}) ->
    Name = get_name(PlayerPID, Players),

    [player:message(
       PlayerPIDs, 
       [Name, " says \"", Message, "\""]) || {PlayerPIDs,_} <- Players],
    {noreply, State};


handle_cast(Msg, State) ->
    io:fwrite("Unknown cast to zone ~p: ~p~n", [self(), Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:fwrite("Unknown info to zone ~p: ~p~n", [self(), Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, {Players, Data}) ->
    [player:kick(PlayerPID) || {PlayerPID,_} <- Players],

    database:write_zone(Data),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Constructs a "look" message
-spec look_message(Players::[player()], Zone::zone()) -> string().

look_message(Players, Zone) ->
    [Zone#zone.desc,
     %%lists:map(fun(NPC) -> ["\n", "Here stand a ", 
     %%			    NPC#npc.name, ":", atom_to_list(NPC#npc.disp)] end,
     %%	       Zone#zone.npc),
     colour:text(blue, lists:map(fun({_, Name}) -> 
					 ["\n", "Here stands ", Name] end,
				 Players))]. %% ++
      %% lists:map(fun({Amount, Item}) -> "Here lies " ++ 
      %% 		format_item(Amount, Item) end, Zone#zone.items),

%% @doc Constructs a "item" message
format_item(Amount, Item) ->
    lists:flatten(
      io_lib:format(
	"~p ~s", [Amount, Item#item.name])).

%% @doc Constructs a "exits" message
exits_message([]) ->
    "You see no exit";
exits_message([{Exit,_}]) ->
    ["There is an exit to the ", atom_to_list(Exit)];
exits_message(Exits) ->
    ["There are exits to ", 
     string:join(
       lists:map(fun ({Dir, _}) -> atom_to_list(Dir) end, Exits), ", ")].

%% @doc Gets the name of the player with PID Player from player list Players
get_name(PlayerPID, Players) ->
    {_, Name} = lists:keyfind(PlayerPID, 1, Players), 
    Name.

%% @doc Constructs a "arrival" message
format_arrival(north) -> " arrives from south";
format_arrival(east) -> " arrives from west";
format_arrival(south) -> " arrives from north";
format_arrival(west) -> " arrives from east";
format_arrival(login) -> " logged in".
    

%%%===================================================================
%%% EUnit Tests
%%%===================================================================


test_setup() ->
    database_setup(),
    register(zonemaster, self()),
    %%register(playermaster, self()),  %%cant register playermaster..
    ok.

database_setup() ->
    mnesia:start(),
    database:create_tables([]).

%% @hidden
fetch() ->
    receive
        Anything ->
            Anything
    end.

zone_go_test_() ->
    {setup, fun test_setup/0, 
     [?_assertEqual(
	 {reply,{ok, 5}, {[{self(),"Arne"}],
			  #zone{id=14, desc="A room!",
				exits=[{south,5}]}}},
	 handle_call({go, self(), south}, self(), 
		     {[{self(),"Kalle"}, {self(),"Arne"}],
		      #zone{id=14, desc="A room!", exits=[{south,5}]}})),

      ?_assertEqual(
	 {'$gen_cast', 
	  {message, ["Kalle", " has left ", "south"]}}, fetch()),

      ?_assertEqual(
	 {reply,{error,doesnt_exist}, 
	  {[],#zone{id=14, desc="A room!", exits=[]}}},
	 handle_call({go, self(), south}, self(), 
		     {[],#zone{id=14, desc="A room!", exits=[]}})),

      ?_assertEqual({reply,{ok, 9}, {[], 
				     #zone{id=12, desc="A room!",
					   exits=[{south,9}]}}},
		    handle_call(
		      {go, self(), south}, self(), 
		      {[{self(),"Arne"}], 
		       #zone{id=12, desc="A room!", exits=[{south,9}]}}))]}.

zone_validate_target_test_() ->
    [?_assertEqual({reply,valid_target, 
		    {[{self(),"Kalle"},{self(),"Arne"}],
		     #zone{id=14, desc="A room!",
			   exits=[{south,5}]}}},
		   handle_call(
		     {validate_target, "Kalle"}, self(), 
		     {[{self(),"Kalle"}, {self(),"Arne"}],
		      #zone{id=14, desc="A room!", exits=[{south,5}]}})),

     ?_assertEqual({reply,no_target, 
		    {[{self(),"Kalle"}],
		     #zone{id=14, desc="A room",
			   exits=[{south,5}]}}},
		   handle_call(
		     {validate_target, "Arne"}, self(), 
		     {[{self(),"Kalle"}],
		      #zone{id=14, desc="A room", exits=[{south,5}]}})),

     ?_assertEqual({reply,no_target, 
		    {[],#zone{id=14, desc="A", exits=[]}}},
		   handle_call(
		     {validate_target, "Arne"}, self(), 
		     {[],#zone{id=14, desc="A", exits=[]}}))].

zone_say_test_() ->
    [?_assertEqual({'$gen_cast', {zone_inactive, 12}}, fetch()),
     ?_assertEqual(
	{noreply, {[{self(),"Arne"}],[]}},
	handle_cast({say, self(), "Message"}, {[{self(),"Arne"}],[]})),
     ?_assertEqual(
	{'$gen_cast', 
	 {message, ["Arne", " says \"", "Message", "\""]}}, fetch())].

zone_exits_test_() ->
    fun () -> handle_cast(
		{exits, self()}, {[], #zone{id=12, exits=[{north,1}]}}),
	      ?assertEqual({'$gen_cast',
			    {message, ["There is an exit to the ", "north"]}}, 
			   fetch())
    end.

zone_enter_test_() ->
    [?_assertEqual({noreply, {[{self(), "Gunde"}], 
			      #zone{id=14, desc="A room!", exits=[]}}},
		   handle_cast({enter, self(), "Gunde", south}, 
			       {[], #zone{id=14, desc="A room!", exits=[]}})),
     fun () ->
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room!", lists:flatten(Message)),
	     fetch() % exits, already tested
     end,

     fun () ->
	     handle_cast({enter, self(), "Gunde", south}, 
			 {[{self(),"gg"}], 
			  #zone{id=14, desc="A room!", exits=[]}}),

	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room!\nHere stands gg", lists:flatten(Message)),
	     fetch(), % exits, already tested

	     ?assertEqual({'$gen_cast',
			   {message, ["Gunde" ," arrives from north"]}}, 
			  fetch())
     end].

zone_look_test_() ->
    [fun () ->
	     handle_cast({look, self()}, {[], #zone{id=14, desc="A room"}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room", lists:flatten(Message))
     end,
     ?_assertEqual({'$gen_cast', {message, "You see no exit"}}, fetch()),
     fun () ->
	     handle_cast({look, self()}, {[{0, "B"}], 
					  #zone{id=14, desc="A small room", 
						exits=[{north, -1}]}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A small room\nHere stands B", lists:flatten(Message))
     end,
     ?_assertEqual({'$gen_cast', 
		    {message, ["There is an exit to the ", "north"]}}, fetch()),

     fun () ->
	     handle_cast(
	       {look, self()}, {[{0, "B"}], 
				#zone{id=14, desc="A room",
				      exits=[{north, -1},{south, 3}]}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room\nHere stands B", lists:flatten(Message))
     end,
     ?_assertEqual(
	{'$gen_cast', 
	 {message, ["There are exits to ", "north, south"]}}, fetch())].

zone_logout_test_() ->
    [fun () -> handle_cast({logout, 3}, {[{3,"C"},{self(),"B"}], 
					 #zone{id=12, exits=[{north,1}]}}),
	       ?assertEqual({'$gen_cast', {message, ["C", " has logged out"]}},
			    fetch())
     end,

     fun () -> handle_cast({logout, 3}, {[{3,"C"},{self(),"B"}], 
					 #zone{id=12, exits=[{north,1}]}}),
	       ?assertEqual({'$gen_cast', {message, ["C", " has logged out"]}},
			    fetch())
     end,

     ?_assertEqual({noreply, {[], #zone{id=8, exits=[]}}},
		   handle_cast({logout, self()}, {[{self(), "Arne"}],
						  #zone{id=8, exits=[]}})),

     ?_assertEqual({'$gen_cast', {zone_inactive, 8}},
		   fetch())
    ].

zone_kick_test_() ->
    [fun () -> handle_cast({kick, "Timmy"}, {[{self(), "Timmy"},{self(), "B"}], 
					     #zone{id=12, exits=[]}}),

	       ?assertEqual({'$gen_cast', kick}, fetch()),
	       ?assertEqual({'$gen_cast', 
			     {message, ["Timmy", " has logged out"]}},fetch())
     end].

zone_attack_test_() ->
    [fun () ->
	     handle_cast({attack, self(), "Kurt", 1}, 
			 {[{self(),"Kurt"},{self(),"Dingo"}],
			  #zone{id=5, exits=[]}}),

	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("You hit Kurt for 1", lists:flatten(Message)),

	     ?assertEqual({'$gen_cast', {damage, 1, "Kurt"}}, fetch())

     end,

     fun () ->
	     handle_cast({attack, self(), "Scurt", 10}, 
			 {[{self(),"Kurt"}], 
			  #zone{id=2, exits=[{north,1},{south,2}]}}),
	     ?assertEqual({'$gen_cast', {stop_attack, "Scurt"}}, fetch())
     end,

     fun () ->
	     handle_cast({attack, self(), "Dingo", miss}, 
			 {[{self(),"Kurt"}, 
			   {self(),"Dingo"}, {self(), "Observer"}],
			  #zone{id=5, exits=[]}}),

	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("Kurt misses Dingo", lists:flatten(Message)),

	     {'$gen_cast', {message, Message2}} = fetch(),
	     ?assertEqual("You miss your attack on Dingo", 
			  lists:flatten(Message2)),

	     {'$gen_cast', {message, Message3}} = fetch(),
	     ?assertEqual("Kurt misses his attack on YOU", 
			  lists:flatten(Message3))

     end].

zone_death_test_() ->
    [
     ?_assertEqual({noreply, {[], #zone{id=7, exits=[]}}},
		   handle_cast({death, self()}, {[{self(), "Arne"}],
						 #zone{id=7, exits=[]}})),

     ?_assertEqual({'$gen_cast', {zone_inactive, 7}}, fetch()),

     fun () ->
	     handle_cast(
	       {death, self()}, {[{self(),"Kurt"}, {self(),"Allan"}], 
				 #zone{id=5, exits=[]}})
	     %%?assertEqual({'$gen_cast',  {broadcast, 
	     %%		["Kurt", " has been slain!"]}}, fetch())

     end,

     ?_assertEqual({'$gen_cast', {stop_attack, "Kurt"}}, fetch())].

zone_test_() ->
    [?_assertEqual(" arrives from south", format_arrival(north)),
     ?_assertEqual(" arrives from west", format_arrival(east)),
     ?_assertEqual(" arrives from north", format_arrival(south)),
     ?_assertEqual(" arrives from east", format_arrival(west)),
     ?_assertEqual(" logged in", format_arrival(login))
    ].

    
