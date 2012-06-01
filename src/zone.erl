%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Magnus Lång <mala7837@fries.it.uu.se>
%%% @doc
%%%
%%% @end
%%% Created : 8 May 2012 by Magnus Lång <mala7837@fries.it.uu.se>
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
%%% Types
%%%===================================================================
-record(state, {players=[], npc=[], dead_npc=[], data}).
-type player() :: {pid(), string()}.
-type npc() :: {pid(), string(), integer()}.
-type dead_npc() :: {erlang:timestamp(), timer:tref()}.
-type state() :: #state {players :: [player()],
			 npc :: [npc()],
			 dead_npc :: [dead_npc()],
			 data :: zone()}.
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
-spec validate_target(pid(), string()) -> {npc, pid()} | {player, pid()} | false.
validate_target(Zone, Target) ->
    gen_server:call(Zone, {validate_target, Target}).


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
    process_flag(trap_exit, true),
    Data = database:read_zone(Id),
    Zone = self(),
    Now = now(),

    {RespawnTimes, AliveIds} = 
	lists:partition(fun (Npc) -> case Npc of
					 {dead, RespawnTime} -> 
					     RespawnTime > Now;
					 _ -> false 
				     end end,
			Data#zone.npc),

    Dead_Npc = lists:map(
		 fun({dead, RespawnTime}) ->
			 {ok, Timer} = timer:send_after(
					 timer:now_diff(RespawnTime, Now) div 1000,
					 {'$gen_cast', {respawn, RespawnTime}}),
			 {RespawnTime, Timer} end, 
		 RespawnTimes),

    NpcsThatCanSpawn = database:find_npc(Data#zone.level_range, Data#zone.habitat),

    NPC = lists:map(
	    fun(Npc) -> 
		    NpcId = case Npc of 
			     {dead, _} -> 
				 random_element(NpcsThatCanSpawn);
                             NId -> NId
			 end,
		    Pid = npc_sup:start_npc(NpcId, Zone),
		    Name = npc:get_name(Pid),
		    {Pid, Name, NpcId}		
	      end,
	      AliveIds),
    
    Players = [],
    {ok, #state{players=Players, npc=NPC, data=Data, dead_npc=Dead_Npc}}.

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
	    _From, State = #state{players=Players, 
                                  data=#zone{id=Id, exits=Exits}}) ->
    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
			 Dir =:= Direction ],
    case E of
	[] ->
	    {reply, {error, doesnt_exist}, State};

	[{_, DirectionID}] -> 
            UpdatedPlayers = lists:keydelete(PlayerPID, 1, Players),
            inactivate_if_empty(UpdatedPlayers, Id),
            
            {player, Name} = get_name(PlayerPID, State),
            message_players(UpdatedPlayers, [Name, " has left ",
                                             atom_to_list(Direction)]),

            {reply, {ok, DirectionID}, State#state{players=UpdatedPlayers}}
    end;


handle_call({validate_target, Target}, _From, State) ->
    {reply, find_target(Target, State), State};


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
	    State=#state{players=Players, data=#zone{exits=Exits}}) ->
    player:message(
      PlayerPID, 
      look_message(State#state{players=lists:keydelete(PlayerPID, 1, Players)})),
    player:message(PlayerPID, exits_message(Exits)),

    {noreply, State};


handle_cast({enter, PlayerPID, Name, Direction}, 
	    State = #state{players=Players, npc=NPC, data=#zone{exits=Exits}}) ->
    player:message(PlayerPID, look_message(State)),
    player:message(PlayerPID, exits_message(Exits)),

    message_players(Players, [Name, format_arrival(Direction)]),
    [npc:player_enter(NpcPid, Name) || {NpcPid, _, _} <- NPC],

    UpdatedPlayers = [{PlayerPID, Name} | Players],

    {noreply, State#state{players=UpdatedPlayers}};


handle_cast({logout, PlayerPID}, State) ->
    logout_player(PlayerPID, State);


handle_cast({exits, PlayerPID}, State = #state{data=#zone{exits=Exits}}) ->
    player:message(PlayerPID, exits_message(Exits)),
    {noreply, State};


handle_cast({kick, Name}, State = #state{players=Players}) ->
    case lists:keyfind(Name, 2, Players) of 
	{PlayerPID, _} ->
	    player:kick(PlayerPID),
            logout_player(PlayerPID, State);

	false ->
	    {noreply, State}
    end;


handle_cast({attack, AttackerPID, Target, Damage}, 
	    State = #state{players=Players}) -> 

    {AttackerType, Attacker} = get_name(AttackerPID, State),
    case find_target(Target, State) of 
        {TargetType, TargetPID} ->
            Bystanders = lists:keydelete(TargetPID, 1, 
                                         lists:keydelete(AttackerPID, 1, Players)),
            case Damage of
                miss ->
                    message_players(Bystanders, [Attacker, " misses ", Target]),
                    AttackerType:message(AttackerPID, ["You miss your attack on ", Target]),
                    TargetType:message(TargetPID, [Attacker, " misses his attack on YOU"]);
                
                Damage ->
                    message_players(Bystanders, [Attacker, " hits ", Target,
                                                 " for ", integer_to_list(Damage)]),
                    AttackerType:message(AttackerPID, ["You hit ", Target, " for ",
                                                     integer_to_list(Damage)]),
                    TargetType:damage(TargetPID, Damage, Attacker)
            end,
            {noreply, State};
        
        false ->
    	    player:stop_attack(AttackerPID, Target),
	    {noreply, State}
    end;


handle_cast({death, PID}, State = #state{players=Players, npc=NPC, 
                                         dead_npc=DeadNpc, data=#zone{id=Id}}) ->
    {Type, Name} = get_name(PID, State),

    case Type of 
	player ->
            UpdatedPlayers = lists:keydelete(PID, 1, Players),
	    inactivate_if_empty(UpdatedPlayers, Id),
            playermaster:broadcast([Name, " has been slain!"]),

	    stop_attack_players(UpdatedPlayers, Name),
	    stop_attack_npcs(NPC, Name),

            {noreply, State#state{players=UpdatedPlayers}};

	npc ->
            npc_sup:stop_npc(npc:get_ref(PID)),
	    UpdatedNpcs = lists:keydelete(PID, 1, NPC),
	    message_players(Players, [Name, " has been killed!"]),

	    stop_attack_players(Players, Name),

            RespawnDelay = 20000,
            RespawnTime = time_add(now(), RespawnDelay),
            {ok, Timer} = timer:send_after(RespawnDelay, 
                                           {'$gen_cast', {respawn, RespawnTime}}),

	    {noreply, State#state{npc=UpdatedNpcs, 
                                  dead_npc=[{RespawnTime, Timer} | DeadNpc]}}
    end;


handle_cast({say, PlayerPID, Message}, State=#state{players=Players}) ->
    {_, Name} = get_name(PlayerPID, State),
    message_players(Players, [Name, " says \"", Message, "\""]),
    {noreply, State};


handle_cast({respawn, RespawnTime}, State=#state{npc=NPC, dead_npc=DeadNPC, 
                                                 data=Data, players=Players}) ->
    NpcsThatCanSpawn = database:find_npc(Data#zone.level_range, Data#zone.habitat),
    Id = random_element(NpcsThatCanSpawn),
    Pid = npc_sup:start_npc(Id, self()),
    Name = npc:get_name(Pid),

    [npc:player_enter(Pid, Name) || {_,Name} <- Players],
    message_players(Players, ["A ", Name, " has ", 
			      random_element(
				["emerged from the bushwork",
				 "sprung from the ground",
				 "descended from the heavens",
				 "appeared from thin air"])]),

    UpdatedDeadNPC = lists:keydelete(RespawnTime, 1, DeadNPC),
    UpdatedNPC = [{Pid, Name, Id} | NPC],
    {noreply, State#state{npc=UpdatedNPC, dead_npc=UpdatedDeadNPC}};


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
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

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
terminate(Reason, #state{players=Players, npc=NPC, 
			 dead_npc=DeadNpc, data=Data}) ->
    case Reason of 
	shutdown -> ok;
	_ ->
	    [player:kick(PlayerPID) || {PlayerPID, _} <- Players],
	    [npc_sup:stop_npc(npc:get_ref(Pid)) || {Pid, _, _} <- NPC]
    end,
    [timer:cancel(Timer) || {_, Timer} <- DeadNpc],
    
    NpcIds = [Id || {_, _, Id} <- NPC],
    DeadTimes = [{dead, RespawnTime} || {RespawnTime, _} <- DeadNpc],
    
    database:write_zone(Data#zone{npc=NpcIds ++ DeadTimes}),
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

%% @doc Advances Timestamp by Delta milliseconds
-spec time_add(Timestamp :: erlang:timestamp(), 
               Delta :: integer()) -> erlang:timestamp().
time_add({MegaS, S, MicroS}, Delta) ->
    NewMicroS = MicroS + Delta * 1000,
    NewS = S + NewMicroS div 1000000,
    NewMegaS = MegaS + NewS div 1000000,
    {NewMegaS, NewS rem 1000000, NewMicroS rem 1000000}.

%% @doc 
%% Logs the player with pid PlayerPID out
%% and returns a handle_cast compatible result
%% @end
logout_player(PlayerPID, State = #state{players=Players, data=#zone{id=Id}}) ->
    UpdatedPlayers = lists:keydelete(PlayerPID, 1, Players),

    inactivate_if_empty(UpdatedPlayers, Id),
    
    {player, Name} = get_name(PlayerPID, State),
    message_players(UpdatedPlayers, [Name, " has logged out"]),

    {noreply, State#state{players=UpdatedPlayers}}.

%% @doc 
%% If the list List is empty, inactivates zone Id, otherwise do nothing
%% @end
-spec inactivate_if_empty([any()], integer()) -> ok.
inactivate_if_empty([], Id) -> zonemaster:zone_inactive(Id);
inactivate_if_empty(_,  _)  -> ok.

%% @doc Sends a message to all the players in the zone
message_players([{PlayerPID, _}|Rest], Arg1) ->
    player:message(PlayerPID, Arg1),
    message_players(Rest, Arg1);
message_players([], _) -> ok.

%% @doc Finds the target with name Target
-spec find_target(Target::string(), State::state()) ->
			 {player | npc, pid()} | false.
find_target(Target, #state{players=Players, npc=NPC}) ->
    case lists:keyfind(Target, 2, NPC) of
	false ->
	    case lists:keyfind(Target, 2, Players) of	
		false ->
		    false;
		{Pid, _} ->
		    {player, Pid}
	    end;
	{Pid, _Name, _Id} ->
    	    {npc, Pid}
    end.

%% @doc Sends stop_attack message to all players
stop_attack_players([{PlayerPID, _}|Rest], Name) ->
    player:stop_attack(PlayerPID, Name),
    stop_attack_players(Rest, Name);
stop_attack_players([], _) -> ok.

%% @doc Sends stop_attack message to all npcs
stop_attack_npcs([{PlayerPID, _, _}|Rest], Name) ->
    npc:stop_attack(PlayerPID, Name),
    stop_attack_npcs(Rest, Name);
stop_attack_npcs([], _) -> ok.

%% @doc Constructs a "look" message
-spec look_message(State :: state()) -> string().
look_message(#state{players=Players, npc=NPC, data=Data}) ->
    [Data#zone.desc,
     lists:map(fun({_, Name, _}) -> ["\n", "Here stands a ", Name] end,
     	       NPC),
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

%% @doc Gets the name of the player or NPC with pid PID
%% currently in the zone with state State.
%% @end
-spec get_name(pid(), state()) -> {npc | player, string()}.
get_name(PID, #state{players=Players, npc=NPC}) ->
    case lists:keyfind(PID, 1, Players) of
	false ->
	    {_, Name, _} = lists:keyfind(PID, 1, NPC),
            {npc, Name};
	{_, Name} ->
	    {player, Name}
    end.


%% @doc Constructs an "arrival" message
format_arrival(north) -> " arrives from south";
format_arrival(east)  -> " arrives from west";
format_arrival(south) -> " arrives from north";
format_arrival(west)  -> " arrives from east";
format_arrival(login) -> " logged in".


%% @doc Selects an element from List at random
-spec random_element([term()]) -> term().
random_element(List) ->
    lists:nth(random:uniform(length(List)), List).


%%%===================================================================
%%% EUnit Tests
%%%===================================================================

test_setup() ->
    database_setup(),
    register(zonemaster, self()),
    ok.

database_setup() ->
    mnesia:start(),
    database:create_tables([]).

%% @hidden
fetch() ->
    receive
        Anything ->
            Anything
    after 100 ->
	    no_message
    end.

flush() ->
    receive 
	_ -> flush()
    after 1 ->
	    ok
    end.

zone_go_test_() ->
    Room14 = #zone{id=14, desc="A room!", exits=[{south,5}]},
    State14 = #state{data=Room14},
    {setup, fun test_setup/0, 
     [?_assertEqual(
	 {reply,{ok, 5}, State14#state{players=[{self(), "Arne"}]}},
	 handle_call({go, self(), south}, self(), 
		     State14#state{players=[{self(), "Kalle"}, 
					    {self(), "Arne"}]})),
      ?_assertEqual(
	 {'$gen_cast', 
	  {message, ["Kalle", " has left ", "south"]}}, fetch()),

      ?_assertEqual(
	 {reply, {error, doesnt_exist}, State14},
	 handle_call({go, self(), west}, self(), State14)),

      ?_assertEqual({reply, {ok, 5}, State14},
		    handle_call(
		      {go, self(), south}, self(), 
		      State14#state{players=[{self(), "Arne"}]}))]}.

zone_find_target_test_() ->
    Room14 = #zone{id=14, desc="A room!", exits=[{south,5}]},
    KRef = make_ref(),
    Kalle = {KRef, "Kalle"},
    Arne = {self(), "Arne"},
    GRef = make_ref(),
    Goblin = {GRef, "Goblin", 4},
    State14 = #state{data=Room14, players=[Kalle, Arne], npc=[Goblin]},
    State14E = #state{data=Room14#zone{exits=[]}},
    [?_assertEqual({player, KRef}, find_target("Kalle", State14)),
     ?_assertEqual({npc, GRef}, find_target("Goblin", State14)),
     ?_assertEqual(false, find_target("Pelle", State14)),
     ?_assertEqual(false, find_target("Arne", State14E))].

zone_say_test() ->
    Room12 = #zone{id=12, desc="#12"},
    Arne = {self(), "Arne"},
    State12 = #state{players=[Arne], data=Room12},
    flush(),
    ?assertEqual({noreply, State12},
		 handle_cast({say, self(), "Message"}, State12)),
    ?assertEqual(
       {'$gen_cast', 
	{message, ["Arne", " says \"", "Message", "\""]}}, fetch()).

zone_enter_test_() ->
    Room14 = #zone{id=14, desc="A room!", exits=[]},
    State14 = #state{data=Room14},
    [?_assertEqual({noreply, State14#state{players=[{self(), "Gunde"}]}},
		   handle_cast({enter, self(), "Gunde", south}, State14)),
     fun () ->
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room!", lists:flatten(Message)),
	     flush() % exits, already tested
     end,

     fun () ->
	     handle_cast({enter, self(), "Gunde", south}, 
			 State14#state{players=[{self(),"gg"}],
				       npc=[{self(), "Goblin", 5}]}),

	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room!\nHere stands a Goblin\nHere stands gg",
			  lists:flatten(Message)),
	     fetch(), % exits, already tested

	     ?assertEqual({'$gen_cast',
			   {message, ["Gunde" ," arrives from north"]}}, 
			  fetch()),

	     ?assertEqual({'$gen_cast', {player_enter, "Gunde"}}, fetch())
     end].

zone_look_test_() ->
    [fun () ->
	     handle_cast({look, self()}, 
			 #state{players=[], 
				data=#zone{id=14, desc="A room"}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room", lists:flatten(Message))
     end,
     ?_assertEqual({'$gen_cast', {message, "You see no exit"}}, fetch()),
     fun () ->
	     handle_cast({look, self()}, 
			 #state{players=[{0, "B"}], 
				data = #zone{id=14, desc="A small room", 
					     exits=[{north, -1}]}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A small room\nHere stands B", lists:flatten(Message))
     end,
     ?_assertEqual({'$gen_cast', 
		    {message, ["There is an exit to the ", "north"]}}, fetch()),

     fun () ->
	     handle_cast(
	       {look, self()}, 
	       #state{players=[{0, "B"}], 
		      data=#zone{id=14, desc="A room",
				 exits=[{north, -1},{south, 3}]}}),
	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("A room\nHere stands B", lists:flatten(Message))
     end,
     ?_assertEqual(
	{'$gen_cast', 
	 {message, ["There are exits to ", "north, south"]}}, fetch())].

zone_logout_test_() ->
    [fun () -> handle_cast({logout, 3}, 
			   #state{players=[{3,"C"},{self(),"B"}], 
				  data=#zone{id=12, exits=[{north,1}]}}),
	       ?assertEqual({'$gen_cast', {message, ["C", " has logged out"]}},
			    fetch())
     end,

     fun () -> handle_cast({logout, 3},
			   #state{players=[{3,"C"},{self(),"B"}], 
				  data=#zone{id=12, exits=[{north,1}]}}),
	       ?assertEqual({'$gen_cast', {message, ["C", " has logged out"]}},
			    fetch())
     end,

     ?_assertEqual({noreply, #state{players=[], data=#zone{id=8, exits=[]}}},
		   handle_cast({logout, self()}, 
			       #state{players=[{self(), "Arne"}],
				      data=#zone{id=8, exits=[]}})),
     
     ?_assertEqual({'$gen_cast', {zone_inactive, 8}},
		   fetch())
    ].

zone_kick_test_() ->
    [fun () -> handle_cast({kick, "Timmy"}, 
			   #state{players=[{self(), "Timmy"},{self(), "B"}], 
				  data=#zone{id=12, exits=[]}}),

	       ?assertEqual({'$gen_cast', kick}, fetch()),
	       ?assertEqual({'$gen_cast', 
			     {message, ["Timmy", " has logged out"]}},fetch())
     end].

zone_attack_test_() ->
    [fun () ->
	     handle_cast({attack, self(), "Kurt", 1}, 
			 #state{players=[{self(),"Kurt"},{self(),"Dingo"}],
				data=#zone{id=5, exits=[]}}),

	     {'$gen_cast', {message, Message}} = fetch(),
	     ?assertEqual("You hit Kurt for 1", lists:flatten(Message)),

	     ?assertEqual({'$gen_cast', {damage, 1, "Kurt"}}, fetch())
     end,

     fun () ->

	     handle_cast({attack, self(), "Goblin", 10}, 
			 #state{data=#zone{id=17, desc="A room!", exits=[{south,5}]}, 
				players=[{self(),"Kurt"},{self(),"Gunnar"}], 
				npc=[{self(), "Goblin", 4}]}),

	     {'$gen_cast', {message, Message2}} = fetch(),
	     ?assertEqual("You hit Goblin for 10", lists:flatten(Message2)),

	     ?assertEqual({'$gen_cast', {damage, 10, "Kurt"}}, fetch())


     end,

     fun () ->
	     handle_cast({attack, self(), "Scurt", 10}, 
			 #state{players=[{self(),"Kurt"}], 
				data=#zone{id=2, exits=[{north,1},{south,2}]}}),
	     ?assertEqual({'$gen_cast', {stop_attack, "Scurt"}}, fetch())
     end,

     fun () ->
	     handle_cast({attack, self(), "Dingo", miss}, 
			 #state{players=[{self(),"Kurt"}, 
					 {self(),"Dingo"}, {self(), "Observer"}],
				data=#zone{id=5, exits=[]}}),
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
    [?_assertEqual({noreply, #state{players=[], data=#zone{id=8, exits=[]}}},
		   handle_cast({death, self()}, 
			       #state{players=[{self(), "Arne"}],
				      data=#zone{id=8, exits=[]}})),

     ?_assertEqual({'$gen_cast', {zone_inactive, 8}}, fetch()),

     fun () ->

	     handle_cast({death, self()}, 
			 #state{data=#zone{id=17, desc="A room!", exits=[{south,5}]}, 
				players=[{self(),"Kurt"},{self(),"Gunnar"}], 
				npc=[]}),

	     ?assertEqual({'$gen_cast', {stop_attack, "Kurt"}}, fetch())
     end].

zone_test_() ->
    [?_assertEqual(" arrives from south", format_arrival(north)),
     ?_assertEqual(" arrives from west", format_arrival(east)),
     ?_assertEqual(" arrives from north", format_arrival(south)),
     ?_assertEqual(" arrives from east", format_arrival(west)),
     ?_assertEqual(" logged in", format_arrival(login))].

    
