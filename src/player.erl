%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arnerl�v
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Michael Bergroth <mibe5739@fries.it.uu.se>
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2012 by Magnus Lang <mala7837@fries.it.uu.se>
%%%-------------------------------------------------------------------
-module(player).
-include("player.hrl").
-include("zone.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

%% API
-export([start_link/2, command/2, message/2, kick/1, damage/3, stop_attack/2]).

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
%% @spec start_link(Name, Console) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Console) ->
    gen_server:start_link(?MODULE, [Name, Console], []).

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command if its a legal command by messaging
%% the concerned modules
%% @end
%%--------------------------------------------------------------------
-spec command(pid(), string()) -> ok.
command(Player, Command) ->
    gen_server:cast(Player, {command, Command}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to the player
%% @end
%%--------------------------------------------------------------------
-spec message(pid(), string()) -> ok.
message(Player, Message) ->
    gen_server:cast(Player, {message, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Inform the player that he has been kicked
%% @end
%%--------------------------------------------------------------------
-spec kick(pid()) -> ok.
kick(Player) ->
    gen_server:cast(Player, kick).

%%--------------------------------------------------------------------
%% @doc
%% Inflict damage to the player and inform the player that 
%% he/she has taken damage
%% @end
%%--------------------------------------------------------------------
-spec damage(pid(), integer(), string()) -> ok.
damage(Player, Damage, Attacker) ->
    gen_server:cast(Player, {damage, Damage, Attacker}).

%%--------------------------------------------------------------------
%% @doc
%% Starts to attack the target if target is in the same zone
%% @end
%%--------------------------------------------------------------------
-spec attack(pid(), string()) -> ok.
attack(Player, Target) ->
    gen_server:cast(Player, {attack, Target}).

%%--------------------------------------------------------------------
%% @doc
%% If Player attacks Target, Player stops attacking
%% @end
%%--------------------------------------------------------------------
-spec stop_attack(pid(), string()) -> ok.
stop_attack(Player, Target) ->
    gen_server:cast(Player, {stop_attack, Target}).

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
init([Name, Console]) ->
    Data = database:read_player(Name),
    Zone = zonemaster:get_zone(Data#player.location),
    zone:enter(Zone, self(), Name, login),
    Console ! {message, "Welcome to Kid-MUD!"},
    {ok, {Console, Zone, Data, {normal, none, none}}}.

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
handle_call(Request, _From, State) ->
    io:fwrite("Unknown call to player ~p: ~p~n", [self(), Request]),
    {noreply, State}.


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
handle_cast({command, Command}, 
	    State = {Console, Zone, Data, CombatState = {_, Target, AttackTimer}}) ->
    case parser:parse(Command) of
	{go, Direction} ->
	    case zone:go(Zone, self(), Direction) of
		{ok, Id} ->
		    Console ! {message, "You successfully moved " ++ 
				   atom_to_list(Direction)},

		    NewZone = zonemaster:get_zone(Id),
		    zone:enter(NewZone, self(), Data#player.name, Direction),
		    timer:cancel(AttackTimer),
		    {noreply, {Console, NewZone, Data#player{location=Id}, {normal, none, none}}};

		{error, doesnt_exist} ->
		    Console ! {message, "You cannot go that way"},
		    {noreply, State}
	    end;

	{say, Message} ->
	    zone:say(Zone, self(), Message),
	    {noreply, State};

	{tell, Recipient, Message} ->
	    case playermaster:tell(Recipient, Message, Data#player.name) of
		ok ->
		    Console ! {message, ["Message was sent to ", Recipient]},
		    {noreply, State};
		msg_failed ->
		    Console ! {message, [Recipient, " seems to be offline"]},
		    {noreply, State}
	    end;

	logout ->
	    zone:logout(Zone, self()),
	    playermaster:stop_player(Data#player.name),
	    {noreply, State};

	exits ->
	    zone:exits(Zone, self()),
	    {noreply, State};

	look ->
	    zone:look(Zone, self()),
	    {noreply, State};

	stop ->
	    Console ! {message , "You stopped all actions"},
	    timer:cancel(AttackTimer),
	    {noreply, {Console, Zone, Data, {normal, none, none}}};

	{attack, NewTarget} ->
	    if 
		NewTarget =:= Data#player.name ->
		    Console ! {message, "You cannot attack yourself"},
		    {noreply, State};
		Target =:= NewTarget ->
		    Console ! {message, ["You are already attacking ", Target]},
		    {noreply, State};
		Target =/= NewTarget ->
		    case zone:validate_target(Zone, NewTarget) of
			no_target ->
			    Console ! {message,
				       ["Can't find any \"", NewTarget, "\" here"] },
			    {noreply, State};
			valid_target ->
			    Console ! {message, 
				       ["You are now attacking ", NewTarget]},
			    timer:cancel(AttackTimer),
			    {_,NewAttackTimer} = timer:send_interval(2000, {'$gen_cast',{attack, NewTarget}}),
			    {noreply, {Console, Zone, Data, 
				       {combat, NewTarget, NewAttackTimer}}}
		    end
	    end;

	parse_error ->
	    Console ! {message, "Command not recognized"},
	    {noreply, State}
    end;

handle_cast({attack, NewTarget}, 
	    {Console, Zone, Data, {PlayerStatus, Target, AttackTimer}}) ->

    ToHit = random:uniform(100),
    if ToHit > 20 ->
	    Damage = 9 + random:uniform(6);
       ToHit =< 20 -> 
	    Damage = miss
    end,

    zone:attack(Zone, self(), NewTarget, Damage),
    {noreply, {Console, Zone, Data, {PlayerStatus, Target, AttackTimer}}};

handle_cast({stop_attack, ZoneTarget}, State =  
		{Console, Zone, Data, {_, Target, AttackTimer}}) ->
    if 
	ZoneTarget =:= Target ->
	    Console ! {message, "You have stopped attacking"},
	    timer:cancel(AttackTimer),
	    {noreply, {Console, Zone, Data, {normal, none, none}}};
	ZoneTarget =/= Target ->
	    {noreply, State}    
    end;

handle_cast({message, Description}, State={Console,_,_,_}) ->
    Console ! {message, Description},
    {noreply, State};

handle_cast(kick, State={Console,_,#player{name=Name},_}) ->
    Console ! {message, "You have been kicked!"},
    playermaster:stop_player(Name),
    {noreply, State};

handle_cast({damage, Damage, Attacker}, {Console, Zone, Data, CombatState}) ->
    NewData = Data#player{health={now(), get_health(Data) - Damage}},
    {_, Health} = NewData#player.health,
    if 
	Health > 0.0 ->
	    Console ! {message, [Attacker ," hits YOU for damage: ", 
				 integer_to_list(Damage)]},
	    {noreply, {Console, Zone, NewData, CombatState}};
	Health =< 0.0 ->
	    Console ! {message, "You are Dead!"},
	    zone:death(Zone, self()),
	    %% Player dies permanently
	    playermaster:stop_player(Data#player.name),
	    {noreply, {Console, Zone, #player{name = Data#player.name}, CombatState}}
    end;

handle_cast(Msg, State) ->
    io:fwrite("Unknown cast to player ~p: ~p~n", [self(), Msg]),
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
    io:fwrite("Unknown info to player ~p: ~p~n", [self(), Info]),
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
terminate(_Reason, {_, _, Data,_}) ->
    database:write_player(Data),
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

get_health(#player{health={Time, Health}}) ->
    min(Health + timer:now_diff(now(), Time) / 6000000.0, 100).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

test_setup() ->
    mnesia:start(),
    database:create_tables([]).

%% @hidden
fetch() ->
    receive
        Anything ->
            Anything
    end.

flush() ->
    receive 
	_ -> flush()
    after 1 ->
	    ok
    end.

player_test_() ->
    {setup, fun test_setup/0, 
     [
      fun () ->		  
	      ?assertEqual(handle_cast({message, "foo"}, {self(),what,ever,ever}),
			   {noreply, {self(),what,ever,ever}}),
	      ?assertEqual({message, "foo"}, fetch()),
	      flush()
      end,
      fun () ->
	      Data = #player{name = "gunnar"},
 	      ?assertEqual(handle_cast(kick, {self(),what,Data,ever}),
 			   {noreply, {self(),what,Data,ever}}),
 	      ?assertEqual(fetch(), {message, "You have been kicked!"}),
	      flush()
      end,
      fun () ->
	      %% Test for handle_cast({damage, integer(), string()}, {pid(),pid(),player()}
	      Data = #player{name = "Pontus"},
	      NewData = element(3, element(2, handle_cast({damage, 20, "hanna"}, {self(), self(), Data, ever}))),
	      ?assertEqual(round(element(2, NewData#player.health)), 80),
	      ?assertEqual({message, ["hanna"," hits YOU for damage: ","20"]}, fetch()),
	      ControlData = NewData#player{health = Data#player.health},
	      ?assertEqual(Data, ControlData),
	      flush()
      end, 
      ?_assertEqual(handle_cast("test", state), {noreply, state}),
      ?_assertEqual(get_health(#player{name = "foo"}), 100),
      fun () ->
	      %% requires some of the other modules to work properly
	      %% Test for handle_cast({command, "go north"}, 
	      %%                       {pid(),pid(),player()}
	      mnesia:start(),
	      application:start(kidmud),
	      database:write_zone(#zone{id=0, exits=[{north, 1}]}),
	      database:write_zone(#zone{id=1, exits=[{south, 0}]}),
	      Data = #player{name = "foo"},
	      Zone = zonemaster:get_zone(Data#player.location),
	      zone:enter(Zone, self(), "foo", login),
	      ?assertEqual({noreply, {self(), zonemaster:get_zone(1), #player{name="foo",location=1, health=Data#player.health},{normal,none,none}}}, 
			   handle_cast({command, "go north"}, {self(), Zone, Data, {normal,none,none}})
			  ),
	      fetch(),
	      fetch(),
	      ?assertEqual({message, "You successfully moved north"}, fetch()),
	      application:stop(kidmud)
      end
     ]}.
