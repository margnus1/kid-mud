%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg, Michael Bergroth and Eric Arnerlöv
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Michael Bergroth <Michael@nl119-149-19.student.uu.se>
%%% @doc
%%%
%%% @end
%%% Created : 15 May 2012 by Michael Bergroth <Michael@nl119-149-19.student.uu.se>
%%%-------------------------------------------------------------------
-module(npc).
-include("npc.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3, get_ref/1, damage/3, stop_attack/2, player_enter/2]).

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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, Zone, Ref) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id, Zone, Ref], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends the NPCs reference as a reply
%% @end
%%--------------------------------------------------------------------
-spec get_ref(pid()) -> ok.
get_ref(Npc) ->
    gen_server:call(Npc, get_ref).

%%--------------------------------------------------------------------
%% @doc
%% Inflict damage to the NPC
%% @end
%%--------------------------------------------------------------------
-spec damage(pid(), integer(), string()) -> ok.
damage(Npc, Damage, Attacker) ->
    gen_server:cast(Npc, {damage, Damage, Attacker}).

%%--------------------------------------------------------------------
%% @doc
%% If NPC attacks Target, NPC stops attacking
%% @end
%%--------------------------------------------------------------------
-spec stop_attack(pid(), string()) -> ok.
stop_attack(Npc, Target) ->
    gen_server:cast(Npc, {stop_attack, Target}).

%%--------------------------------------------------------------------
%% @doc
%% Attack entering player if NPC is hostile and not in combat already
%% @end
%%--------------------------------------------------------------------
-spec player_enter(pid(), string()) -> ok.
player_enter(Npc, Name) ->
    gen_server:cast(Npc, {player_enter, Name}).

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
init([Id, Zone, Ref]) ->
    Data = database:read_npc(Id),
    {ok, {Ref, Zone, Data, {normal, none, none}}}.

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
handle_call(get_ref, _From, State =
	   {Ref, _Zone, _Data, _CombatState}) ->
    {reply, Ref, State}.

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
handle_cast({damage, Damage, Attacker}, State = 
		{Ref, Zone, Data, {NpcStatus, _Target, _AttackTimer}}) ->
    NewData = Data#npc{health={erlang:now(),
			       get_health(Data) - Damage,
			       element(3, Data#npc.health)}},
    {_,Health,_} = NewData#npc.health,
    if 
	Health > 0.0 ->
	    if
		NpcStatus =:= normal ->
		    {_, NewAttackTimer} = 
			timer:send_interval(2000, {'$gen_cast', 
						   {attack, Attacker}}),
		    {noreply, {Ref, Zone, NewData, {combat, Attacker, 
					       NewAttackTimer}}};
		NpcStatus =:= combat ->
		    {noreply, State}
	    end;
	Health =< 0.0 ->    
	    zone:death(Zone, self()),
	    {noreply, State}
    end;

handle_cast({attack, NewTarget}, State =  
	    {_Ref, Zone, _Data, _CombatState}) ->
    ToHit = random:uniform(100),
    if ToHit > 20 ->
	    Damage = 4 + random:uniform(6);
       ToHit =< 20 -> 
	    Damage = miss
    end,
    zone:attack(Zone, self(), NewTarget, Damage),
    {noreply, State};

handle_cast({stop_attack, ZoneTarget}, State =  
		{Ref, Zone, Data, {_, Target, AttackTimer}}) ->
    if 
	ZoneTarget =:= Target ->
	    timer:cancel(AttackTimer),
	    {noreply, {Ref, Zone, Data, {normal, none, none}}};
	ZoneTarget =/= Target ->
	    {noreply, State}    
    end;

handle_cast({player_enter, Name}, State =
		{Ref, Zone, Data, {NpcStatus, _Target, _AttackTimer}}) ->
    case {Data#npc.disp, NpcStatus} of 
	{hostile, normal} ->
	    {_, NewAttackTimer} = 
		timer:send_interval(2000, {'$gen_cast', {attack, Name}}),
	    {noreply, {Ref, Zone, Data, {combat, Name, NewAttackTimer}}};
	{neutral, _} -> 
	    {noreply, State};
	{_, combat} ->
	    {noreply, State}
    end;    

handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

%% @doc Calculates the current health of a NPC
-spec get_health(npc()) -> float().
get_health(#npc{health={Time, Health, MaxHealth}}) ->
    min(Health + timer:now_diff(now(), Time) / 6000000.0, MaxHealth).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

