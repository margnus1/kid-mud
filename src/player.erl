%%%-------------------------------------------------------------------
%%% @author Michael Bergroth <mibe5739@fries.it.uu.se>
%%% @copyright MIT license
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
-export([start_link/2, command/2, message/2, kick/1, damage/2]).

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
%%
%% @spec command(Player, Command) -> {noreply, Console, Zone, Data} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, logout, State}
%% @end
%%--------------------------------------------------------------------
command(Player, Command) ->
    gen_server:cast(Player, {command, Command}).

%%--------------------------------------------------------------------
%% @doc
%% Writes a message to the player
%%
%% @end
%%--------------------------------------------------------------------
message(Player, Message) ->
    gen_server:cast(Player, {message, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Kick the player
%%
%% @end
%%--------------------------------------------------------------------
kick(Player) ->
    gen_server:cast(Player, kick).

%%--------------------------------------------------------------------
%% @doc
%% Inform the player that he has taken damage
%%
%% @end
%%--------------------------------------------------------------------
damage(Player, Damage) ->
    gen_server:cast(Player, {damage, Damage}).

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
    {ok, {Console, Zone, Data}}.

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
handle_cast({command, Command}, OldState = {Console, Zone, Data}) ->
    case parser:parse(Command) of
	{go, Direction} ->
	    case zone:go(Zone, self(), Direction) of
		{ok, Id} ->
		    Console ! {message, "You successfully moved " ++ 
				   atom_to_list(Direction)},
		    
		    NewZone = zonemaster:get_zone(Id),
		    zone:enter(NewZone, self(), Data#player.name, Direction),
		    {noreply, {Console, NewZone, Data#player{location=Id}}};

		{error, doesnt_exist} ->
		    Console ! {message, "You cannot go that way"},
		    {noreply, OldState}
	    end;

	{say, Message} ->
	    zone:say(Zone, self(), Message),
	    {noreply, OldState};

	logout ->
	    zone:logout(Zone, self()),
	    {stop, normal, OldState};

	exits ->
	    zone:exits(Zone, self()),
	    {noreply, OldState};

	look ->
	    zone:look(Zone, self()),
	    {noreply, OldState};

	{attack, Target} ->
	    zone:attack(Zone, self(), Target, 10),
	    {noreply, OldState};

	parse_error ->
	    Console ! {message, "Command not recognized"},
	    {noreply, OldState}
    end;

handle_cast({message, Description}, State={Console,_,_}) ->
    Console ! {message, Description},
    {noreply, State};


handle_cast(kick, State={Console,_,_}) ->
    Console ! {message, "You have been kicked!"},
    {stop, normal, State};

handle_cast({damage, Damage}, {Console, Zone, Data}) ->
    NewData = Data#player{health={now(), get_health(Data) - Damage}},
    {_, Health} = NewData#player.health,
    if 
	Health > 0.0 ->					     
	    {noreply, {Console, Zone, NewData}};
	Health =< 0.0 ->
	    Console ! {message, "You are Dead!"},
	    zone:death(Zone, self()),
	    %% Player dies permanently
	    {stop, normal, {Console, Zone, #player{name = Data#player.name}}}
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
terminate(_Reason, {_, _, Data}) ->
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


%%test_setup() ->
%%    mnesia:start(),
%%    database:create_tables([]),
%%    ok.

%% @hidden
fetch() ->
    receive
        Anything ->
            Anything
    end.

player_test_() ->
    [
     fun () ->		  
	     ?assertEqual(handle_cast({message, "foo"}, {self(),what,ever}),
			  {noreply, {self(),what,ever}}),
	     ?assertEqual(fetch(), {message, "foo"})
     end,
     fun () ->		  
	     ?assertEqual(handle_cast(kick, {self(),what,ever}),
			  {stop, normal, {self(),what,ever}}),
	     ?assertEqual(fetch(), {message, "You have been kicked!"})
     end,
     fun () ->
	     %% Test for handle_cast({damage, integer()}, {pid(),pid(),player()}
	     Data = #player{name = "Pontus"},
	     {noreply,{what,ever, NewData}} = 
		 handle_cast({damage, 20}, {what,ever, Data}),
	     ?assertEqual(round(element(2, NewData#player.health)), 80),
	     ControlData = NewData#player{health = Data#player.health},
	     ?assertEqual(Data, ControlData)
     end, 
     ?_assertEqual(handle_cast("test", state), {noreply, state}),
     ?_assertEqual(get_health(#player{name = "foo"}), 100),
     fun () ->
	     %% requires some of the other modules to work properly
	     %% Test for handle_cast({command, "go north"}, 
	     %%                       {pid(),pid(),player()}
	     mnesia:start(),
	     master_supervisor:start(),
	     database:write_zone(#zone{id=0, exits=[{north, 1}]}),
	     database:write_zone(#zone{id=1, exits=[{south, 0}]}),
	     Data = #player{name = "foo"},
	     Zone = zonemaster:get_zone(Data#player.location),
	     zone:enter(Zone, self(), "foo", login),
	     ?assertEqual(handle_cast({command, "go north"}, 
				      {self(), Zone, Data}),
			  {noreply, {self(), zonemaster:get_zone(1), 
				     #player{name="foo",location=1,
					     health=Data#player.health}}}),
	     fetch(),
	     fetch(),
	     ?assertEqual({message, "You successfully moved north"}, fetch())
     end
    ].
