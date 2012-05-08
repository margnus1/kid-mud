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
-behaviour(gen_server).

%% API
-export([start_link/2, command/2, player_logout/2, message/2, say/3, player_leave/3, player_enter/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

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
start_link(Name, Console) ->
    gen_server:start_link(?MODULE, [Name, Console], []).

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
    Player = database:read_player(Name),
    ZonePID = zonemaster:get_zone(Player#player.location),
    ZonePID ! {enter, self(), Name, login},
    {ok, {Console, ZonePID, Player}}.

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
handle_call(_Request, _From, State) ->
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
handle_cast({command, Command}, OldState = {Console, ZonePID, Player}) ->
    case parser:parse(Command) of

	{go, Direction} ->
	    ZonePID ! {go, self(), Direction},
	    receive 
		{go, Id} ->
		    Console ! {message, "You successfully moved " ++ atom_to_list(Direction)},

		    NewZonePID = zonemaster:get_zone(Id),
		    NewZonePID ! {enter, self(), Player#player.name, Direction},
		    {noreply, {Console, NewZonePID, Player#player{location=Id}}};

		{go, error, doesnt_exist} ->
		    Console ! {message, "You cannot go that way"},
		    {noreply, OldState}
	    end;

	{say, Message} ->
	    ZonePID ! {say, self(), Message},
	    {noreply, OldState};

	logout ->
	    ZonePID ! {logout, self(), Player#player.name},
	    {stop, logout, OldState};

	exits ->
	    ZonePID ! {exits, self()},
	    {noreply, OldState};

	look ->
	    ZonePID ! {look, self()},
	    {noreply, OldState};

	parse_error ->
	    Console ! {message, "Command not recognized"},
	    {noreply, OldState}

    end;


handle_cast({player_logout, Name}, OldState={Console,_,_}) ->
    Console ! {message, Name ++ " logged out"},
    {noreply, OldState};


handle_cast({message, Description}, OldState={Console,_,_}) ->
    Console ! {message, Description},
    {noreply, OldState};


handle_cast({say, Name, Message}, OldState={Console,_,_}) ->
    Console ! {message, Name ++ " says: " ++ Message},
    {noreply, OldState};


handle_cast({player_leave, Name, Direction}, OldState={Console,_,_}) ->
    Console ! {message, Name ++ " went " ++ atom_to_list(Direction)},
    {noreply, OldState};


handle_cast({player_enter, Name, Direction}, OldState={Console,_,_}) ->
    Message = case Direction of 
		  north -> " arrives from south";
		  east -> " arrives from west";
		  south -> " arrives from north";
		  west -> " arrives from east";
		  login -> " logged in"
	      end,
    Console ! {message, Name ++ Message},
    {noreply, OldState};


handle_cast(_, State) ->
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
terminate(_Reason, {_, _, Player}) ->
    database:write_player(Player),
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

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
command(Player, Command) ->
    gen_server:cast(Player, {command, Command}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
player_logout(Player, Name) ->
    gen_server:cast(Player, {player_logout, Name}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
message(Player, Message) ->
    gen_server:cast(Player, {message, Message}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
say(Player, Name, Message) ->
    gen_server:cast(Player, {say, Name, Message}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
player_leave(Player, Name, Direction) ->
    gen_server:cast(Player, {player_leave, Name, Direction}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
player_enter(Player, Name, Direction) ->
    gen_server:cast(Player, {player_enter, Name, Direction}).
