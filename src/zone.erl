%%%-------------------------------------------------------------------
%%% @author Magnus Lang <mala7837@fries.it.uu.se>
%%% @copyright (C) 2012, Magnus Lang
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2012 by Magnus Lang <mala7837@fries.it.uu.se>
%%%-------------------------------------------------------------------
-module(zone).
-include("zone.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1, go/3, look/2, enter/4, logout/2, exits/2,
	 kick/2, death/2, say/3]).

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
%% 
%% @end
%%--------------------------------------------------------------------
go(Zone, Player, Direction) ->
    gen_server:call(Zone, {go, Player, Direction}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
look(Zone, Player) ->
    gen_server:cast(Zone, {go, Player}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
enter(Zone, Player, Name, Direction) ->
    gen_server:cast(Zone, {enter, Player, Name, Direction}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
logout(Zone, Player) ->
    gen_server:cast(Zone, {logout, Player}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
exits(Zone, Player) ->
    gen_server:cast(Zone, {exits, Player}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kick(Zone, Name) ->
    gen_server:cast(Zone, {kick, Name}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
death(Zone, Player) ->
    gen_server:cast(Zone, {death, Player}).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
say(Zone, Player, Message) ->
    gen_server:cast(Zone, {say, Player, Message}).



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
handle_call({go, Player, Direction}, _From, {Players, Data = #zone{exits=Exits}}) ->
    E = [CurrentExits || CurrentExits = {Dir, _} <- Exits,
			 Dir =:= Direction ],
    case E of
	[] ->
	    {reply, {error, doesnt_exist}, {Players, Data}};

	[{_, DirectionID}] -> 
	    case lists:keydelete(Player, 1, Players) of

		[] ->
		    {stop, zone_empty, {ok, DirectionID}, {Players, Data}};

	       UpdatedPlayers ->
		    Name = get_name(Player, Players),
		    messagePlayers(UpdatedPlayers, message, 
				   Name ++ " has left to the " ++ 
				       atom_to_list(Direction)),

		    {reply, {ok, DirectionID}, {UpdatedPlayers, Data}}
	    end
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

handle_cast({look, Player}, State={Players, Data = #zone{exits=Exits}}) ->
    %% Sends the description to the player
    player:message(Player, look_message(lists:keydelete(Player, 1, Players), Data)),
    player:message(Player, exits_message(Exits)),
    {noreply, State};


handle_cast({enter, Player, Name, Direction}, {Players, Data = #zone{exits=Exits}}) ->
    player:message(Player, look_message(Players, Data)),
    player:message(Player, exits_message(Exits)),

    messagePlayers(Players, player_enter, Name, Direction),

    UpdatedPlayers = [{Player, Name} | Players],

    {noreply, {UpdatedPlayers, Data}};


handle_cast({logout, Player}, {Players, Data}) ->
    case lists:keydelete(Player, 1, Players) of 

	[] ->
	    {stop, zone_empty, {[], Data}};

	UpdatedPlayers ->
	    messagePlayers(UpdatedPlayers, message, 
			   get_name(Player, Players) ++ " has logged out"),
	    {noreply, {UpdatedPlayers, Data}}
    end;


handle_cast({exits, Player}, State={_,#zone{exits=Exits}}) ->
    player:message(Player, exits_message(Exits)),
    {noreply, State};


handle_cast({kick, Name}, {Players, Data}) ->
    case lists:keyfind(Name, 2, Players) of 
	{Player, _} ->
	    player:kick(Player),

	    case lists:delete({Player,Name}, Players) of 
		[] ->
		    {stop, zone_empty, {[], Data}};

		UpdatedPlayers ->
		    messagePlayers(UpdatedPlayers, message, Name ++ " has logged out"),
		    {noreply, {UpdatedPlayers, Data}}
	    end;

	false ->
	    {noreply, {Players, Data}}
    end;


handle_cast({death, Player}, {Players, Data}) ->
    Name = get_name(Player, Players),

    %% @todo Stop any attacking mobs

    case lists:keydelete(Player, 1, Players) of
	[] ->
	    {stop, zone_empty, {[], Data}};

	UpdatedPlayers ->
	    messagePlayers(UpdatedPlayers, message, 
			   Name ++ " has been slain!"),
	    {noreply, {UpdatedPlayers, Data}}
    end;


handle_cast({say, Player, Message}, State={Players,_}) ->
    Name = get_name(Player, Players),
    messagePlayers(Players, message, [Name, " says \"", Message, "\""]),
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
terminate(_Reason, {Players, Data=#zone{id=Id}}) ->
    %% @todo Inform players properly that the zone is shutting down
    [player:kick(Player) || Player <- Players],

    database:write_zone(Data),
    zonemaster:zone_inactive(Id),
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

%% @doc Sends a message to all the players in the zone
messagePlayers([{Player, _}|Rest], Notice, Arg1, Arg2, Arg3) ->
    player:Notice(Player, Arg1, Arg2, Arg3), 
    messagePlayers(Rest, Notice, Arg1, Arg2, Arg3);

messagePlayers([], _, _, _, _) -> ok.

%% @doc Sends a message to all the players in the zone
messagePlayers([{Player, _}|Rest], Notice, Arg1, Arg2) ->
    player:Notice(Player, Arg1, Arg2),
    messagePlayers(Rest, Notice, Arg1, Arg2);

messagePlayers([], _, _, _) -> ok.

%% @doc Sends a message to all the players in the zone when a player logout
messagePlayers([{Player, _}|Rest], Notice, Arg1) ->
    player:Notice(Player, Arg1), 
    messagePlayers(Rest, Notice, Arg1);

messagePlayers([], _, _) -> ok.

%% @doc Constructs a "look" message
-spec look_message(Players::[player()], Zone::zone()) -> string().

look_message(Players, Zone) ->
    string:join(
      [Zone#zone.desc] ++
	  %% lists:map(fun(NPC) -> "Here stands " ++ NPC#npc.name end,
	  %% 		       Zone#zone.npc) ++
	  lists:map(fun({_, Name}) -> "Here stands " ++ Name end,
		    Players), %% ++
      %% lists:map(fun({Amount, Item}) -> "Here lies " ++ 
      %% 		format_item(Amount, Item) end, Zone#zone.items),
      "\n").

format_item(Amount, Item) ->
    lists:flatten(
      io_lib:format(
	"~d ~s", [Amount, Item#item.name])).

exits_message([{Exit,_}]) ->
    "There is an exit to the " ++ atom_to_list(Exit);
exits_message(Exits) ->
    "There are exits to " ++ 
	string:join(lists:map(fun ({Dir, _}) -> atom_to_list(Dir) end, Exits), ", ").

%% @doc Gets the name of the player with PID Player from player list Players
get_name(Player, Players) ->
    {_, Name} = lists:keyfind(Player, 1, Players), 
    Name.

format_arrival(north) -> " arrives from south";
format_arrival(east) -> " arrives from west";
format_arrival(south) -> " arrives from north";
format_arrival(west) -> " arrives from east";
format_arrival(login) -> " logged in".
    
