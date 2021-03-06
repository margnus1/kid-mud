%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arn\erl�v
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Michael Bergroth
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(playermaster).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, start_player/2, stop_player/1, broadcast/1, tell/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the player process for the player with name Name
%%
%% @end
%%--------------------------------------------------------------------
-spec start_player(string(), pid()) -> ok.
start_player(Name, Console) ->
    gen_server:call(?SERVER, {start_player, Name, Console}).

%%--------------------------------------------------------------------
%% @doc
%% Stops the player process for the player with name Name
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_player(string()) -> ok.
stop_player(Name) ->
    gen_server:cast(?SERVER, {stop_player, Name}).

%%--------------------------------------------------------------------
%% @doc
%% Broadcasts the message Msg to all the players processes
%% @end
%%--------------------------------------------------------------------
-spec broadcast(string()) -> ok.
broadcast(Msg) ->
    gen_server:cast(?SERVER, {broadcast, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a specific player
%% @end
%%--------------------------------------------------------------------
-spec tell(string(), string(), string()) -> ok.
tell(Name, Msg, Sender) ->
    gen_server:call(?SERVER, {tell, Name, Msg, Sender}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server without linking processes
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
init([]) ->
    NpcNames = database:get_npc_names(),
    process_flag(trap_exit, true),
    {ok, {[], NpcNames}}.

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

handle_call({start_player, Name, Console}, _From, {PlayerList, NpcNames}) ->
    SameName = fun(X) -> if X =:= Name -> true; true -> false end end,
    case {lists:keyfind(Name,2,PlayerList), lists:any(SameName, NpcNames)} of
	{{_, _}, _} ->
	    {reply, login_failed, {PlayerList, NpcNames}};
	{false, true} ->
	    {reply, login_failed, {PlayerList, NpcNames}};
	{false, false} ->
	    PlayerPID = player_sup:start_player(Name, Console),
	    {reply, {ok, PlayerPID}, {[{PlayerPID, Name} | PlayerList], NpcNames}}
    end;

handle_call({tell, Name, Msg, Sender}, _From, State = {PlayerList, _NpcNames}) ->
    case lists:keyfind(Name, 2,PlayerList) of
	{Player, _} ->
	    player:message(Player, [Sender," tells you \"" ,Msg, "\""]),
	    {reply, ok, State};
	false ->
	    {reply, msg_failed, State}
    end;   

handle_call(Request, _From, State) ->
    io:fwrite("Unknown call to playermaster: ~p~n", [Request]),
    {reply, ok, State}.


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
handle_cast({stop_player, Name}, {PlayerList, NpcNames}) ->
    case lists:keyfind(Name, 2, PlayerList) of
	{_, _} ->
	    player_sup:stop_player(Name),
	    {noreply, {lists:keydelete(Name, 2, PlayerList), NpcNames}};
	false ->
	    {noreply, {PlayerList, NpcNames}}
    end;


handle_cast({broadcast, Msg}, {PlayerList, NpcNames}) ->
    broadcast_msg(PlayerList, Msg),
    {noreply, {PlayerList, NpcNames}};


handle_cast(Msg, State) ->
    io:fwrite("Unknown cast to playermaster: ~p~n", [Msg]),
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
    io:fwrite("Unknown info to playermaster: ~p~n", [Info]),
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

%% @doc Broadcasts the message Msg to all the players
broadcast_msg([{PlayerPID, _}|Rest], Msg) ->
    player:message(PlayerPID, Msg), 
    broadcast_msg(Rest, Msg);
broadcast_msg([], _) -> ok.


%%%===================================================================
%%% EUnit tests
%%%===================================================================

playermaster_test_() ->
    [?_assertEqual({noreply, []}, handle_cast({stop_player, "Kurt"}, []))].
