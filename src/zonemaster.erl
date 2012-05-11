%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Eric Arnerlöv
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(zonemaster).

-include_lib("eunit/include/eunit.hrl").
-include("zone.hrl").
-include("player.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, get_zone/1, zone_inactive/1,kick_player/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%      Gets the pid of the zone in question
%% @end
%%--------------------------------------------------------------------
get_zone(Id) ->
    gen_server:call(?SERVER, {get_zone, Id}).

%%--------------------------------------------------------------------
%% @doc
%%      Call every zone to kick a player by the player Name.
%% @end
%%--------------------------------------------------------------------
kick_player(Name) ->
    gen_server:cast(?SERVER, {kick_player, Name}).


%%--------------------------------------------------------------------
%% @doc
%%     Message that a zone is going inactive
%% @end
%%--------------------------------------------------------------------
zone_inactive(Id) ->
    gen_server:cast(?SERVER, {zone_inactive, Id}).


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
    process_flag(trap_exit, true),
    {ok, gb_trees:empty()}.

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
handle_call({get_zone, Id}, _From, ActiveZonesTree) ->
    case gb_trees:lookup(Id, ActiveZonesTree) of
	none ->
	    %% Spawn a new zone
	    Zone = zone_sup:start_zone(Id),
	    NewTree = gb_trees:insert(Id, Zone, ActiveZonesTree),
	    
	    {reply, Zone, NewTree};
	
	{value, Zone} ->
	    {reply, Zone, ActiveZonesTree}
    end;



handle_call(Request, _From, State) ->
    io:fwrite("Unknown call to zonemaster: ~p~n", [Request]),
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
handle_cast({zone_inactive, Id}, ActiveZonesTree) ->
    case gb_trees:lookup(Id, ActiveZonesTree) of
	none ->
	    %%io:fwrite("Zonemaster: Got zone_inactive with an id ~p that is not active!", [Id]),
	    {noreply, ActiveZonesTree};
	
	{value, _} ->
	    zone_sup:stop_zone(Id),
	    NewTree = gb_trees:delete(Id,ActiveZonesTree),	    
	    {noreply, NewTree}
    end;

handle_cast({kick_player,Name},ActiveZonesTree)->
    ZoneLists = gb_trees:values(ActiveZonesTree),
    lists:foreach(fun(H)-> zone:kick(H,Name) end, ZoneLists),
    {noreply, ActiveZonesTree};
	
handle_cast(Msg, State) ->
    io:fwrite("Unknown cast to zonemaster: ~p~n", [Msg]),
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
    io:fwrite("Unknown info to zonemaster: ~p~n", [Info]),
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



%%%===================================================================
%%% EUnit tests
%%%===================================================================

test_setup() ->
    mnesia:start(),
    database:write_zone(#zone{id=1234}),    
    database:write_zone(#zone{id=1235}),
    start_link(),
    zone_sup:start_link(),
    ok.

%% zonemaster_test_() will test following functions.
%% Create a zone.
%% Create a zone and add the player Tomas.
%% Kick Tomas from the zone and tests if it becomes inactive. 
zonemaster_test_() ->
    {setup, fun test_setup/0, 
     [?_assertEqual(get_zone(1234), get_zone(1234)),
      ?_assertNotEqual(get_zone(1234), get_zone(1235)),
      fun () ->
	      %% Creates a zone and a player named "Tomas".
	      %% Tests if "Tomas" were created and then if "Tomas" were kicked.
	      TempId = get_zone(1234),
	      database:write_player(#player{name="Tomas", location=1234}),
	      player:start_link("Tomas", self()),
	      Test = (#player{name="Tomas", location=1234}),

	      ?_assertEqual(Test,database:read_player("Tomas")),
	      zonemaster:kick_player("Tomas"),

	      ?_assertNotEqual(Test,database:read_player("Tomas")),
	      %% The sleep is needed because it will take some time for zone to become inactive 
	      %% when the only player in zone 1234 get kicked.
	      timer:sleep(100),
	      ?assert(TempId =/= get_zone(1234))
      end
     ]}.
