%%%-------------------------------------------------------------------
%%% @author Magnus Lång <mala7837@beurling.it.uu.se>
%%% @copyright (C) 2012, Magnus Lang
%%% @doc
%%%      Main supervisor for the Kid-MUD server
%%% @end
%%% Created :  9 May 2012 by Magnus Laåg <mala7837@beurling.it.uu.se>
%%%-------------------------------------------------------------------
-module(master_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0, start/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor whithout a link.
%% To be used from shell when debugging only.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    unlink(Pid).


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000, % timeout

    %% How do we supervise this?
    database:start(),

    Zonemaster = {"Zone master", {zonemaster, start_link, []},
		  Restart, Shutdown, worker, [zonemaster]},

    ZoneSup = {"Zone Supervisor", {zone_sup, start_link, []},
	       Restart, Shutdown, supervisor, [zone_sup]},

    {ok, {SupFlags, [Zonemaster, ZoneSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
