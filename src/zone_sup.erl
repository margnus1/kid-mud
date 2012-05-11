%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth, Eric Arn\erlöv
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Magnus Lång <mala7837@beurling.it.uu.se>
%%% @doc
%%% Dynamic supervisor for zone
%%% @end
%%% Created :  9 May 2012 by Magnus Lång <mala7837@beurling.it.uu.se>
%%%-------------------------------------------------------------------
-module(zone_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_zone/1, stop_zone/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%--------------------------------------------------------------------
%% @doc
%% Starts the zone with id Id
%%
%% @spec start_zone(Id) -> pid() | {error, term()}
%% @end
%%--------------------------------------------------------------------
start_zone(Id) ->
    Child = {{zone, Id}, {zone, start_link, [Id]}, permanent,
	     2000, worker, [zone]},
    case supervisor:start_child(?SERVER, Child) of
	{ok, Pid} -> Pid;	  
	{ok, Pid, _} -> Pid;
	{error, Info} -> {error, Info}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stop the zone with id Id
%%
%% @spec stop_zone(Id) -> ok | {error, term()}
%% @end
%%--------------------------------------------------------------------
stop_zone(Id) ->
    supervisor:terminate_child(?SERVER, {zone, Id}),
    supervisor:delete_child(?SERVER, {zone, Id}).

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

    
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
