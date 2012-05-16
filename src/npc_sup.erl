%%%-------------------------------------------------------------------
%%% @author Magnus Lang <mala7837@fries.it.uu.se>
%%% @copyright (C) 2012, Magnus Lang
%%% @doc
%%%
%%% @end
%%% Created : 15 May 2012 by Magnus Lang <mala7837@fries.it.uu.se>
%%%-------------------------------------------------------------------
-module(npc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_npc/2, stop_npc/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a npc of type Id attached to zone ZonePID
%% @end
%%--------------------------------------------------------------------
-spec start_npc(integer(), pid()) -> pid() | {error, term()}.
start_npc(Id, ZonePID) ->
    Reference = make_ref(),
    Child = {{npc, Reference}, {npc, start_link, [Id, ZonePID, Reference]},
	     permanent, 2000, worker, [npc]},
    case supervisor:start_child(?SERVER, Child) of
	{ok, Pid} -> Pid;	  
	{ok, Pid, _} -> Pid;
	{error, Info} -> {error, Info}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the npc with reference Reference
%% @end
%%--------------------------------------------------------------------
stop_npc(Reference) ->
    supervisor:terminate_child(?SERVER, {npc, Reference}),
    supervisor:delete_child(?SERVER, {npc, Reference}).

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

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
