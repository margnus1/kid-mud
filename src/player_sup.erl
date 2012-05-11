%% Copyright (c) 2012 Magnus Lång, Mikael Wiberg and Michael Bergroth
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Michael Bergroth
%%% @doc
%%% Dynamic supervisor for player
%%% @end
%%%-------------------------------------------------------------------
-module(player_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_player/2, stop_player/1]).

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
%% Starts the player with name Name 
%%
%% @spec start_zone(Id) -> pid() | {error, term()}
%% @end
%%--------------------------------------------------------------------
start_player(Name, Console) ->
    Child = {{player, Name}, {player, start_link,[Name, Console]}, permanent,
	     2000, worker, [player]},
    case supervisor:start_child(?SERVER, Child) of
	{ok, Pid} -> Pid;	  
	{ok, Pid, _} -> Pid;
	{error, Info} -> {error, Info}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stop the player with name Name
%%
%% @spec stop_zone(Id) -> ok | {error, term()}
%% @end
%%--------------------------------------------------------------------
stop_player(Name) ->
    supervisor:terminate_child(?SERVER, {player, Name}),
    supervisor:delete_child(?SERVER, {player, Name}).

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
