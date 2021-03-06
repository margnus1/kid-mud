%% Copyright (c) 2012 Magnus L�ng, Mikael Wiberg and Michael Bergroth, Eric Arnerl�v
%% See the file license.txt for copying permission.

%%%-------------------------------------------------------------------
%%% @author Magnus Lang <mala7837@svedberg.it.uu.se>
%%% @doc
%%%    The Kid-MUD application module
%%% @end
%%% Created :  9 May 2012 by Magnus Lang <mala7837@svedberg.it.uu.se>
%%%-------------------------------------------------------------------
-module(kidmud).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    database:init(),
    random:seed(now()),
    case master_supervisor:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @doc
%% This function is used for starting Kid-MUD in a yaws instance with
%% the runmod argument. It will wait until mnesia starts and then
%% start Kid-MUD.
%% @end
%%--------------------------------------------------------------------
start() ->
    case application:start(kidmud) of
	{error,{not_started,mnesia}} ->
	    timer:apply_after(50, kidmud, start, []),
	    {warning, "Waiting for mnesia to start..."};
	Answer -> Answer
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
