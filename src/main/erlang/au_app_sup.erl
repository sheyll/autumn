%%%=============================================================================
%%% @doc
%%% Supervisor that will be the autumn controlled toplevel supervisor
%%% for an application controlled by autumn.

%%% This supervisor contains all processes started by autumn for this
%%% applicaiton. The supervisor contains a child `au_app_worker' which
%%% handles the injection for one application and the interaction to
%%% the outside world.

%%% @end
%%%=============================================================================
-module(au_app_sup).

-behaviour(supervisor).

-include("autumn.hrl").

%% API
-export([start_link/2,
	 get_app_worker/1]).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc

%% Starts an autumn wired OTP application with an `au_app_worker'
%% child, that loads the application and starts the autumn dependency
%% injection.

%% The first parameter must be a name of an applicaiton that has an
%% autumn compatible application descriptor, i.e. contains no start
%% function and application callback module, and is from OTPs
%% perspective only a library application.

%% The second parameter configures autumn. It holds i.e. the module
%% used to juice the meta information required for auto-wirering.

%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), #au_main_config{}) ->
			term().
start_link(AppId, Config) ->
    supervisor:start_link(?MODULE, {AppId, Config}).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the `au_app_worker' pid, which controlls injection per app
%% @end
%%------------------------------------------------------------------------------
-spec get_app_worker(pid()) -> pid().
get_app_worker(Sup) ->
    hd([P || {au_app_worker, P, _, _}  <- supervisor:which_children(Sup)]).

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init({AppId, Config}) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxTSeconds = 1,
    AppWorker = {au_app_worker,
		 {au_app_worker, start_link, [AppId, Config]},
		 permanent, 300000, worker, [au_app_worker]},
    {ok, {{RestartStrategy, MaxRestarts, MaxTSeconds}, [AppWorker]}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

