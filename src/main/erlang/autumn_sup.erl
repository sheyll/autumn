%%%=============================================================================
%%% @doc

%%% Autumn main supervisor, this module is soley boilerplate. I
%%% *THINK* it is necessary to return a pid from a supervisor process
%%% to the `application' callback.

%%% This supervisor will start the autumn server.

%%% @end
%%%=============================================================================
-module(autumn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxTSeconds = 1800,
    AutumnServer = {autumn,
		    {autumn, start_link, []},
		    permanent, 300000, worker, [autumn]},
    {ok,
     {{RestartStrategy, MaxRestarts, MaxTSeconds},
      [AutumnServer]}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
