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
-export([start_link/1]).

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
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(Config) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxTSeconds = 1800,
    AutumnServer = {autumn_server,
		    {autumn_server, start_link, [Config]},
		    permanent, 5000, worker, [autumn_server]},
    {ok, {{RestartStrategy, MaxRestarts, MaxTSeconds}, [AutumnServer]}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
