%%%=============================================================================
%%% @doc
%%% A supervisor for a processes running the same module.
%%% Used exclusively by {@link au_factory_sup}.
%%% @end
%%%=============================================================================
-module(au_factory).

-behaviour(supervisor).

%% API
-export([start_link/1,
	 start_child/2]).

%% Types
-type id() :: module(). %% a factory can always by identified with a module
-type start_child_retval() :: {ok, pid()} | {error, term()}.

-export_type([id/0, start_child_retval/0]).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% Includes
%%%=============================================================================

-include("au_factory.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the supervisor for a certain factory description. {@link
%% start_child/2} will start supervised processes on the function
%% defined in the factory record.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(#factory{}) ->
			{ok, pid()}.
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).


%%------------------------------------------------------------------------------
%% @doc
%% This adds a child to the supervisor by calling the start function
%% defined in the factory record passed in {@link start_link/1}.
%% @end
%%------------------------------------------------------------------------------
-spec start_child(pid(), [au_item:ref()]) ->
			{ok, pid()} | {ok, pid(), term()} | {error, term()}.
start_child(Factory, Items) ->
    supervisor:start_child(Factory, Items).

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(#factory{start = {M, F}}) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxTSeconds = 1,
    {ok, {{RestartStrategy, MaxRestarts, MaxTSeconds}, []}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

