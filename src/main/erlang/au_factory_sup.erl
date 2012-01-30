%%%=============================================================================
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2012, Sven Heyll
%%%
%%% @doc
%%% Internal module that {@link autumn} uses to start processes.
%%% @end
%%%=============================================================================
-module(au_factory_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 add_factory/1,
	 remove_factory/1,
	 start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-registered([?SERVER]).

%%%=============================================================================
%%% Includes
%%%=============================================================================

-include("au_factory.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%%------------------------------------------------------------------------------
%% @doc
%% Adds a factory to the supervisor. Required before {@link
%% start_child/2}. If a factory with the same id was already added an
%% error is returned.
%% @end
%%------------------------------------------------------------------------------
-spec add_factory(#factory{}) ->
			 ok |
			 {error, {factory_not_added, term()}}.
add_factory(Factory = #factory{id = Id}) ->
    case supervisor:start_child(?MODULE, {Id,
					  {au_factory, start_link, [Factory]},
					  transient,
					  infinity,
					  supervisor,
					  [au_factory]}) of
	{ok, _} ->
	    ok;
	{error, Reason} ->
	    {error, {factory_not_added, Reason}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Removes a factory to the supervisor. This will also stop all
%% running instances of that factory. If the factory id does
%% not identify a factory currently added an error is returned.
%% @end
%%------------------------------------------------------------------------------
-spec remove_factory(au_factory:id()) ->
			    ok |
			    {error, {factory_not_found, au_factory:id()}}.
remove_factory(FactoryId) ->
    case supervisor:terminate_child(?MODULE, FactoryId) of
	{error, not_found} ->
	    {error, {factory_not_found, FactoryId}};
	ok ->
	    supervisor:delete_child(?MODULE, FactoryId),
	    ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Starts a process using a factory previously added using {@link
%% add_factory/1}. If the factory does not exist an error is returned.
%% @end
%%------------------------------------------------------------------------------
-spec start_child(au_factory:id(), [au_item:ref()]) ->
			 au_factory:start_child_retval() |
			 {error, {factory_not_found, au_factory:id()}}.
start_child(FactoryId, Items) ->
    Kids = supervisor:which_children(?MODULE),
    case lists:keysearch(FactoryId, 1, Kids) of
	false ->
	    {error, {factory_not_found, FactoryId}};
	{value, {_, FactoryPid, _, _}} ->
	    au_factory:start_child(FactoryPid, Items)
    end.

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxTSeconds = 1,
    {ok, {{RestartStrategy, MaxRestarts, MaxTSeconds}, []}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

