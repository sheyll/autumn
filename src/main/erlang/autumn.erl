%%%=============================================================================
%%% @doc

%%% This server is the head of the autumn application. It will do it
%%% (the dependency injection and all the rest).

%%% @end
%%%=============================================================================

-module(autumn).

%%%=============================================================================
%%% Exports
%%%=============================================================================

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% API that can be called only by processes created by an autumn server
-export([push/2, push_link/2, pull/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%=============================================================================
%%% Includes
%%%=============================================================================

-include("autumn.hrl").

%%%=============================================================================
%%% Types
%%%=============================================================================

-define(SERVER, ?MODULE).
-registered([?SERVER]).

-record(active_app, {
	  name :: atom(),
	  sup :: pid(),
	  modules :: [module()]
	 }).

-record(module_info, {
	  name :: atom(),
	  dependers :: [module()],
	  containers :: [module()]
	 }).

-record(item_info, {
	  id :: reference(),
	  name :: atom(),
	  value :: term(),
	  creator :: pid()
	 }).

-record(process_info, {
	  pid :: pid(),
	  mod :: #module_info{},
	  dependencies :: [#item_info{}]
	 }).

-record(state, {
	  meta_info_loader :: module(),
	  app_sup :: pid(),
	  apps :: [#active_app{}],
	  modules :: [#module_info{}],
	  procs :: [#process_info{}]
	}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Start the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() ->
			{ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% API for processes managed by autumn.
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% Push a value into the dependency injection mechanism. This might
%% lead to new processes being spawned.
%%
%% The `Key' is used to identify the item. Other processes can
%% articulate a dependency by specifying such a key as requirement.
%%
%% Autumn will add the key value pair to a tree containing all
%% processes and configurations and will call `start' on all modules
%% whose start arguments are completed by this push.
%%
%% All processes that listen to pushes and pulles of
%% the key will get a call to  `notify_push/3'.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec push(atom(), term()) ->
		      ok.
push(_Key, _Value) ->
    todo.

%%------------------------------------------------------------------------------
%% @doc
%%
%% This is like `push/2' with the difference that the key value pair
%% is `pull/3'ed automatically when the calling process exits.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec push_link(atom(), term()) ->
		      ok.
push_link(_Key, _Value) ->
    todo.

%%------------------------------------------------------------------------------
%% @doc
%%
%% Pulls a value, killing all dependend processes.
%%
%% All processes that listen to pushes and pulles of
%% the key will get a call to  `notify_pull/4'.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec pull(atom(), term(), term()) ->
		      ok.
pull(_Key, _Value, _Reason) ->
    todo.

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_call(_Reg, _From, State) ->
    {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast(Request, State) ->
    {stop, unexpected_cast, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
