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
-export([add_factory/4,
	 remove_factory/1,
	 push/2,
	 pull/3]).

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

-record(factory,
	{}).

-record(state,
	{factories = [] :: [#factory{}]}).

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
%% Adds factory function defined by a module and a function.
%%
%% The same Id value must be passed to `remove_factory' to remove the
%% factory.
%%
%% The function defined by the last three parameters is supposed to
%% start and link a process that requires the start args referred to
%% by the list of ids passed as second parameter. The function will be
%% invoked for every kosher set of start arguments and the resulting
%% process will be terminated as soon as an item passed in as start
%% argument is invalidated.
%%
%% The arguments of the function `M:F' begin with `ExtraArgs' followed
%% by a proplist of the items requested by the first parameter.
%%
%% The third parameter is a list of item keys that the process created
%% by the factory creates. It is helpful for both the implementation
%% of autumn as well as the user of autumn if for every module managed
%% by autumn the emerging items are explicitly stated.
%%
%% Return values:
%%
%%  * `ok' the factory was added
%%  * `{error, function_not_exported}'
%%  * `{error, already_added}'
%%  * `{error, {required_items_not_provided, [au_item:key()]}}'
%%
%%
%% @end
%% ------------------------------------------------------------------------------
-spec add_factory(Id       :: term(),
		  Requires :: [au_item:key()],
		  Provides :: [au_item:key()],
		  {M :: module(), F :: atom(), A :: [term()]}) ->
			 ok |
			 {error,
			  function_not_exported |
			  already_added |
			  {required_items_not_provided, [au_item:key()]}}.
add_factory(Id, Requires, Provides, {M,F,A}) ->
    case erlang:function_exported(M, F, length(A) + 1) of
	true ->
	    gen_server:call(?SERVER,
			    {add_factory, Id, Requires, Provides, {M,F,A}});
	_ ->
	    {error, function_not_exported}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%
%% Removes a factory definition added by `add_factory'. The processes
%% started by the factory will continue to run.
%%
%% Return values:
%%
%%  * `ok' the factory was removed successfully
%%  * `{error, not_found}'
%%
%% @end
%%------------------------------------------------------------------------------
-spec remove_factory(term()) ->
			 ok.
remove_factory(Id) ->
    todo.

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
%% Autumn will automatically pull the values away when the process
%% calling push dies.
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
%% Pulls a value, killing all dependend processes.
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
handle_call({add_factory, Id, Requires, Provides, {M,F,A}}, _From, State) ->

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
