%%%=============================================================================
%%% @doc
%%%
%%% This server is the head of the autumn application. It will do it
%%% (the dependency injection and all the rest).
%%%
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
-export([add_factory/3,
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

-type factory_id() :: term().
-type start_args() :: [au_item:ref()].

-record(state,
	{factories = dict:new() :: dict(),%% id -> #factory{}
	 active    = dict:new() :: dict(),%% {factory_id(), [au_item:ref()]} -> pid()
	 items     = dict:new() :: dict() %% au_item:key() -> au_item:ref()
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
%% Return values:
%%
%%  * `ok' the factory was added
%%  * `{error, {already_added, Id}}'
%%
%% @end
%% ------------------------------------------------------------------------------
-spec add_factory(Id       :: factory_id(),
		  Requires :: [au_item:key()],
		  {M :: module(), F :: atom(), A :: [term()]}) ->
			 ok | {error, {already_added, term()}}.
add_factory(Id, Requires, {M,F,A}) ->
    gen_server:call(?SERVER,
		    {add_factory, Id, Requires, {M,F,A}}).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Removes a factory definition added by `add_factory'. The processes
%% started by the factory will continue to run.
%%
%% Return values:
%%
%%  * `ok' the factory was removed successfully
%%  * `{error, {not_found, Id}}'
%%
%% @end
%%------------------------------------------------------------------------------
-spec remove_factory(Id :: term()) ->
			 ok | {error, {not_found, Id :: term()}}.
remove_factory(Id) ->
    gen_server:call(?SERVER, {remove_factory, Id}).

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
-spec push(au_item:key(), au_item:value()) ->
		      ok.
push(Key, Value) ->
    Item = au_item:start_link(Key, Value),
    gen_server:call(?SERVER, {push, Item}).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Pulls a value, killing all dependend processes.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec pull(au_item:key(), au_item:value(), term()) ->
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
handle_call({add_factory, Id, Requires, {M,F,A}}, _, S) ->
    case get_factory_by_id(Id, S) of
	{ok, _} ->
	    {reply, {error, {already_added, Id}}, S};
	error ->
	    {reply, ok, add_factory(Id, Requires, {M,F,A}, S)}
    end;

handle_call({remove_factory, Id}, _, S) ->
    case get_factory_by_id(Id, S) of
	{ok, _} ->
	    {reply, ok, remove_factory(Id, S)};
	error ->
	    {reply, {error, {not_found, Id}}, S}
    end.

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

%%%                                                            Factory Functions

%%------------------------------------------------------------------------------
%% @private Look up the a factory by its id
%%-----------------------------------------------------------------------------
get_factory_by_id(Id, #state{factories = Fs}) ->
    dict:find(Id, Fs).

%%------------------------------------------------------------------------------
%% @private Add a factory to the set of factories.If possible apply factory.
%%------------------------------------------------------------------------------
add_factory(Id, Requires, MFA, S) ->
    Fs = S#state.factories,
    Factory = #factory{id = Id, req = Requires, start = MFA},
    apply_factory(Factory, S#state{factories = dict:store(Id, Factory, Fs)}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
remove_factory(Id, S) ->
    Fs = S#state.factories,
    S#state{factories = dict:erase(Id, Fs)}.

%%------------------------------------------------------------------------------
%% @doc
%% This invokes the factory to create a process for every unique and
%% valid set of start args.
%% @end
%%------------------------------------------------------------------------------
apply_factory(F, S) ->
    Reqs = F#factory.req,
    %% fetch all items for all required start arguments
    StartArgsValues= [get_values_by_key(R, S) || R <- Reqs],

    %% create the cartesian product of all types of start arguments...
    StartArgsSets = [StartArgsSet || StartArgsSet <- perms(StartArgsValues),
				     %% .. filtering start arg sets alredy used
				     not is_active(F, StartArgsSet, S)],

    %% start a child for every set of start args
    lists:foldl(start_factory_child(F), S, StartArgsSets).

%%------------------------------------------------------------------------------
%% @private Create all permutations of a list of lists.
%%------------------------------------------------------------------------------
-spec perms([[term()]]) -> [[term()]].
perms([S|Sets]) ->
    [[X|P] || X <- S, P <- perms(Sets)];
perms([]) ->
    [[]].

%%------------------------------------------------------------------------------
%% @doc
%% Returns `true' if a factory has already been applied to a start arg set.
%% @end
%%------------------------------------------------------------------------------
is_active(#factory{id = Id}, StartArgsSet, S) ->
    not (dict:find({Id, StartArgsSet}, S#state.active) =:= error).

%%------------------------------------------------------------------------------
%% @doc
%% Starts a new child of a factory for some start args and adds it to the state.
%% @end
%%------------------------------------------------------------------------------
start_factory_child(F) ->
    fun(StartArgSet, S) ->
	    {ok, Pid} = au_factory:start_child(F, StartArgSet),
	    S#state{active = dict:store({F#factory.id, StartArgSet}, Pid,
					S#state.active)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Fetches a list of items associated to an item key.
%% @end
%%------------------------------------------------------------------------------
get_values_by_key(ItemId, S) ->
    case dict:find(ItemId, S#state.items) of
	error ->
	    [];
	{ok, Items} ->
	    Items
    end.
