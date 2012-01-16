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
-export([add_factory/1,
	 remove_factory/1,
         push/2,
         push/1]).

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

-record(state,
	{ %% id -> #factory{}
	  factories = dict:new() :: dict(),
	  %% {au_factory:id(), [au_item:ref()]} -> pid()
	  active    = dict:new() :: dict(),
	  %% pid() -> {au_factory:id(), [au_item:ref()]}
	  reverse_active = dict:new() :: dict(),
	  %% au_item:key() -> au_item:ref()
	  items     = dict:new() :: dict(),
	  %% reference() -> fun/2
	  down_handler = dict:new() :: dict()
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
%% When a child crashes, the items defined as start arguments in
%% `invalidate_on_crash' are invalidated and removed. This might
%% cause more processes to die, especially those that created and
%% linked to the items.  When a child exitted with reason `normal'
%% or `shutdown' the start argument items in
%% `invalidate_on_shutdown' are invalidated.
%%
%% Return values:
%%
%%  * `ok' the factory was added
%%  * `{error, {already_added, Id}}'
%%
%% @end
%% ------------------------------------------------------------------------------
-spec add_factory(Factory  :: #factory{}) ->
			 ok | {error, {already_added, term()}}.
add_factory(Factory) ->
    gen_server:call(?SERVER,
		    {add_factory, Factory}).

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
%% Provide an item, that factories may use to start new
%% processes. NOTE: A processes MUST NOT push an item that was
%% injected as start argument. There is no reason why this should be
%% necessary. When this is done some autumn functions might get into
%% infinite loops.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec push(au_item:ref()) ->
		      ok.
push(Item) ->
    gen_server:cast(?SERVER, {push, Item}).

%%------------------------------------------------------------------------------
%% @doc
%%
%% Provide an item, that factories may use to start new
%% processes. This is a conveniece function that will create a new
%% item process and link it with the calling process.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec push(au_item:key(), au_item:value()) ->
		      {ok, au_item:ref()}.
push(Key, Value) ->
    Item = au_item:new_link(Key, Value),
    gen_server:cast(?SERVER, {push, Item}),
    {ok, Item}.

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
handle_call({add_factory, Factory =  #factory{id = Id}}, _, S) ->
    case get_factory_by_id(Id, S) of
	{ok, _} ->
	    {reply, {error, {already_added, Id}}, S};
	error ->
	    error_logger:info_report(autumn, [{adding_factory, Id}]),
	    {reply, ok, add_factory(Factory, S)}
    end;

handle_call({remove_factory, Id}, _, S) ->
    case get_factory_by_id(Id, S) of
	{ok, _} ->
	    error_logger:info_report(autumn, [{removing_factory, Id}]),
	    {reply, ok, remove_factory(Id, S)};
	error ->
	    {reply, {error, {not_found, Id}}, S}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_cast({push, Item}, S) ->
    error_logger:info_report(autumn, [{pushed, Item}]),
    S2 = add_item(Item, S),
    Factories = find_factory_by_dependency(au_item:key(Item), S2),
    S3 = lists:foldl(fun apply_factory_to_each_item_set/2, S2, Factories),
    {noreply, S3}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info({'DOWN',Ref,_,_,Reason}, #state{down_handler=DH} = S) ->
    S2 = (dict:fetch(Ref, DH))(S, Reason),
    {noreply, S2#state{down_handler = dict:erase(Ref, DH)}}.

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
add_factory(Factory = #factory{id = Id}, S) ->
    au_factory_sup:add_factory(Factory),
    Fs = S#state.factories,
    apply_factory_to_each_item_set(Factory,
				   S#state{factories =
					       dict:store(Id, Factory, Fs)}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
remove_factory(Id, S) ->
    au_factory_sup:remove_factory(Id),
    Fs = S#state.factories,
    S#state{factories = dict:erase(Id, Fs)}.

%%------------------------------------------------------------------------------
%% @doc
%% This invokes the factory to create a process for every unique and
%% valid set of start args.
%% @end
%%------------------------------------------------------------------------------
apply_factory_to_each_item_set(F, S) ->
    Reqs = F#factory.req,
    UniqueIs = F#factory.unique_items,
    %% fetch all items for all required start arguments
    StartArgsValues= [get_values_by_key(R, S) || R <- Reqs],

    %% create the cartesian product of all types of start arguments...
    StartArgsSets = [StartArgsSet || StartArgsSet <- perms(StartArgsValues),
				     %% .. filtering start arg sets alredy used
				     not is_active(F, StartArgsSet, S),

				     %% check uniqueness of every unique start arg
				     %% for each ancestor:
				     assert_items_unique(UniqueIs, StartArgsSet, S)],

    %% start a child for every set of start args
    lists:foldl(start_factory_child(F#factory.id), S, StartArgsSets).

%%------------------------------------------------------------------------------
%% @doc
%% A small wrapper around `is_item_unique/3' that applies each item or returns
%% `true' if there are no items that need to be unique.
%% @end
%%------------------------------------------------------------------------------
-spec assert_items_unique([au_item:id()], [au_item:ref()], #state{}) ->
			      boolean().
assert_items_unique(UniqueItems, Items, State) ->
    lists:all(fun(I) ->
		      is_item_unique(I, Items, State)
	      end,
	      UniqueItems).

%%------------------------------------------------------------------------------
%% @doc
%% This is a complex test, that checks if, among a set of items and
%% all items that were requrired to build all processes, that created
%% the items, there are no two items with the key specified in
%% parameter one but diffrent values.
%% @end
%%------------------------------------------------------------------------------
-spec is_item_unique(au_item:id(), [au_item:ref()], #state{}) ->
			    boolean().
is_item_unique(UniqueItemId, Items, S) ->
    %% get all items required to build all processes that creators of
    %% the items in `Items'
    AllItems = Items ++ [AI || I  <- Items,
			       AI <- get_ancestor_items(I, S)],

    %% Filter all items with key `UniqueItemId'
    ItemsWithKey = [I || I <- AllItems,
			 au_item:key(I) =:= UniqueItemId],

    %% Check that all items are equal.
    case ItemsWithKey of
	[] ->
	    true;

	[H|T] ->
	    not lists:any(fun(E) ->
				  E =/= H
			  end,
			  T)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Recursively gathers all items required to build all processes up to
%% the processes, that build the item in parameter one.
%% @end
%%------------------------------------------------------------------------------
-spec get_ancestor_items(au_item:ref(), #state{}) ->
				[au_item:ref()].
get_ancestor_items(Item, S) ->
    Creator = au_item:get_creator(Item),
    StartArgs = get_start_args_of(Creator, S),
    AncItems =
	[I ||
	    %% Get the start arguments of the process that created `Item'
	    StartArg <- StartArgs,
	    %% For every start arg item get the ancestors recursively
	    AIs <- get_ancestor_items(StartArg, S),
	    %% This implicitly joins the lists of lists
	    I <- AIs],
    StartArgs ++ AncItems.

%%------------------------------------------------------------------------------
%% @doc
%% If `Creator' is a process managed by `autumn' return a list of its
%% start args.
%% @end
%%------------------------------------------------------------------------------
get_start_args_of(Creator, S) ->
    %% look up the process in the dict of active processes
    case dict:find(Creator, S#state.reverse_active) of
	{ok, {_FactoryId, StartArgs}} ->
	    StartArgs;

	error ->
	    %% seems the process is not managed by autumn, that's okay.
	    []
    end.

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
-spec start_factory_child(au_factory:id()) ->
				 fun(([au_item:ref()], #state{}) -> #state{}).
start_factory_child(Id) ->
    fun(StartArgSet, S) ->
	    error_logger:info_report(autumn, [{starting_child_of, Id},
					      {start_args, StartArgSet}]),
	    case au_factory_sup:start_child(Id, StartArgSet) of
		{ok, Pid} ->
		    error_logger:info_report(autumn, [{started_child_of, Id},
						      {start_args, StartArgSet},
						      {pid, Pid}]),
		    S#state{active = dict:store({Id, StartArgSet}, Pid,
						S#state.active),
			    reverse_active = dict:store(Pid, {Id, StartArgSet},
							S#state.reverse_active)};
		Error ->
		    error_logger:error_report(autumn, [{error_starting_child_of, Id},
						       {start_args, StartArgSet},
						       {error, Error}]),
		    S
	    end
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

%%------------------------------------------------------------------------------
%% @doc
%% Return a list of factories that depend on a specific item key.
%% @end
%%------------------------------------------------------------------------------
-spec find_factory_by_dependency(au_item:key(), #state{}) ->
					[#factory{}].
find_factory_by_dependency(K, #state{factories = Fs}) ->
    [F || {_,F} <- dict:to_list(Fs),
	  lists:member(K, F#factory.req)].

%%------------------------------------------------------------------------------
%% @doc
%% Add an item to the set of available items.
%% @end
%%------------------------------------------------------------------------------
-spec add_item(au_item:ref(), #state{}) ->
		      #state{}.
add_item(Item, S) ->
    K = au_item:key(Item),
    Ref = au_item:monitor(Item),
    ItemDown = fun(State, Reason) ->
		       remove_item(Item, State, Reason)
	       end,
    ItemsWithSameKey = get_values_by_key(K, S),
    S#state{
      items = dict:store(K, [Item|ItemsWithSameKey], S#state.items),
      down_handler = dict:store(Ref, ItemDown, S#state.down_handler)
     }.

%%------------------------------------------------------------------------------
%% @doc
%% Remove an item from the set of available items. No effect if the item
%% is not available. All depending processes will be terminated.
%% @end
%%------------------------------------------------------------------------------
-spec remove_item(au_item:ref(), #state{}, term()) ->
			 #state{}.
remove_item(Item, S, Reason) ->
    error_logger:info_report(autumn, [{remove_item, Item}]),
    OtherItems = [I || I <- get_values_by_key(au_item:key(Item), S),
		       I =/= Item],
    ItemsLeft = dict:store(au_item:key(Item), OtherItems, S#state.items),
    S2 = S#state{items = ItemsLeft},
    stop_dependent(Item, S2, Reason).

%%------------------------------------------------------------------------------
%% @doc
%% Exits all factory instances that depend on a specific item.
%% @end
%%------------------------------------------------------------------------------
-spec stop_dependent(au_item:ref(), #state{}, term()) ->
			     #state{}.
stop_dependent(I, S = #state{active = As, reverse_active = RAs}, Reason) ->
    {NewAs, NewRAs} =
	dict:fold(fun({Id, Reqs}, Pid, {NAs, NRAs}) ->
			  case lists:member(I, Reqs) of
			      true ->
				  error_logger:info_report(
				    autumn,
				    [{stopping_child_of, Id}]),
				  %% stop the process, conserving the
				  %% actual reason
				  exit(Pid, Reason),
				  %% remove the entries from the active and
				  %% reverse_active dicts
				  {dict:erase({Id, Reqs}, NAs),
				   dict:erase(Pid, NRAs)};

			      _ ->
				  {NAs, NRAs}
			  end
		  end,
		  {As, RAs},
		  As),
    S#state{active = NewAs, reverse_active = NewRAs}.
