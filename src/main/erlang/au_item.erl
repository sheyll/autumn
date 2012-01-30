%%%=============================================================================
%%% @doc
%%% A container with a key and a value field. An item has a lifecycle
%%% and a process associated to it. When created the item links to the
%%% calling process. The item can be monitored or linked.
%%% @end
%%%=============================================================================

-module(au_item).

-type key() :: atom().

-type value() :: term().

-record(au_item, {key     :: key(),
		  val     :: value(),
		  pid     :: pid(),
		  creator :: pid()}).

-type ref() :: #au_item{}.

-export_type([key/0,
	      value/0,
	      ref/0]).


%% Public API exports
-export([new/2,
	 new_link/2,
	 key/1,
	 value/1,
	 monitor/1,
	 is_valid/1,
	 invalidate/1,
	 get_creator/1]).

%%%=============================================================================
%%%Public API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Create a new item process with a key value data pair.
%% @end
%%------------------------------------------------------------------------------
-spec new(key(), value()) ->
		 ref().
new(K, V) ->
    Pid = spawn(fun guard_proc/0),
    #au_item{key = K,
	     val = V,
	     pid = Pid,
	     creator = self()}.

%%------------------------------------------------------------------------------
%% @doc
%% Create a new item process with a key value data pair.
%% @end
%%------------------------------------------------------------------------------
-spec new_link(key(), value()) ->
		      ref().
new_link(K, V) ->
    Pid = spawn_link(fun guard_proc/0),
    #au_item{key = K,
	     val = V,
	     pid = Pid,
	     creator = self()}.

%%------------------------------------------------------------------------------
%% @doc
%% Get the key of an item.
%% @end
%%------------------------------------------------------------------------------
-spec key(ref()) ->
		 key().
key(#au_item{key = K}) ->
    K.

%%------------------------------------------------------------------------------
%% @doc
%% Get the value of an item.
%% @end
%%------------------------------------------------------------------------------
-spec value(ref()) ->
		   value().
value(#au_item{val = V}) ->
    V.

%%------------------------------------------------------------------------------
%% @doc
%% This will invalidate the item and subsequent calls to `value/1' or
%% `key/1' will throw. If the item is already invalid, the call has no
%% effect. Processes linked to this item will receive an exit signal
%% with reason `invalidated'.
%% @end
%%------------------------------------------------------------------------------
-spec invalidate(ref()) ->
		   ok.
invalidate(I) ->
    case is_valid(I) of
	true ->
	    error_logger:info_report(autumn, [{invalidated, I}]),
	    exit(I#au_item.pid, {invalidated, I}),
	    R = monitor(process, I#au_item.pid),
	    receive {'DOWN', R,_,_,_} ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Put a monitor on the process representing the item.
%% @end
%%------------------------------------------------------------------------------
-spec monitor(ref()) ->
		   reference().
monitor(#au_item{pid = P}) ->
    monitor(process, P).


%%------------------------------------------------------------------------------
%% @doc
%% Returns `true' if the process representing the item is valid.
%% @end
%%------------------------------------------------------------------------------
is_valid(#au_item{pid = P}) ->
    is_process_alive(P).

%%------------------------------------------------------------------------------
%% @doc
%% Returns the process id of the process that called `new/2' or
%% `new_link/2'.
%% @end
%%------------------------------------------------------------------------------
get_creator(#au_item{creator = P}) ->
    P.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
guard_proc() ->
    UnsendableMsg = {now(), make_ref()},
    receive
	UnsendableMsg ->
	    ok
    end.
