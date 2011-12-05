-module(actor).

%%%=============================================================================
%%% @doc
%%% Simple actor that allows to implement flexible server
%%% processes. This actor adheres the OTP sys module so code uploads
%%% etc shoud work.
%%%
%%% As opposed to gen_* the implementation has to implement only three
%%% functions:
%%%
%%%  * create_initial_state/1 - will be called with a proplist containing all
%%%  requirered parameters.
%%%
%%% In addition the callback module must implement a callback function
%%% defined by the return value of `create_initial_state/1'. If `{ok, Fun, State}'
%%% is returned, `ActorModule:Fun/3' will be called if the process
%%% receives a message, or if `actor:rpc/2,3' is called.
%%%
%%% This dynamic callback will get a special first argument: a reply
%%% function.  This way the implementation can be decoupled from
%%% wheter the caller wants to wait for an answer, or not.
%%%
%%% @end
%%%=============================================================================

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Public API
-export([start/3,
	 start/2,
	 rpc/2,
	 rpc/3]).

%% Internal API
-export([new_process_entry/4,
	 behaviour_info/1,
	 info/1,
	 receive_loop/3]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type start_options() :: [{timeout, Millis :: non_neg_integer() | infinity} |
			  trap_exit].

-type create_initial_state_result() :: {ok, Fun :: atom(), State :: term()}
			| {error, Reason :: term()}.

-type reply_fun() :: fun((term()) ->
				ok).
-type callback_result() :: {next, Fun :: atom(), State :: term()}
			 | no_change
			 | {exit, Reason :: term()}.

-export_type([reply_fun/0,
	      callback_result/0,
	      start_options/0,
	      create_initial_state_result/0]).

-record(actor_info,
	{module      :: module(),
	 current_fun :: atom(),
	 start_args  :: term()}).

%%%=============================================================================
%%% Constants
%%%=============================================================================

-define(DEFAULT_START_TIMEOUT, 5000).

%%%=============================================================================
%%% Behaviour
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{create_initial_state, 1}];
behaviour_info(_) ->
    undefined.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% Starts new actor. This spawns a new process, wich invokes
%% `ActorModule:create_initial_state/1'.
%%
%% `ActorModule:create_initial_state/1' must return a value of type
%% `create_initial_state_retval()', which has intuitve semantics (I hope).
%%
%% When the `create_initial_state' function returns no error, `{ok, ActorPid}' is
%% returned, otherwise `{error, Reason}' will be returned.
%%
%% `ActorPid' can be used to send message to the actor process or for `rpc/2,3'.
%%
%% `Options' is a proplist with self explaining semantics of type
%% `start_options()'.
%%
%% The actor process does not trap exits by default, pass `trap_exit'
%% to change.
%%
%% This function returns `{ok, Pid}' when the processes was
%% successfully started, and when an error occurres `{error, Reason}'.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start(module(), term(), start_options()) ->
		   {ok, pid()} | {error, term()}.
start(Module, Args, Options) ->
    wait_for_new_process(
      erlang:spawn(?MODULE, new_process_entry,
		   [self(), Module, Args, Options]),
      Options).
%%------------------------------------------------------------------------------
%% @doc
%% Convenience function. Like `start/3' but with `[]' as `Options'.
%% @end
%%------------------------------------------------------------------------------
-spec start(module(), term()) ->
		   {ok, pid()} | {error, term()}.
start(Module, Args) ->
    start(Module, Args, []).

%%------------------------------------------------------------------------------
%% @doc
%% Sends a message to an Actor and blocks until the actor answers.
%%
%% The actor's callback function according to the current state is
%% invoked with three parameters: A continuation function that will
%% send the answer to the current process, the message, and the
%% current state. The invokation hence will have the form
%% `ActorModule:CurrentStateFun(AnswerFun, Msg, State)' and should
%% return a value of type `callback_result()'.
%%
%% When the actor exits before sending a reply or is already dead
%% before soending the message, an exception will be thrown.
%% @end
%%------------------------------------------------------------------------------
rpc(ActorPid, Msg) ->
    rpc(ActorPid, Msg, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Same as `rpc/2' but with an additional timeout parameter containing
%% the time in milli seconds the actor has to answer - otherwise an
%% exception will be thrown.
%% @end
%%------------------------------------------------------------------------------
rpc(ActorPid, Msg, Timeout) ->
    Info = case ?MODULE:info(ActorPid) of
	       {error, Reason} ->
		   throw({actor_rpc_error,
			  {msg, Msg},
			  {reason, Reason}});
	       ActorInfo ->
		   ActorInfo
	   end,
    Ref = monitor(process, ActorPid),
    From = self(),
    Answerer = fun(Reply) ->
		       From ! {'@rpc_reply', Ref, Reply}
	       end,
    ActorPid ! {'@actor_message', Answerer, Msg},
    receive
	{'@rpc_reply', Ref, Reply} ->
	    demonitor(Ref),
	    %% TODO must potential DOWN messages be purged
	    Reply;

	{'DOWN', Ref, _, _, Reason2} ->
	    throw({actor_rpc_error,
		   Info,
		   {msg, Msg},
		   {reason, {exit_before_reply,
			     Reason2}}})

    after Timeout ->
	    throw({actor_rpc_error,
		   Info,
		   {msg, Msg},
		   {reason, {timeout, Timeout}}})
    end.

%%------------------------------------------------------------------------------
%% @doc
%%
%% This function returns a tuple with the start arguments of the
%% actor process. This information is usefull for error analysis,
%% tracing and debugging.
%%
%% If the pid is not the pid of an actor or if the process does not
%% exist anymore, either `{error, not_an_actor}' or `{error, noproc}'
%% is returned.
%%
%% @end
%%------------------------------------------------------------------------------
-spec info(pid()) ->
		  #actor_info{} |
		  {error, not_an_actor} |
		  {error, noproc}.
info(ActorPid) ->
    case erlang:process_info(ActorPid, dictionary) of
	undefined ->
	    {error, noproc};
	{dictionary, Dict} ->
	    case proplists:lookup('@actor_info', Dict) of
		none ->
		    {error, not_an_actor};
		{'@actor_info', Info} ->
		    Info
	    end
    end.

%%%=============================================================================
%%% Internal functions.
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
new_process_entry(Parent, Module, Args, Options) ->
    initialize_actor_info(Module, Args),
    process_flag(trap_exit, proplists:get_bool(trap_exit, Options)),
    try Module:create_initial_state(Args) of
	{ok, NextFun, NextState} ->
	    Parent ! {self(), ok},
	    ?MODULE:receive_loop(Module, NextFun, NextState);

	{error, Error} ->
	    Parent ! {self(), error, Error};

	Other ->
	    Parent ! {self(), error, {invalid_return_value, Other}}
    catch
	throw:E ->
	    Parent ! {self(), error, {caught_exception, E}};

	exit:R ->
	    Parent ! {self(), error, {unexpected_exit, R}};

	error:R ->
	    Parent ! {self(), error, {runtime_error, R}}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
receive_loop(ActorModule, Fun, State) ->
    update_actor_info(Fun),
    receive
	{'@actor_message', Answerer, Msg} ->
	    ok;

	Msg ->
	    Answerer = fun(_) -> ok end
    end,
    case
	ActorModule:Fun(Answerer,
			Msg,
			State)
    of
	{next, F, D} ->
	    receive_loop(ActorModule, F, D);

	no_change ->
	    receive_loop(ActorModule, Fun, State);

	{exit, Reason} ->
	    exit(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
wait_for_new_process(Pid, Options) ->
    TimeOut = proplists:get_value(timeout, Options, ?DEFAULT_START_TIMEOUT),
    receive
	{Pid, ok} ->
	    {ok, Pid};
	{Pid, error, Error} ->
	    {error, Error}
    after TimeOut ->
	    exit(Pid, kill),
	    Ref = monitor(process, Pid),
	    receive
		{'DOWN', Ref, process, Pid, _} ->
		    ok
	    end,
	    {error, timeout}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
initialize_actor_info(Module, Args) ->
    put('@actor_info', #actor_info{module      = Module,
				   start_args  = Args,
				   current_fun = create_initial_state}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
update_actor_info(NewFun) ->
    Info = get('@actor_info'),
    put('@actor_info', Info#actor_info{current_fun = NewFun}).

