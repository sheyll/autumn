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
-export([spawn/3,
	 spawn/2,
	 rpc/2,
	 rpc/3]).

%% Internal API
-export([new_process_entry/4,
	 behaviour_info/1,
	 info/1,
	 receive_loop/4]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type start_options() :: [trap_exit
                          %% TODO: | link | monitor
                         ].

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
%% `create_initial_state_retval()', which has intuitve semantics (I
%% hope).
%%
%% When the `create_initial_state' function returns no error, `{ok,
%% ActorPid}' is returned, otherwise `{error, Reason}' will be
%% returned.
%%
%% If the callback module contains a function called `actor_started'
%% with one parameter, this function is called asynchronously by the
%% new processes.
%%
%% `ActorPid' can be used to send message to the actor process or for
%% `rpc/2,3'.
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
%% The initial callback which is called by the new process must return
%% immediately. It should not do anything thats blocks for a long
%% time. The timeout is currently 500 milli seconds.
%%
%% If anything more complicated and long lasting shall be done after
%% the process was started, the callback module must do it an the
%% optional callback function `actor_started/1'.
%%
%% @end
%%------------------------------------------------------------------------------
-spec spawn(module(), term(), start_options()) ->
		   {ok, pid()} | {error, term()}.
spawn(Module, Args, Options) ->
    proc_lib:start(?MODULE,
                   new_process_entry,
                   [self(), Module, Args, Options],
                   500).
%%------------------------------------------------------------------------------
%% @doc
%% Convenience function. Like `spawn/3' but with `[]' as `Options'.
%% @end
%%------------------------------------------------------------------------------
-spec spawn(module(), term()) ->
		   {ok, pid()} | {error, term()}.
spawn(Module, Args) ->
    ?MODULE:spawn(Module, Args, []).

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
-spec rpc(pid(), term()) ->
                 term().
rpc(ActorPid, Msg) ->
    rpc(ActorPid, Msg, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Same as `rpc/2' but with an additional timeout parameter containing
%% the time in milli seconds the actor has to answer - otherwise an
%% exception will be thrown.
%% @end
%%------------------------------------------------------------------------------
-spec rpc(pid(), term(), non_neg_integer() | infinity) ->
                 term().
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
    ActorPid ! {'@actor_rpc', Answerer, Msg, self()},
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
	    proc_lib:init_ack(Parent, {ok, self()}),
            Debug = sys:debug_options([]),
	    ?MODULE:receive_loop(Module, NextFun, NextState, Debug);

	{error, Error} ->
	    proc_lib:init_ack(Parent, {error, Error});

	Other ->
	    proc_lib:init_ack(Parent, {error, {invalid_return_value, Other}})
    catch
	throw:E ->
	    proc_lib:init_ack(Parent, {error, {caught_exception, E}});

	exit:R ->
	    proc_lib:init_ack(Parent, {error, {unexpected_exit, R}});

	error:R ->
	    proc_lib:init_ack(Parent, {error, {runtime_error, R}})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
receive_loop(ActorModule, Fun, State, Debug) ->
    update_actor_info(Fun),
    Extra = {sys_dbg_extra, self()},
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request,

	Msg = {'@actor_rpc', RawAnswerer, Msg, From} ->
            InEvent = {in, Msg, From},
	    Answerer = fun(Answer) ->
                               sys:handle_debug(Debug,
                                                fun print_sys_debug/3,
                                                Extra,
                                                {out, Answer, From}),
                               RawAnswerer(Answer)
                       end;

	Msg ->
            InEvent = {in, Msg},
	    Answerer = fun(Answer) ->
                               sys:handle_debug(Debug,
                                                fun print_sys_debug/3,
                                                Extra,
                                                {answer_ignored, Answer})
                       end
    end,
    Debug2 = sys:handle_debug(Debug, fun print_sys_debug/3, Extra, InEvent),
    case
	ActorModule:Fun(Answerer,
			Msg,
			State)
    of
	{next, F, D} ->
            Debug3 = sys:handle_debug(Debug2,
                                      fun print_sys_debug/3,
                                      Extra,
                                      {actor_state_change, F}),
	    receive_loop(ActorModule, F, D, Debug3);

	no_change ->
            Debug3 = sys:handle_debug(Debug2,
                                      fun print_sys_debug/3,
                                      Extra,
                                      {actor_no_change, Fun}),
	    receive_loop(ActorModule, Fun, State, Debug3);

	{exit, Reason} ->
	    exit(Reason)
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
    Info = info(self()),
    put('@actor_info', Info#actor_info{current_fun = NewFun}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
format_from_info(Pid) ->
    lists:flatten(
      case get('@actor_info') of
          undefined ->
              io_lib:format("~w", [Pid]);
          #actor_info{module = M,
                      current_fun = F} ->
              io_lib:format("~w:~w ~w", [M,F,Pid])
      end).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
print_sys_debug(Dev, SysMsg, Extra) ->
    ok.
