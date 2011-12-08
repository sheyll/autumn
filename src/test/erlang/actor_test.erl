-module(actor_test).

-include_lib("eunit/include/eunit.hrl").


%%%=============================================================================
%%% start error tests
%%%=============================================================================
start_invalid_return_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, something}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params, []),
    em:verify(M),
    ?assertMatch({error, {invalid_return_value, something}}, Res),
    ok.

start_error_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {error, test_error}}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params),
    em:verify(M),
    ?assertMatch({error, test_error}, Res),
    ok.

start_timeout_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {function,
	      fun(_) ->
		      receive after 501 -> {ok, a, b} end
	      end}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params),
    em:verify(M),
    ?assertMatch({error, timeout}, Res),
    ok.

start_throw_test() ->
    Time = 10,
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {function,
	      fun(_) ->
		      throw(test_exception)
	      end}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params, [{timeout, Time}]),
    em:verify(M),
    ?assertMatch({error, {caught_exception, test_exception}}, Res),
    ok.

start_kill_test() ->
    Time = 10,
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {function,
	      fun(_) ->
		      exit(kill)
	      end}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params, [{timeout, Time}]),
    em:verify(M),
    ?assertMatch({error, {unexpected_exit, kill}}, Res),
    ok.

start_exit_test() ->
    Time = 10,
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {function,
	      fun(_) ->
		      exit(some_reason)
	      end}),
    em:replay(M),
    Res = actor:spawn(test_actor, start_params, [{timeout, Time}]),
    em:verify(M),
    ?assertEqual({error, {unexpected_exit, some_reason}},
		 Res),
    ok.

start_runtime_error_test() ->
    Time = 10,
    M = em:new(),
    Zero = 0,
    em:strict(M, test_actor, create_initial_state, [Zero],
	     {function,
	      fun([Z]) ->
		      10 / Z
	      end}),
    em:replay(M),
    Res = actor:spawn(test_actor, Zero, [{timeout, Time}]),
    em:verify(M),
    ?assertEqual({error, {runtime_error, badarith}},
		 Res),
    ok.

%%%=============================================================================
%%% start ok test
%%%=============================================================================

start_ok_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, actor_fun_1, actor_state_1}}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    em:verify(M),
    ?assert(is_process_alive(Actor)),
    ok.

%%%=============================================================================
%%% RPC and state change error tests without `trap_exit'
%%%=============================================================================

rpc_callback_exits_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, fun_1, state_1}}),
    em:strict(M, test_actor, fun_1, [em:any(), request_msg, state_1],
	     {function, fun(_) -> exit(normal) end}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    ?assert(is_process_alive(Actor)),
    Res = (catch actor:rpc(Actor, request_msg)),
    em:verify(M),
    ?assertMatch({actor_rpc_error,
		  {actor_info, test_actor, fun_1, start_params},
		  {msg, request_msg},
		  {reason, {exit_before_reply, normal}}},
		 Res),
    ok.

rpc_not_an_actor_test() ->
    Pid = spawn_link(fun() -> receive stop -> ok end end),
    Res = (catch actor:rpc(Pid, request_msg)),
    ?assertMatch({actor_rpc_error,
		  {msg, request_msg},
		  {reason, not_an_actor}},
		 Res),
    ok.

rpc_noproc_test() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Pid ! stop,
    R = monitor(process, Pid), receive {'DOWN',R,_,_,_} -> ok end,
    Res = (catch actor:rpc(Pid, request_msg)),
    ?assertMatch({actor_rpc_error,
		  {msg, request_msg},
		  {reason, noproc}},
		 Res),
    ok.

rpc_timeout_test() ->
    Timeout = 10,
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, fun_1, state_1}}),
    em:strict(M, test_actor, fun_1, [em:any(), request_msg, state_1],
	     {function,
	      fun(_) -> receive after Timeout + 1 -> no_change end end}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    ?assert(is_process_alive(Actor)),
    Res = (catch actor:rpc(Actor, request_msg, Timeout)),
    em:verify(M),
    ?assertMatch({actor_rpc_error,
		  {actor_info, test_actor, fun_1, start_params},
		  {msg, request_msg},
		  {reason, {timeout, Timeout}}},
		 Res),
    ?assert(is_process_alive(Actor)),
    ok.

rpc_callback_throw_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, fun_1, state_1}}),
    em:strict(M, test_actor, fun_1, [em:any(), request_msg, state_1],
	     {function, fun(_) -> throw(test_exception) end}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    ?assert(is_process_alive(Actor)),
    Res = (catch actor:rpc(Actor, request_msg)),
    em:verify(M),
    ?assertMatch({actor_rpc_error,
		  {actor_info, test_actor, fun_1, start_params},
		  {msg, request_msg},
		  {reason, {exit_before_reply, _}}},
		 Res),
    ok.

rpc_callback_runtime_error_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, fun_1, state_1}}),
    em:strict(M, test_actor, fun_1, [em:any(), request_msg, state_1],
	     {function, fun(Args) -> Args * 4  end}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    ?assert(is_process_alive(Actor)),
    Res = (catch actor:rpc(Actor, request_msg)),
    em:verify(M),
    ?assertMatch({actor_rpc_error,
		  {actor_info, test_actor, fun_1, start_params},
		  {msg, request_msg},
		  {reason, {exit_before_reply, _}}},
		 Res),
    ok.

%%%=============================================================================
%%% RPC with delayed answer and state change and to tests
%%%=============================================================================

rpc_delayed_answer_complete_test() ->
    M = em:new(),
    em:strict(M, test_actor, create_initial_state, [start_params],
	     {return, {ok, fun_1, state_1}}),
    %% first rpc will be answered later. The plan is that the first
    %% rpc is done by another process that will exit with with what
    %% ever was answered as reason.  then some rpcs follow and after
    %% the last the answerer is applied to the reason.
    em:strict(M, test_actor, fun_1, [em:any(), msg_1, state_1],
	     {function,
	      fun([Answerer, _, _]) ->
		      %% store answerer and reason in state
		      {next, fun_2, Answerer}
	      end}),
    em:strict(M, test_actor, fun_2, [em:any(), msg_2, em:any()],
	     {function,
	      fun(_) ->
		      %% msg_2 is for testing no_change
		      no_change
	      end}),
    em:strict(M, test_actor, fun_2, [em:any(), msg_3, em:any()],
	     {function,
	      fun([_, _, Answerer]) ->
		      %% msg_3 will answer the answerer from msg_1 and
		      %% exit.
		      Answerer(msg_1),
		      {exit, test_reason}
	      end}),
    em:replay(M),
    {ok, Actor} = actor:spawn(test_actor, start_params, []),
    monitor(process, Actor),
    ?assert(is_process_alive(Actor)),
    Test = self(),
    spawn(fun() ->
		  Res = (catch actor:rpc(Actor, msg_1)),
		  Test ! {msg_1_res, Res}
	  end),
    receive after 5 -> msg_1_delivered end,
    Actor ! msg_2,
    Actor ! msg_3,
    receive
	{msg_1_res, Res} ->
	    ?assertMatch(msg_1, Res)
    end,
    receive
	{'DOWN',_,_,Actor,test_reason} ->
	    ok
    after 100 ->
	    throw(actor_exit_expected)
    end,
    em:verify(M),
    ok.


