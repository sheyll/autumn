-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

%%%................................................................Factory Tests

add_factory_not_exported_test() ->
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    ?assertEqual({error, {function_not_exported, test_m, test_f, 2}}, Res).

add_factory_already_added_test() ->
    M = em:new(),
    Pid = start(),
    em:stub(M, au_factory, start_child, [em:any(), em:any()],
	    {return, {ok, Pid}}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    MFA = {test_m, test_f, [test_arg]},
    Res1 = autumn:add_factory(test_id, [], [], MFA),
    Res2 = autumn:add_factory(test_id, [], [], MFA),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual({error, {already_added, test_id}}, Res2).

remove_not_existant_factory_test() ->
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:remove_factory(test_id),
    ?assertEqual({error, {not_found, test_id}}, Res).

remove_existant_factory_test() ->
    M = em:new(),
    Pid = start(),
    em:stub(M, au_factory, start_child, [em:any(), em:any()],
	    {return, {ok, Pid}}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    Res2 = autumn:remove_factory(test_id),
    Res3 = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual(ok, Res2),
    ?assertEqual(ok, Res3).

%%%.............................................................Dependency Tests

independent_factory_test() ->
    M = em:new(),
    Pid = start(),
    MFA = {test_m, test_f, [test_arg]},
    Factory = #factory{id = test_id,
		       req = [],
		       prov = [],
		       start = MFA},
    em:strict(M, au_factory, start_child, [Factory, []],
	      {return, {ok, Pid}}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:add_factory(test_id, [], [], MFA),
    em:verify(M),
    ?assertEqual(ok, Res).

dependend_factory_test() ->
    stop_autumn(),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(test_id, [xxx], [], {test_m, test_f, [test_arg]}),
    ?assertEqual(ok, Res1).

create_dependent_processes_test() ->
    stop_autumn(),
    {ok, _Pid} = autumn:start_link(),
    ok = autumn:add_factory(parent, [], [parent_info],
			    {parent_m, start_l, [test_arg]}),
    ok = autumn:add_factory(child, [parent_info], [],
			    {child_m, start_l, [test_arg]}),
    stop_autumn().

%%%............................................................Boilerplate Tests

unhandled_info_test() ->
    ?assertEqual({noreply, state}, autumn_server:handle_info(info, state)).

code_change_test() ->
    ?assertEqual({ok, state}, autumn_server:code_change(oldvsn, state, extra)).

terminate_test() ->
    ?assertMatch(ok, autumn_server:terminate(reason, state)).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

stop_autumn() ->
    case whereis(autumn) of
	undefined ->
	    ok;
	Pid ->
	    monitor(process, Pid),
	    exit(Pid, shutdown),
	    receive
		{'DOWN', _, _, Pid, _} -> ok
	    end
    end.

start() ->
    spawn_link(fun() ->
		       receive
			   A -> A

		       after 300000 ->
			       ok
		       end
	       end).
