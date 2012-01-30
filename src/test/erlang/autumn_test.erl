-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").
-include("au_factory.hrl").

-import(au_test_utils, [stop_registered/1,
			stop_proc/1,
			start/0,
			start_mon/0]).

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

%%%................................................................Factory Tests

add_factory_already_added_test() ->
    stop_autumn(),
    Fac = #factory{id = test_id, req = [x]},
    M = em:new(),
    em:strict(M, au_factory_sup, add_factory, [Fac]),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(Fac),
    Res2 = autumn:add_factory(Fac),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual({error, {already_added, test_id}}, Res2).

remove_not_existant_factory_test() ->
    stop_autumn(),
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:remove_factory(test_id),
    ?assertEqual({error, {not_found, test_id}}, Res).

remove_existant_factory_test() ->
    stop_autumn(),
    Fac = #factory{id = test_id, req = [x]},
    M = em:new(),
    em:strict(M, au_factory_sup, add_factory, [Fac]),
    em:strict(M, au_factory_sup, remove_factory, [Fac#factory.id]),
    em:strict(M, au_factory_sup, add_factory, [Fac]),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(Fac),
    Res2 = autumn:remove_factory(test_id),
    Res3 = autumn:add_factory(Fac),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual(ok, Res2),
    ?assertEqual(ok, Res3).

%%%.............................................................Dependency Tests

independent_factory_test() ->
    stop_autumn(),
    M = em:new(),
    Pid = start(),
    MFA = {test_m, test_f},
    Factory = #factory{id = test_id,
		       req = [],
		       start = MFA},
    em:strict(M, au_factory_sup, add_factory, [Factory]),
    em:strict(M, au_factory_sup, start_child, [Factory#factory.id, []],
	      {return, {ok, Pid}}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:add_factory(Factory),
    em:verify(M),
    ?assertEqual(ok, Res).

%%%...................................................................Push Tests

simple_push_test() ->
    stop_autumn(),
    M = em:new(),
    %% two modules with the same requirement, both must be started
    Pid1 = start(),
    MFA1 = {test_m_1, test_f},
    Factory1 = #factory{id = test_id_1,
			req = [xxx],
			start = MFA1},
    MFA2 = {test_m_2, test_f},
    Factory2 = #factory{id = test_id_2,
			req = [xxx],
			start = MFA2},
    %% third module with unsatisfied dependency to yyy
    Factory3 = #factory{id = test_id_3,
			req = [yyy],
			start = {test_m_3, test_f}},
    em:strict(M, au_factory_sup, add_factory, [Factory1]),
    em:strict(M, au_factory_sup, add_factory, [Factory2]),
    em:strict(M, au_factory_sup, add_factory, [Factory3]),

    em:strict(M, au_factory_sup, start_child,
	      [Factory1#factory.id, fun([I]) ->
					    au_item:key(I) == xxx andalso
						au_item:value(I) == some_val
				    end],
	      {return, {ok, Pid1}}),

    Pid2 = start(),
    em:strict(M, au_factory_sup, start_child,
	      [Factory2#factory.id, fun([I]) ->
					    au_item:key(I) == xxx andalso
						au_item:value(I) == some_val
				    end],
	      {return, {ok, Pid2}}),

    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(Factory1),
    Res2 = autumn:add_factory(Factory2),
    Res3 = autumn:add_factory(Factory3),

    {ok, Item} = autumn:push(xxx, some_val),
    ?assertEqual(xxx, au_item:key(Item)),
    ?assertEqual(some_val, au_item:value(Item)),
    receive after 100 -> ok end,
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual(ok, Res2),
    ?assertEqual(ok, Res3).

start_child_error_test() ->
    stop_autumn(),
    M = em:new(),
    MFA1 = {test_m_1, test_f},
    Factory1 = #factory{id = test_id_1,
			req = [xxx],
			start = MFA1},
    em:strict(M, au_factory_sup, add_factory, [Factory1]),
    em:strict(M, au_factory_sup, start_child,
	      [Factory1#factory.id, em:any()],
	      {return, {error, this_is_a_test}}),
    em:replay(M),
    {ok, Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(Factory1),
    {ok, _Item} = autumn:push(xxx, some_val),
    receive after 100 -> ok end,
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assert(is_process_alive(Pid)),
    ok.

complex_push_test() ->
    stop_autumn(),
    M = em:new(),
    %% two modules with the same requirement, both must be started
    Pid1 = start(),
    MFA1 = {test_m_1, test_f},
    Factory1 = #factory{id = test_id_1,
			req = [xxx],
			start = MFA1},
    Pid2 = start(),
    MFA2 = {test_m_2, test_f},
    Factory2 = #factory{id = test_id_2,
			req = [xxx],
			start = MFA2},
    %% third module with unsatisfied dependency to yyy
    MFA3 = {test_m_3, test_f},
    Factory3 = #factory{id = test_id_3,
			req = [xxx,yyy],
			start = MFA3},

    %% the fourth modules is invoked foreach zzz that factory2 pushed
    Pid4 = start(),
    MFA4 = {test_m_4, test_f},
    Factory4 = #factory{id = test_id_4,
			req = [xxx, zzz],
			start = MFA4},

    em:strict(M, au_factory_sup, add_factory, [Factory1]),
    em:strict(M, au_factory_sup, add_factory, [Factory2]),
    em:strict(M, au_factory_sup, add_factory, [Factory3]),
    em:strict(M, au_factory_sup, add_factory, [Factory4]),

    em:strict(M, au_factory_sup, start_child,
	      [Factory1#factory.id, fun([I]) ->
					    au_item:key(I) == xxx andalso
						au_item:value(I) == some_val
				    end],
	      {return, {ok, Pid1}}),

    em:strict(M, au_factory_sup, start_child,
	      [Factory2#factory.id, fun([I]) ->
					    au_item:key(I) == xxx andalso
						au_item:value(I) == some_val
				    end],
	      {function, fun(_) ->
				 %% the second factory pushes zzz
				 autumn:push(zzz, cool_value),
				 autumn:push(zzz, cooler_value),
				 {ok, Pid2}
			 end}),
    em:strict(M, au_factory_sup, start_child,
	      [Factory4#factory.id, fun(_) -> true end],
	      {return, {ok, Pid4}}),

    Pid5 = start(),
    TestProc = self(),
    em:strict(M, au_factory_sup, start_child,
	      [Factory4#factory.id, fun(_) -> true end],
	      {function, fun(_) ->
				 TestProc ! finished,
				 {ok, Pid5}
			 end}),

    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(Factory1),
    Res2 = autumn:add_factory(Factory2),
    Res3 = autumn:add_factory(Factory3),
    autumn:add_factory(Factory4),

    autumn:push(xxx, some_val),
    receive
	finished -> ok
    end,
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual(ok, Res2),
    ?assertEqual(ok, Res3).

item_exit_test() ->
    stop_autumn(),
    M = em:new(),
    %% single process will be created - and destroyed as soon as
    %% the item is inactivated.
    Pid1 = start(),
    MFA1 = {test_m_1, test_f},
    Factory1 = #factory{id = test_id_1,
			req = [xxx],
			start = MFA1},
    em:strict(M, au_factory_sup, add_factory, [Factory1]),

    em:strict(M, au_factory_sup, start_child,
	      [Factory1#factory.id, em:any()],
	      {function, fun([_, [Item]]) ->
				 au_item:invalidate(Item),
				 {ok, Pid1}
			 end}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    autumn:add_factory(Factory1),
    monitor(process, Pid1),
    Item = au_item:new(xxx, v),
    IRef = au_item:monitor(Item),
    autumn:push(Item),
    receive
	{'DOWN', IRef, _, _, Reason} ->
	    receive
		%% expect the item to be killed
		{'DOWN', _, process, Pid1, Reason} -> ok
	    end
    end,
    em:verify(M),
    ok.

%%%...................................................BoundBy Relationship tests
bound_by_test() ->
    %% ItemA1 -> ProcA1 -> ItemB1 -> ProcB2
    %%       \----------------------/
    %%
    %% ItemA2 -> ProcA3 -> ItemB2 -> ProcB4
    %%       \----------------------/
    %% ProcB depends on Item A and Item B, but is bound to ItemA by the user.
    %% ProcA is naturally bound to ItemA.
    %% ProcB would be started 4 times of it were not bound.

    stop_autumn(),
    TestProc = self(),
    M = em:new(),

    ProcAFac = #factory{id = proc_a,
    			req = [a]},
    ProcBFac = #factory{id = proc_b,
    			req = [a, b],
			unique_items = [a]},

    Ia1 = au_item:new(a, 1),
    Ia2 = au_item:new(a, 2),

    em:strict(M, au_factory_sup, add_factory, [ProcAFac]),
    em:strict(M, au_factory_sup, add_factory, [ProcBFac]),

    em:strict(M, au_factory_sup, start_child, [ProcAFac#factory.id, [Ia1]],
    	      {function, fun(_) ->
				 ProcA1 = spawn(fun() ->
							autumn:push(b, 1),
							receive
							after 5000 -> ok end
						end),
				 {ok, ProcA1}
    			 end}),
    em:strict(M, au_factory_sup, start_child, [ProcBFac#factory.id, em:any()],
    	      {function, fun(_) ->
				 TestProc ! finished,
				 {ok, start()}
    			 end}),
    em:strict(M, au_factory_sup, start_child, [ProcAFac#factory.id, [Ia2]],
    	      {function, fun(_) ->
				 ProcA2 = spawn(fun() ->
							autumn:push(b, 2),
							receive
							after 5000 -> ok end
						end),
				 {ok, ProcA2}
    			 end}),
    em:strict(M, au_factory_sup, start_child, [ProcBFac#factory.id, em:any()],
    	      {function, fun(_) ->
				 TestProc ! finished,
				 {ok, start()}
    			 end}),
    em:replay(M),
    {ok, _Pid} = autumn:start_link(),
    autumn:add_factory(ProcAFac),
    autumn:add_factory(ProcBFac),
    autumn:push(Ia1),
    receive finished -> ok end,
    autumn:push(Ia2),
    receive finished -> ok end,
    em:verify(M),
    ok.

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
    stop_registered(autumn).

