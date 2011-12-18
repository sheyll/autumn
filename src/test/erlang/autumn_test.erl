-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

add_factory_not_exported_test() ->
    {ok, _Pid} = autumn:start_link(),
    Res = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    ?assertEqual({error, function_not_exported}, Res).

add_factory_already_added_test() ->
    M = em:new(),
    em:stub(M, test_m, test_f, [test_arg, []]),
    em:replay(M),

    {ok, _Pid} = autumn:start_link(),
    Res1 = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    Res2 = autumn:add_factory(test_id, [], [], {test_m, test_f, [test_arg]}),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual({error, already_added}, Res2).


unhandled_info_test() ->
    ?assertEqual({noreply, state}, autumn_server:handle_info(info, state)).

code_change_test() ->
    ?assertEqual({ok, state}, autumn_server:code_change(oldvsn, state, extra)).

terminate_test() ->
    ?assertMatch(ok, autumn_server:terminate(reason, state)).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

