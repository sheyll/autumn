-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

empty_start_app_stop_app_test() ->
    M = em:new(),
    AppSupPid = spawn(fun() -> receive xxx -> ok after 5000 -> ok end end),
    %% start/stop of client app
    em:strict(M, au_app_sup, start_link, [test_app1],
	      {ok, AppSupPid}),
    em:replay(M),
    autumn:start_link(#au_main_config{meta_loader=test_loader}),
    ?assertEqual({ok, AppSupPid}, autumn:start_app(test_app1)),
    ?assertEqual({error, already_started}, autumn:start_app(test_app1)),
    ?assertEqual(ok, autumn:stop_app(test_app1)),

    em:verify(M),
    ok.

start_app_with_one_module_test() ->
    ok.


unhandled_info_test() ->
    ?assertEqual({noreply, state}, autumn_server:handle_info(info, state)).

code_change_test() ->
    ?assertEqual({ok, state}, autumn_server:code_change(oldvsn, state, extra)).

terminate_test() ->
    ?assertMatch(ok, autumn_server:terminate(reason, state)).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

