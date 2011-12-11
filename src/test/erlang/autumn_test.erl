-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

start_app_test() ->
    autumn:start_link(#au_main_config{app_info_loader

    autumn:start_app(test_app1, test_app1_cfg),

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

