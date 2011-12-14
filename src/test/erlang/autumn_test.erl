-module(autumn_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% Tests to start the application
%%%=============================================================================

empty_start_app_test() ->
    M = em:new(),
    AppSupPid1 = spawn(fun() -> receive xxx -> ok after 5000 -> ok end end),
    AppSupPid2 = spawn(fun() -> receive xxx -> ok after 5000 -> ok end end),
    Config = #au_main_config{},
    %% start/stop of client app
    em:strict(M, au_app_sup, start_link, [test_app1, Config],
	      {function, fun(_) ->
				 link(AppSupPid1),
				 {ok, AppSupPid1}
			 end}),
    em:strict(M, au_app_sup, start_link, [test_app1, Config],
	      {return, {ok, AppSupPid2}}),
    em:replay(M),
    autumn:start_link(),
    ?assertEqual({ok, AppSupPid1}, autumn:start_app(test_app1, Config)),
    process_flag(trap_exit, true),
    exit(AppSupPid1, shutdown),
    monitor(process, AppSupPid1),
    receive {'DOWN', _, _, AppSupPid1, _} -> ok end,
    ?assertEqual({ok, AppSupPid2}, autumn:start_app(test_app1, Config)),
    ?assertEqual({error, already_started}, autumn:start_app(test_app1, Config)),
    em:verify(M),
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

