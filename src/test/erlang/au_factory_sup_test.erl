%%%=============================================================================
%%%
%%%               |  o __   _|  _  __  |_   _       _ _   (TM)
%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | |
%%%
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>
%%% @copyright (C) 2011, Lindenbaum GmbH
%%%
%%%=============================================================================

-module(au_factory_sup_test).

-include_lib("eunit/include/eunit.hrl").
-include("au_factory.hrl").

-import(au_test_utils, [stop_registered/1,
			stop_proc/1,
			start/0,
			start_link/0,
			start_mon/0,
			expect_exit/1]).


%%%=============================================================================
%%% TESTS
%%%=============================================================================

add_factory_test() ->
    stop_registered(au_factory_sup),
    Factory = #factory{},
    M = em:new(),
    em:strict(M, au_factory, start_link, [Factory],
	      {function, fun(_) ->
				 {ok, start_link()}
			 end}),
    em:replay(M),
    au_factory_sup:start_link(),
    Res = au_factory_sup:add_factory(Factory),
    stop_registered(au_factory_sup),
    em:verify(M),
    ?assertEqual(ok, Res).

add_same_factory_twice_test() ->
    stop_registered(au_factory_sup),
    Factory = #factory{id = xxx},
    M = em:new(),
    em:strict(M, au_factory, start_link, [Factory],
	      {function, fun(_) ->
				 {ok, start_link()}
			 end}),
    em:replay(M),
    au_factory_sup:start_link(),
    au_factory_sup:add_factory(Factory),
    Res = au_factory_sup:add_factory(Factory),
    stop_registered(au_factory_sup),
    em:verify(M),
    ?assertMatch({error, {factory_not_added, {already_started, _}}},
		 Res).

remove_factory_test() ->
    process_flag(trap_exit, true),
    stop_registered(au_factory_sup),
    Factory = #factory{id = xxx},
    M = em:new(),
    F1 = start(),
    F2 = start(),
    em:strict(M, au_factory, start_link, [Factory],
	      {function, fun(_) ->
				 link(F1),
				 {ok, F1}
			 end}),
    em:strict(M, au_factory, start_link, [Factory],
	      {function, fun(_) ->
				 link(F2),
				 {ok, F2}
			 end}),
    em:replay(M),
    au_factory_sup:start_link(),
    Res1 = au_factory_sup:add_factory(Factory),
    Res2 = au_factory_sup:remove_factory(Factory#factory.id),
    expect_exit(F1),
    Res3 = au_factory_sup:add_factory(Factory),
    stop_registered(au_factory_sup),
    em:verify(M),
    ?assertEqual(ok, Res1),
    ?assertEqual(ok, Res2),
    ?assertEqual(ok, Res3).

remove_non_existant_factory_test() ->
    stop_registered(au_factory_sup),
    au_factory_sup:start_link(),
    Res = au_factory_sup:remove_factory(xxx),
    stop_registered(au_factory_sup),
    ?assertEqual({error, {factory_not_found, xxx}}, Res).

start_child_non_existant_factory_test() ->
    stop_registered(au_factory_sup),
    au_factory_sup:start_link(),
    Res = au_factory_sup:start_child(xxx, []),
    stop_registered(au_factory_sup),
    ?assertEqual({error, {factory_not_found, xxx}}, Res).

start_child_test() ->
    stop_registered(au_factory_sup),
    Factory = #factory{},
    M = em:new(),
    F1 = start(),
    Items = [],
    em:strict(M, au_factory, start_link, [Factory],
	      {function, fun(_) ->
				 link(F1),
				 {ok, F1}
			 end}),
    em:strict(M, au_factory, start_child, [F1, Items],
	      {return, some_retval}),
    em:replay(M),
    au_factory_sup:start_link(),
    au_factory_sup:add_factory(Factory),
    Res = au_factory_sup:start_child(Factory#factory.id, Items),
    stop_registered(au_factory_sup),
    em:verify(M),
    ?assertEqual(some_retval, Res).

