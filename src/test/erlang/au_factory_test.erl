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

-module(au_factory_test).

-include_lib("eunit/include/eunit.hrl").
-include("au_factory.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

start_worker_test() ->
    M = em:new(),
    TestFactory = #factory{start = {worker_mod, start_worker}},
    StartArgs = [arg1, arg2],
    Child = au_test_utils:start(),
    em:strict(M, worker_mod, start_worker, [arg1, arg2],
	      {function,
	       fun(_) ->
		       link(Child),
		       {ok, Child}
	       end}),
    em:replay(M),
    {ok, Factory} = au_factory:start_link(TestFactory),
    {ok, Child} = au_factory:start_child(Factory, StartArgs),
    em:verify(M),
    au_test_utils:stop_proc(Factory),
    au_test_utils:expect_exit(Child),
    ok.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

