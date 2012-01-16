-module(autumn_sup_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

start_test() ->
    M = em:new(),
    em:strict(M, au_factory_sup, start_link, [],
	      {function,
	       fun(_) ->
		       {ok, au_test_utils:start_link()}
	       end}),
    em:strict(M, autumn, start_link, [],
	      {function,
	       fun(_) ->
		       {ok, au_test_utils:start_link()}
	       end}),
    em:replay(M),

    {ok, Pid} = autumn_sup:start_link(),
    unlink(Pid),
    exit(Pid, shutdown),

    em:verify(M).
