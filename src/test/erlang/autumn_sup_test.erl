-module(autumn_sup_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

start_test() ->
    M = em:new(),
    em:strict(M, autumn, start_link, [],
	      {function,
	       fun(_) ->
		       Pid = spawn_link(fun() ->
						receive
						    X -> X
						after
						    10000 -> ok
						end
					end),
		       {ok, Pid}
	       end}),
    em:replay(M),

    {ok, Pid} = autumn_sup:start_link(),
    unlink(Pid),
    exit(Pid, shutdown),

    em:verify(M).
