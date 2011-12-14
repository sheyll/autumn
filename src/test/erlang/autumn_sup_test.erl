-module(autumn_sup_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

start_test() ->
    Config = #au_main_config{},
    M = em:new(),
    em:strict(M, autumn_server, start_link, [Config],
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

    {ok, Pid} = autumn_sup:start_link(Config),
    unlink(Pid),
    exit(Pid, shutdown),

    em:verify(M).
