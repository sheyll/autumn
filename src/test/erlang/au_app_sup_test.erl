-module(au_app_sup_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

life_cylce_test() ->
    M = em:new(),

    P = spawn(fun() ->
		      receive after 5000 ->
				      ok end
	      end),

    em:strict(M, au_app_worker, start_link, [appId, config],
	      {function,
	       fun(_) ->
		       link(P),
		       {ok, P}
	       end}),
    em:replay(M),
    {ok, S} = au_app_sup:start_link(appId, config),
    ?assertEqual(P, au_app_sup:get_app_worker(S)),
    em:verify(M).



%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

