-module(autumn_app_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test() ->
    Cfg = #au_main_config{},

    M = em:new(),
    em:strict(M, autumn_sup, start, [Cfg]),
    em:strict(M, autumn_sup, stop, []),
    em:replay(M),

    autumn_app:start(normal, Cfg),
    autumn_app:stop(xxx),

    em:verify(M).



