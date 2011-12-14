-module(autumn_app_test).

-include_lib("eunit/include/eunit.hrl").
-include("autumn.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_stop_test() ->
    M = em:new(),
    em:strict(M, autumn_sup, start, []),
    em:replay(M),

    autumn_app:start(normal, xxx),
    autumn_app:stop(xxx),

    em:verify(M).



