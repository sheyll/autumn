-module(di_simple_test).

-application(simple_test).

%-behaviour(actor).

start(Parms) ->
    actor:start(?MODULE, Parms, []).

create_initial_state(_Parms) ->
    {ok, ready, no_state}.

ready(Reply, In, _) ->
    Reply({di_simple_test_echo, In}),
    no_change.

