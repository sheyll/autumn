-module(di_simple_test).

-application(simple_test).

%-behaviour(actor).

start(Parms) ->
    actor:start(?MODULE, Parms, []).

create_initial_state(_Parms) ->
    {ok, ready, 0}.

ready(Reply, get, Count) ->
    Reply(Count),
    no_change;

ready(Reply, stop, Count) ->
    Reply(Count/0),
    no_change;

ready(Reply, inc, Count) ->
    Reply(ok),
    {next, ready, Count + 1}.


