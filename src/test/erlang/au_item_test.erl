-module(au_item_test).

-include_lib("eunit/include/eunit.hrl").


new_test() ->
    I = au_item:new(k,v),
    ?assertEqual(k, au_item:key(I)),
    ?assertEqual(v, au_item:value(I)).

new_link_test() ->
    spawn_monitor(fun() ->
		  I = au_item:new_link(k,v),
		  ?assertEqual(k, au_item:key(I)),
		  ?assertEqual(v, au_item:value(I)),
		  exit(I)
	  end),
    receive
	{'DOWN',_,_,_,I} ->
	    ?assertEqual(false, au_item:is_valid(I))
    end.

invalidate_test() ->
    TestP = self(),
    spawn_monitor(fun() ->
			  I = au_item:new_link(k,v),
			  TestP ! I,
			  receive after 100000 -> ok end
		  end),
    receive
	I ->
	    ?assertEqual(k, au_item:key(I)),
	    ?assertEqual(v, au_item:value(I)),
	    ?assertEqual(true, au_item:is_valid(I)),
	    au_item:invalidate(I),
	    ?assertEqual(false, au_item:is_valid(I)),
	    ?assertEqual(k, au_item:key(I)),
	    ?assertEqual(v, au_item:value(I)),
	    receive
		{'DOWN', _,_,_, {invalidated, I}} ->
		    ok
	    end
    end.

invalidate_after_user_exits_unnormal_test() ->
    TestP = self(),
    Pid = spawn(fun() ->
		       I = au_item:new_link(k,v),
		       TestP ! I,
		       receive after 100000 -> ok end
	       end),
    receive
	I ->
	    Ref = au_item:monitor(I),
	    exit(Pid, killlllll),
	    receive
		{'DOWN', Ref,_,_, _} ->
		    ok
	    end
    end.

monitor_test() ->
    I = au_item:new(k,v),
    Ref = au_item:monitor(I),
    au_item:invalidate(I),
    receive
	{'DOWN',Ref,_,_,_} ->
	    ?assertEqual(false, au_item:is_valid(I))
    end.

get_creator_test() ->
    I = au_item:new(a,1),
    ?assertEqual(self(), au_item:get_creator(I)),
    IL = au_item:new_link(a, 2),
    ?assertEqual(self(), au_item:get_creator(IL)).
