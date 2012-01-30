%%%=============================================================================
%%% @doc
%%% This module contains functions used by most unit tests.
%%% @end
%%%=============================================================================

-module(au_test_utils).

-export([stop_registered/1,
	 stop_proc/1,
	 start/0,
	 start_link/0,
	 start_mon/0,
	 expect_exit/1]).


%%------------------------------------------------------------------------------
%% @doc
%% Stop a process using {@link stop_proc/1} when `Name' is a registered name.
%% @end
%%------------------------------------------------------------------------------
stop_registered(Name) ->
    case whereis(Name) of
	undefined ->
	    ok;
	Pid ->
	    stop_proc(Pid)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Monitors, unlinks and exits a process. Returns after the `DOWN'
%% messages has arrived.
%% @end
%%------------------------------------------------------------------------------
stop_proc(Pid) ->
    unlink(Pid),
    monitor(process, Pid),
    exit(Pid, shutdown),
    receive
	{'DOWN', _, _, Pid, _} -> ok
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Starts a process and monitors the process calling this
%% function. When the process calling this function exits, the test
%% process exits, too. Otherwise it will exit after 5 minutes, or when
%% any message was sent to the process.
%% @end
%%------------------------------------------------------------------------------
start() ->
    TestP = self(),
    spawn(fun() ->
		  monitor(process, TestP),
		  receive
		      A -> A

		  after 300000 ->
			  ok
		  end
	  end).

%%------------------------------------------------------------------------------
%% @doc
%% Behaves very similar to {@link start/0} but the created process is
%% also monitored. The return value is hence a tuple with process
%% id and monitor reference (PID first).
%% @end
%%------------------------------------------------------------------------------
start_mon() ->
    TestP = self(),
    spawn_monitor(fun() ->
			  monitor(process, TestP),
			  receive
			      A -> A

			  after 300000 ->
				  ok
			  end
		  end).

%%------------------------------------------------------------------------------
%% @doc
%% Starts a process linked to the calling process. Automatically exits
%% after 5 minutes. NOTE: The created process also exits when the
%% linked process exits with reason `normal'.
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    spawn_link(fun() ->
		       process_flag(trap_exit, true),
		       receive
			   A -> A

		       after 300000 ->
			       ok
		       end
	       end).

%%------------------------------------------------------------------------------
%% @doc
%% Waits for a process to exit. The exit reason is returned.
%% @end
%%------------------------------------------------------------------------------
expect_exit(Pid) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    receive
	{'DOWN', Ref, _, _, _} -> ok
    end.

