-module(autumn_app).
%%%=============================================================================
%%% @doc
%%%
%%% Pluggable supervision and dependency injection application.
%%%
%%% Manages starting and stopping of processes, and passing of
%%% parameters.
%%%
%%% == Design goals ==
%%%
%%% Robustness, ease of maintenance, less code to write and read.
%%%
%%% == Starting a process ==
%%%
%%% Processes are not started explicitly. Autumn starts them
%%% automatically, when the conditions described below are met.
%%%
%%% Configuration items consist of a key and a value and have the form
%%% `{Key :: atom(), Value :: term()}', processes have the form `{Key
%%% :: atom(), Pid :: pid()}' the `Key' could be the module name of
%%% the process.
%%%
%%% Start arguments are required by processes before to be
%%% started. Processes are always associated with a single erlang
%%% module. Start arguments are defined by enumeration of the keys of
%%% the required configuration items and processes.
%%%
%%% All start arguments are combined to a single
%%% `proplists:proplist()' and passed to the start function of the
%%% module that defined the requirements.
%%%
%%% If a start argument is a process Autum will automatically exit the
%%% process started by the start function of the module defining the
%%% requirement when the required process exits.
%%%
%%% Autumn keeps track of what process provided what configuration
%%% item; if a process that provided an item exits, autumn will not
%%% exit the processes which require this item unless this is
%%% explicitly defined in the configuration of the requirement.
%%%
%%% A module can define a set of keys and when a process is started by
%%% that module, autumn will create a configuration item for each with
%%% the form `{Key, pid()}'; these are added to the set of active
%%% configuration items know by autumn, with the result that new
%%% processes might be spawned.
%%%
%%% Autumn spawns a processes using the start function of the
%%% corresponding module for every unique set of start arguments,
%%% hence making the distinction between singleton and prototype
%%% "bean" obsolete.
%%%
%%% Let me rephrase: for each complete set of configuration items that
%%% are the required start arguments of a module, the start function
%%% is called by autumn, with the expectation that a new process is
%%% started - or a new configuration item is returned - with the form
%%% `{ok, Pid}' or `{ok, ConfigItem}'.
%%%
%%% Besides start arguments, a process can be a container for
%%% configuration items. In this case those configuration items are
%%% not required to start the process. The Autumn will then tell the
%%% processes when a configuration item appers and when it
%%% dissappears.
%%%
%%% How the required start arguments are found, how the container
%%% processes can be configured is not contained in this
%%% moduled. Autumn uses a helper module defined in the configuration
%%% that is passed to the start function. The module defined there
%%% must implement the {@link au_meta_loader} behaviour.
%%%
%%% The default implementation seems to be {@link au_module_attributes}.
%%%
%%% Another topic of interest is the correct shutdown behaviour. In
%%% conjunction with restarts race conditions might occur when the
%%% user requests new values from a registry which has not yet
%%% cleanedup invalid resources.
%%%
%%% Autumn handles this somehow.
%%%
%%% Process tree management is tricky. If c_worker requires b1_worker
%%% and b2_worker, and both b-workers require a-worker, it might be
%%% the case that it is not desired that a c-worker with b-workers
%%% with diffrent a-workers is created. Autumn will combine
%%% requirements to a set of start args when the dependencies are
%%% compatible. Two dependencies are compatible when their start args
%%% are compatible, meaning that same keys have same values. New
%%% processes are only spawned for compatible sets of start arguments.
%%%
%%% @end
%%%=============================================================================

-behaviour(application).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% OTP Application API
-export([start/2, stop/1]).

%%%=============================================================================
%%% Types
%%%=============================================================================

%%%=============================================================================
%%% Includes
%%%=============================================================================

-include("autumn.hrl").

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Starts the dependency injection actor, and registers it with the
%% name `autumn'.
%% @end
%%------------------------------------------------------------------------------
start(normal, #au_main_config{} = Config) ->
    autumn_sup:start(Config).

%%------------------------------------------------------------------------------
%% @doc
%% Stops autumn and kills all applications.
%% @end
%%------------------------------------------------------------------------------
stop(_) ->
    autumn_sup:stop().

%%%=============================================================================
%%% internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
