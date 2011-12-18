%%%=============================================================================
%%% @doc
%%%
%%% This module contains the `start_app' function that can be used by
%%% OTP Applications, that are designed to rely on autumn completely,
%%% to start a toplevel supervisor for that application, that connects
%%% to the autumn server to announce the modules of that application,
%%% and removes these modules when the application shutsdown.
%%%
%%% Under the hood this is an OTP supervisor, that contains all the
%%% processes that are started on modules contained in the user
%%% application.
%%%
%%% @end
%%%=============================================================================

-module(autumn_otp).
-behaviour(supervisor).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API for other OTP Applications that wish to be managed by the
%% autumn container.
-export([start_app/1]).

%% Supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% Includes
%%%=============================================================================

-include("autumn.hrl").

%%%=============================================================================
%%% API for OTP conform applications using autumn
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%
%% This function must be used by OTP applications using autumn. A
%% process is spawned and registered by the name of the application
%% and is returned as `{ok, Pid}', this process will be an OTP conform
%% supervisor that will contain all processes Autumn will start for
%% this application.
%%
%% The parameter must be a name of an applicaiton that has an autumn
%% compatible application descriptor, i.e. contains no start function
%% and application callback module, and is from OTPs perspective only
%% a library application.
%%
%% @end
%% ------------------------------------------------------------------------------
-spec start_app(module()) ->
		   ok | {error, already_started}.
start_app(AppId) ->
    supervisor:start_link(?MODULE, AppId).

%%%=============================================================================
%%% supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init(_AppId) ->
    %% TODO
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxTSeconds = 1,
    {ok, {{RestartStrategy, MaxRestarts, MaxTSeconds}, []}}.


