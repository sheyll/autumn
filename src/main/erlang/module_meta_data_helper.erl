-module(module_meta_data_helper).

%%%=============================================================================
%%% @doc
%%%
%%% Behaviour definition for a module that contains the functions that
%%% extract meta data necessary for autumns dependency injection.
%%%
%%% An implementation might i.e. read module attributes, xml files or
%%% call functions on the modules of an application to extract meta data.
%%%
%%% @end
%%%=============================================================================

%%%=============================================================================
%%% Exports
%%%=============================================================================
-export([behaviour_info/1]).


%%------------------------------------------------------------------------------
%% @doc

%%

%% @end
%%------------------------------------------------------------------------------
behaviour_info(callbacks) ->
    [];
behaviour_info(_) ->
    undefined.
