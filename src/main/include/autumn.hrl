-ifndef(AUTUMN_HRL).
-define(AUTUMN_HRL, true).

%%%=============================================================================
%%% This file contains some record definitions and useful macros
%%%=============================================================================

%% Configures the autumn meta data strategy etc...
-record(au_main_config, {
	  %% this defines the helper module that will be used to
	  %% create the configuration needed for dependency injection
	  %% and auto wireing.
	  meta_loader = au_module_attributes :: module()
	 }).

-endif.
