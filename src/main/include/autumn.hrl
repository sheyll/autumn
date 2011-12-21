-ifndef(AUTUMN_HRL).
-define(AUTUMN_HRL, true).

%%%=============================================================================
%%% This file contains some record definitions and useful macros
%%%=============================================================================

%% internal record to describe factories(modules with a function
%% spawning processes).
-record(factory,
	{id :: term(),
	 req :: [au_item:id()],
	 prov :: [au_item:id()],
	 start :: {module(), atom(), [term()]}}).

-endif.
