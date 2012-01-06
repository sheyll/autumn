-ifndef(AUTUMN_HRL).
-define(AUTUMN_HRL, true).

%%%=============================================================================
%%% This file contains some record definitions and useful macros
%%%=============================================================================

%% internal record to describe factories(modules with a function
%% spawning processes).
-record(factory,
	{ %% A term uniquely identifying the type or meaning of a set
	  %% of processes, commonly started with the same set of
	  %% parameter types.
	  id :: term(),
	  %% items that are required for a child of the factory
	  req :: [au_item:id()],
	  %% items that must be unique among all ancestors
	  unique_items = [] :: [au_item:id()],
	  %% the start MFA. The items specified in the `req' field above
	  %% will be added as a list as last parameter.
	  start :: {module(), atom(), [term()]}}).

-endif.
