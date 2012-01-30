-ifndef(AU_FACTORY_HRL).
-define(AU_FACTORY_HRL, true).

%%%=============================================================================
%%% This file contains the record definition for processes started and
%%% managed by autumn.
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
	  %% Unique items is a list of items for wich different values
	  %% for ancestors of the items in the start arg set are not
	  %% allowed.
	  unique_items = [] :: [au_item:id()],
	  %% The function that will be called to create a new process. The items specified in the `req' field above
	  %% will be the parameters.
	  start :: {module(), atom()}}).
-endif.
