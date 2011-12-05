-module(di_context).

-compile([export_all]).

-record(dependency,
	{
	  key :: atom()
	}).

-record(activation_filter,
	{}).

-record(di_process,
	{
	  impl_module                :: module(),
	  dependencies = []          :: [#dependency{}]
	}).

-record(di_config, {
	  module_meta_data_reader = au_module_attribute_reader :: module()
	 }).

start_application(AppName) ->
    Modules = find_application_modules(AppName).




find_application_modules(AppName) ->
    [M || {M, _} <- code:all_loaded(),
	  get_application(M) == {just, AppName}].



get_application(Module) ->
    case  proplists:lookup (application, Module:module_info(attributes)) of
	{_, [App]} ->
	    {just, App};

	{_, _} ->
	    throw({invalid_application_attribute, Module});

	none ->
	    nothing
    end.
