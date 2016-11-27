-module(test).
-compile(export_all).
-import(x, [g/0]).

f() ->
	g().
%	{ok, _} = application:get_evn(lala).
