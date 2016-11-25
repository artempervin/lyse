-module(test).
-compile(export_all).

f() ->
%%	process_flag(trap_exit, true),
	try exit(kill) of
		X -> X
    catch Class:Reason ->
    	io:format("Class: ~p, Reason: ~p~n", [Class, Reason])
	end,
	io:format("exited!~n").
