-module(aeva_compile_tests).

-include_lib("eunit/include/eunit.hrl").

compile_test() ->
    {ok,Cwd} = file:get_cwd(),
    Dir = "apps/aevarna/test/contracts",
    {ok,Fs} = file:list_dir(Dir),
    Cfun = fun (F) ->
		   case string:split(F, ".") of
		       [_,"aev"] ->
			   Name = Cwd ++ "/" ++ Dir ++ "/" ++ F,
			   ?assertMatch({ok,_}, aeva_compile:file(Name));
		       _ -> ok
		   end
	   end,
    lists:foreach(Cfun, Fs).
