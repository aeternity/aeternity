-module(contract_tests).

-include_lib("eunit/include/eunit.hrl").

make_cmd() -> "make -C " ++ aeso_test_utils:contract_path().

contracts_test_() ->
    {setup,
     fun()  -> os:cmd(make_cmd()) end,
     fun(_) -> os:cmd(make_cmd() ++ " clean") end,
     [ {"Testing the " ++ Contract ++ " contract",
        fun() ->
          ?assertCmdOutput(Expected, filename:join(aeso_test_utils:contract_path(), Contract ++ "_test"))
        end} || {Contract, Expected} <- contracts() ]}.

contracts() ->
    [].
  %% [{"voting",
  %%   "Delegate before vote\n"
  %%   "Cake: 1\n"
  %%   "Beer: 2\n"
  %%   "Winner: Beer\n"
  %%   "Delegate after vote\n"
  %%   "Cake: 1\n"
  %%   "Beer: 2\n"
  %%   "Winner: Beer\n"
  %% }].

