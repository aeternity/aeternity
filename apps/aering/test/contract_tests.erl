-module(contract_tests).

-include_lib("eunit/include/eunit.hrl").

make_cmd() -> "make -C " ++ aer_test_utils:contract_path().

contracts_test_() ->
    {foreach,
     fun()  -> os:cmd(make_cmd()) end,
     fun(_) -> os:cmd(make_cmd() ++ " clean") end,
     [ {"Testing the " ++ Contract ++ " contract",
        fun() ->
          Output = os:cmd(filename:join(aer_test_utils:contract_path(), Contract ++ "_test")),
          ?assertEqual(Expected, Output)
        end} || {Contract, Expected} <- contracts() ]}.

contracts() ->
  [{"voting",
    "Delegate before vote\n"
    "Cake: 1\n"
    "Beer: 2\n"
    "Winner: Beer\n"
    "Delegate after vote\n"
    "Cake: 1\n"
    "Beer: 2\n"
    "Winner: Beer\n"
  }].

