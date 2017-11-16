-module(aer_scan_tests).

-include_lib("eunit/include/eunit.hrl").

empty_contract_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"Parse a contract with an identity function.",
       fun() ->
               Text = " ",
               {ok, [], _} = aer_scan:string(Text),
               ok
       end}
     ]}.

identy_fun_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"Scan a contract with an identity function.",
       fun() ->
               Text = "contract one

"                     "export one/1

"                     "let one () =
"                     "  1
",
               {ok,_Tokens, 7} = aer_scan:string(Text),
               ok
       end}
     ]}.
