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
               Text = "contract one\n"
                      "export one;\n"
                      "fun pure one x =\n"
                      "  x;",
               {ok, Tokens, _} = aer_scan:string(Text),
               [{contract,1},
                {symbol,{symbol,1,<<"one">>}},
                {export,2},
                {symbol,{symbol,2,<<"one">>}},
                {';',2},
                {symbol,{symbol,3,<<"fun">>}},
                {symbol,{symbol,3,<<"pure">>}},
                {symbol,{symbol,3,<<"one">>}},
                {symbol,{symbol,3,<<"x">>}},
                {assign,3},
                {symbol,{symbol,4,<<"x">>}},
                {';',4}] = Tokens,
               ok
       end}
     ]}.
