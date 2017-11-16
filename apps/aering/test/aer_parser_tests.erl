-module(aer_parser_tests).

-include_lib("eunit/include/eunit.hrl").

empty_contract_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"Parse an empty contract.",
       fun() ->
            Text = "contract",
            {ok, Tokens, _} = aer_scan:string(Text),
            {ok, {contract,1,none,"the_contract",[],[]}} = aer_parser:parse(Tokens),
            ok
       end}
     ]}.
