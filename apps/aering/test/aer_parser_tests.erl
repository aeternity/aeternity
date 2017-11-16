-module(aer_parser_tests).

-include_lib("eunit/include/eunit.hrl").

identy_fun_test_() ->
    {foreach,
     fun() ->
             ok
     end,
     fun(_) ->
             ok
     end,
     [{"Parse a contract with an identity function.",
       fun() ->
               Tokens1 =
                   [{contract,1},
                    {symbol,{symbol,1,<<"one">>}}],
               Tokens2 =
                   [{export,3},
                    {symbol,{symbol,3,<<"f">>}},
                    {'/',3},
                    {int,3,1}],
               Tokens3 =
                   [{'let',5},
                    {symbol,{symbol,5,<<"f">>}},
                    {'(',5},
                    {')',5},
                    {assign,5},
                    {'Int',6,1}],

               {ok, {contract,one,1}} = aer_parser:parse(Tokens1),
               {ok, {export,[{<<"f">>,1}]}} = aer_parser:parse(Tokens2),
               % {ok, []} = aer_parser:parse(Tokens3),
               ok
       end}
     ]}.
