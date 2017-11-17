-module(aer_parser_tests).

-include_lib("eunit/include/eunit.hrl").

simple_contracts_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Parse an empty contract.",
       fun() ->
            Text = "contract Empty",
            {contract, 1, "Empty", [], []} = parse_string(Text),
            ok
       end},
      {"Parse an contract with an identity function.",
       fun() ->
            Text = "contract Identity\n"
                   "export id\n"
                   "pure fun id x = x\n",
            {contract, 1, "Identity",
                [{id, _, "id"}],
                [{'fun', _, [pure], {id, _, "id"},
                    [{id, _, "x"}],
                    {id, _, "x"}}]}
                = parse_string(Text),
            ok
       end},
      {"Parse the counter contract.",
       fun() ->
            _Contract = parse_contract(counter),
            ok
       end},
      {"Operator precedence test.",
        fun() ->
            %% + is left associative
            ?assertEqual(parse_expr("a + b + c"),
                         parse_expr("(a + b) + c")),
            %% Check precedences
            ?assertEqual(parse_expr("a * b % c + d * e / f - g"),
                         parse_expr("(((a * b) % c) + ((d * e) / f)) - g")),
            ok
        end}
     ]}.

parse_contract(Name) ->
    parse_string(aer_test_utils:read_contract(Name)).

parse_string(Text) ->
    {ok, Tokens, _} = aer_scan:string(Text),
    {ok, Contract} = aer_parser:parse(Tokens),
    Contract.

parse_expr(Text) ->
    {contract, _, _, _, [{'fun', _, _, _, _, Expr}]} =
        parse_string("contract dummy\nfun expr _ = " ++ Text),
    Expr.

