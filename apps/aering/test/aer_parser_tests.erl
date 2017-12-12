-module(aer_parser_tests).

-export([parse_contract/1]).

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
            ?assertEqual(parse_expr("a * b mod c + d * e / f - g"),
                         parse_expr("(((a * b) mod c) + ((d * e) / f)) - g")),
            ok
        end}
     ]}.

parse_contract(Name) ->
    parse_string(aer_test_utils:read_contract(Name)).

scan_string(Text) ->
    case aer_scan:string(Text) of
        {ok, Tokens, _} -> Tokens;
        Err = {error, {Line, aer_scan, {user, Reason}}, _} ->
            io:format("Lexical error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

parse_string(Text) ->
    Tokens = scan_string(Text),
    case aer_parser:parse(Tokens) of
        {ok, Contract} -> Contract;
        Err = {error, {Line, aer_parser, Reason}} ->
            io:format("Parse error at line ~p:\n  ~s\n", [Line, Reason]),
            error(Err)
    end.

parse_expr(Text) ->
    {contract, _, _, _, [{'fun', _, _, _, _, Expr}]} =
        parse_string("contract dummy\nfun expr _ = " ++ Text),
    Expr.

