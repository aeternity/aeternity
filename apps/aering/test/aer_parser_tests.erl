-module(aer_parser_tests).

-export([parse_contract/1]).

-include_lib("eunit/include/eunit.hrl").

simple_contracts_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Parse a contract with an identity function.",
       fun() ->
            Text = "contract Identity =\n"
                   "  function id(x) = x\n",
            ?assertMatch(
                [{contract, _, {con, _, "Identity"},
                    [{letfun, _, {id, _, "id"}, [{arg, _, {id, _, "x"}, {id, _, "_"}}], {id, _, "_"},
                        {id, _, "x"}}]}], parse_string(Text)),
            ok
       end},
      {"Operator precedence test.",
        fun() ->
            NoPar = fun NoPar(X) when is_atom(X) -> atom_to_list(X);
                        NoPar({A, Op, B})        -> lists:concat([NoPar(A), " ", Op, " ", NoPar(B)]);
                        NoPar({Op, A})           -> lists:concat([Op, " ", NoPar(A)])
                    end,
            Par   = fun Par(X) when is_atom(X) -> atom_to_list(X);
                        Par({A, Op, B})        -> lists:concat(["(", Par(A), " ", Op, " ", Par(B), ")"]);
                        Par({Op, A})           -> lists:concat(["(", Op, " ", Par(A), ")"])
                    end,
            Parse = fun(S) ->
                    try remove_line_numbers(parse_expr(S))
                    catch _:_ -> ?assertMatch(ok, {parse_fail, S}) end
                end,
            CheckParens = fun(Expr) ->
                    ?assertEqual(Parse(NoPar(Expr)), Parse(Par(Expr)))
                end,
            LeftAssoc  = fun(Op) -> CheckParens({{a, Op, b}, Op, c}) end,
            RightAssoc = fun(Op) -> CheckParens({a, Op, {b, Op, c}}) end,
            NonAssoc   = fun(Op) ->
                            OpAtom = list_to_atom(Op),
                            ?assertError({error, {_, parse_error, _}},
                                         parse_expr(NoPar({a, Op, {b, Op, c}}))) end,
            Stronger = fun(Op1, Op2) ->
                    CheckParens({{a, Op1, b}, Op2, c}),
                    CheckParens({a, Op2, {b, Op1, c}})
                end,

            Tiers = [["||"], ["&&"], ["==", "!=", "<", ">", "=<", ">="], ["::", "++"],
                     ["+", "-"], ["*", "/", "mod"]],

            %% associativity
            [ RightAssoc(Op) || Op <- ["||", "&&", "::", "++"] ],
            [ NonAssoc(Op)   || Op <- ["==", "!=", "<", ">", "=<", ">="] ],
            [ LeftAssoc(Op)  || Op <- ["+", "-", "*", "/", "mod"] ],

            %% precedence
            [ Stronger(Op2, Op1) || [T1 , T2 | _] <- tails(Tiers), Op1 <- T1, Op2 <- T2 ],
            ok
        end}
     ] ++
     %% Parse tests of example contracts
     [ {lists:concat(["Parse the ", Contract, " contract."]),
        fun() -> roundtrip_contract(Contract) end}
        || Contract <- [counter, voting, all_syntax, '05_greeter', aeproof, multi_sig, simple_storage, withdrawal] ]
    }.

parse_contract(Name) ->
    parse_string(aer_test_utils:read_contract(Name)).

roundtrip_contract(Name) ->
    round_trip(aer_test_utils:read_contract(Name)).

parse_string(Text) ->
    case aer_parser:string(Text) of
        {ok, Contract} -> Contract;
        Err -> error(Err)
    end.

parse_expr(Text) ->
    [{letval, _, _, _, Expr}] =
        parse_string("let _ = " ++ Text),
    Expr.

round_trip(Text) ->
    Contract  = parse_string(Text),
    Text1     = prettypr:format(aer_pretty:decls(Contract)),
    Contract1 = parse_string(Text1),
    NoSrcLoc  = remove_line_numbers(Contract),
    NoSrcLoc1 = remove_line_numbers(Contract1),
    ?assertMatch(NoSrcLoc, diff(NoSrcLoc, NoSrcLoc1)).

remove_line_numbers({line, _L}) -> {line, 0};
remove_line_numbers({col,  _C}) -> {col, 0};
remove_line_numbers([H|T]) ->
  [remove_line_numbers(H) | remove_line_numbers(T)];
remove_line_numbers(T) when is_tuple(T) ->
  list_to_tuple(remove_line_numbers(tuple_to_list(T)));
remove_line_numbers(M) when is_map(M) ->
  maps:from_list(remove_line_numbers(maps:to_list(M)));
remove_line_numbers(X) -> X.

diff(X, X) -> X;
diff([H | T], [H1 | T1]) ->
    [diff(H, H1) | diff(T, T1)];
diff(T, T1) when tuple_size(T) == tuple_size(T1) ->
    list_to_tuple(diff(tuple_to_list(T), tuple_to_list(T1)));
diff(X, Y) -> {X, '/=', Y}.

tails(Zs) -> lists:foldr(fun(X, [Xs|Xss]) -> [[X|Xs], Xs | Xss] end, [[]], Zs).

