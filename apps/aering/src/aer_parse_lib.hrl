
-define(LET_P(X, P, Q), aer_parse_lib:bind(P, fun(X) -> Q end)).
-define(LAZY_P(P),      aer_parse_lib:lazy(fun() -> P end)).
-define(MEMO_P(P),      aer_parse_lib:lazy(aer_parse_lib:memoised(fun() -> P end))).

-define(GUARD_P(G, P),
    case G of
        true  -> P;
        false -> fail()
    end).

-define(RULE(A,                Do), map(fun(_1) ->                       Do end, A                 )).
-define(RULE(A, B,             Do), map(fun({_1, _2}) ->                 Do end, {A, B}            )).
-define(RULE(A, B, C,          Do), map(fun({_1, _2, _3}) ->             Do end, {A, B, C}         )).
-define(RULE(A, B, C, D,       Do), map(fun({_1, _2, _3, _4}) ->         Do end, {A, B, C, D}      )).
-define(RULE(A, B, C, D, E,    Do), map(fun({_1, _2, _3, _4, _5}) ->     Do end, {A, B, C, D, E}   )).
-define(RULE(A, B, C, D, E, F, Do), map(fun({_1, _2, _3, _4, _5, _6}) -> Do end, {A, B, C, D, E, F})).

-import(aer_parse_lib,
        [tok/1, tok/2, between/3, many/1, many1/1, sep/2, sep1/2,
         infixl/1, infixr/1, choice/1, choice/2, return/1, layout/0,
         fail/0, fail/1, map/2, infixl/2, infixr/2, infixl1/2, infixr1/2,
         left/2, right/2, optional/1]).


