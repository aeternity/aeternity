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
     [{"Scan an empty contract.",
       fun() ->
               Text = " ",
               {ok, []} = aer_scan:scan(Text),
               ok
       end}
     ]}.

all_tokens_test_() ->
    {foreach, fun() -> ok end,
              fun(_) -> ok end,
     [{"Check that we can scan all tokens.",
        fun() ->
            Tokens = all_tokens(),
            Text = string:join(lists:map(fun show_token/1, Tokens), " "),
            io:format("~s\n", [Text]),
            {ok, Tokens1} = aer_scan:scan(Text),
            true = compare_tokens(Tokens, Tokens1),
            ok
        end}
    ]}.

all_tokens() ->
    Lit = fun(T) -> {T, 1} end,
    Tok = fun(T, V) -> {T, 1, V} end,
    Hash = list_to_binary([ I * 8 || I <- lists:seq(0, 31) ]),
    %% Symbols
    lists:map(Lit, [',', '.', ';', '|', ':', '(', ')', '[', ']', '{', '}']) ++
    %% Operators
    lists:map(Lit, ['=', '==', '!=', '>', '<', '>=', '=<', '-', '+', '++', '*', '/', mod, ':', '::', '->', '=>', '||', '&&', '!']) ++
    %% Keywords
    lists:map(Lit, [contract, type, 'let', switch, rec, 'and']) ++
    %% Comment token (not an actual token), just for tests
    [{comment, 0, "// *Comment!\"\n"},
     {comment, 0, "/* bla /* bla bla */*/"}] ++
    %% Literals
    [ Lit(true), Lit(false)
    , Tok(id, "foo"), Tok(id, "_"), Tok(con, "Foo")
    , Tok(hash, Hash)
    , Tok(int, 1234567890), Tok(hex, 9876543210)
    , Tok(string, <<"bla\"\\\b\e\f\n\r\t\vbla">>)
    ].

compare_tokens([], []) -> true;
compare_tokens([{T, _} | Ts1], [{T, _} | Ts2]) ->
    compare_tokens(Ts1, Ts2);
compare_tokens([{T, _, V} | Ts1], [{T, _, V} | Ts2]) ->
    compare_tokens(Ts1, Ts2);
compare_tokens([{comment, _, _} | Ts1], Ts2) ->
    compare_tokens(Ts1, Ts2);
compare_tokens(Ts1, Ts2) ->
    case length(Ts1) == length(Ts2) of
        true  ->
            {token_mismatch, [ {expected, T1, got, T2} || {T1, T2} <- lists:zip(Ts1, Ts2), T1 /= T2]};
        false ->
            {token_mismatch, {expected, Ts1, got, Ts2}}
    end.

fmt(X) -> fmt("~p", X).
fmt(Fmt, X) -> lists:flatten(io_lib:format(Fmt, [X])).

show_token({T, _}) -> atom_to_list(T);
show_token({id, _, X}) -> X;
show_token({con, _, C}) -> C;
show_token({param, _, P}) -> "@" ++ P;
show_token({string, _, S}) -> fmt(binary_to_list(S));
show_token({int, _, N}) -> fmt(N);
show_token({hex, _, N}) -> fmt("0x~.16b", N);
show_token({hash, _, <<N:256>>}) -> fmt("#~.16b", N);
show_token({comment, _, S}) -> S;
show_token({_, _, _}) -> "TODO".

