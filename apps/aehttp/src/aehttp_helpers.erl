-module(aehttp_helpers).

-export([ parse_request/2
        , parse_map_to_atom_keys/0
        , read_required_params/1
        , base58_decode/1
        , hexstrings_decode/1
        , ttl_decode/1
        , relative_ttl_decode/1
        , get_nonce/1
        , print_state/0
        , verify_contract_existence/1
        , verify_oracle_existence/1
        , verify_oracle_query_existence/2
        ]).

parse_request(FunsList, Req) ->
    parse_request(FunsList, Req, #{}).

parse_request([], _Req, Result) ->
    {ok, Result};
parse_request([Fun | T], Req, Result0) ->
    case Fun(Req, Result0) of
        ok ->
            parse_request(T, Req, Result0);
        {ok, Result} ->
            parse_request(T, Req, Result);
        {ok, Req1, Result} ->
            parse_request(T, Req1, Result);
        {error, _ErrMsg} = Err ->
            Err
    end.

read_required_params(ParamNames) ->
    params_read_fun(ParamNames,
        fun(Name, Req, _) ->
            case maps:get(Name, Req) of
                undefined -> error;
                Val -> {ok, Val}
            end
        end,
        "Not found").

base58_decode(Params) ->
    params_read_fun(Params,
        fun({Name, Type}, _, Data) ->
            Encoded = maps:get(Name, Data),
            case aec_base58c:safe_decode(Type, Encoded) of
                {error, _} ->
                    error;
                {ok, Hash} ->
                    {ok, Hash}
            end
        end,
        "Invalid hash").

hexstrings_decode(ParamNames) ->
    params_read_fun(ParamNames,
        fun(Name, _, State) ->
            Hex = maps:get(Name, State),
            try {ok, aeu_hex:hexstring_decode(Hex)}
            catch _:_ -> error
            end
        end,
        "Not hex string").

params_read_fun(ParamNames, ReadFun, ErrMsg) ->
    fun(Req, State0) ->
        {Founds, NotFounds} =
            lists:foldl(
                fun(Key0, {F, NotF}) -> 
                    {ReqKey, Name, Props} = 
                        case Key0 of
                            {K, N} -> {K, N, none};
                            {K, N, P} -> {K, N, P};
                            K -> {K, K, none}
                        end,
                    CallParam =
                        case Props of
                            none -> ReqKey;
                            _ -> {ReqKey, Props}
                        end,
                    case ReadFun(CallParam, Req, State0) of
                        error -> {F, [ReqKey | NotF]};
                        {ok, Val} -> {maps:put(Name, Val, F), NotF}
                    end
                end,
                {#{}, []},
                ParamNames),
        case NotFounds =/= [] of
            true ->
                Keys = string:join([atom_to_list(A) || A <- NotFounds], ","),
                Msg = ErrMsg ++ ": " ++ Keys,
                {error, {400, [], #{<<"reason">> => list_to_binary(Msg)}}};
            false ->
                {ok, maps:merge(State0, Founds)}
        end
    end.

get_nonce(AccountKey) ->
    fun(Req, State) ->
        case maps:get(nonce, Req, undefined) of
            undefined ->
                Pubkey = maps:get(AccountKey, State),
                case aec_next_nonce:pick_for_account(Pubkey) of
                    {ok, Nonce} ->
                        {ok, maps:put(nonce, Nonce, State)};
                    {error, account_not_found} ->
                        Msg = "Account of " ++ atom_to_list(AccountKey) ++ " not found",
                        {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}}
                end;
            Nonce ->
                {ok, maps:put(nonce, Nonce, State)}
        end
    end.

print_state() ->
    fun(Req, State) ->
        lager:info("Req: ~p", [Req]),
        lager:info("State: ~p", [State])
    end.

verify_contract_existence(ContractKey) ->
    verify_key_in_state_tree(ContractKey, fun aec_trees:contracts/1,
                             fun aect_state_tree:lookup_contract/2,
                             "Contract address"). 

verify_oracle_existence(OracleKey) ->
    verify_key_in_state_tree(OracleKey, fun aec_trees:oracles/1,
                             fun aeo_state_tree:lookup_oracle/2,
                             "Oracle address"). 

verify_oracle_query_existence(OracleKey, QueryKey) ->
    fun(Req, State) ->
        OraclePubKey = maps:get(OracleKey, State),
        Lookup =
            fun(QId, Tree) ->
                aeo_state_tree:lookup_query(OraclePubKey, QId, Tree)
            end,
        Fun = verify_key_in_state_tree(QueryKey, fun aec_trees:oracles/1,
                                        Lookup, "Oracle query"),
        Fun(Req, State)
    end.

verify_key_in_state_tree(Key, StateTreeFun, Lookup, Entity) ->
    fun(_Req, State) ->
        ReceivedAddress = maps:get(Key, State),
        TopBlockHash = aec_conductor:top_block_hash(),
        {value, Trees} = aec_db:find_block_state(TopBlockHash),
        Tree = StateTreeFun(Trees),
        case Lookup(ReceivedAddress, Tree) of
            none ->
                Msg = Entity ++ " for key " ++ atom_to_list(Key) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, _} ->
                ok
        end
    end.

parse_map_to_atom_keys() ->
    fun(Req, State) ->
        Parse =
            fun P(Map) ->
                maps:from_list(
                    lists:map(
                        fun({K0, V0}) ->
                            K = binary_to_existing_atom(K0, utf8),
                            V = case is_map(V0) of
                                  true -> P(V0);
                                  false -> V0
                                end,
                            {K, V}
                        end,
                        maps:to_list(Map)))
            end,
        Req1 = Parse(Req), 
        {ok, Req1, State}
    end.

ttl_decode(TTLKey) ->
    ttl_decode(TTLKey, [<<"delta">>, <<"block">>]).

relative_ttl_decode(TTLKey) ->
    ttl_decode(TTLKey, [<<"delta">>]).

ttl_decode(TTLKey, AllowedTypes) ->
    fun(Req, State) ->
        TTLObj = maps:get(TTLKey, Req),
        TTLKeyBin = atom_to_binary(TTLKey, utf8),
        ErrorBrokenTTL = {error, {400, [],
                          #{<<"reason">> => <<"Broken TTL for key ",TTLKeyBin/binary>>}}},
        case {maps:get(type, TTLObj, undefined), maps:get(value, TTLObj,
                                                          undefined)} of
            {undefined, _} -> ErrorBrokenTTL;
            {_, undefined} -> ErrorBrokenTTL;
            {Type, Value} ->
                case lists:member(Type, AllowedTypes) of
                    true ->
                        TTL = {binary_to_existing_atom(Type, utf8), Value},
                        {ok, maps:put(TTLKey, TTL, State)};
                    false ->
                        TypesBin = list_to_binary(string:join(
                            [binary_to_list(T) || T <- AllowedTypes],
                            ",")),
                        ErrMsg = <<"Invalid TTL type for key ",
                                  TTLKeyBin/binary, "; Allowed: ",
                                  TypesBin/binary>>,
                        {error, {400, [], #{<<"reason">> => ErrMsg}}}
                end
        end
    end.
