-module(aehttp_helpers).

-export([ process_request/2
        , parse_map_to_atom_keys/0
        , read_required_params/1
        , read_optional_params/1
        , base58_decode/1
        , hexstrings_decode/1
        , ttl_decode/1
        , parse_tx_encoding/1
        , relative_ttl_decode/1
        , nameservice_pointers_decode/1
        , get_nonce/1
        , print_state/0
        , get_contract_code/2 
        , verify_oracle_existence/1
        , verify_oracle_query_existence/2
        , verify_name/1
        , compute_contract_call_data/0
        ]).

-export([ get_transaction/2
        , encode_transaction/3
        ]).

-export([ ok_response/1
        , unsigned_tx_response/1]).

process_request(FunsList, Req) ->
    process_request(FunsList, Req, #{}).

process_request([], _Req, {Code, _, _Response} = Result)
    when is_integer(Code) ->
    Result;
process_request([Fun | T], Req, Result0) ->
    case Fun(Req, Result0) of
        ok ->
            process_request(T, Req, Result0);
        {ok, Result} ->
            process_request(T, Req, Result);
        {ok, Req1, Result} ->
            process_request(T, Req1, Result);
        {error, ErrMsg}->
            ErrMsg
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

read_optional_params(Params) ->
    params_read_fun(Params,
        fun({Name, DefaultValue}, Req, _) ->
            Val =
                %% swagger puts an 'undefined' value for missing not reqired
                %% params
                case maps:get(Name, Req) of
                    undefined -> DefaultValue;
                    V -> V
                end,
            {ok, Val}
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

get_contract_code(ContractKey, CodeKey) ->
    fun(_Req, State) ->
        ContractPubKey = maps:get(ContractKey, State),
        TopBlockHash = aec_chain:top_block_hash(),
        {ok, Trees} = aec_chain:get_block_state(TopBlockHash),
        Tree = aec_trees:contracts(Trees),
        case aect_state_tree:lookup_contract(ContractPubKey, Tree) of
            none ->
                Msg = "Contract address for key " ++ atom_to_list(ContractKey) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, Contract} ->
                {ok, maps:put(CodeKey, aect_contracts:code(Contract), State)}
        end
    end.

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
        TopBlockHash = aec_chain:top_block_hash(),
        {ok, Trees} = aec_chain:get_block_state(TopBlockHash),
        Tree = StateTreeFun(Trees),
        case Lookup(ReceivedAddress, Tree) of
            none ->
                Msg = Entity ++ " for key " ++ atom_to_list(Key) ++ " not found",
                {error, {404, [], #{<<"reason">> => list_to_binary(Msg)}}};
            {value, _} ->
                ok
        end
    end.

verify_name(NameKey) ->
    fun(_Req, State) ->
        Name = maps:get(NameKey, State),
        case aens:get_name_hash(Name) of
            {ok, _} -> ok;
            {error, Reason} ->
                ReasonBin = atom_to_binary(Reason, utf8),
                {error, {400, [],
                        #{reason => <<"Name validation failed with a reason: ",
                                                ReasonBin/binary>>}}}
        end
    end.

nameservice_pointers_decode(PointersKey) ->
    fun(_Req, State) ->
        Pointers = maps:get(PointersKey, State),
        try {ok, maps:put(PointersKey, jsx:decode(Pointers), State)}
        catch _:_ ->
            {error, {400, [], #{<<"reason">> => <<"Invalid pointers">>}}}
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

ok_response(Fun) ->
    fun(Req, State) ->
        Res =
            case Fun of
                _ when is_function(Fun, 1) ->
                    Fun(State);
                _ when is_function(Fun, 2) ->
                    Fun(Req, State)
            end,
        {ok, {200, [], Res}}
    end.

unsigned_tx_response(NewTxFun) when is_function(NewTxFun, 1) ->
    RespFun =
        fun(State) ->
            {ok, Tx} = NewTxFun(State),
            #{tx => aec_base58c:encode(transaction,
                                       aetx:serialize_to_binary(Tx)),
              tx_hash => aec_base58c:encode(tx_hash, aetx:hash(Tx))
            }
        end,
    ok_response(RespFun).

compute_contract_call_data() ->
    fun(_Req, State) ->
        #{contract_code := Code,
            function := Function,
            arguments := Argument} = State,
        case aect_dispatch:encode_call_data(<<"ring">>,
                      Code, Function, Argument) of
          {ok, HexCallData} ->
              CallData = aeu_hex:hexstring_decode(HexCallData),
              {ok, maps:put(call_data, CallData, State)};
          {error, ErrorMsg} when is_binary(ErrorMsg) ->
                Reason = <<"Failed to compute call_data, reason: ",
                                        ErrorMsg/binary>>,
                {error, {400, [], #{<<"reason">> => Reason}}}
        end
    end.

get_transaction(TxKey, TxStateKey) ->
    fun(_Req, State) ->
        TxHash = maps:get(TxKey, State),
        case aec_db:read_tx(TxHash) of
            [] ->
                {error, {404, [], #{<<"reason">> => <<"Transaction not found">>}}};
            [{Block0, Tx}] ->
                BlockHash =
                    case Block0 of
                        mempool -> mempool;
                        BH when is_binary(BH) -> BH
                    end,
                {ok, maps:put(TxStateKey, #{tx => Tx,
                                            tx_block_hash => BlockHash},
                             State)}
        end
    end.

parse_tx_encoding(TxEncodingKey) ->
    fun(_Req, State) ->
        TxEncoding = maps:get(TxEncodingKey, State),
        case lists:member(TxEncoding, [message_pack, json]) of
            true ->
                {ok, maps:put(TxEncodingKey, TxEncoding, State)};
            false ->
                {error, {400, [], #{reason => <<"Unsupported transaction encoding">>}}}
        end
    end.
    
encode_transaction(TxKey, TxEncodingKey, EncodedTxKey) ->
    fun(_Req, State) ->
        #{tx := Tx,
          tx_block_hash := BlockHash} = maps:get(TxKey, State),
        TxEncoding = maps:get(TxEncodingKey, State),
        DataSchema =
            case TxEncoding of
                json ->
                    <<"SingleTxJSON">>;
                message_pack ->
                    <<"SingleTxMsgPack">>
            end,
        T =
            case BlockHash of
                mempool ->
                    aetx_sign:serialize_for_client_pending(TxEncoding, Tx);
                _ when is_binary(BlockHash) -> 
                    H = aec_db:get_header(BlockHash),
                    aetx_sign:serialize_for_client(TxEncoding, H, Tx)
            end,
        {ok, maps:put(EncodedTxKey, #{tx => T, schema => DataSchema}, State)}
    end.

