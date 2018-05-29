-module(aehttp_helpers).

-export([ process_request/2
        , parse_map_to_atom_keys/0
        , read_required_params/1
        , read_optional_params/1
        , base58_decode/1
        , hexstrings_decode/1
        , ttl_decode/1
        , poi_decode/1
        , parse_tx_encoding/1
        , relative_ttl_decode/1
        , nameservice_pointers_decode/1
        , get_nonce/1
        , print_state/0
        , get_contract_code/2
        , get_contract_call_object_from_tx/2
        , verify_oracle_existence/1
        , verify_oracle_query_existence/2
        , verify_name/1
        , compute_contract_call_data/0
        , read_tx_encoding_param/1
        , read_optional_param/3
        , parse_filter_param/2
        , get_block/2
        , get_block/3
        , get_block/4
        , get_block_from_chain/1
        ]).

-export([ get_transaction/2
        , encode_transaction/3
        ]).

-export([ ok_response/1
        , unsigned_tx_response/1]).

-import(aeu_debug, [pp/1]).

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
        fun({Name, Default}, Req, _) ->
            %% swagger puts an 'undefined' value for missing not reqired
            %% params
            case maps:get(Name, Req, undefined) of
                undefined when Default == '$no_value' -> no_value;
                undefined                             -> {ok, Default};
                V                                     -> {ok, V}
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
                            {K, N}    -> {K, N, none};
                            {K, N, P} -> {K, N, P};
                            K         -> {K, K, none}
                        end,
                    CallParam =
                        case Props of
                            none -> ReqKey;
                            _    -> {ReqKey, Props}
                        end,
                    case ReadFun(CallParam, Req, State0) of
                        error     -> {F, [ReqKey | NotF]};
                        no_value  -> {F, NotF};
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

get_contract_call_object_from_tx(TxKey, CallKey) ->
    fun(_Req, State) ->
            case maps:get(TxKey, State) of
                #{tx_block_hash := mempool} ->
                    {error, {400, [], #{<<"reason">> => <<"Tx not mined">>}}};
                #{tx_block_hash := BlockHash, tx := STx} ->
                    Tx = aetx_sign:tx(STx),
                    case aetx:specialize_type(Tx) of
                        {contract_call_tx, CallTx} ->
                            {CB, _} = aetx:specialize_callback(Tx),
                            Nonce  = CB:nonce(CallTx),
                            Caller = CB:caller(CallTx),
                            Contract = CB:contract(CallTx),
                            CallId = aect_call:id(Caller, Nonce, Contract),
                            case aec_chain:get_contract_call(Contract, CallId, BlockHash) of
                                {error, Why} ->
                                    Msg = atom_to_binary(Why, utf8),
                                    {error, {400, [], #{<<"reason">> => Msg}}};
                                {ok, CallObject} ->
                                    {ok, maps:put(CallKey, CallObject, State)}
                            end;
                        {_, _} ->
                            {error, {400, [], #{<<"reason">> => <<"Tx is not a call">>}}}
                    end
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
                            case safe_binary_to_atom(K0) of
                                {ok, K} ->
                                    V = case is_map(V0) of
                                          true -> P(V0);
                                          false -> V0
                                        end,
                                    {K, V};
                                {error, non_existing} ->
                                    throw({unknown_key, K0})
                            end
                        end,
                        maps:to_list(Map)))
            end,
        try Parse(Req) of
            Req1 ->
              {ok, Req1, State}
        catch
            throw:{unknown_key, K} when is_binary(K) ->
                {error, {400, [], #{<<"reason">> => <<"Invalid parameter: ", K/binary>>}}}
        end
    end.

poi_decode(PoIKey) ->
    fun(_Req, State) ->
        PoIBin = maps:get(PoIKey, State),
        PoI = aec_trees:deserialize_poi(PoIBin),
        {ok, maps:put(PoIKey, PoI, State)}
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
                case {lists:member(Type, AllowedTypes), safe_binary_to_atom(Type)} of
                    {true, {ok, TypeAtom}} ->
                        TTL = {TypeAtom, Value},
                        {ok, maps:put(TTLKey, TTL, State)};
                    _ ->
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
                                       aetx:serialize_to_binary(Tx))
            }
        end,
    ok_response(RespFun).

compute_contract_call_data() ->
    fun(_Req, State) ->
        #{contract_code := Code,
            function := Function,
            arguments := Argument} = State,
        case aect_dispatch:encode_call_data(<<"sophia">>,
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
        case aec_chain:find_tx_with_location(TxHash) of
            none ->
                {error, {404, [], #{<<"reason">> => <<"Transaction not found">>}}};
            {Tag, Tx} ->
                Val = #{tx => Tx, tx_block_hash => Tag},
                {ok, maps:put(TxStateKey, Val, State)}
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
                    {ok, H} = aec_chain:get_header(BlockHash),
                    aetx_sign:serialize_for_client(TxEncoding, H, Tx)
            end,
        {ok, maps:put(EncodedTxKey, #{tx => T, schema => DataSchema}, State)}
    end.

-spec read_tx_encoding_param(map()) -> {ok, json | message_pack} |
                                       {error, {integer(), list(), map()}}.
read_tx_encoding_param(Req) ->
    read_tx_encoding_param(Req, message_pack).

-spec read_tx_encoding_param(map(), DefaultEncoding :: json | message_pack) ->
                                    {ok, json | message_pack} |
                                    {error, {integer(), list(), map()}}.
read_tx_encoding_param(Req, DefaultEncoding) ->
    case read_optional_enum_param(tx_encoding, Req, DefaultEncoding,
                                  [message_pack, json]) of
        {error, unexpected_value} ->
            {error, {404, [], #{reason => <<"Unsupported transaction encoding">>}}};
        {ok, Encoding} ->
            {ok, Encoding}
    end.

-spec read_optional_enum_param(atom(), map(), term(), list()) -> {ok, term()} |
                                                                 {error, atom()}.
read_optional_enum_param(Key, Req, Default, Enums) ->
    Val = read_optional_param(Key, Req, Default),
    case lists:member(Val, Enums) of
        true -> {ok, Val};
        false -> {error, unexpected_value}
    end.

-spec read_optional_param(atom(), map(), term()) -> term().
read_optional_param(Key, Req, Default) ->
    %% swagger does not take into consideration the 'default'
    %% if a query param is missing, swagger adds it to Req with a value of
    %% 'undefined'
    case maps:get(Key, Req) of
        undefined -> Default;
        Val -> Val
    end.

-spec parse_filter_param(atom(), map()) -> {ok, list()} | {error, unknown_type}.
parse_filter_param(ParamName, Req) when is_atom(ParamName) ->
    Vals = binary:split(
        read_optional_param(ParamName, Req, <<>>),
        [<<",">>],
        [global]),
    case Vals =:= [<<>>] of
        false ->
            KnownTypes = lists:filter(fun aetx:is_tx_type/1, Vals),
            case length(Vals) =:= length(KnownTypes) of
                true -> {ok, KnownTypes};
                false -> {error, unknown_type}
            end;
        true ->
            {ok, []}
    end.

-spec get_block_from_chain(fun(() -> {ok, aec_blocks:block()} | error | {error, atom()}))
    -> {ok, aec_blocks:block()}| {404, [], map()}.
get_block_from_chain(Fun) when is_function(Fun, 0) ->
    case Fun() of
        {ok, _Block} = OK ->
            OK;
        error ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, no_top_header} ->
            {404, [], #{reason => <<"No top header">>}};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}};
        {error, miner_starting} ->
            {404, [], #{reason => <<"Starting mining, pending block not available yet">>}};
        {error, not_mining} ->
            {404, [], #{reason => <<"Not mining, no pending block">>}}
    end.

get_block(Fun, Req) ->
    get_block(Fun, Req, message_pack, true).

get_block(Fun, Req, DefaultEncoding) ->
    get_block(Fun, Req, DefaultEncoding, true).

get_block(Fun, Req, DefaultEncoding, AddHash) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            case read_tx_encoding_param(Req, DefaultEncoding) of
                {error, Err} ->
                    Err;
                {ok, TxEncoding} ->
                    DataSchema =
                        case TxEncoding of
                            message_pack ->
                                <<"BlockWithMsgPackTxs">>;
                            json ->
                                <<"BlockWithJSONTxs">>
                        end,
                    Resp0 = aehttp_logic:cleanup_genesis(
                        aehttp_api_parser:encode_client_readable_block(Block, TxEncoding)),
                    % we add swagger's definition name to the object so the
                    % result could be validated against the schema
                    Resp1 = Resp0#{data_schema => DataSchema},
                    Resp =
                        case AddHash of
                            true ->
                                {ok, Hash} = aec_blocks:hash_internal_representation(Block),
                                Resp1#{hash => aec_base58c:encode(block_hash, Hash)};
                            false ->
                                Resp1
                        end,
                    lager:debug("Resp = ~p", [pp(Resp)]),
                    {200, [], Resp}
              end;
        {_Code, _, _Reason} = Err ->
            Err
    end.

-spec safe_binary_to_atom(binary()) -> {ok, atom()} | {error, non_existing}.
safe_binary_to_atom(Binary) when is_binary(Binary) ->
    try binary_to_existing_atom(Binary, utf8) of
        Atom -> {ok, Atom}
    catch
        error:badarg -> {error, non_existing}
    end.
