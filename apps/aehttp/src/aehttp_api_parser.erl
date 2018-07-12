-module(aehttp_api_parser).

-export([encode/2,
         encode/3,
         encode_client_readable_key_block/2,
         encode_client_readable_block/2,
         decode/2]).

-include_lib("apps/aecore/include/blocks.hrl").

-define(HEADER_OBJ, [{<<"prev_hash">>, block_hash},
                     {<<"state_hash">>, block_state_hash},
                     {<<"pow">>, {fun aec_headers:serialize_pow_evidence/1,
                                    fun(Pow) ->
                                        {ok, aec_headers:deserialize_pow_evidence(Pow)}
                                    end}},
                     {<<"key_hash">>, block_hash},
                     {<<"txs_hash">>, block_tx_hash},
                     {<<"signature">>, signature},
                     {<<"miner">>, {fun(Val) -> aec_base58c:encode(account_pubkey, Val) end,
                                    fun decode_miner/1}},
                     {<<"beneficiary">>, {fun(Val) -> aec_base58c:encode(account_pubkey, Val) end,
                                          fun decode_beneficiary/1}}]).

-define(OBJECTS, #{header_map => ?HEADER_OBJ,
                   block_map => [ {<<"transactions">>, {list, tx}} | ?HEADER_OBJ],
                   block_hash =>  {fun(Block) ->
                                       {ok, Hash} = aec_blocks:hash_internal_representation(Block),
                                       aec_base58c:encode(block_hash, Hash)
                                   end,
                                   fun(_) -> {error, not_implemented} end},
                   block => {fun(Block) ->
                                BMap = aehttp_logic:cleanup_genesis(
                                            aec_blocks:serialize_to_map(Block)),
                                encode(block_map, BMap)
                            end,
                            fun(BMap) ->
                                FullBMap = aehttp_logic:add_missing_to_genesis_block(BMap),
                                case decode(block_map, FullBMap) of
                                    {ok, Decoded} ->
                                        {ok, _Block} = aec_blocks:deserialize_from_map(Decoded);
                                    Err -> Err
                                end
                            end},
                   header => {fun(Block) ->
                                case aec_blocks:type(Block) of
                                    key ->
                                        BMap0 = aec_blocks:serialize_to_map(Block),
                                        BMap = aehttp_logic:cleanup_genesis(BMap0),
                                        encode(header_map, BMap);
                                    micro ->
                                        {ok, HMap} = aec_headers:serialize_to_map(aec_blocks:to_header(Block)),
                                        encode(header_map, HMap#{<<"signature">> => aec_blocks:signature(Block)})
                                end
                            end,
                            fun(HMap) ->
                                FullHMap = aehttp_logic:add_missing_to_genesis_block(HMap),
                                case decode(header_map, FullHMap) of
                                    {ok, Decoded} ->
                                        {ok, aec_headers:deserialize_from_map(Decoded)};
                                    Err -> Err
                                end
                            end},
                   client_key_block => {fun(Block, TxEncoding) ->
                                                Resp = aehttp_logic:cleanup_genesis(
                                                  encode_block_for_client(Block, TxEncoding)),
                                                Resp#{
                                                  data_schema => encode(tx_encoding, TxEncoding),
                                                  hash => encode(block_hash, Block)}
                                        end,
                                        fun(_) -> {error, not_implemented} end},
                   client_block => {fun encode_block_for_client/2,
                                    fun(_) -> {error, not_implemented} end},
                   tx_encoding => {fun(message_pack) -> <<"BlockWithMsgPackTxs">>;
                                      (json) -> <<"BlockWithJSONTxs">>
                                   end,
                                   fun(_) -> {error, not_implemented} end},
                   tx => {fun(Tx) ->
                              #{<<"tx">> => aec_base58c:encode(transaction,
                                  aetx_sign:serialize_to_binary(Tx))}
                          end,
                          fun Decode(#{<<"tx">> := EncodedTx}) ->
                              Decode(EncodedTx);
                              Decode(EncodedTx) when is_binary(EncodedTx) ->
                              case aec_base58c:safe_decode(transaction, EncodedTx) of
                                  {error, _} = Err ->  Err;
                                  {ok, DecodedTx} ->
                                      try {ok, aetx_sign:deserialize_from_binary(DecodedTx)}
                                      catch _:_ -> {error, broken_tx}
                                      end
                              end
                          end},
                   tx_list => {list, tx},
                   account_balance => [{<<"pub_key">>, account_pubkey}],
                   account_balances => {list, account_balance},
                   node_version => [{<<"genesis_hash">>, block_hash}],

                   oracle => [{<<"address">>, oracle_pubkey}],
                   oracle_list => {list, oracle},

                   oracle_query => [{<<"query_id">>, oracle_query_id}],
                   oracle_queries_list => {list, oracle_query}

                  }).

-spec encode(atom(), term() | list()) -> map() | list().
encode(ObjectType, Data) ->
    encode(ObjectType, Data, []).

encode_client_readable_key_block(Block, TxEncoding) ->
    encode(client_key_block, Block, TxEncoding).

encode_client_readable_block(Block, TxEncoding) ->
    encode(client_block, Block, TxEncoding).

encode(ObjectType, Data, Params) ->
    Rule = parsing_rules(ObjectType),
    EncodeFun = encode_fun(Rule),
    EncodeFun(Rule, Data, Params).

-spec decode(atom(), map() | list()) -> {ok, term()} | {error, map()}.
decode(ObjectType, Data) ->
    Rule = parsing_rules(ObjectType),
    DecodeFun = decode_fun(Rule),
    DecodeFun(Rule, Data).

encode_object(Rules, Object, Params) when is_list(Rules) ->
    lists:foldl(
        fun({Key, _} = Rule, Accum) ->
            case maps:find(Key, Accum) of
                {ok, Val} ->
                    EncodeFun = encode_fun(Rule),
                    EncodedVal = EncodeFun(Rule, Val, Params),
                    maps:put(Key, EncodedVal, Accum);
                error ->
                    Accum
            end
        end,
        Object,
        Rules).

decode_object(Rules, Object) ->
    decode_object(Rules, Object, []).

decode_object([], Object, []) ->
    {ok, Object};
decode_object([], _Object, Errors) ->
    {error, maps:from_list(Errors)};
decode_object([{Key, _} = Rule | T], Object, Errors) ->
    case maps:find(Key, Object) of
        error -> decode_object(T, Object, Errors);
        {ok, EncodedVal} ->
            DecodeFun = decode_fun(Rule),
            case DecodeFun(Rule, EncodedVal) of
                {ok, Val} ->
                    decode_object(T, maps:put(Key, Val, Object), Errors);
                {error, Reason} ->
                    decode_object(T, Object, [{Key, Reason} | Errors])
            end
    end.

encode_value({EncodeFun, _}, Val, _) when is_function(EncodeFun, 1) ->
    EncodeFun(Val);
encode_value({_, {EncodeFun, _}}, Val, _) when is_function(EncodeFun, 1) ->
    EncodeFun(Val);
encode_value({EncodeFun, _}, Val, Params) when is_function(EncodeFun, 2) ->
    EncodeFun(Val, Params);
encode_value({_, {EncodeFun, _}}, Val, Params) when is_function(EncodeFun, 2) ->
    EncodeFun(Val, Params);
encode_value({_, HashType}, Val, _) when is_atom(HashType) ->
    aec_base58c:encode(HashType, Val).

decode_value({_, DecodeFun}, EncodedVal) when is_function(DecodeFun, 1) ->
    DecodeFun(EncodedVal);
decode_value({_, {_, DecodeFun}}, EncodedVal) when is_function(DecodeFun, 1) ->
    DecodeFun(EncodedVal);
decode_value({_, HashType}, Val) when is_atom(HashType) ->
    aec_base58c:safe_decode(HashType, Val).

encode_list({_, {list, Type}}, Elems, Params) when is_list(Elems) ->
    encode_list({list, Type}, Elems, Params);
encode_list({list, Type}, Elems, Params) when is_list(Elems) ->
    lists:map(
        fun(Element) ->
            encode(Type, Element, Params)
        end,
        Elems).

decode_list({_, {list, Type}}, Elems) when is_list(Elems) ->
    decode_list({list, Type}, Elems);
decode_list({list, Type}, Elems) when is_list(Elems) ->
    {Succeeded, Failed} =
        %% preserve order!
        lists:foldr(
            fun(El, {Oks, Errs}) ->
                case decode(Type, El) of
                    {ok, Decoded} -> {[Decoded | Oks], Errs};
                    {error, ErrMsg} -> {Oks, [{El, ErrMsg} | Errs]}
                end
            end,
            {[], []},
            Elems),
    case Failed =/= [] of
        true -> {error, Failed};
        false -> {ok, Succeeded}
    end.

parsing_rules(ObjectType) ->
    maps:get(ObjectType, ?OBJECTS).

encode_fun(Rule) ->
    case rule_type(Rule) of
        list -> fun encode_list/3;
        value -> fun encode_value/3;
        object -> fun encode_object/3
    end.

decode_fun(Rule) ->
    case rule_type(Rule) of
        list -> fun decode_list/2;
        value -> fun decode_value/2;
        object -> fun decode_object/2
    end.

rule_type({_, {list, _Type}}) -> list;
rule_type({list, _Type}) -> list;
rule_type({_, _}) -> value;
rule_type(L) when is_list(L) -> object.

encode_block_for_client(Block, Encoding) ->
    Header = aec_blocks:to_header(Block),
    EncodedHeader = encode(header, Block),
    Txs =
        lists:map(
            fun(Tx) ->
                aetx_sign:serialize_for_client(Encoding, Header, Tx)
            end,
            aec_blocks:txs(Block)),
    maps:put(<<"transactions">>, Txs, EncodedHeader).

decode_miner(Miner) ->
    case aec_base58c:safe_decode(account_pubkey, Miner) of
        {error, _} = Err -> Err;
        {ok, X} when is_binary(X) ->
            case byte_size(X) of
                ?MINER_PUB_BYTES -> {ok, X};
                Size -> {error, {bad_miner_size, Size}}
            end
    end.

decode_beneficiary(Beneficiary) ->
    case aec_base58c:safe_decode(account_pubkey, Beneficiary) of
        {error, _} = Err -> Err;
        {ok, X} when is_binary(X) ->
            case byte_size(X) of
                ?BENEFICIARY_PUB_BYTES -> {ok, X};
                Size -> {error, {bad_beneficiary_size, Size}}
            end
    end.
