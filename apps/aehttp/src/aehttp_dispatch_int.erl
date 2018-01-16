-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: map()}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, SenderPubkey, Nonce} ->
            #{<<"recipient_pubkey">> := EncodedRecipientPubkey,
              <<"amount">>           := Amount,
              <<"fee">>              := Fee} = SpendTxObj,
            case aec_base58c:safe_decode(
                   account_pubkey, EncodedRecipientPubkey) of
                {ok, DecodedRecipientPubkey} ->
                    {ok, SpendTx} =
                        aec_spend_tx:new(
                          #{sender    => SenderPubkey,
                            recipient => DecodedRecipientPubkey,
                            amount    => Amount,
                            fee       => Fee,
                            nonce     => Nonce}),
                    sign_and_push_to_mempool(SpendTx),
                    {200, [], #{}};
                {error, __} ->
                    {404, [], #{reason => <<"Invalid key">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleRegisterTx', #{'OracleRegisterTx' := OracleRegisterTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"query_format">>    := QueryFormat,
              <<"response_format">> := ResponseFormat,
              <<"query_fee">>       := QueryFee,
              <<"fee">>             := Fee,
              <<"ttl">>             := TTL} = OracleRegisterTxObj,
            TTLType = binary_to_existing_atom(maps:get(<<"type">>, TTL), utf8),
            TTLValue = maps:get(<<"value">>, TTL),
            {ok, OracleRegisterTx} =
                aeo_register_tx:new(
                  #{account       => Pubkey,
                    nonce         => Nonce,
                    query_spec    => QueryFormat,
                    response_spec => ResponseFormat,
                    query_fee     => QueryFee,
                    ttl           => {TTLType, TTLValue},
                    fee           => Fee}),
            sign_and_push_to_mempool(OracleRegisterTx),
            {200, [], #{oracle_id => aec_base58c:encode(oracle_pubkey, Pubkey)}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleQueryTx', #{'OracleQueryTx' := OracleQueryTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"oracle_pubkey">> := EncodedOraclePubkey,
              <<"query">>         := Query,
              <<"query_fee">>     := QueryFee,
              <<"query_ttl">>     := QueryTTL,
              <<"response_ttl">>  :=
                  #{<<"type">>    := <<"delta">>,
                    <<"value">>   := ResponseTTLValue},
              <<"fee">>           := Fee} = OracleQueryTxObj,
            QueryTTLType = binary_to_existing_atom(maps:get(<<"type">>, QueryTTL), utf8),
            QueryTTLValue= maps:get(<<"value">>, QueryTTL),
            case aec_base58c:safe_decode(oracle_pubkey, EncodedOraclePubkey) of
                {ok, DecodedOraclePubkey} ->
                    {ok, OracleQueryTx} =
                        aeo_query_tx:new(
                          #{sender       => Pubkey,
                            nonce        => Nonce,
                            oracle       => DecodedOraclePubkey,
                            query        => Query,
                            query_fee    => QueryFee,
                            query_ttl    => {QueryTTLType, QueryTTLValue},
                            response_ttl => {delta, ResponseTTLValue},
                            fee          => Fee}),
                    sign_and_push_to_mempool(OracleQueryTx),
                    QId = aeo_interaction:id(Pubkey, Nonce, DecodedOraclePubkey),
                    {200, [], #{query_id => aec_base58c:encode(oracle_interaction_id, QId)}};
                {error, _} ->
                    {404, [], #{reason => <<"Invalid key">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleResponseTx', #{'OracleResponseTx' := OracleResponseTxObj}, _Context) ->
    case get_local_pubkey_with_next_nonce() of
        {ok, Pubkey, Nonce} ->
            #{<<"interaction_id">> := EncodedInteractionId,
              <<"response">>       := Response,
              <<"fee">>            := Fee} = OracleResponseTxObj,
            case aec_base58c:safe_decode(oracle_interaction_id, EncodedInteractionId) of
                {ok, DecodedInteractionId} ->
                    {ok, OracleResponseTx} =
                        aeo_response_tx:new(
                          #{oracle         => Pubkey,
                            nonce          => Nonce,
                            interaction_id => DecodedInteractionId,
                            response       => Response,
                            fee            => Fee}),
                    sign_and_push_to_mempool(OracleResponseTx),
                    {200, [], #{query_id => EncodedInteractionId}};
                {error, _} ->
                    {404, [], #{reason => <<"Invalid interaction Id">>}}
            end;
        {error, account_not_found} ->
            {404, [], #{reason => <<"No funds in an account">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetActiveRegisteredOracles', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('GetOracleQuestions', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('PostOracleSubscribe', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('PostOracleUnsubscribe', _Req, _Context) ->
    %% TODO: Implement me
    {501, [], #{}};

handle_request('GetPubKey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            {200, [], #{pub_key => aec_base58c:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetBlockNumber', _Req, _Context) ->
    TopHeader = aec_conductor:top_header(),
    Height = aec_headers:height(TopHeader),
    {200, [], #{height => Height}};

handle_request('GetBlockByHeightInternal', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block(fun() -> aec_conductor:get_block_by_height(Height) end, Req);

handle_request('GetBlockByHashInternal', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aec_conductor:get_block_by_hash(Hash) end, Req)
    end;

handle_request('GetBlockGenesis', Req, _Context) ->
    get_block(fun aec_conductor:genesis_block/0, Req);

handle_request('GetBlockLatest', Req, _Context) ->
    get_block(
        fun() ->
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end, Req);

handle_request('GetBlockPending', Req, _Context) ->
    get_block(fun aec_conductor:get_block_candidate/0, Req);

handle_request('GetBlockTxsCountByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block_txs_count(fun() -> aec_conductor:get_block_by_hash(Hash) end)
    end;

handle_request('GetBlockTxsCountByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block_txs_count(fun() -> aec_conductor:get_block_by_height(Height) end);

handle_request('GetGenesisBlockTxsCount', _Req, _Context) ->
    get_block_txs_count(fun aec_conductor:genesis_block/0);

handle_request('GetLatestBlockTxsCount', _Req, _Context) ->
    get_block_txs_count(
        fun() ->
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end);

handle_request('GetPendingBlockTxsCount', _Req, _Context) ->
    get_block_txs_count(fun aec_conductor:get_block_candidate/0);

handle_request('GetTransactionFromBlockHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    Index = maps:get('tx_index', Req),
    get_block_tx_by_index(fun() -> aec_conductor:get_block_by_height(Height) end,
                          Index,
                          Req);

handle_request('GetTransactionFromBlockHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            Index = maps:get('tx_index', Req),
            get_block_tx_by_index(
                fun() -> aec_conductor:get_block_by_hash(Hash) end,
                Index,
                Req)
    end;

handle_request('GetTransactionFromBlockLatest', Req, _Context) ->
    Index = maps:get('tx_index', Req),
    get_block_tx_by_index(
        fun() -> 
            TopBlock = aec_conductor:top(),
            {ok, TopBlock}
        end,
        Index,
        Req);

handle_request('GetTxsListFromBlockRangeByHeight', Req, _Context) ->
    HeightFrom = maps:get('from', Req),
    HeightTo = maps:get('to', Req),
    get_block_range(
        fun() ->
            aec_conductor:get_block_range_by_height(HeightFrom, HeightTo)
        end, Req);

handle_request('GetTxsListFromBlockRangeByHash', Req, _Context) ->
    case {aec_base58c:safe_decode(block_hash, maps:get('from', Req)),
          aec_base58c:safe_decode(block_hash, maps:get('to', Req))} of
        {{ok, HashFrom}, {ok, HashTo}} ->
            get_block_range(
                fun() ->
                    aec_conductor:get_block_range_by_hash(HashFrom, HashTo)
                end, Req);
        _ ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;
          
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

%% Internals

get_local_pubkey_with_next_nonce() ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            lager:debug("SenderPubKey matches ours"),
            case aec_next_nonce:pick_for_account(Pubkey) of
                {ok, Nonce} ->
                    lager:debug("Nonce = ~p", [Nonce]),
                    {ok, Pubkey, Nonce};
                {error, account_not_found} = Error ->
                    lager:debug("Account not found"),
                    %% Account was not found in state trees
                    %% so effectively there have been no funds
                    %% ever granted to it.
                    Error
            end;
        {error, key_not_found} = Error ->
            Error
    end.

sign_and_push_to_mempool(Tx) ->
    lager:debug("Tx = ~p", [pp(Tx)]),
    {ok, SignedTx} = aec_keys:sign(Tx),
    ok = aec_tx_pool:push(SignedTx),
    lager:debug("pushed; peek() -> ~p",
                [pp(aec_tx_pool:peek(10))]).

get_block(Fun, Req) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            TxObjects = read_optional_bool_param('tx_objects', Req, false),
            %% swagger generated code expects the Resp to be proplist or map
            %% and always runs jsx:encode/1 on it - even if it is already
            %% encoded to a binary; that's why we use
            %% aec_blocks:serialize_to_map/1 instead of
            %% aec_blocks:serialize_for_network/1
            {SerializeFun, DataSchema} =
                case TxObjects of
                    true ->
                        {fun aec_blocks:serialize_client_readable/1, <<"BlockWithTxs">>};
                    false ->
                        {fun aec_blocks:serialize_to_map/1, <<"BlockWithTxsHashes">>}
                end,
            Resp0 = aehttp_dispatch_ext:cleanup_genesis(SerializeFun(Block)),
            % we add swagger's definition name to the object so the
            % result could be validated against the schema
            Resp = Resp0#{data_schema => DataSchema},
            lager:debug("Resp = ~p", [pp(Resp)]),
            {200, [], Resp};
        {_Code, _, _Reason} = Err ->
            Err
    end.

get_block_txs_count(Fun) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            {200, [], #{count => length(aec_blocks:txs(Block))}};
        {_Code, _, _Reason} = Err ->
            Err
    end.

get_block_tx_by_index(Fun, Index, Req) when is_function(Fun, 0) ->
    case get_block_from_chain(Fun) of
        {ok, Block} ->
            Txs = aec_blocks:txs(Block),
            case try {ok, lists:nth(Index, Txs)} catch _:_ -> not_found end of
                not_found ->
                    {404, [], #{reason => <<"Transaction not found">>}};
                {ok, Tx} ->
                    TxObjects = read_optional_bool_param('tx_objects', Req, false),
                    {SerializeFun, DataSchema} =
                        case TxObjects of
                            true ->
                                {fun aec_tx_sign:serialize_for_client/1,
                                 <<"SingleTxObject">>};
                            false ->
                                {fun(T) ->
                                     #{tx =>
                                       aec_base58c:encode(transaction,
                                                          aec_tx_sign:serialize_to_binary(T))}
                                 end,
                                 <<"SingleTxHash">>}
                        end,
                    {200, [], #{transaction => SerializeFun(Tx),
                                data_schema => DataSchema}}
            end;
        {_Code, _, _Reason} = Err ->
            Err
    end.

get_block_from_chain(Fun) when is_function(Fun, 0) ->
    case Fun() of
        {ok, _Block} = OK->
            OK;
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


-spec read_optional_bool_param(atom(), map(), boolean()) -> boolean().
read_optional_bool_param(Key, Req, Default) when Default =:= true
                                          orelse Default =:= false ->
    %% swagger does not take into consideration the 'default'
    %% if a query param is missing, swagger adds it to Req with a value of
    %% 'undefined'
    case maps:get(Key, Req) of
        undefined -> Default;
        Bool when is_boolean(Bool) -> Bool
    end.

get_block_range(GetFun, Req) when is_function(GetFun, 0) ->
    {SerializeFun, DataSchema} =
        case read_optional_bool_param('tx_objects', Req, false) of
            false ->
                {fun(Tx) ->
                    #{<<"tx">> => aec_base58c:encode(transaction,
                                                     aec_tx_sign:serialize_to_binary(Tx))}
                  end, <<"TxMsgPackHashes">>};
            true ->
                {fun aec_tx_sign:serialize_for_client/1, <<"TxObjects">>}
          end,
    case GetFun() of
        {error, invalid_range} ->
            {400, [], #{reason => <<"From's height is bigger than To's">>}};
        {error, range_too_big} ->
            {400, [], #{reason => <<"Range too big">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {ok, Blocks} ->
            Txs = lists:foldl(
                fun(Block, Accum) ->
                    BlockTxs = aec_blocks:txs(Block),
                    lists:map(SerializeFun, BlockTxs) ++ Accum
                end,
                [],
                Blocks),
            {200, [], #{transactions => Txs, data_schema => DataSchema}}
    end.

