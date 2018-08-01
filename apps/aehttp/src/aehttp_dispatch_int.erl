-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ parse_filter_param/2
                        , get_block/2
                        , get_block/3
                        , get_block_from_chain/1
                        ]).

-spec handle_request(
        OperationID :: atom(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request('PostSpendTx', #{'SpendTx' := SpendTxObj}, _Context) ->
    #{<<"recipient_id">>     := EncodedRecipientPubkey,
      <<"amount">>           := Amount,
      <<"fee">>              := Fee,
      <<"payload">>          := Payload} = SpendTxObj,
    TTL = maps:get(<<"ttl">>, SpendTxObj, 0),
    case aehttp_int_tx_logic:spend(EncodedRecipientPubkey, Amount, Fee, TTL, Payload) of
        {ok, STx} ->
            {200, [], #{<<"tx_hash">> => aec_base58c:encode(tx_hash, aetx_sign:hash(STx))}};
        {error, invalid_key} ->
            {404, [], #{reason => <<"Invalid key">>}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"Account not found">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleRegisterTx', #{'OracleRegisterTx' := OracleRegisterTxObj}, _Context) ->
    #{<<"query_format">>    := QueryFormat,
      <<"response_format">> := ResponseFormat,
      <<"query_fee">>       := QueryFee,
      <<"oracle_ttl">>      := OracleTTL,
      <<"fee">>             := Fee} = OracleRegisterTxObj,
    TTL = maps:get(<<"ttl">>, OracleRegisterTxObj, 0),
    TTLType = binary_to_existing_atom(maps:get(<<"type">>, OracleTTL), utf8),
    TTLValue = maps:get(<<"value">>, OracleTTL),
    case aehttp_int_tx_logic:oracle_register(QueryFormat, ResponseFormat,
                                             QueryFee, Fee, TTLType, TTLValue, TTL) of
        {ok, Tx} ->
            {Pubkey, TxHash} = aehttp_int_tx_logic:sender_and_hash(Tx),
            {200, [], #{oracle_id => aec_base58c:encode(oracle_pubkey, Pubkey),
                        tx_hash => aec_base58c:encode(tx_hash, TxHash)}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"Account not found">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleExtendTx', #{'OracleExtendTx' := OracleExtendTxObj}, _Context) ->
    #{<<"oracle_ttl">> := OracleTTL,
      <<"fee">>        := Fee} = OracleExtendTxObj,
    TTL = maps:get(<<"ttl">>, OracleExtendTxObj, 0),
    TTLType = delta,
    TTLValue = maps:get(<<"value">>, OracleTTL),
    case aehttp_int_tx_logic:oracle_extend(Fee, TTLType, TTLValue, TTL) of
        {ok, Tx} ->
            {Pubkey, TxHash} = aehttp_int_tx_logic:sender_and_hash(Tx),
            {200, [], #{oracle_id => aec_base58c:encode(oracle_pubkey, Pubkey),
                        tx_hash => aec_base58c:encode(tx_hash, TxHash)}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"Account not found">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleQueryTx', #{'OracleQueryTx' := OracleQueryTxObj}, _Context) ->
    #{<<"oracle_pubkey">> := EncodedOraclePubkey,
      <<"query">>         := Query,
      <<"query_fee">>     := QueryFee,
      <<"query_ttl">>     := QueryTTL,
      <<"response_ttl">>  :=
          #{<<"type">>    := <<"delta">>,
            <<"value">>   := ResponseTTLValue},
      <<"fee">>           := Fee} = OracleQueryTxObj,
    TTL = maps:get(<<"ttl">>, OracleQueryTxObj, 0),
    QueryTTLType = binary_to_existing_atom(maps:get(<<"type">>, QueryTTL), utf8),
    QueryTTLValue= maps:get(<<"value">>, QueryTTL),
    case aehttp_int_tx_logic:oracle_query(EncodedOraclePubkey, Query, QueryFee, QueryTTLType,
             QueryTTLValue, ResponseTTLValue, Fee, TTL) of
        {ok, Tx, QId} ->
            {_, TxHash} = aehttp_int_tx_logic:sender_and_hash(Tx),
            {200, [], #{query_id => aec_base58c:encode(oracle_query_id, QId),
                        tx_hash => aec_base58c:encode(tx_hash, TxHash)}};
        {error, invalid_key} ->
            {404, [], #{reason => <<"Invalid key">>}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"Account not found">>}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('PostOracleResponseTx', #{'OracleResponseTx' := OracleResponseTxObj}, _Context) ->
    #{<<"query_id">> := EncodedQueryId,
      <<"response">> := Response,
      <<"fee">>      := Fee} = OracleResponseTxObj,
    TTL = maps:get(<<"ttl">>, OracleResponseTxObj, 0),
    case aec_base58c:safe_decode(oracle_query_id, EncodedQueryId) of
        {ok, DecodedQueryId} ->
            case aehttp_int_tx_logic:oracle_response(DecodedQueryId, Response,
                                                     Fee, TTL) of
                {ok, Tx} ->
                    {_, TxHash} = aehttp_int_tx_logic:sender_and_hash(Tx),
                    {200, [], #{query_id => EncodedQueryId,
                                tx_hash => aec_base58c:encode(tx_hash, TxHash)}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}};
                {error, key_not_found} ->
                    {404, [], #{reason => <<"Keys not configured">>}}
            end;
        {error, _} ->
            {404, [], #{reason => <<"Invalid Query Id">>}}
    end;

handle_request('GetActiveRegisteredOracles', Req, _Context) ->
    try
        From = case maps:get(from, Req) of
                   undefined -> '$first';
                   X1        -> {ok, OracleId} = aec_base58c:safe_decode(oracle_pubkey, X1),
                                OracleId
               end,
        Max  = case maps:get(max, Req) of
                   undefined -> 20;
                   X2        -> X2
               end,
        {ok, Oracles} = aehttp_int_tx_logic:get_oracles(From, Max),
        {200, [], aehttp_api_parser:encode(oracle_list, Oracles)}
    catch _:_ ->
        {400, [], #{reason => <<"Invalid Oracle Id">>}}
    end;

handle_request('GetOracleQuestions', Req, _Context) ->
    try
        EncodedOId =  maps:get(oracle_pub_key, Req),
        {ok, OracleId} = aec_base58c:safe_decode(oracle_pubkey, EncodedOId),
        From = case maps:get(from, Req) of
                   undefined -> '$first';
                   X1        -> {ok, QueryId} = aec_base58c:safe_decode(oracle_query_id, X1),
                                QueryId
               end,
        Max  = case maps:get(max, Req) of
                   undefined -> 20;
                   X2        -> X2
               end,
        {ok, Queries} = aehttp_int_tx_logic:get_oracle_questions(OracleId, From, Max),
        {200, [], aehttp_api_parser:encode(oracle_queries_list, Queries)}
    catch _:_ ->
        {400, [], #{reason => <<"Invalid parameters">>}}
    end;


handle_request('PostNamePreclaimTx', #{'NamePreclaimTx' := NamePreclaimTxObj}, _Context) ->
    #{<<"commitment_id">> := CommitmentId,
      <<"fee">>           := Fee} = NamePreclaimTxObj,
    TTL = maps:get(<<"ttl">>, NamePreclaimTxObj, 0),
     case aec_base58c:safe_decode({id_hash, [commitment]}, CommitmentId) of
        {ok, DecodedCommitment} ->
            case aehttp_int_tx_logic:name_preclaim(DecodedCommitment, Fee, TTL) of
                {ok, _Tx} ->
                    {200, [], #{commitment_id => CommitmentId}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}};
                {error, key_not_found} ->
                    {404, [], #{reason => <<"Keys not configured">>}}
            end;
        {error, _Reason} ->
            {400, [], #{reason => <<"Invalid commitment hash">>}}
    end;

handle_request('PostNameClaimTx', #{'NameClaimTx' := NameClaimTxObj}, _Context) ->
    #{<<"name">>      := Name,
      <<"name_salt">> := NameSalt,
      <<"fee">>       := Fee} = NameClaimTxObj,
    TTL = maps:get(<<"ttl">>, NameClaimTxObj, 0),
    case aehttp_int_tx_logic:name_claim(Name, NameSalt, Fee, TTL) of
        {ok, _Tx, NameHash} ->
            {200, [], #{name_id => aec_base58c:encode(name, NameHash)}};
        {error, account_not_found} ->
            {404, [], #{reason => <<"Account not found">>}};
        {error, key_not_found} ->
            {400, [], #{reason => <<"Keys not configured">>}};
          {error, Reason} ->
              ReasonBin = atom_to_binary(Reason, utf8),
              {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('PostNameUpdateTx', #{'NameUpdateTx' := NameUpdateTxObj}, _Context) ->
    #{<<"name_id">>     := NameId,
      <<"name_ttl">>    := NameTTL,
      <<"pointers">>    := Pointers,
      <<"client_ttl">>  := ClientTTL,
      <<"fee">>         := Fee} = NameUpdateTxObj,
    TTL = maps:get(<<"ttl">>, NameUpdateTxObj, 0),
    case aec_base58c:safe_decode({id_hash, [name]}, NameId) of
        {ok, DecodedNameHash} ->
            case aehttp_helpers:decode_pointers(Pointers) of
                {ok, Pointers1} ->
                    case aehttp_int_tx_logic:name_update(DecodedNameHash, NameTTL, Pointers1, ClientTTL, Fee, TTL) of
                        {ok, _Tx} ->
                            {200, [], #{name_id => NameId}};
                        {error, account_not_found} ->
                            {404, [], #{reason => <<"Account not found">>}};
                        {error, key_not_found} ->
                            {400, [], #{reason => <<"Keys not configured">>}}
                    end;
                {error, _Reason} ->
                    {400, [], #{reason => <<"Invalid pointers">>}}
            end;
        {error, _Reason} ->
            {400, [], #{reason => <<"Invalid name hash">>}}
    end;

handle_request('PostNameTransferTx', #{'NameTransferTx' := NameTransferTxObj}, _Context) ->
    #{<<"name_id">>          := NameId,
      <<"recipient_id">>     := RecipientId,
      <<"fee">>              := Fee} = NameTransferTxObj,
    TTL = maps:get(<<"ttl">>, NameTransferTxObj, 0),
    case {aec_base58c:safe_decode({id_hash, [name]}, NameId),
          aec_base58c:safe_decode({id_hash, [account_pubkey]}, RecipientId)} of
        {{ok, DecodedName}, {ok, DecodedRecipient}} ->
            case aehttp_int_tx_logic:name_transfer(DecodedName,
                                                   DecodedRecipient,
                                                   Fee, TTL) of
                {ok, _Tx} ->
                    {200, [], #{name_id => NameId}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}};
                {error, key_not_found} ->
                    {400, [], #{reason => <<"Keys not configured">>}}
            end;
        {{error, _Reason}, _} ->
            {400, [], #{reason => <<"Invalid name hash">>}};
        {_, {error, _Reason}} ->
            {400, [], #{reason => <<"Invalid recipient pubkey">>}}
    end;

handle_request('PostNameRevokeTx', #{'NameRevokeTx' := NameRevokeTxObj}, _Context) ->
    #{<<"name_id">> := NameId,
      <<"fee">>     := Fee} = NameRevokeTxObj,
    TTL = maps:get(<<"ttl">>, NameRevokeTxObj, 0),
    case aec_base58c:safe_decode({id_hash, [name]}, NameId) of
        {ok, DecodedName} ->
            case aehttp_int_tx_logic:name_revoke(DecodedName, Fee, TTL) of
                {ok, _Tx} ->
                    {200, [], #{name_id => NameId}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}};
                {error, key_not_found} ->
                    {400, [], #{reason => <<"Keys not configured">>}}
            end;
        {error, _Reason} ->
            {400, [], #{reason => <<"Invalid name hash">>}}
    end;

handle_request('GetPubKey', _, _Context) ->
    case aehttp_logic:miner_key() of
        {ok, Pubkey} ->
            {200, [], #{pub_key => aec_base58c:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request('GetTxsListFromBlockRangeByHeight', Req, _Context) ->
    HeightFrom = maps:get('from', Req),
    HeightTo = maps:get('to', Req),
    get_block_range(
        fun() ->
            aehttp_logic:get_block_range_by_height(HeightFrom, HeightTo)
        end, Req);

handle_request('GetTxsListFromBlockRangeByHash', Req, _Context) ->
    case {aec_base58c:safe_decode(block_hash, maps:get('from', Req)),
          aec_base58c:safe_decode(block_hash, maps:get('to', Req))} of
        {{ok, HashFrom}, {ok, HashTo}} ->
            get_block_range(
                fun() ->
                    aehttp_logic:get_block_range_by_hash(HashFrom, HashTo)
                end, Req);
        _ ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request('GetPeers', _Req, _Context) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"debug">>],
                                    aehttp, enable_debug_endpoints, false) of
        true ->
            Peers = aehttp_logic:connected_peers(all),
            InboundPeers = aehttp_logic:connected_peers(inbound),
            OutboundPeers = aehttp_logic:connected_peers(outbound),
            Blocked = aehttp_logic:blocked_peers(),

            {200, [], #{peers => lists:map(fun aec_peers:encode_peer_address/1, Peers),
                        inbound => lists:map(fun aec_peers:encode_peer_address/1, InboundPeers),
                        outbound => lists:map(fun aec_peers:encode_peer_address/1, OutboundPeers),
                        blocked => lists:map(fun aec_peers:encode_peer_address/1, Blocked)}};
        false ->
            {403, [], #{reason => <<"Call not enabled">>}}
    end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

%% Internals
filter_transaction_list(Req, TxList) ->
    case {parse_filter_param(tx_types, Req),
          parse_filter_param(exclude_tx_types, Req)} of
        {{error, unknown_type} = Err, _} ->
            Err;
        {_, {error, unknown_type} = Err} ->
            Err;
        {{ok, KeepTxTypes}, {ok, DropTxTypes}} ->
            Filtered =
                lists:filter(
                    fun(SignedTx) ->
                        Tx = aetx_sign:tx(SignedTx),
                        TxType = aetx:tx_type(Tx),
                        Drop = lists:member(TxType, DropTxTypes),
                        Keep = KeepTxTypes =:= []
                            orelse lists:member(TxType, KeepTxTypes),
                        Keep andalso not Drop
                    end,
                    TxList),
            {ok, Filtered}
    end.

get_block_range(GetFun, Req) when is_function(GetFun, 0) ->
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
                fun(_Block, {error, _} = Err) ->
                    Err;
                    (Block, Accum) ->
                    case filter_transaction_list(Req, aec_blocks:txs(Block)) of
                        {error, unknown_type} = Err ->
                            Err;
                        {ok, FilteredTxs} ->
                            H = aec_blocks:to_header(Block),
                            lists:map(
                                fun(Tx) ->
                                    aetx_sign:serialize_for_client(H, Tx)
                                end,
                                FilteredTxs) ++ Accum
                    end
                end,
                [],
                Blocks),
            case Txs of
                {error, unknown_type} ->
                    {400, [], #{reason => <<"Unknown transaction type">>}};
                _ ->
                    {200, [], #{transactions => Txs}}
            end
    end.

