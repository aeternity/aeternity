

-module(aehttp_dispatch_rosetta).

-export([forbidden/2]).
-export([handle_request/3]).
-export([convert_bootstrap_accounts/2]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [when_stable/1]).

-compile({parse_transform, lager_transform}).

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).

%% dry run limits

-define(TC(Expr, Msg),
        begin
            {Time, Res} = timer:tc(fun() -> Expr end),
            lager:debug("[~p] Msg = ~p", [Time, Msg]),
            Res
        end).
-define(ROSETTA_ERR_NW_STATUS_ERR, 1).
-define(ROSETTA_ERR_INVALID_NETWORK, 2).
-define(ROSETTA_ERR_BLOCK_NOT_FOUND, 3).
-define(ROSETTA_ERR_CHAIN_TOO_SHORT, 4).
-define(ROSETTA_ERR_TX_NOT_FOUND, 5).
-define(ROSETTA_ERR_TX_INVALID_ACCOUNT, 6).
-define(ROSETTA_ERR_TX_INVALID_TRANSACTION, 7).
-define(ROSETTA_ERR_INVALID_INPUT, 8).

-spec forbidden(Mod :: module(), OperationID :: atom()) -> boolean().
forbidden(_Mod, _OpId) ->
    false.

-spec handle_request(OperationID :: atom(), Req :: cowboy_req:req(), Context :: #{}) ->
                        {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.
handle_request(OperationID, Req, Context) ->
    run(queue(OperationID),
        fun() -> ?TC(handle_request_(OperationID, Req, Context), Req) end).

%% run(no_queue, F) -> F();
run(Queue, F) ->
    try
        when_stable(fun() -> aec_jobs_queues:run(Queue, F) end)
    catch
        error:{rejected, _} ->
            {503, [], #{reason => <<"Temporary overload">>}};
        error:timeout ->
            {503, [], #{reason => <<"Not yet started">>}};
        Class:Reason:Stacktrace ->
            lager:error("CRASH ~p ~p, ~p", [Class, Reason, Stacktrace]),
            {500, [], #{reason => <<"Internal server error">>}}
    end.

%% read transactions
%% Data API - Network
queue(networkList) ->
    ?READ_Q;
queue(networkOptions) ->
    ?READ_Q;
queue(networkStatus) ->
    ?READ_Q;
%% Data API - Account
queue(accountBalance) ->
    ?READ_Q;
queue(accountCoins) ->
    ?READ_Q;
%% Data API - Block
queue(block) ->
    ?READ_Q;
queue(blockTransaction) ->
    ?READ_Q;
%% Data API - Mempool
queue(mempool) ->
    ?READ_Q;
queue(mempoolTransaction) ->
    ?READ_Q;
%% Construction API - Construction
queue(constructionCombine) ->
    ?WRITE_Q;
queue(constructionDerive) ->
    ?WRITE_Q;
queue(constructionHash) ->
    ?WRITE_Q;
queue(constructionMetadata) ->
    ?WRITE_Q;
queue(constructionParse) ->
    ?WRITE_Q;
queue(constructionPayloads) ->
    ?WRITE_Q;
queue(constructionPreprocess) ->
    ?WRITE_Q;
queue(constructionSubmit) ->
    ?WRITE_Q;
%% Indexers
queue(call) ->
    ?READ_Q;
queue(eventsBlocks) ->
    ?READ_Q;
queue(searchTransactions) ->
    ?READ_Q;
queue(_) ->
    ?WRITE_Q.

handle_request_(networkList, _, _Context) ->
    Resp =
        #{<<"network_identifiers">> =>
              [#{<<"blockchain">> => aeapi:blockchain_name(),
                 <<"network">> => aeapi:network_id()}]},
    {200, [], Resp};
handle_request_(networkOptions, _, _Context) ->
    Resp =
        #{<<"version">> =>
              #{<<"rosetta_version">> => <<"1.4.10">>,
                <<"node_version">> => aeapi:node_version(),
                <<"metadata">> => #{<<"node_revision">> => aeapi:node_revision()}},
          <<"allow">> =>
              #{<<"operation_statuses">> =>
                    [#{<<"status">> => <<"SUCCESS">>, <<"successful">> => true},
                     #{<<"status">> => <<"FAILED">>, <<"successful">> => false}],
                <<"operation_types">> => ae_operation_types(),
                <<"errors">> => rosetta_errors(),
                <<"historical_balance_lookup">> => true,
                <<"timestamp_start_index">> => 1,
                <<"call_methods">> => [<<"TODO">>],
                <<"balance_exemptions">> => [],
                <<"mempool_coins">> => false}},
    {200, [], Resp};
handle_request_(networkStatus, #{'NetworkRequest' :=
                                #{<<"network_identifier">> :=
                                    #{<<"blockchain">> := <<"aeternity">>,
                                      <<"network">> := Network}}}, _Context) ->
    try
        case aec_governance:get_network_id() of
            Network ->
                ok;
            _ ->
                throw(invalid_network)
        end,
        {ok, TopBlock} = aeapi:top_key_block(),
        CurrentBlock =
            case aec_blocks:height(TopBlock) of
                0 ->
                    TopBlock;
                _ ->
                    {ok, Prev} = aeapi:prev_key_block(TopBlock),
                    Prev
            end,
        CurrentBlockIdentifier = format_block_identifier(CurrentBlock),
        CurrentBlockTimestamp = aec_blocks:time_in_msecs(CurrentBlock),
        GenesisBlockIdentifier = format_block_identifier(aec_chain:genesis_block()),
        {Syncing, SyncProgress, TargetHeight} = aeapi:sync_progress(),
        Synced =
            case {Syncing, SyncProgress} of
                {false, 100.0} ->
                    true;
                _ ->
                    false
            end,
        SyncStatus0 = #{<<"synced">> => Synced},
        SyncStatus =
            case Synced of
                true ->
                    SyncStatus0;
                false ->
                    SyncStatus0#{<<"target_index">> => TargetHeight}
            end,
        Peers = aeapi:connected_peers(),
        PeersFormatted =
            lists:map(fun(Peer) ->
                         #{<<"peer_id">> =>
                               aeapi:format(peer_pubkey, aec_peer:id(Peer)),
                           <<"metadata">> =>
                               #{<<"ip">> => aec_peer:ip(Peer), <<"port">> => aec_peer:port(Peer)}}
                      end,
                      Peers),
        Resp =
            #{<<"current_block_identifier">> => CurrentBlockIdentifier,
              <<"current_block_timestamp">> => CurrentBlockTimestamp,
              <<"genesis_block_identifier">> => GenesisBlockIdentifier,
              <<"sync_status">> => SyncStatus,
              <<"peers">> => PeersFormatted},
        {200, [], Resp}
    catch
        invalid_network ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
        Class:Rsn:Stacktrace ->
            lager:error(">>> Error determining networkStatus: ~p, ~p~n~p~n",
                        [Class, Rsn, Stacktrace]),
            ErrResp = rosetta_error_response(?ROSETTA_ERR_NW_STATUS_ERR),
            {500, [], ErrResp}
    end;
handle_request_(accountBalance,
                #{'AccountBalanceRequest' :=
                      #{<<"network_identifier">> :=
                            #{<<"blockchain">> := <<"aeternity">>, <<"network">> := Network},
                        <<"account_identifier">> := #{<<"address">> := Address}} =
                          Req},
                _Context) ->
    try
        case aec_governance:get_network_id() of
            Network ->
                ok;
            _ ->
                throw(invalid_network)
        end,
        AllowedTypes = [account_pubkey, contract_pubkey],
        case aeapi:create_id(Address, AllowedTypes) of
            {ok, Id} ->
                {ok, Pubkey} = aeapi:id_value(Id),
                %% Request might specify a block. If absent use top block
                case catch retrieve_block_and_account_from_partial_block_identifier(Pubkey, Req) of
                    {'EXIT', _} ->
                        throw(invalid_pubkey);
                    {Block, Account} ->
                        Balance = aec_accounts:balance(Account),
                        Resp =
                            #{<<"balances">> => [amount(Balance)],
                              <<"block_identifier">> => format_block_identifier(Block)},
                        {200, [], Resp}
                end;
            _ ->
                throw(invalid_pubkey)
        end
    catch
        invalid_network ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
        block_not_found ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        chain_too_short ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_CHAIN_TOO_SHORT)};
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)}
    end;
handle_request_(accountCoins, _, _Context) ->
    {501, [], #{}};
%% Data API - Block
handle_request_(block,
                #{'BlockRequest' :=
                      #{<<"network_identifier">> :=
                            #{<<"blockchain">> := <<"aeternity">>, <<"network">> := Network}} =
                          Req},
                _Context) ->
    try
        case aec_governance:get_network_id() of
            Network ->
                ok;
            _ ->
                throw(invalid_network)
        end,
        Block = retrieve_block_from_partial_block_identifier(Req),
        BlockFmt = format_block(Block),
        Resp = #{<<"block">> => BlockFmt},
        {200, [], Resp}
    catch
        invalid_network ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
        block_not_found ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        chain_too_short ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_CHAIN_TOO_SHORT)};
        {dry_run_err, Err} ->
            dry_run_err(Err)
    end;
handle_request_(blockTransaction,
                #{'BlockTransactionRequest' :=
                   #{<<"block_identifier">> := BlockIdentifier,
                     <<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>},
                     <<"transaction_identifier">> := #{<<"hash">> := TxHash}}},
                _Context) ->
    try
        %%
        %% Fetch block
        %%
        Block = retrieve_block(BlockIdentifier),
        %%
        %% Decode the transaction hash
        %%
        TxHashInternal =
            case aeapi:decode(TxHash) of
                {tx_hash, TxHash0} ->
                    TxHash0;
                _ ->
                    throw(tx_not_found)
            end,
        %%
        %% Fetch the transaction, ensuring it was in the specified block
        %%
        {SignedTx, Microblock} =
            case aec_db:find_tx_with_location(TxHashInternal) of
                none ->
                    throw(block_not_found);
                {mempool, _} ->
                    throw(block_not_found);
                {BlockHash, STx} ->
                    %% Tx was in this microblock. Check this microblock is in the
                    %% Requested keyblock
                    case aec_chain:get_block(BlockHash) of
                        {ok, MBlock} ->
                            KeyBlockHash = aec_blocks:prev_key_hash(MBlock),
                            Header = aec_blocks:to_header(Block),
                            {ok, RequestedBlockHash} = aec_headers:hash_header(Header),
                            if KeyBlockHash == RequestedBlockHash ->
                                   {STx, MBlock};
                               true ->
                                   throw(block_not_found)
                            end;
                        _ ->
                            throw(block_not_found)
                    end
            end,
        Resp = #{<<"transaction">> => format_tx(SignedTx, Microblock)},
        {200, [], Resp}
    catch
        block_not_found ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        tx_not_found ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_NOT_FOUND)}
    end;
handle_request_(call, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionDerive, #{'ConstructionDeriveRequest' := Req}, _Context) ->
    try
        {HexBytes, CurveType} =
            case Req of
                #{<<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>},
                  <<"public_key">> :=
                      #{<<"curve_type">> := CurveType0, <<"hex_bytes">> := HexBytes0}} ->
                    {HexBytes0, CurveType0};
                _ ->
                    throw(invalid_input)
            end,

        case CurveType of
            <<"edwards25519">> ->
                PKBytes = aeu_hex:hex_to_bin(HexBytes),
                Account = aeapi:format(account_pubkey, PKBytes),
                case aeapi:create_id(Account, [account_pubkey]) of
                    {ok, _} ->
                        ok;
                    _ ->
                        throw(invalid_pubkey)
                end,
                AccountIdentifier =
                    #{<<"address">> => Account,
                      <<"account_identifier">> => #{<<"address">> => Account}},
                {200, [], AccountIdentifier};
            _ ->
                throw(unsupported_curve_type)
        end
    catch
        unsupported_curve_type ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)};
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionPreprocess,
                #{'ConstructionPreprocessRequest' := Req},
                _Context) ->
    try
        {From, To} =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"operations">> := [From0, To0]} ->
                    {From0, To0};
                _ ->
                    throw(invalid_input)
            end,

        {FromAcc, _FromAmt} = parse_construction_op(From),
        %% Validate from address
        case aeapi:create_id(FromAcc, [account_pubkey]) of
            {ok, _} ->
                ok;
            _ ->
                throw(invalid_pubkey)
        end,

        {ToAcc, _ToAmt} = parse_construction_op(To),
        %% Validate to address
        case aeapi:create_id(ToAcc, [account_pubkey]) of
            {ok, _} ->
                ok;
            _ ->
                throw(invalid_pubkey)
        end,
        Resp = #{<<"options">> => #{<<"from">> => FromAcc}},

        {200, [], Resp}
    catch
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)};
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionMetadata,
                #{'ConstructionMetadataRequest' := Req},
                _Context) ->
    try
        From =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"options">> := #{<<"from">> := From0}} ->
                    From0;
                _ ->
                    throw(invalid_input)
            end,
        case aeapi:create_id(From, [account_pubkey]) of
            {ok, _} ->
                ok;
            _ ->
                throw(invalid_pubkey)
        end,
        SuggestedFee = integer_to_binary(20000 * aec_tx_pool:minimum_miner_gas_price()),
        {ok, NextNonce} = aeapi:next_nonce(From),
        Resp =
            #{<<"suggested_fee">> =>
                  [#{<<"value">> => SuggestedFee,
                     <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18}}],
              <<"metadata">> => #{<<"nonce">> => NextNonce, <<"fee">> => SuggestedFee}},
        {200, [], Resp}
    catch
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)};
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionPayloads,
                #{'ConstructionPayloadsRequest' := Req},
                _Context) ->
    %% Create and serialize a transaction to pass through to the next rosetta stage (/construction/parse)
    try
        {Nonce, Fee, Ops} =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"metadata">> := #{<<"nonce">> := Nonce0, <<"fee">> := Fee0},
                  <<"operations">> := Ops0} ->
                    {Nonce0, Fee0, Ops0};
                _ ->
                    throw(invalid_input)
            end,

        [From, To] = Ops,
        {FromAcc, _FromAmt} = parse_construction_op(From),
        %% Validate from address
        FromId =
            case aeapi:create_id(FromAcc, [account_pubkey]) of
                {ok, FId} ->
                    FId;
                _ ->
                    throw(invalid_pubkey)
            end,
        {ToAcc, ToAmt} = parse_construction_op(To),
        %% Validate to address
        ToId =
            case aeapi:create_id(ToAcc, [account_pubkey]) of
                {ok, TId} ->
                    TId;
                _ ->
                    throw(invalid_pubkey)
            end,
        TxArgs =
            #{sender_id => FromId,
              recipient_id => ToId,
              amount => ToAmt,
              nonce => Nonce,
              fee => binary_to_integer(Fee),
              payload => <<"">>},
        {ok, Tx} = aec_spend_tx:new(TxArgs),
        TxSer = aetx:serialize_to_binary(Tx),
        TxBin = aeapi:format(transaction, TxSer),
        UnsignedBin = aec_hash:hash(tx, TxSer),
        %% FIXME: check network id is same as provided in the request?
        BinForNetwork = aec_governance:add_network_id(UnsignedBin),

        %% Create the signing details for use later. hex_bytes is the binary
        %% that the client will sign, which includes the network-id
        Signer =
            #{<<"account_identifier">> => #{<<"address">> => FromAcc},
              <<"hex_bytes">> => list_to_binary(aeu_hex:bin_to_hex(BinForNetwork)),
              <<"signature_type">> => <<"ed25519">>},
        Resp = #{<<"payloads">> => [Signer], <<"unsigned_transaction">> => TxBin},
        {200, [], Resp}
    catch
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)};
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionParse, #{'ConstructionParseRequest' := Req}, _Context) ->
    try
        {Signed, Tx} =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"signed">> := Signed0,
                  <<"transaction">> := Tx0} ->
                    {Signed0, Tx0};
                _ ->
                    throw(invalid_input)
            end,

        {ok, TxBin} = aeapi:decode(transaction, Tx),
        AeTx =
            case Signed of
                true ->
                    Deser = aetx_sign:deserialize_from_binary(TxBin),
                    aetx_sign:tx(Deser);
                false ->
                    aetx:deserialize_from_binary(TxBin)
            end,
        {aec_spend_tx, SpendTx} = aetx:specialize_callback(AeTx),
        FromId = aec_spend_tx:sender_id(SpendTx),
        ToId = aec_spend_tx:recipient_id(SpendTx),
        Value = aec_spend_tx:amount(SpendTx),
        FromOp =
            #{<<"account">> => #{<<"address">> => aeapi:format_id(FromId)},
              <<"amount">> => amount(-Value),
              <<"operation_identifier">> => #{<<"index">> => 0},
              <<"type">> => <<"Spend.amount">>},
        ToOp =
            #{<<"account">> => #{<<"address">> => aeapi:format_id(ToId)},
              <<"amount">> => amount(Value),
              <<"operation_identifier">> => #{<<"index">> => 1},
              <<"type">> => <<"Spend.amount">>},
        Signers =
            case Signed of
                true ->
                    [#{<<"address">> => aeapi:format_id(FromId)}];
                false ->
                    []
            end,
        Resp = #{<<"operations">> => [FromOp, ToOp], <<"account_identifier_signers">> => Signers},
        {200, [], Resp}
    catch
        invalid_pubkey ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)};
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionCombine, #{'ConstructionCombineRequest' := Req}, _Context) ->
    %% The user signed the Tx and sent the signature to us in hex_bytes.
    %% Our job is just to add the provided signature to the Tx and re-serialise
    try
        {HexBytesSignature, UnsignedTx} =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"signatures">> :=
                      [#{<<"hex_bytes">> := HexBytesSignature0,
                         <<"public_key">> :=
                             #{<<"curve_type">> := <<"edwards25519">>,
                               <<"hex_bytes">> := _HexBytes},
                         <<"signature_type">> := <<"ed25519">>,
                         <<"signing_payload">> :=
                             #{<<"account_identifier">> := #{<<"address">> := _},
                               <<"address">> := _,
                               <<"hex_bytes">> := _Payload,
                               <<"signature_type">> := <<"ed25519">>}}],
                  <<"unsigned_transaction">> := UnsignedTx0} ->
                    {HexBytesSignature0, UnsignedTx0};
                _ ->
                    throw(invalid_input)
            end,

        SigBin = aeu_hex:hex_to_bin(HexBytesSignature),
        {ok, UnsignedBin} = aeapi:decode(transaction, UnsignedTx),
        AeTx = aetx:deserialize_from_binary(UnsignedBin),
        SignedTx = aetx_sign:new(AeTx, [SigBin]),
        TxSer = aetx_sign:serialize_to_binary(SignedTx),
        SignedTxBin = aeapi:format(transaction, TxSer),
        Resp = #{<<"signed_transaction">> => SignedTxBin},
        {200, [], Resp}
    catch
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionHash, #{'ConstructionHashRequest' := Req}, _Context) ->
    try
        SignedTxBin =
            case Req of
                #{<<"network_identifier">> :=
                      #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                  <<"signed_transaction">> := SignedTxBin0} ->
                    SignedTxBin0;
                _ ->
                    throw(invalid_input)
            end,
        {ok, SignedTx} = aeapi:decode(transaction, SignedTxBin),
        Deser = aetx_sign:deserialize_from_binary(SignedTx),
        Hash = aetx_sign:hash(Deser),
        Resp = #{<<"transaction_identifier">> =>
                    #{<<"hash">> => aeapi:format(tx_hash, Hash)}},
        {200, [], Resp}
    catch
        invalid_input ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)};
        _:_ ->
            {500, [], rosetta_error_response(?ROSETTA_ERR_INVALID_INPUT)}
    end;
handle_request_(constructionSubmit,
                #{'ConstructionSubmitRequest' :=
                      #{<<"network_identifier">> :=
                            #{<<"blockchain">> := <<"aeternity">>, <<"network">> := _Network},
                        <<"signed_transaction">> := SignedTxBin}},
                _Context) ->
    %% Woohoo. Finally get to post the Tx
    {ok, SignedTx} = aeapi:decode(transaction, SignedTxBin),
    Deser = aetx_sign:deserialize_from_binary(SignedTx),
    case aec_tx_pool:push(Deser) of
        ok ->
            Hash = aetx_sign:hash(Deser),
            Resp = #{<<"transaction_identifier">> =>
                        #{<<"hash">> => aeapi:format(tx_hash, Hash)}},
            {200, [], Resp};
        {error, E} ->
            lager:debug("Transaction ~p failed to be pushed to pool because: ~p", [SignedTx, E]),
            {500, [], rosetta_error_response(?ROSETTA_ERR_TX_NOT_FOUND)}
    end;
handle_request_(eventsBlocks, _, _Context) ->
    {501, [], #{}};
handle_request_(mempool,
                #{'NetworkRequest' :=
                      #{<<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>}}},
                _Context) ->
    {ok, SignedTxList} = aec_tx_pool:peek(infinity),
    SignedTxHashList =
        [#{<<"hash">> =>
               aeapi:format(tx_hash, aetx_sign:hash(X))}
         || X <- SignedTxList],
    Resp = #{<<"transaction_identifiers">> => SignedTxHashList},
    {200, [], Resp};
handle_request_(mempoolTransaction,
                #{'MempoolTransactionRequest' :=
                      #{<<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>},
                        <<"transaction_identifier">> := #{<<"hash">> := TxHash}}},
                _Context) ->
    %%
    %% Decode the transaction hash
    %%
    TxHashInternal =
        case aeapi:decode(TxHash) of
            {tx_hash, TxHash0} ->
                TxHash0;
            _ ->
                throw(tx_not_found)
        end,
    case aec_db:find_tx_location(TxHashInternal) of
        mempool ->
            ok;
        _ ->
            throw(tx_not_found)
    end,
    case aec_db:find_signed_tx(TxHashInternal) of
        {value, SignedTx} ->
            Tx = aetx_sign:tx(SignedTx),
            %% TODO:
            %% The way format_tx/2 is implemented, it requires a Block
            %% to format it for TxType of contract_create_tx or
            %% contract_call_tx. Perhaps we should exclude these from
            %% the /mempool API call response itself?
            case aetx:tx_type(Tx) of
                TxType when TxType == contract_create_tx; TxType == contract_call_tx ->
                    throw(tx_not_found);
                _ ->
                    Resp = #{<<"transaction">> => format_tx(SignedTx, undefined)},
                    {200, [], Resp}
            end;
        _ ->
            throw(tx_not_found)
    end;
handle_request_(searchTransactions, _, _Context) ->
    {501, [], #{}};
handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(">>> Got not implemented request to process: ~p~n",
                           [{OperationID, Req, Context}]),
    {501, [], #{}}.

ae_operation_types() ->
    [<<"Spend.amount">>,
     <<"Oracle.queryfee">>,
     <<"Oracle.refund">>,
     <<"Name.lock">>,
     <<"Name.refund">>,
     <<"Channel.amount">>,
     <<"Channel.fee">>,
     <<"Channel.withdraw">>,
     <<"Channel.settle">>,
     <<"Contract.amount">>,
     <<"Contract.gas">>,
     <<"Call.clone">>,
     <<"Call.create">>,
     <<"Call.amount">>,
     <<"GA.amount">>,
     <<"Chain.lock">>,
     <<"Chain.fee">>,
     <<"Chain.refund">>,
     <<"Chain.reward">>,
     <<"Chain.amount">>].

rosetta_errors() ->
    [rosetta_error_response(X)
     || X
            <- [?ROSETTA_ERR_NW_STATUS_ERR,
                ?ROSETTA_ERR_INVALID_NETWORK,
                ?ROSETTA_ERR_BLOCK_NOT_FOUND,
                ?ROSETTA_ERR_CHAIN_TOO_SHORT,
                ?ROSETTA_ERR_TX_NOT_FOUND,
                ?ROSETTA_ERR_TX_INVALID_TRANSACTION,
                ?ROSETTA_ERR_INVALID_INPUT]].

%% Format all the Txs in a KeyBlock
-spec format_block(aec_blocks:block()) -> #{}.
format_block(Block) ->
    {ok, PrevBlock} = aeapi:prev_key_block(Block),
    #{<<"block_identifier">> => format_block_identifier(Block),
      <<"parent_block_identifier">> => format_block_identifier(PrevBlock),
      <<"timestamp">> => aeapi:block_time_in_msecs(Block),
      <<"transactions">> => format_keyblock_txs(Block)}.

format_block_identifier(undefined) ->
    #{<<"index">> => 0, <<"hash">> => <<"0">>};
format_block_identifier(Block) ->
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(Block)),
    #{<<"index">> => aeapi:block_height(Block),
      <<"hash">> => aeapi:format(key_block_hash, Hash)}.

format_keyblock_txs(KeyBlock) ->
    aeapi:balance_change_events_in_block(KeyBlock).

%% FIXME: Re-instate events for Txs in the mempool
format_tx(_SignedTx, undefined) ->
    [];
format_tx(SignedTx, MicroBlock) ->
    aeapi:balance_change_events_in_tx(SignedTx, MicroBlock).

amount(Amount) ->
    #{<<"value">> => integer_to_binary(Amount),
      <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18}}.

parse_construction_op(#{<<"account">> := #{<<"address">> := Address},
                        <<"amount">> :=
                            #{<<"currency">> := #{<<"decimals">> := 18, <<"symbol">> := <<"AE">>},
                              <<"value">> := ValueBin},
                        <<"operation_identifier">> := #{<<"index">> := _Ix},
                        <<"type">> := <<"Spend.amount">>}) ->
    {Address, binary_to_integer(ValueBin)}.

dry_run_err(Err) when is_list(Err) ->
    dry_run_err(list_to_binary(Err));
dry_run_err(Err) ->
    {403, [], #{reason => <<"Bad request: ", Err/binary>>}}.

rosetta_error_response(ErrCode) ->
    rosetta_error_response(ErrCode, rosetta_err_retriable(ErrCode)).

rosetta_error_response(ErrCode, Retriable)
    when is_integer(ErrCode), is_boolean(Retriable) ->
    rosetta_error_response(ErrCode, Retriable, undefined).

rosetta_error_response(ErrCode, Retriable, _Details)
    when is_integer(ErrCode), is_boolean(Retriable) ->
    #{<<"code">> => ErrCode,
      <<"message">> => rosetta_err_msg(ErrCode),
      <<"retriable">> => Retriable}.

    % Remove this for now to help dialyzer pass CI. It knows Details can only be undefined..
    % case Details of
    %     undefined ->
    %         Err;
    %     #{} ->
    %         Err#{<<"details">> => Details}
    % end.

rosetta_err_msg(?ROSETTA_ERR_NW_STATUS_ERR) ->
    <<"Error determining networkStatus">>;
rosetta_err_msg(?ROSETTA_ERR_INVALID_NETWORK) ->
    <<"Invalid network specified">>;
rosetta_err_msg(?ROSETTA_ERR_BLOCK_NOT_FOUND) ->
    <<"Specified block not found">>;
rosetta_err_msg(?ROSETTA_ERR_TX_NOT_FOUND) ->
    <<"Specified transaction not found">>;
rosetta_err_msg(?ROSETTA_ERR_CHAIN_TOO_SHORT) ->
    <<"Chain too short">>;
rosetta_err_msg(?ROSETTA_ERR_TX_INVALID_ACCOUNT) ->
    <<"Invalid account format">>;
rosetta_err_msg(?ROSETTA_ERR_TX_INVALID_TRANSACTION) ->
    <<"Invalid transaction">>;
rosetta_err_msg(?ROSETTA_ERR_INVALID_INPUT) ->
    <<"Invalid input">>.

rosetta_err_retriable(?ROSETTA_ERR_NW_STATUS_ERR) ->
    true;
rosetta_err_retriable(?ROSETTA_ERR_BLOCK_NOT_FOUND) ->
    true;
rosetta_err_retriable(?ROSETTA_ERR_TX_NOT_FOUND) ->
    true;
rosetta_err_retriable(?ROSETTA_ERR_CHAIN_TOO_SHORT) ->
    true;
rosetta_err_retriable(?ROSETTA_ERR_TX_INVALID_ACCOUNT) ->
    false;
rosetta_err_retriable(_) ->
    false.

retrieve_block(BlockIdentifier) ->
    BlockHeight = maps:get(<<"index">>, BlockIdentifier, undefined),
    BlockHash = maps:get(<<"hash">>, BlockIdentifier, undefined),
    retrieve_block(BlockHeight, BlockHash).

retrieve_block(undefined, undefined) ->
    throw(block_not_found);
retrieve_block(BlockHeight, undefined) ->
    {ok, BlockByHeight} = aeapi:key_block_at_height(BlockHeight),
    BlockByHeight;
retrieve_block(undefined, BlockHash) ->
    {ok, BlockByHash} = aeapi:key_block_at_hash(BlockHash),
    BlockByHash;
retrieve_block(BlockHeight, BlockHash) ->
    {ok, BlockByHeight} = aeapi:key_block_at_height(BlockHeight),
    {ok, BlockByHash} = aeapi:key_block_at_hash(BlockHash),
    case BlockByHeight =:= BlockByHash of
        true ->
            BlockByHeight;
        false ->
            throw(block_not_found)
    end.

%% Rosetta calls a block identifier with optional index and hash params a PartialBlockIdentifier
retrieve_block_from_partial_block_identifier(Req) ->
    case maps:get(<<"block_identifier">>, Req) of
        #{<<"index">> := Index} ->
            {ok, Block} = aeapi:key_block_at_height(Index),
            Block;
        #{<<"hash">> := Hash} ->
            case aeapi:key_block_at_hash(Hash) of
                {error, block_not_found} ->
                    throw(block_not_found);
                {ok, Block} ->
                    Block
            end;
        _ ->
            case aeapi:top_key_block() of
                error ->
                    throw(block_not_found);
                {ok, Block} ->
                    {ok, PrevBlock} = aeapi:prev_key_block(Block),
                    PrevBlock
            end
    end.

%% For rosetta we need the balance after the transactions in the block have been applied.
%% It assumes everything is done in the single block that's being queried, which makes sense for
%% BTC and ETH, but not really for us with our microblocks.
retrieve_block_and_account_from_partial_block_identifier(PubKey, Req) ->
    case Req of
        #{<<"block_identifier">> := #{<<"index">> := Index}} ->
            {ok, Block} = aeapi:key_block_at_height(Index),
            {value, Account} = aec_chain:get_account_at_height(PubKey, Index + 1),
            {Block, Account};
        #{<<"block_identifier">> := #{<<"hash">> := Hash}} ->
            case aeapi:key_block_at_hash(Hash) of
                {error, Reason} ->
                    throw(Reason);
                {ok, Block} ->
                    Height =
                        aec_headers:height(
                            aec_blocks:to_header(Block)),
                    {value, Account} = aec_chain:get_account_at_height(PubKey, Height + 1),
                    {Block, Account}
            end;
        _ ->
            {ok, TopBlock} = aeapi:top_key_block(),
            {ok, TopHash} =
                aec_headers:hash_header(
                    aec_blocks:to_header(TopBlock)),
            {ok, Block} = aeapi:prev_key_block(TopBlock),
            {value, Account} = aec_chain:get_account_at_hash(PubKey, TopHash),
            {Block, Account}
    end.

convert_bootstrap_accounts(InFile, OutFile) ->
    {ok, InData} = file:read_file(InFile),
    InJson = jsx:decode(InData),
    %% rosetta-cli barfs on bootstrap acounts with zero balances
    ExcludingZeroBalances = lists:filter(fun({_Acct, Balance}) -> Balance /= 0 end, InJson),
    OutJson =
        lists:map(fun({Acct, Balance}) ->
                     #{<<"account_identifier">> => #{<<"address">> => Acct},
                       <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18},
                       <<"value">> => integer_to_binary(Balance)}
                  end,
                  ExcludingZeroBalances),
    JSON =
        jsx:prettify(
            jsx:encode(OutJson)),
    ok = file:write_file(OutFile, JSON).

%% Eth traces:
%% POST /network/status {"network_identifier":{"blockchain":"Ethereum","network":"Mainnet"}}
%% RESP: {"current_block_identifier":
%%         {"index":576637,
%%          "hash":"0x948f06389af2bc01cf2be00ec0081f6d3fbb7b3c5c1a6e69207748cfcea9d907"},
%%        "current_block_timestamp":1448146083000,
%%        "genesis_block_identifier":{"index":0,
%%                                    "hash":"0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"},
%%        "sync_status":{"current_index":576638,"target_index":13773036},
%%        "peers":[{"peer_id":"cff5f364e684a0b4354e863f11744898d468d0311382cfd0340f060534c62f15",
%%        "metadata":{"caps":["eth/65","eth/66","snap/1"],
%%                    "enode":"enode://958eb6a893741a0456dbd34cac7f9c7cf30891b6c1cdcbc11762f88431c84e97ab3dd02bf1333e3037a088c518abaf60815b382c595606b76775f21d031c5860@107.148.133.79:30303",
%%                    "enr":"enr:-J24QC-dLXo-c95hianVNl2DnqZPuEzY1uktGjHjrONAaha4fzeOWf_gw_xnxHUQmXEB-4PtrqNFXX8zA8Wc_EalBFwtg2V0aMfGhLcVB32AgmlkgnY0gmlwhGuUhU-Jc2VjcDI1NmsxoQKVjraok3QaBFbb00ysf5x88wiRtsHNy8EXYviEMchOl4RzbmFwwIN0Y3CCdl-DdWRwgnZf",
%%                    "name":"Geth/ppcoin/v1.10.9-unstable-8dbf261f-20210824/linux-amd64/go1.16.4",
%%                    "protocols":{"eth":{"difficulty":3.6206751599115524e+22,
%%                                        "head":"0xfeb27336ca7923f8fab3bd617fcb6e75841538f71c1bcfc267d7838489d9e13d",
%%                                        "version":66},"snap":{"version":1}}}}]}
%% POST /network/options {"network_identifier":{"blockchain":"Ethereum","network":"Mainnet"}}
%% RESP: {"version":{"rosetta_version":"1.4.10","node_version":"1.9.24","middleware_version":"0.0.4"},
%%        "allow":{"operation_statuses":[{"status":"SUCCESS","successful":true},{"status":"FAILURE","successful":false}],
%%                 "operation_types":["MINER_REWARD","UNCLE_REWARD","FEE","CALL","CREATE","CREATE2","SELFDESTRUCT","CALLCODE","DELEGATECALL","STATICCALL","DESTRUCT"],
%%                 "errors":[{"code":0,"message":"Endpoint not implemented","retriable":false},
%%                           {"code":1,"message":"Endpoint unavailable offline","retriable":false},
%%                           {"code":2,"message":"geth error","retriable":false},
%%                           {"code":3,"message":"unable to decompress public key","retriable":false},
%%                           {"code":4,"message":"Unable to parse intent","retriable":false},
%%                           {"code":5,"message":"Unable to parse intermediate result","retriable":false},
%%                           {"code":6,"message":"Signature invalid","retriable":false},
%%                           {"code":7,"message":"Unable to broadcast transaction","retriable":false},
%%                           {"code":8,"message":"Call parameters invalid","retriable":false},
%%                           {"code":9,"message":"Call output marshal failed","retriable":false},
%%                           {"code":10,"message":"Call method invalid","retriable":false},
%%                           {"code":11,"message":"Block orphaned","retriable":true},
%%                           {"code":12,"message":"Invalid address","retriable":false},
%%                           {"code":13,"message":"geth not ready","retriable":true},
%%                           {"code":14,"message":"invalid input","retriable":false}],
%%                   "historical_balance_lookup":true,
%%                   "call_methods":["eth_getBlockByNumber","eth_getTransactionReceipt","eth_call","eth_estimateGas"],
%%                   "balance_exemptions":null,"mempool_coins":false}}
%% POST /network/list {}
%% RESP: {"network_identifiers":[{"blockchain":"Ethereum","network":"Mainnet"}]}
%% POST /block {"network_identifier":{"blockchain":"Ethereum","network":"Mainnet"},"block_identifier":{"index":0}}
%% RESP: {"block":{"block_identifier":
%%                   {"index":0,"hash":"0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"},
%%                 "parent_block_identifier":{"index":0,"hash":"0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"},
%%                 "timestamp":0,
%%                 "transactions":[{"transaction_identifier":{"hash":"0xd4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"},
%%                                  "operations":[{"operation_identifier":{"index":0},
%%                                                 "type":"MINER_REWARD","status":"SUCCESS",
%%                                                 "account":{"address":"0x0000000000000000000000000000000000000000"},
%%                                                 "amount":{"value":"5000000000000000000",
%%                                                 "currency":{"symbol":"ETH","decimals":18}}}]}]}}
