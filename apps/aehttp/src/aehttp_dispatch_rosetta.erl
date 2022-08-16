-module(aehttp_dispatch_rosetta).

-export([forbidden/2]).
-export([handle_request/3]).

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
                <<"call_methods">> => [<<"TODO">>],
                <<"balance_exemptions">> => [],
                <<"mempool_coins">> => false}},
    {200, [], Resp};
handle_request_(networkStatus, _, _Context) ->
    try
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
                         #{<<"peer_id">> => aeapi:format(peer_pubkey, aec_peer:id(Peer)),
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
        Class:Rsn:Stacktrace ->
            lager:error(">>> Error determining networkStatus: ~p, ~p~n~p~n",
                        [Class, Rsn, Stacktrace]),
            ErrResp = rosetta_error_response(?ROSETTA_ERR_NW_STATUS_ERR),
            {200, [], ErrResp}
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
                Pubkey = aeapi:id_value(Id),
                %% Request might specify a block. If absent use top block
                {Block, Account} =
                    retrieve_block_and_account_from_partial_block_identifier(Pubkey, Req),
                Balance = aec_accounts:balance(Account),
                Resp =
                    #{<<"balances">> => [amount(Balance)],
                      <<"block_identifier">> => format_block_identifier(Block)},
                {200, [], Resp};
            _ ->
                throw(invalid_pubkey)
        end
    catch
        invalid_network ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
        block_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        chain_too_short ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_CHAIN_TOO_SHORT)};
        invalid_pubkey ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_TX_INVALID_ACCOUNT)}
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
        Resp = BlockFmt#{<<"metadata">> => #{}},
        {200, [], Resp}
    catch
        invalid_network ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
        block_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        chain_too_short ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_CHAIN_TOO_SHORT)};
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
            {200, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
        tx_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_TX_NOT_FOUND)}
    end;
handle_request_(call, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionCombine, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionDerive, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionHash, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionMetadata, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionParse, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionPayloads, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionPreprocess, _, _Context) ->
    {501, [], #{}};
handle_request_(constructionSubmit, _, _Context) ->
    {501, [], #{}};
handle_request_(eventsBlocks, _, _Context) ->
    {501, [], #{}};
handle_request_(mempool,
                #{'NetworkRequest' :=
                      #{<<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>}}},
                _Context) ->
    {ok, SignedTxList} = aec_tx_pool:peek(infinity),
    SignedTxHashList =
        [#{<<"hash">> => aeapi:format(tx_hash, aetx_sign:hash(X))} || X <- SignedTxList],
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
    [aetx:type_to_swagger_name(X)
     || X
            <- [spend_tx,
                oracle_register_tx,
                oracle_extend_tx,
                oracle_query_tx,
                oracle_response_tx,
                name_preclaim_tx,
                name_claim_tx,
                name_transfer_tx,
                name_update_tx,
                name_revoke_tx,
                contract_create_tx,
                contract_call_tx,
                ga_attach_tx,
                ga_meta_tx,
                channel_create_tx,
                channel_deposit_tx,
                channel_withdraw_tx,
                channel_force_progress_tx,
                channel_close_mutual_tx,
                channel_close_solo_tx,
                channel_slash_tx,
                channel_settle_tx,
                paying_for_tx]].

rosetta_errors() ->
    [rosetta_error_response(X)
     || X
            <- [?ROSETTA_ERR_NW_STATUS_ERR,
                ?ROSETTA_ERR_INVALID_NETWORK,
                ?ROSETTA_ERR_BLOCK_NOT_FOUND,
                ?ROSETTA_ERR_CHAIN_TOO_SHORT,
                ?ROSETTA_ERR_TX_NOT_FOUND]].

%% Format all the Txs in a KeyBlock
-spec format_block(aec_blocks:block()) -> #{}.
format_block(Block) ->
    {ok, PrevBlock} = aeapi:prev_key_block(Block),
    #{<<"block_identifier">> => format_block_identifier(Block),
      <<"parent_block_identifier">> => format_block_identifier(PrevBlock),
      <<"timestamp">> => aeapi:block_time_in_msecs(Block),
      <<"transactions">> => format_keyblock_txs(Block)}.

format_block_identifier(undefined) ->
    #{<<"index">> => 0, <<"hash">> => <<>>};
format_block_identifier(Block) ->
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(Block)),
    #{<<"index">> => aeapi:block_height(Block),
      <<"hash">> => aeapi:format(key_block_hash, Hash)}.

format_keyblock_txs(Block) ->
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(Block)),
    format_txs(aeapi:key_block_txs(Block), Hash).

%% Format a single Tx in a block. We need to dry run all the Txs in the microblock
%% up to and including the one we want to know about. dry run works on dummy signed
%% Txs so the tx_hash included in the results is not the same as the one requested.
%% Solve this by pre-filtering, then patching in the correct hash values afterwards.
format_tx(SignedTx, MicroBlock) ->
    {ok, PrevBlock} = aeapi:prev_block(MicroBlock),
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(PrevBlock)),
    BlockTxs = aeapi:micro_block_txs(MicroBlock),
    NeededTxs = keep_tx_until(BlockTxs, aetx_sign:hash(SignedTx)),
    Ops = format_txs(NeededTxs, Hash),
    lists:last(Ops).

keep_tx_until(Txs, TxHash) ->
    keep_tx_until(Txs, TxHash, []).

keep_tx_until([], _UntilTxHash, _Acc) ->
    [];
keep_tx_until([SignedTx | Txs], UntilTxHash, Acc) ->
    TxHash = aetx_sign:hash(SignedTx),
    if TxHash == UntilTxHash ->
            lists:reverse([SignedTx | Acc]);
        true ->
            keep_tx_until(Txs, UntilTxHash, [SignedTx | Acc])
    end.

format_txs(Txs, Hash) ->
    DryTxs = [{tx, aetx_sign:tx(Tx)} || Tx <- Txs],
    case aec_dry_run:dry_run(Hash, [], DryTxs, [tx_events]) of
        {ok, {Results, _Events}} ->
            lager:debug("dry_run Result: ~p", [Results]),
            TxHashes = [aetx_sign:hash(Tx) || Tx <- Txs],
            Res = lists:zip(TxHashes, Results),
            tx_spend_operations(Res);
        {error, Reason} ->
            lager:debug("dry_run_error: ~p", [Reason]),
            throw({dry_run_err, Reason})
    end.

tx_spend_operations(Results) ->
    lists:map(fun({TxHash, Result}) ->
                 {Res, _} = tx_spend_ops(Result),
                 #{<<"transaction_identifier">> => #{<<"hash">> => aeapi:format(tx_hash, TxHash)},
                   <<"operations">> => lists:reverse(Res)}
              end,
              Results).

tx_spend_ops({_Type, {ok, Events}}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
tx_spend_ops({_Type, {ok, Events, CallObj}}) ->
    Call = aect_call:serialize_for_client(CallObj),
    #{<<"return_type">> := ReturnType} = Call,
    case ReturnType of
        <<"ok">> ->
            lists:foldl(fun tx_spend_op/2, {[], 0}, Events);
        <<"error">> ->
            %% Just take the fees
            lists:foldl(fun tx_spend_op/2, {[], 0}, Events)
    end.

tx_spend_op({{internal_call_tx, Key},
             #{type := Type,
               info := Tx}},
            {Acc, Ix}) ->
    {CB, TxI} = aetx:specialize_callback(Tx),
    TxS = CB:for_client(TxI),
    SwaggerType = aetx:type_to_swagger_name(Type),
    #{<<"sender_id">> := From,
      <<"recipient_id">> := To,
      <<"amount">> := Amount} =
        TxS,
    FromOp = spend_tx_op(Ix, SwaggerType, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, SwaggerType, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{spend, {SenderPubkey, RecipientId, Amount}},
             #{type := Type}},
            {Acc, Ix}) ->
    From = aeapi:format(account_pubkey, SenderPubkey),
    To = aeapi:format_id(RecipientId),
    SwaggerType = aetx:type_to_swagger_name(Type),
    FromOp = spend_tx_op(Ix, SwaggerType, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, SwaggerType, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{delta, {Pubkey, Amount}}, #{info := Info}},
            {Acc, Ix}) ->
    From = aeapi:format(account_pubkey, Pubkey),
    DeltaOp = spend_tx_op(Ix, Info, From, Amount),
    {[DeltaOp | Acc], Ix + 1};
tx_spend_op({{channel, _Pubkey}, #{}}, {Acc, Ix}) ->
    {Acc, Ix}.

spend_tx_op(Index, Type, Address, Amount) ->
    #{<<"operation_identifier">> => #{<<"index">> => Index},
      <<"type">> => Type,
      <<"status">> => <<"SUCCESS">>,
      <<"account">> => #{<<"address">> => Address},
      <<"amount">> => amount(Amount)}.

amount(Amount) ->
    #{<<"value">> => integer_to_binary(Amount),
      <<"currency">> => #{<<"symbol">> => <<"aettos">>, <<"decimals">> => 18}}.

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
    <<"Invalid account format">>.

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
    {ok, BlockByHeight} = aeapi:key_block_by_height(BlockHeight),
    BlockByHeight;
retrieve_block(undefined, BlockHash) ->
    {ok, BlockByHash} = aeapi:key_block_by_hash(BlockHash),
    BlockByHash;
retrieve_block(BlockHeight, BlockHash) ->
    {ok, BlockByHeight} = aeapi:key_block_by_height(BlockHeight),
    {ok, BlockByHash} = aeapi:key_block_by_hash(BlockHash),
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
            {ok, Block} = aeapi:key_block_by_height(Index),
            Block;
        #{<<"hash">> := Hash} ->
            case aeapi:key_block_by_hash(Hash) of
                error ->
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

%% For rosetta we need to use the state trees from the Keyblock after
%% the height the Tx is included in
retrieve_block_and_account_from_partial_block_identifier(PubKey, Req) ->
    case Req of
        #{<<"block_identifier">> := #{<<"index">> := Index}} ->
            {ok, Block} = aeapi:key_block_by_height(Index),
            {value, Account} = aec_chain:get_account_at_height(PubKey, Index + 1),
            {Block, Account};
        #{<<"block_identifier">> := #{<<"hash">> := Hash}} ->
            case aeapi:key_block_by_hash(Hash) of
                error ->
                    throw(block_not_found);
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
