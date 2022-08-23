-module(aehttp_dispatch_rosetta).

-export([forbidden/2]).
-export([handle_request/3]).
-export([convert_bootstrap_accounts/2]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [when_stable/1]).

-include_lib("aecontract/include/hard_forks.hrl").

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
                <<"timestamp_start_index">> => 1,
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
        Resp = #{<<"block">> => BlockFmt},
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
    [<<"Spend.fee">>,
     <<"Spend.amount">>,
     <<"Oracle.fee">>,
     <<"Oracle.queryfee">>,
     <<"Name.lock">>,
     <<"Name.fee">>,
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
     <<"PayingFor.fee">>,
     <<"Chain.fee">>,
     <<"Chain.refund">>,
     <<"Chain.reward">>].

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
    #{<<"index">> => 0, <<"hash">> => <<"0">>};
format_block_identifier(Block) ->
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(Block)),
    #{<<"index">> => aeapi:block_height(Block),
      <<"hash">> => aeapi:format(key_block_hash, Hash)}.

format_keyblock_txs(Block) ->
    Header = aec_blocks:to_header(Block),
    {ok, Hash} = aec_headers:hash_header(Header),
    ForkTxs = format_fork_txs(aec_blocks:height(Block)),
    RewardTxs = format_reward_txs(Block),
    BlockTxs = format_txs(aeapi:key_block_txs(Block), Hash),
    tx_spend_operations(ForkTxs ++ RewardTxs ++ BlockTxs).

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
    SpendOps = tx_spend_operations(Ops),
    lists:last(SpendOps).

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
            lists:zip(TxHashes, Results);
        {error, Reason} ->
            lager:debug("dry_run_error: ~p", [Reason]),
            throw({dry_run_err, Reason})
    end.

format_reward_txs(Block) ->
    %% Block rewards.
    %% In this block the reward for the beneficiary 180 blocks earlier will be paid
    %% We don't store the amount on chain, so need to re-calculate
    Delay = aec_governance:beneficiary_reward_delay(),
    {ok, TopBlock} = aeapi:top_key_block(),
    TopHeight = aec_blocks:height(TopBlock),
    Height = aec_blocks:height(Block),
    if Height >= Delay, Height < TopHeight ->
           %% For Rosetta the reward Txs need to be in the block before the Beneficiary
           %% account has their updated balance. No rewards yet at the top
           {ok, NextBlock} = aeapi:key_block_by_height(Height + 1),
           Node = aec_chain_state:wrap_block(NextBlock),
           Trees = aec_chain_state:grant_fees(Node, aec_trees:new(), Delay, false, nil),
           Accounts =
               aeu_mtrees:to_list(
                   aec_trees:accounts(Trees)),
           %% Supply empty tx hash. Could invent one??
           [{<<"">>,
            {reward,
             lists:map(fun({K, V}) ->
                          Acct = aec_accounts:deserialize(K, V),
                          {reward, {aec_accounts:pubkey(Acct), aec_accounts:balance(Acct)}}
                       end,
                       Accounts)}}];
       true ->
           []
    end.

%% Inject balance change operations at each of our historical hard forks
%% The balance changes need to be in the block before the fork for Rosetta
format_fork_txs(Height) ->
    Forks =  maps:to_list(aec_hard_forks:protocols()),
    case lists:keyfind(Height + 1, 2, Forks) of
        false ->
            [];
        {?MINERVA_PROTOCOL_VSN, _} ->
            Deltas = aec_fork_block_settings:minerva_accounts(),
            Ops = fork_ops(Deltas),
            [{<<"">>, {fork, Ops}}];
        {?FORTUNA_PROTOCOL_VSN, _} ->
            Deltas = aaec_fork_block_settings:fortuna_accounts(),
            Ops = fork_ops(Deltas),
            [{<<"">>, {fork, Ops}}];
        {?LIMA_PROTOCOL_VSN, _} ->
            Deltas = aec_fork_block_settings:lima_accounts(),
            Ops = fork_ops(Deltas, []),
            ExtraDeltas = aec_fork_block_settings:lima_extra_accounts(),
            Ops1 = fork_ops(ExtraDeltas, Ops),
            Contracts = aec_fork_block_settings:lima_contracts(),
            Ops2 = fork_contract_ops(Contracts, Ops1),
            [{<<"">>, {fork, lists:reverse(Ops2)}}];
        {?IRIS_PROTOCOL_VSN, _} ->
            [];
        _ ->
            []
    end.

fork_ops(Deltas) ->
    lists:map(fun({K, V}) ->
                          {fork, {K, V}}
                       end,
                       Deltas).

fork_ops(Deltas, Acc0) ->
    lists:foldl(fun({PubKey, Amount}, Acc) ->
        [{fork, {PubKey, Amount}} | Acc]
    end, Acc0, Deltas).

fork_contract_ops(Contracts, Acc0) ->
    lists:foldl(fun(#{pubkey := PubKey, amount := Amount}, Acc) ->
        [{fork, {PubKey, Amount}} | Acc]
    end, Acc0, Contracts).

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
    end;
tx_spend_ops({reward, RewardOps}) ->
    lists:foldl(fun tx_spend_op/2, {[], 0}, RewardOps).

tx_spend_op({{internal_call_tx, _Key}, #{info := Tx}}, {Acc, Ix}) ->
    {CB, TxI} = aetx:specialize_callback(Tx),
    TxS = CB:for_client(TxI),
    #{<<"sender_id">> := From,
      <<"recipient_id">> := To,
      <<"amount">> := Amount} =
        TxS,
    FromOp = spend_tx_op(Ix, <<"Spend.amount">>, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, <<"Spend.amount">>, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{spend, {SenderPubkey, RecipientPubkey, Amount}}, #{type := _Type}},
            {Acc, Ix}) ->
    From = aeapi:format(account_pubkey, SenderPubkey),
    To = aeapi:format(account_pubkey, RecipientPubkey),
    FromOp = spend_tx_op(Ix, <<"Spend.amount">>, From, -Amount),
    ToOp = spend_tx_op(Ix + 1, <<"Spend.amount">>, To, Amount),
    {[ToOp, FromOp | Acc], Ix + 2};
tx_spend_op({{delta, {Pubkey, Amount}}, #{info := Info}}, {Acc, Ix}) ->
    From = aeapi:format(account_pubkey, Pubkey),
    DeltaOp = spend_tx_op(Ix, Info, From, Amount),
    {[DeltaOp | Acc], Ix + 1};
tx_spend_op({reward, {Pubkey, Amount}}, {Acc, Ix}) ->
    To = aeapi:format(account_pubkey, Pubkey),
    Op = spend_tx_op(Ix, <<"Chain.reward">>, To, Amount),
    {[Op | Acc], Ix + 1};
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
      <<"currency">> => #{<<"symbol">> => <<"AE">>, <<"decimals">> => 18}}.

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

%% For rosetta we need the balance after the transactions in the block have been applied
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



