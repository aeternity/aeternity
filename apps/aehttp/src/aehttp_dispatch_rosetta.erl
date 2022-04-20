-module(aehttp_dispatch_rosetta).

-export([forbidden/2]).
-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ 
                          when_stable/1
                        ]).

-compile({parse_transform, lager_transform}).

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).
-define(NO_Q, no_queue).

%% dry run limits
-define(DEFAULT_GAS_LIMIT, 6000000).
-define(DEFAULT_CALL_REQ_GAS_LIMIT, 1000000).

-define(TC(Expr, Msg), begin {Time, Res} = timer:tc(fun() -> Expr end), lager:debug("[~p] Msg = ~p", [Time, Msg]), Res end).

-define(ROSETTA_ERR_NW_STATUS_ERR,   1).
-define(ROSETTA_ERR_INVALID_NETWORK, 2).
-define(ROSETTA_ERR_BLOCK_NOT_FOUND, 3).
-define(ROSETTA_ERR_CHAIN_TOO_SHORT, 4).
-define(ROSETTA_ERR_TX_NOT_FOUND,    5).

-spec forbidden( Mod :: module(), OperationID :: atom() ) -> boolean().
forbidden(_Mod, _OpId) -> false.

-spec handle_request(
        OperationID :: atom(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.
handle_request(OperationID, Req, Context) ->
    run(queue(OperationID),
        fun() ->
                ?TC(handle_request_(OperationID, Req, Context), Req)
        end).

%% run(no_queue, F) -> F();
run(Queue, F) ->
    try when_stable(
          fun() ->
                  aec_jobs_queues:run(Queue, F)
          end)
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
queue('networkList')            -> ?READ_Q;
queue('networkOptions')         -> ?READ_Q;
queue('networkStatus')          -> ?READ_Q;

%% Data API - Account
queue('accountBalance')         -> ?READ_Q;
queue('accountCoins')           -> ?READ_Q;

%% Data API - Block
queue('block')                  -> ?READ_Q;
queue('blockTransaction')       -> ?READ_Q;

%% Data API - Mempool
queue('mempool')                -> ?READ_Q;
queue('mempoolTransaction')     -> ?READ_Q;

%% Construction API - Construction
queue('constructionCombine')    -> ?WRITE_Q;
queue('constructionDerive')     -> ?WRITE_Q;
queue('constructionHash')       -> ?WRITE_Q;
queue('constructionMetadata')   -> ?WRITE_Q;
queue('constructionParse')      -> ?WRITE_Q;
queue('constructionPayloads')   -> ?WRITE_Q;
queue('constructionPreprocess') -> ?WRITE_Q;
queue('constructionSubmit')     -> ?WRITE_Q;

%% Indexers
queue('call')                   -> ?READ_Q;
queue('eventsBlocks')           -> ?READ_Q;
queue('searchTransactions')     -> ?READ_Q;

queue(_)                        -> ?WRITE_Q.

handle_request_('networkList', _, _Context) ->
    Resp = #{<<"network_identifiers">> => 
                 [#{<<"blockchain">> => aeapi:blockchain_name(),
                    <<"network">> => aeapi:network_id()
                   }
                 ]
            },
    {200, [], Resp};
handle_request_('networkOptions', _, _Context) ->
    Resp = #{<<"version">> => #{<<"rosetta_version">> => <<"1.4.10">>,
                                <<"node_version">> => aeapi:node_version(),
                                <<"metadata">> => #{<<"node_revision">> => aeapi:node_revision()}},
             <<"allow">> => #{
                              <<"operation_statuses">> => [#{<<"status">> => <<"SUCCESS">>,
                                                             <<"successful">> => true},
                                                           #{<<"status">> => <<"FAILED">>,
                                                             <<"successful">> => false}],
                              <<"operation_types">> => ae_operation_types(),
                              <<"errors">> => rosetta_errors(),
                              <<"historical_balance_lookup">> => true,
                              <<"call_methods">> => [<<"TODO">>],
                              <<"balance_exemptions">> => [],
                              <<"mempool_coins">> => false
                             }
            },
    {200, [], Resp};
handle_request_('networkStatus', _, _Context) ->
    try
        {ok, TopBlock} = aeapi:top_key_block(),
        CurrentBlock = case aec_blocks:height(TopBlock) of
                           0 ->
                               TopBlock;
                           _ ->
                               aeapi:prev_block(TopBlock)
                       end,
        CurrentBlockIdentifier = format_block_identifier(CurrentBlock),
        CurrentBlockTimestamp = aec_blocks:time_in_msecs(CurrentBlock),
        GenesisBlockIdentifier = format_block_identifier(aec_chain:genesis_block()),
        {Syncing, SyncProgress, TargetHeight} = aeapi:sync_progress(),
        Synced = case {Syncing, SyncProgress} of
                     {false, 100.0} -> true;
                     _ -> false
                 end,
        SyncStatus0 = #{<<"synced">> => Synced},
        SyncStatus = case Synced of
                         true ->
                             SyncStatus0;
                         false ->
                             SyncStatus0#{<<"target_index">> => TargetHeight}
                     end,
        Peers = aeapi:connected_peers(),
        PeersFormatted = lists:map(
                           fun(Peer) ->
                                   #{<<"peer_id">> => aeser_api_encoder:encode(peer_pubkey, aec_peer:id(Peer)),
                                     <<"metadata">> => #{<<"ip">> => aec_peer:ip(Peer),
                                                         <<"port">> => aec_peer:port(Peer)}}
                           end, Peers),
        Resp = #{<<"current_block_identifier">> => CurrentBlockIdentifier,
                 <<"current_block_timestamp">> => CurrentBlockTimestamp,
                 <<"genesis_block_identifier">> => GenesisBlockIdentifier,
                 <<"sync_status">> => SyncStatus,
                 <<"peers">> => PeersFormatted},
        {200, [], Resp}
    catch Class:Rsn:Stacktrace ->
            lager:error(
              ">>> Error determining networkStatus: ~p, ~p~n~p~n", [Class, Rsn, Stacktrace]),
            ErrResp = rosetta_error_response(?ROSETTA_ERR_NW_STATUS_ERR),
            {200, [], ErrResp}
    end;

handle_request_('accountBalance', _, _Context) ->
    {501, [], #{}};
handle_request_('accountCoins', _, _Context) ->
    {501, [], #{}};

%% Data API - Block
handle_request_('block', #{'BlockRequest' := 
                               #{<<"network_identifier">> := 
                                     #{<<"blockchain">> := <<"aeternity">>,
                                       <<"network">> := Network}} = Req}, _Context) ->
    try
        case aec_governance:get_network_id() of
            Network ->
                ok;
            _ ->
                throw(invalid_network)
        end,
        Block = case maps:get(<<"block_identifier">>, Req) of
                    #{<<"index">> := Index} ->
                        {ok, Block0} = aeapi:key_block_by_height(Index),
                        Block0;
                    #{<<"hash">> := Hash} ->
                        case aeapi:key_block_by_hash(Hash) of
                            error ->
                                throw(block_not_found);
                            {ok, Block0} ->
                                Block0
                        end;
                    _ ->
                        aeapi:prev_block(aeapi:current_block())
                end,
        BlockFmt = format_block(Block),
        Resp = BlockFmt#{<<"metadata">> => #{}},
        {200, [], Resp}
    catch throw:invalid_network ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_INVALID_NETWORK)};
          throw:block_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
          throw:chain_too_short ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_CHAIN_TOO_SHORT)}
    end;
handle_request_('blockTransaction', #{'BlockTransactionRequest' :=
                                          #{<<"block_identifier">> :=
                                                #{<<"hash">> := BlockHash ,
                                                  <<"index">> := BlockHeight},
                                            <<"network_identifier">> := #{<<"blockchain">> := <<"aeternity">>},
                                            <<"transaction_identifier">> :=
                                                #{<<"hash">> := TxHash}}}, _Context) ->
    try
        %%
        %% Fetch key block by height & hash and verify they match
        %%
        {ok, BlockByHeight} = aeapi:key_block_by_height(BlockHeight),
        {ok, BlockByHash} = aeapi:key_block_by_hash(BlockHash),
        case BlockByHeight =:= BlockByHash of
            true ->
                ok;
            false ->
                throw(block_not_found)
        end,
        %%
        %% Decode the transaction hash
        %%
        TxHashInternal = case aeser_api_encoder:decode(TxHash) of
                             {tx_hash, TxHash0} ->
                                 TxHash0;
                             _ ->
                                 throw(tx_not_found)
                         end,
        %%
        %% Fetch all the transactions from the block
        %%
        SignedTxs = case aeapi:block_txs(BlockByHeight) of
                        error ->
                            throw(block_not_found);
                        Txs0 ->
                            Txs0
                    end,
        %%
        %% Find the transaction using the hash
        %%        
        case find_tx(SignedTxs, TxHashInternal) of
            not_found ->
                throw(tx_not_found);
            {ok, SignedTx, Offset} ->
                Tx = aetx_sign:tx(SignedTx),
                TxType = aetx:tx_type(Tx),
                Resp = #{<<"transaction">> => format_tx(SignedTx, Offset, TxType)},
                io:format("Resp: ~p~n", [Resp]),
                {200, [], Resp}
        end
    catch throw:block_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_BLOCK_NOT_FOUND)};
          throw:tx_not_found ->
            {200, [], rosetta_error_response(?ROSETTA_ERR_TX_NOT_FOUND)}
    end;
handle_request_('call', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionCombine', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionDerive', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionHash', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionMetadata', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionParse', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionPayloads', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionPreprocess', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionSubmit', _, _Context) ->
    {501, [], #{}};
handle_request_('eventsBlocks', _, _Context) ->
    {501, [], #{}};
handle_request_('mempool', _, _Context) ->
    {501, [], #{}};
handle_request_('mempoolTransaction', _, _Context) ->
    {501, [], #{}};
handle_request_('searchTransactions', _, _Context) ->
    {501, [], #{}};

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

ae_operation_types() ->
    [aetx:type_to_swagger_name(X) || X <- [spend_tx,
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
    [rosetta_error_response(X) || X <- [?ROSETTA_ERR_NW_STATUS_ERR,
                                        ?ROSETTA_ERR_INVALID_NETWORK,
                                        ?ROSETTA_ERR_BLOCK_NOT_FOUND,
                                        ?ROSETTA_ERR_CHAIN_TOO_SHORT,
                                        ?ROSETTA_ERR_TX_NOT_FOUND
                                       ]].

-spec format_block(aec_blocks:block()) -> #{}.
format_block(Block) ->
    PrevBlock = aeapi:prev_block(Block),
    #{
      <<"block_identifier">> => format_block_identifier(Block),
      <<"parent_block_identifier">> => format_block_identifier(PrevBlock),
      <<"timestamp">> => aeapi:block_time_in_msecs(Block),
      <<"transactions">> => format_block_txs(aeapi:block_txs(Block))
     }.

format_block_identifier(undefined) ->
    #{<<"index">> => 0,
      <<"hash">> => <<>>};
format_block_identifier(Block) ->
    #{<<"index">> => aeapi:block_height(Block),
      <<"hash">> => aeapi:printable_block_hash(Block)}.

format_block_txs(Txs) ->
    lists:map(fun(SignedTx) ->
                      format_tx(SignedTx)
              end, Txs).

format_tx(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    TxType = aetx:tx_type(Tx),
    #{
      <<"transaction_identifier">> => #{<<"hash">> => aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx))},
      <<"operations">> => tx_operations(SignedTx, TxType)
     }.


tx_operations(SignedTx, spend_tx) ->
    Tx = aetx_sign:tx(SignedTx),
    SpendTx = aetx:tx(Tx),
    From = aeser_api_encoder:encode(account_pubkey, aec_spend_tx:sender_pubkey(SpendTx)),
    %% The magic value 'id_hash' here does feel a little like it's
    %% an an entry to an obfuscated code competition ;)
    To = aeser_api_encoder:encode(id_hash, aec_spend_tx:recipient_id(SpendTx)),
    Type = aetx:type_to_swagger_name(spend_tx),
    [spend_tx_op(0, Type, From, -aec_spend_tx:amount(SpendTx)),
     spend_tx_op(1, Type, To, aec_spend_tx:amount(SpendTx)),
     spend_tx_op(2, <<"Fee">>, From, -aec_spend_tx:fee(SpendTx))];
tx_operations(_Tx, TxType) ->
    [#{
       <<"operation_identifier">> => #{<<"index">> => 0},
       <<"type">> => aetx:type_to_swagger_name(TxType)
      }].

spend_tx_op(Index, Type, Address, Amount) ->
    #{
       <<"operation_identifier">> => #{<<"index">> => Index},
       <<"type">> => Type,
       <<"status">> => <<"SUCCESS">>,
       <<"account">> => #{<<"address">> => Address,
       <<"amount">> => amount(Amount)}
      }.
amount(Amount) ->
    #{<<"value">> => integer_to_binary(Amount),
      <<"currency">> => #{<<"symbol">> => <<"aettos">>,
                          <<"decimals">> => 18}
     }.

rosetta_error_response(ErrCode) ->
    rosetta_error_response(ErrCode, rosetta_err_retriable(ErrCode)).

rosetta_error_response(ErrCode, Retriable) when is_integer(ErrCode),
                                                is_boolean(Retriable) ->
    rosetta_error_response(ErrCode, Retriable, undefined).

rosetta_error_response(ErrCode, Retriable, _Details) when is_integer(ErrCode),
                                                         is_boolean(Retriable) ->
    #{<<"code">> => ErrCode,
      <<"message">> => rosetta_err_msg(ErrCode),
      <<"retriable">> => Retriable
    }.
    % Remove this for now to help dialyzer pass CI. It knows Details can only be undefined..
    % case Details of
    %     undefined ->
    %         Err;
    %     #{} ->
    %         Err#{<<"details">> => Details}
    % end.

rosetta_err_msg(?ROSETTA_ERR_NW_STATUS_ERR)   -> <<"Error determining networkStatus">>;
rosetta_err_msg(?ROSETTA_ERR_INVALID_NETWORK) -> <<"Invalid network specified">>;
rosetta_err_msg(?ROSETTA_ERR_BLOCK_NOT_FOUND) -> <<"Specified block not found">>;
rosetta_err_msg(?ROSETTA_ERR_TX_NOT_FOUND)    -> <<"Specified transaction not found">>;
rosetta_err_msg(?ROSETTA_ERR_CHAIN_TOO_SHORT) -> <<"Chain too short">>.

rosetta_err_retriable(?ROSETTA_ERR_NW_STATUS_ERR)   -> true;
rosetta_err_retriable(?ROSETTA_ERR_BLOCK_NOT_FOUND) -> true;
rosetta_err_retriable(?ROSETTA_ERR_TX_NOT_FOUND)    -> true;
rosetta_err_retriable(?ROSETTA_ERR_CHAIN_TOO_SHORT) -> true;
rosetta_err_retriable(_)                            -> false.

find_tx(SignedTxs, TxHash) ->
    find_tx(SignedTxs, TxHash, 0).

find_tx([], _, _) ->
    not_found;
find_tx([SignedTx | T], TxHash, Offset) ->
    case aetx_sign:hash(SignedTx) == TxHash of
        true ->
            {ok, SignedTx, Offset};
        false ->
            find_tx(T, TxHash, Offset + 1)
    end.

