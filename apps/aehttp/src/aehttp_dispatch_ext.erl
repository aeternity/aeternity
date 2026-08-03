-module(aehttp_dispatch_ext).

-export([forbidden/2]).
-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ process_request/2
                        , read_required_params/1
                        , read_optional_params/1
                        , parse_map_to_atom_keys/0
                        , api_decode/1
                        , nameservice_pointers_decode/1
                        , get_nonce/1
                        , get_nonce_from_account_id/1
                        , print_state/0
                        , get_contract_code/2
                        , get_info_object_from_tx/3
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , verify_name/1
                        , ttl_decode/1
                        , poi_decode/1
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/2
                        , encode_generation/3
                        , when_stable/1
                        , ok_response/1
                        , read_optional_param/3
                        , get_poi/3
                        , get_block_hash_optionally_by_hash_or_height/1
                        , do_dry_run/0
                        ]).

-compile({parse_transform, lager_transform}).

-include_lib("aecontract/include/aecontract.hrl").

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).
-define(NO_Q, no_queue).

%% dry run limits
-define(DEFAULT_GAS_LIMIT, 6000000).
-define(DEFAULT_CALL_REQ_GAS_LIMIT, 1000000).

-define(TC(Expr, Msg), begin {Time, Res} = timer:tc(fun() -> Expr end), lager:debug("[~p] Msg = ~p", [Time, Msg]), Res end).

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
                lager:debug("operationId: ~p", [OperationID]),
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
            aehttp_helpers:service_unavailable(overload);
        error:timeout ->
            aehttp_helpers:service_unavailable(not_stable);
        Class:Reason:Stacktrace ->
            lager:error("CRASH ~p ~p, ~p", [Class, Reason, Stacktrace]),
            {500, [], #{reason => <<"Internal server error">>}}
    end.

%% read transactions
queue('GetTopBlock')                            -> ?READ_Q;
queue('GetTopHeader')                           -> ?READ_Q;
queue('GetCurrentKeyBlock')                     -> ?READ_Q;
queue('GetCurrentKeyBlockHash')                 -> ?READ_Q;
queue('GetCurrentKeyBlockHeight')               -> ?READ_Q;
queue('GetPendingKeyBlock')                     -> ?READ_Q;
queue('GetKeyBlockByHash')                      -> ?READ_Q;
queue('GetKeyBlockByHeight')                    -> ?READ_Q;
queue('GetMicroBlockHeaderByHash')              -> ?READ_Q;
queue('GetMicroBlockTransactionsByHash')        -> ?READ_Q;
queue('GetMicroBlockTransactionByHashAndIndex') -> ?READ_Q;
queue('GetMicroBlockTransactionsCountByHash')   -> ?READ_Q;
queue('GetCurrentGeneration')                   -> ?READ_Q;
queue('GetGenerationByHash')                    -> ?READ_Q;
queue('GetGenerationByHeight')                  -> ?READ_Q;
queue('GetAccountByPubkey')                     -> ?READ_Q;
queue('GetAccountByPubkeyAndHeight')            -> ?READ_Q;
queue('GetPendingAccountTransactionsByPubkey')  -> ?READ_Q;
queue('GetAccountNextNonce')                    -> ?READ_Q;
queue('GetTransactionByHash')                   -> ?READ_Q;
queue('GetTransactionInfoByHash')               -> ?READ_Q;
queue('GetContract')                            -> ?READ_Q;
queue('GetContractCode')                        -> ?READ_Q;
queue('GetContractPoI')                         -> ?READ_Q;
queue('GetOracleByPubkey')                      -> ?READ_Q;
queue('GetOracleQueriesByPubkey')               -> ?READ_Q;
queue('GetOracleQueryByPubkeyAndQueryId')       -> ?READ_Q;
queue('GetNameEntryByName')                     -> ?READ_Q;
queue('GetNameEntryByNameHash')                 -> ?READ_Q;
queue('GetAuctionEntryByName')                  -> ?READ_Q;
queue('GetChannelByPubkey')                     -> ?READ_Q;
queue('GetPeerPubkey')                          -> ?READ_Q;
queue('GetStatus')                              -> ?READ_Q;
queue('GetSyncStatus')                          -> ?READ_Q;
queue('GetPeerKey')                             -> ?READ_Q;
queue('GetChainEnds')                           -> ?READ_Q;
queue('GetRecentGasPrices')                     -> ?READ_Q;
queue('GetProtocolParameters')                  -> ?READ_Q;
queue('GetNodeSettings')                        -> ?READ_Q;
queue('GetPinningTx')                           -> ?READ_Q;
queue('GetHyperchainContractPubkeys')           -> ?READ_Q;
%% update transactions (default to update in catch-all)
queue('PostTransaction')                        -> ?WRITE_Q;
queue(_)                                        -> ?WRITE_Q.

handle_request_('GetTopBlock', _, _Context) ->
    case aec_chain:top_block() of
        Block when Block =/= undefined ->
            case aec_blocks:height(Block) of
                0 ->
                    Header = aec_blocks:to_header(Block),
                    {200, [], #{key_block => aec_headers:serialize_for_client(Header, key)}};
                _ ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case prev_block_type(PrevBlockHash) of
                        {ok, PrevBlockType} ->
                            Header = aec_blocks:to_header(Block),
                            Type =
                                case aec_headers:type(Header) of
                                    key -> key_block;
                                    micro -> micro_block
                                end,
                            SerializedHeader = aec_headers:serialize_for_client(Header, PrevBlockType),
                            {200, [], #{Type => SerializedHeader}};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end
            end;
        undefined ->
            {404, [], #{reason => <<"Block not found">>}}
    end;

handle_request_('GetTopHeader', _, _Context) ->
    case aec_chain:top_block() of
        Block when Block =/= undefined ->
            case aec_blocks:height(Block) of
                0 ->
                    Header = aec_blocks:to_header(Block),
                    {200, [], aec_headers:serialize_for_client(Header, key)};
                _ ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case prev_block_type(PrevBlockHash) of
                        {ok, PrevBlockType} ->
                            Header = aec_blocks:to_header(Block),
                            SerHeader = aec_headers:serialize_for_client(Header, PrevBlockType),
                            {200, [], SerHeader};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end
            end;
        undefined ->
            {404, [], #{reason => <<"Block not found">>}}
    end;


handle_request_('GetCurrentKeyBlock', _Req, _Context) ->
    case aec_chain:top_key_block() of
        {ok, Block} ->
            case aec_blocks:height(Block) of
                0 ->
                    Header = aec_blocks:to_header(Block),
                    {200, [], aec_headers:serialize_for_client(Header, key)};
                _Height ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case prev_block_type(PrevBlockHash) of
                        {ok, PrevBlockType} ->
                            Header = aec_blocks:to_header(Block),
                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end
            end;
        error ->
            {404, [], #{reason => <<"Block not found">>}}
    end;

handle_request_('GetCurrentKeyBlockHash', _, _Context) ->
    Hash = aec_chain:top_key_block_hash(),
    EncodedHash = aeser_api_encoder:encode(key_block_hash, Hash),
    {200, [], #{hash => EncodedHash}};

handle_request_('GetCurrentKeyBlockHeight', _, _Context) ->
    TopBlock = aec_chain:top_block(),
    Height = aec_blocks:height(TopBlock),
    {200, [], #{height => Height}};

handle_request_('GetPendingKeyBlock', _Req, _Context) ->
    case aec_conductor:get_key_block_candidate() of
        {ok, Block} ->
            PrevBlockHash = aec_blocks:prev_hash(Block),
            case prev_block_type(PrevBlockHash) of
                {ok, PrevBlockType} ->
                    Header = aec_blocks:to_header(Block),
                    {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, beneficiary_not_configured = Code} ->
            {400, [], #{reason => <<"Beneficiary not configured">>,
                        error_code => atom_to_binary(Code, utf8)}};
        {error, Code} ->
            {404, [], #{reason => <<"Block not found">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetKeyBlockByHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(key_block_hash, maps:get('hash', Params)) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aec_chain:get_block(Hash) of
                {ok, Block} ->
                    case aec_blocks:is_key_block(Block) of
                        true ->
                            Header = aec_blocks:to_header(Block),
                            case aec_blocks:height(Block) of
                                0 ->
                                    {200, [], aec_headers:serialize_for_client(Header, key)};
                                _ ->
                                    PrevBlockHash = aec_blocks:prev_hash(Block),
                                    case prev_block_type(PrevBlockHash) of
                                        {ok, PrevBlockType} ->
                                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                                        error ->
                                            {404, [], #{reason => <<"Block not found">>}}
                                    end
                            end;
                        false ->
                            {404, [], #{reason => <<"Block not found">>,
                                        error_code => <<"no_key_block">>}}
                    end;
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request_('GetKeyBlockByHeight', Params, _Context) ->
    Height =  aehttp_helpers:to_int(maps:get(height, Params)),
    case aec_chain:get_key_block_by_height(Height) of
        {ok, Block} ->
            Header = aec_blocks:to_header(Block),
            case aec_blocks:height(Block) of
                0 ->
                    {200, [], aec_headers:serialize_for_client(Header, key)};
                _ ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case prev_block_type(PrevBlockHash) of
                        {ok, PrevBlockType} ->
                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end
            end;
        {error, Code} ->
            {404, [], #{reason => <<"Block not found">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetMicroBlockHeaderByHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case prev_block_type(PrevBlockHash) of
                        {ok, PrevBlockType} ->
                            Header = aec_blocks:to_header(Block),
                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end;
                {error, block_not_found = Code} ->
                    {404, [], #{reason => <<"Block not found">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid hash">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetMicroBlockTransactionsByHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    Header = aec_blocks:to_header(Block),
                    Txs = [ aetx_sign:serialize_for_client(Header, Tx)
                            || Tx <- aec_blocks:txs(Block)],
                    {200, [], #{transactions => Txs}};
                {error, block_not_found = Code} ->
                    {404, [], #{reason => <<"Block not found">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid hash">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetMicroBlockTransactionByHashAndIndex', Params, _Context) ->
    HashDec = aeser_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)),
    IndexDec = aehttp_helpers:to_int(maps:get(index, Params)),
    case {HashDec, IndexDec} of
        {{ok, Hash}, Index} when is_integer(Index) ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    Txs = aec_blocks:txs(Block),
                    TxsCount = length(Txs),
                    case Index of
                        I when I > 0, I =< TxsCount ->
                            Header = aec_blocks:to_header(Block),
                            Tx = lists:nth(I, Txs),
                            {200, [], aetx_sign:serialize_for_client(Header, Tx)};
                        _Other ->
                            {400, [], #{reason => <<"Invalid hash or index">>}}
                    end;
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {_, _} ->
            {400, [], #{reason => <<"Invalid hash or index">>}}
    end;


handle_request_('GetMicroBlockTransactionsCountByHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    {200, [], #{count => length(aec_blocks:txs(Block))}};
                {error, block_not_found = Code} ->
                    {404, [], #{reason => <<"Block not found">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid hash">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetCurrentGeneration', _, _Context) ->
    generation_rsp(aec_chain:get_current_generation());

handle_request_('GetGenerationByHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(key_block_hash, maps:get('hash', Params)) of
        {error, Code} -> {400, [], #{reason => <<"Invalid hash">>,
                                     error_code => atom_to_binary(Code, utf8)}};
        {ok, Hash} ->
            case aec_chain:get_generation_by_hash(Hash, forward) of
                Ok = {ok, _G} -> generation_rsp(Ok);
                error         -> {400, [], #{reason => <<"Hash not on main chain">>}}
            end
    end;
handle_request_('GetGenerationByHeight', Params, _Context) ->
    Height = aehttp_helpers:to_int(maps:get('height', Params)),
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {404, [], #{reason => <<"Chain too short">>}};
        {ok, Hash} -> generation_rsp(aec_chain:get_generation_by_hash(Hash, forward))
    end;

handle_request_('GetAccountByPubkey', Params, _Context) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, maps:get(pubkey, Params)) of
        {ok, Id} ->
            {_IdType, Pubkey} = aeser_id:specialize(Id),
            case aec_chain:get_account(Pubkey) of
                {value, Account} ->
                    {200, [], aec_accounts:serialize_for_client(Account)};
                none ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetAccountByPubkeyAndHeight', Params, _Context) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, maps:get(pubkey, Params)) of
        {ok, Id} ->
            {_IdType, Pubkey} = aeser_id:specialize(Id),
            Height = aehttp_helpers:to_int(maps:get(height, Params)),
            case aec_chain:get_account_at_height(Pubkey, Height) of
                {value, Account} ->
                    {200, [], aec_accounts:serialize_for_client(Account)};
                none ->
                    {404, [], #{reason => <<"Account not found">>}};
                {error, chain_too_short = Code} ->
                    {404, [], #{reason => <<"Height not available">>,
                                error_code => atom_to_binary(Code, utf8)}};
                {error, garbage_collected = Code} ->
                    {410, [], #{reason => <<"State data at the requested height has been garbage-collected">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetAccountByPubkeyAndHash', Params, _Context) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aeser_api_encoder:safe_decode({id_hash, AllowedTypes}, maps:get(pubkey, Params)) of
        {ok, Id} ->
            {_IdType, Pubkey} = aeser_id:specialize(Id),
            EncodedHash = maps:get(hash, Params),
            case aeser_api_encoder:safe_decode(block_hash, EncodedHash) of
                {error, What} ->
                    BinWhat = atom_to_binary(What, utf8),
                    {400, [], #{reason => <<"Illegal hash: ", BinWhat/binary>>}};
                {ok, Hash} ->
                    case aec_chain:get_account_at_hash(Pubkey, Hash) of
                        {value, Account} ->
                            {200, [], aec_accounts:serialize_for_client(Account)};
                        none ->
                            {404, [], #{reason => <<"Account not found">>}};
                        {error, no_state_trees = Code} ->
                            {404, [], #{reason => <<"Hash not available">>,
                                        error_code => atom_to_binary(Code, utf8)}}
                    end
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetPendingAccountTransactionsByPubkey', Params, _Context) ->
    case aeser_api_encoder:safe_decode(account_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_account(Pubkey) of
                {value, _} ->
                    {ok, Txs0} = aec_tx_pool:peek(infinity, Pubkey),
                    Txs = [aetx_sign:serialize_for_client_pending(T) || T <- Txs0],
                    {200, [], #{transactions => Txs}};
                _ ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetAccountNextNonce', Params, _Context) ->
    case aeser_api_encoder:safe_decode(account_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            Strategy =
                case maps:get(strategy, Params) of
                    max -> max;
                    continuity -> continuity
                end,
            case aec_next_nonce:pick_for_account(Pubkey, Strategy) of
                {ok, NextNonce} ->
                    {200, [], #{next_nonce => NextNonce}};
                {error, account_not_found = Code} ->
                    {404, [], #{reason => <<"Account not found">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetTransactionByHash', Params, _Config) ->
    case aeser_api_encoder:safe_decode(tx_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aec_chain:find_tx_with_location(Hash) of
                none ->
                    {404, [], #{<<"reason">> => <<"Transaction not found">>}};
                {mempool, Tx} ->
                    SerializedTx = aetx_sign:serialize_for_client_pending(Tx),
                    {200, [], SerializedTx};
                {BlockHash, Tx} ->
                    {ok, Header} = aec_chain:get_header(BlockHash),
                    Response = aetx_sign:serialize_for_client(Header, Tx),
                    {200, [], Response}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid hash">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetTransactionInfoByHash', Params, _Config) ->
    ParseFuns = [read_required_params([hash]),
                 api_decode([{hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 get_info_object_from_tx(tx, tx_type, info),
                 ok_response(
                    fun(#{info := Info, tx_type := ga_meta_tx}) ->
                            #{<<"ga_info">> => aega_call:serialize_for_client(Info)};
                       (#{info := Info, tx_type := TxType}) when TxType =:= contract_create_tx;
                                                                 TxType =:= contract_call_tx;
                                                                 TxType =:= ga_attach_tx;
                                                                 TxType =:= paying_for_tx;
                                                                 TxType =:= channel_force_progress_tx ->
                            #{<<"call_info">> => aect_call:serialize_for_client(Info)};
                       (#{info := Info, tx_type := _}) ->
                            %% info is assumed to be a binary
                            #{<<"tx_info">> => Info}
                    end)
                ],
    process_request(ParseFuns, Params);


handle_request_('PostTransaction', #{'Tx' := Tx}, _Context) -> %% swagger2
    handle_request_('PostTransaction', Tx, _Context);
handle_request_('PostTransaction', #{'EncodedTx' := Tx}, _Context) -> %% oas3
    handle_request_('PostTransaction', Tx, _Context);
handle_request_('PostTransaction', #{<<"tx">> := Tx}, _Context) ->
    case aeser_api_encoder:safe_decode(transaction, Tx) of
        {ok, TxDec} ->
            case deserialize_transaction(TxDec) of
                {ok, SignedTx} ->
                    case aec_tx_pool:push(SignedTx) of
                        ok ->
                            Hash = aetx_sign:hash(SignedTx),
                            {200, [], #{<<"tx_hash">> => aeser_api_encoder:encode(tx_hash, Hash)}};
                        {error, E} ->
                            lager:debug("Transaciton ~p failed to be pushed to pool because: ~p", [SignedTx, E]),
                            {400, [], #{reason => <<"Invalid tx">>,
                                        error_code => atom_to_binary(E, utf8)}}
                    end;
                {error, broken_tx = Code} ->
                    {400, [], #{reason => <<"Invalid tx">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid api encoding">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetContract', Req, _Context) ->
    case aeser_api_encoder:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, Code} -> {404, [], #{reason => <<"Contract not found">>,
                                             error_code => atom_to_binary(Code, utf8)}};
                {ok, Contract} ->
                    Response = aect_contracts:serialize_for_client(Contract),
                    {200, [], Response}
            end
    end;

handle_request_('GetContractCode', Req, _Context) ->
    case aeser_api_encoder:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, ErrCode} -> {400, [], #{reason => <<"Invalid public key">>,
                                        error_code => atom_to_binary(ErrCode, utf8)}};
        {ok, PubKey} ->
            case aec_chain:get_contract_with_code(PubKey) of
                {error, ErrCode} -> {404, [], #{reason => <<"Contract not found">>,
                                                error_code => atom_to_binary(ErrCode, utf8)}};
                {ok, _Contract, Code} ->
                    {200, [], #{ <<"bytecode">> => aeser_api_encoder:encode(contract_bytearray, Code) }}
            end
    end;

handle_request_('GetContractPoI', Req, _Context) ->
    ParseFuns = [read_required_params([pubkey]),
                 api_decode([{pubkey, pubkey, contract_pubkey}]),
                 get_poi(contract, pubkey, poi),
                 ok_response(
                    fun(#{poi := PoI}) ->
                        #{poi => aeser_api_encoder:encode(poi, aec_trees:serialize_poi(PoI))}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request_('GetOracleByPubkey', Params, _Context) ->
    case aeser_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_oracle(Pubkey) of
                {ok, Oracle} ->
                    {200, [], aeo_oracles:serialize_for_client(Oracle)};
                {error, Code} ->
                    {404, [], #{reason => <<"Oracle not found">>,
                                error_code => atom_to_binary(Code, utf8) }}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetOracleQueriesByPubkey', Params, _Context) ->
    case aeser_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            Limit = maps:get(limit, Params),
            FromQueryId = case maps:get(from, Params) of
                              Id when Id =/= undefined ->
                                  {ok, OracleQueryId} = aeser_api_encoder:safe_decode(oracle_query_id, Id),
                                  OracleQueryId;
                              undefined ->
                                  '$first'
                          end,
            QueryType = case maps:get(type, Params) of
                            T when T =/= undefined -> T;
                            undefined -> all
                        end,
            case aec_chain:get_oracle_queries(Pubkey, FromQueryId, QueryType, Limit) of
                {ok, Queries} ->
                    Queries1 = [aeo_query:serialize_for_client(Query) || Query <- Queries],
                    {200, [], #{oracle_queries => Queries1}};
                {error, _} ->
                    {200, [], #{oracle_queries => []}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetOracleQueryByPubkeyAndQueryId', Params, _Context) ->
    case aeser_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aeser_api_encoder:safe_decode(oracle_query_id, maps:get('query-id', Params)) of
                {ok, QueryId} ->
                    case aec_chain:get_oracle_query(Pubkey, QueryId) of
                        {ok, Query} ->
                            {200, [], aeo_query:serialize_for_client(Query)};
                        {error, Code} ->
                            {404, [], #{reason => <<"Query not found">>,
                                        error_code => atom_to_binary(Code, utf8)}}
                    end;
                {error, Code} ->
                    {400, [], #{reason => <<"Invalid public key or query ID">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key or query ID">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetAuctionEntryByName', Params, _Context) ->
    Name = maps:get(name, Params),
    case aec_chain:auction_entry(Name) of
        {ok, #{id       := Id,
               bidder   := Bidder,
               started  := Started,
               ttl      := TTL,
               bid      := Bid}} ->
            {200, [], #{<<"id">>             => aeser_api_encoder:encode(id_hash, Id),
                        <<"started_at">>     => Started,
                        <<"ends_at">>        => TTL,
                        <<"highest_bidder">> => aeser_api_encoder:encode(account_pubkey, Bidder),
                        <<"highest_bid">>    => Bid
                        }};
        {error, name_not_found = Code} ->
            {404, [], #{reason => <<"Name not found">>,
                        error_code => atom_to_binary(Code, utf8)}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>,
                        error_code => ReasonBin}}
    end;

handle_request_('GetNameEntryByName', Params, _Context) ->
    Name = maps:get(name, Params),
    handle_get_name_entry(aec_chain:name_entry(Name));

handle_request_('GetNameEntryByNameHash', Params, _Context) ->
    case aeser_api_encoder:safe_decode(name, maps:get(name_hash, Params)) of
        {ok, NameHash} ->
            handle_get_name_entry(aec_chain:name_entry_by_hash(NameHash));
        {error, Code} ->
            {400, [], #{reason => <<"Invalid name hash">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetChannelByPubkey', Params, _Context) ->
    case aeser_api_encoder:safe_decode(channel, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_channel(Pubkey) of
                {ok, Channel} ->
                    {200, [], aesc_channels:serialize_for_client(Channel)};
                {error, Code} ->
                    {404, [], #{reason => <<"Channel not found">>,
                                error_code => atom_to_binary(Code, utf8)}}
            end;
        {error, Code} ->
            {400, [], #{reason => <<"Invalid public key">>,
                        error_code => atom_to_binary(Code, utf8)}}
    end;

handle_request_('GetPeerPubkey', _Params, _Context) ->
    {ok, Pubkey} = aec_keys:peer_pubkey(),
    {200, [], #{pubkey => aeser_api_encoder:encode(peer_pubkey, Pubkey)}};

handle_request_('GetSyncStatus', _Params, _Context) ->
    {Syncing, SyncProgress, TargetHeight, SyncTaskId} = aec_sync:sync_progress(),
    case Syncing of
        false ->
            {404, [], #{reason => <<"Node is not syncing">>}};
        true ->

            case aec_sync_stats:get_estimate(SyncTaskId) of
               {ok, #{ speed := Speed }} ->
                    CurrentTop = aec_chain:top_height(),
                    Remaining = round((TargetHeight - CurrentTop) / Speed) * 60,
                    {200, [], #{<<"progress">> => SyncProgress,
                                <<"target">> => TargetHeight,
                                <<"speed">> => Speed,
                                <<"estimate">> => Remaining}};
               _ ->
                    {200, [], #{<<"progress">> => SyncProgress,
                                <<"target">> => TargetHeight,
                                <<"speed">> => 0,
                                <<"estimate">> => 0 }}
            end
    end;

handle_request_('GetStatus', _Params, _Context) ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    Consensus = aec_blocks:consensus_module(TopKeyBlock),
    GenesisBlockHash = aec_consensus:get_genesis_hash(),
    Solutions = 0, %% TODO
    Difficulty = difficulty(aec_blocks:difficulty(TopKeyBlock), Consensus),
    HashRate = target_to_hashrate(aec_blocks:target(TopKeyBlock), Consensus),
    {Syncing, SyncProgress, _, _} = aec_sync:sync_progress(),
    Uptime = uptime(),
    Listening = true, %% TODO
    Protocols =
        maps:fold(fun(Vsn, Height, Acc) ->
                          [#{<<"version">> => Vsn, <<"effective_at_height">> => Height} | Acc]
                  end, [],
                  protocols_with_signalled_fork(aec_blocks:version(TopKeyBlock))),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = aec_peers:count(peers),
    PeerConns = #{<<"inbound">> => aec_peers:count(inbound),
                  <<"outbound">> => aec_peers:count(outbound)},
    PendingTxsCount = aec_tx_pool:size(),
    {ok, PeerPubkey} = aec_keys:peer_pubkey(),
    TopBlock = aec_chain:top_block(),
    TopBlockHeight = aec_blocks:height(TopBlock),
    TopBlockHash = aec_chain:top_key_block_hash(),
    {200, [],
     #{<<"genesis_key_block_hash">>     => aeser_api_encoder:encode(key_block_hash, GenesisBlockHash),
       <<"solutions">>                  => Solutions,
       <<"difficulty">>                 => Difficulty,
       <<"hashrate">>                   => HashRate,
       <<"syncing">>                    => Syncing,
       <<"sync_progress">>              => SyncProgress,
       <<"uptime">>                     => Uptime,
       <<"listening">>                  => Listening,
       <<"protocols">>                  => Protocols,
       <<"node_version">>               => NodeVersion,
       <<"node_revision">>              => NodeRevision,
       <<"peer_count">>                 => PeerCount,
       <<"peer_connections">>           => PeerConns,
       <<"pending_transactions_count">> => PendingTxsCount,
       <<"network_id">>                 => aec_governance:get_network_id(),
       <<"peer_pubkey">>                => aeser_api_encoder:encode(peer_pubkey, PeerPubkey),
       <<"top_key_block_hash">>         => aeser_api_encoder:encode(key_block_hash, TopBlockHash),
       <<"top_block_height">>           => TopBlockHeight}};

handle_request_('GetCurrency', _Params, _Context) ->
    {ok, NameCcy} = aeu_env:find_config([<<"chain">>, <<"currency">>, <<"name">>],[user_config, schema_default]),
    {ok, SymNameCcy} = aeu_env:find_config([<<"chain">>, <<"currency">>, <<"symbol">>],[user_config, schema_default]),
    {ok, Subunit} = aeu_env:find_config([<<"chain">>, <<"currency">>, <<"subunit">>],[user_config, schema_default]),
    {ok, SubunitsPerUnit} = aeu_env:find_config([<<"chain">>, <<"currency">>, <<"subunits_per_unit">>],[user_config, schema_default]),
    CcyMeta0 = #{ <<"name">>                   => NameCcy,
                  <<"symbol">>                 => SymNameCcy,
                  <<"subunit">>                => Subunit,
                  <<"subunits_per_unit">>      => SubunitsPerUnit
                },
    FiatUrl = lists:foldl(
        fun(Key, Acc) ->
            case aeu_env:find_config([<<"chain">>, <<"currency">>, Key],[user_config]) of
                undefined -> Acc;
                {ok, FiatUrl} -> FiatUrl
            end
        end,
        undefined,
        [<<"fiat_converstion_url">>, <<"fiat_conversion_url">>]),
    CcyMeta = case FiatUrl of
                undefined -> CcyMeta0;
                FiatUrl -> maps:merge(CcyMeta0,
                                #{ <<"fiat_converstion_url">> => FiatUrl,
                                   <<"fiat_conversion_url">>  => FiatUrl})
                end,
    {ok, PrimaryColour} = aeu_env:find_config([<<"chain">>, <<"display">>, <<"primary_colour">>],[user_config, schema_default]),
    {ok, SecondaryColour} = aeu_env:find_config([<<"chain">>, <<"display">>, <<"secondary_colour">>],[user_config, schema_default]),
    {ok, NetworkName} = aeu_env:find_config([<<"chain">>, <<"display">>, <<"network_name">>],[user_config, {value, get_default_network_name()}]),
    Display0 = #{ <<"primary_colour">>   => PrimaryColour,
                  <<"secondary_colour">> => SecondaryColour,
                  <<"network_name">>     => NetworkName
                },
    Display = case aeu_env:find_config([<<"chain">>, <<"display">>, <<"logo_file">>],[user_config]) of
                undefined ->
                    Display0;
                {ok, FileName} ->
                    case file:read_file(FileName) of
                        {ok, FileBin} ->
                            case filename:extension(FileName) of
                                <<_,Ext/binary>> ->
                                    maps:put(<<"logo">>,
                                                #{<<"type">> => Ext,
                                                  <<"data">> => base64:encode(FileBin)}, Display0);
                            _ ->
                                Display0
                            end;
                        _ ->
                            Display0
                    end
              end,
    {200, [], maps:merge(CcyMeta, Display)};

handle_request_('GetChainEnds', _Params, _Context) ->
    {200, [], [aeser_api_encoder:encode(key_block_hash, H) || H <- aec_db:find_chain_end_hashes()]};

handle_request_('ProtectedDryRunTxs', #{ 'DryRunInput' := Req }, _Context) ->
    ParseFuns = [ parse_map_to_atom_keys(),
                  read_required_params([txs]),
                  read_optional_params([{top, top, top}, {accounts, accounts, []},
                                        {tx_events, tx_events, false}]),
                  fun(_Req, #{txs := Txs} = State) ->
                      TopBlock = aec_chain:top_block(),
                      Height = aec_blocks:height(TopBlock),
                      Protocol = aec_hard_forks:protocol_effective_at_height(Height),
                      TxGasLimit= lists:sum(
                          lists:map(
                              fun(#{<<"tx">> := ETx}) ->
                                  try {ok, DTx} = aeser_api_encoder:safe_decode(transaction, ETx),
                                      Tx = aetx:deserialize_from_binary(DTx),
                                      aetx:gas_limit(Tx, Height, Protocol)
                                  catch _:_ ->
                                      0 %% this is handled later on
                                  end;
                                 (#{<<"tx_hash">> := TxHash}) ->
                                  try {ok, TxHashInternal} = aeser_api_encoder:safe_decode(tx_hash, TxHash),
                                      {mempool, SignedTx} = aec_chain:find_tx_with_location(TxHashInternal),
                                      Tx = aetx_sign:tx(SignedTx),
                                      aetx:gas_limit(Tx, Height, Protocol)
                                  catch _:_ ->
                                      0 %% this is handled later on
                                  end;
                                 (#{<<"call_req">> := CallReq}) ->
                                    maps:get(<<"gas">>, CallReq, ?DEFAULT_CALL_REQ_GAS_LIMIT)
                              end,
                              Txs)),
                      MaxGas = dry_run_gas_limit(),
                      case TxGasLimit =< MaxGas of
                          true -> {ok, State};
                          false -> {error, {403, [], #{<<"reason">> => <<"Over the gas limit">>}}}
                      end
                  end,
                  do_dry_run()],
    process_request(ParseFuns, Req);

handle_request_('GetRecentGasPrices', _Params, _Context) ->
    Minutes = [1, 5, 15, 60],
    case aehttp_logic:get_top_blocks_gas_price_summary(Minutes) of
        {ok, GasPrices} ->
            %% Applied here rather than in get_top_blocks_gas_price_summary/1 so
            %% the summary keeps returning what was observed on chain.
            MkGasPrice =
                fun({Ms, GasPrice0, Utilization0}) ->
                    {GasPrice, Utilization} =
                        aehttp_logic:apply_min_relay_gas_price(GasPrice0, Utilization0),
                    #{ <<"minutes">> => Ms,
                       <<"min_gas_price">> => GasPrice,
                       <<"utilization">> => Utilization }
                end,
            {200, [], lists:map(MkGasPrice, GasPrices)};
        {error, _} ->
            {404, [], #{reason => <<"Block unexpectedly not found">>}}
    end;

handle_request_('GetProtocolParameters', _Params, _Context) ->
    CurrentVersion = current_protocol_version(),
    BidTimeoutOverride = name_claim_bid_timeout_override(),
    Params =
        #{<<"network_id">> => aec_governance:get_network_id(),
          <<"current_protocol_version">> => CurrentVersion,
          <<"locked_coins_holder_account">> =>
              aeser_api_encoder:encode(account_pubkey, aec_governance:locked_coins_holder_account()),
          <<"micro_block_cycle">> => aec_governance:micro_block_cycle(),
          <<"protocols">> =>
              [ protocol_consensus_parameters(Vsn, EffectiveAtHeight, BidTimeoutOverride)
                || {Vsn, EffectiveAtHeight} <- current_and_pending_protocols(CurrentVersion) ]},
    {200, [], maps:merge(Params, block_interval_setting())};

handle_request_('GetNodeSettings', _Params, _Context) ->
    DryRunGasLimit = dry_run_gas_limit(),
    {200, [],
     #{<<"min_miner_gas_price">>  => aettos_string(aec_tx_pool:minimum_miner_gas_price()),
       <<"max_auth_fun_gas">>     => aec_tx_pool:maximum_auth_fun_gas(),
       <<"mempool_tx_ttl">>       => aec_tx_pool:tx_ttl(),
       <<"mempool_nonce_offset">> => aec_tx_pool:nonce_offset(),
       <<"dry_run_gas_limit">>    => DryRunGasLimit,
       <<"block_gas_limit">>      => aec_governance:block_gas_limit()}};

handle_request_('GetPinningTx', _Params, _Context) ->
    case aec_parent_connector:get_pinning_data() of
        {ok, #{epoch := Epoch,
               height := CCHeight,
               block_hash := EpochBlockHash,
               parent_payload := Payload,
               last_leader := Leader,
               parent_type := Type,
               parent_network_id := Id}} ->
            {200, [], #{<<"epoch">> => Epoch,
                        <<"height">> => CCHeight,
                        <<"block_hash">> => aeser_api_encoder:encode(key_block_hash, EpochBlockHash),
                        <<"parent_payload">> => Payload,
                        <<"last_leader">> => aeser_api_encoder:encode(account_pubkey, Leader),
                        <<"parent_type">> => atom_to_binary(Type),
                        <<"parent_network_id">> => Id}};
        {error, _} ->
           {404, [], #{reason => <<"No pin data available">>}}
    end;

handle_request_('GetHyperchainContractPubkeys', _Params, _Context) ->
    % TODO handle not in HC at all or consensus not initialized?
    lager:debug("HyperchainsGetContract"),
    {200, [], #{<<"staking">> => aeser_api_encoder:encode(contract_pubkey, aec_consensus_hc:get_contract_pubkey(staking)),
                <<"election">> => aeser_api_encoder:encode(contract_pubkey, aec_consensus_hc:get_contract_pubkey(election)),
                <<"rewards">> => aeser_api_encoder:encode(contract_pubkey, aec_consensus_hc:get_contract_pubkey(rewards))
            }};

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

%% The name fee table is indexed by name length; the last entry also applies
%% to all longer names (see aec_governance:name_claim_size_fee/1).
-define(NAME_FEE_TABLE_MAX_LENGTH, 31).

%% Every VM version aect_contracts:is_legal_version_at_protocol/3 knows about; it
%% decides which are legal at each protocol. A new VM version has to be added
%% here as well, or the completeness check in aehttp_integration_SUITE fails.
-define(KNOWN_VM_VERSIONS, [ ?VM_AEVM_SOPHIA_1, ?VM_AEVM_SOLIDITY_1, ?VM_AEVM_SOPHIA_2
                           , ?VM_AEVM_SOPHIA_3, ?VM_FATE_SOPHIA_1, ?VM_AEVM_SOPHIA_4
                           , ?VM_FATE_SOPHIA_2, ?VM_FATE_SOPHIA_3 ]).

%% aec_hard_forks:protocols/0 is the static per-network fork table, which by
%% construction cannot contain a community-fork version: assert_fork_version/2
%% requires it to be greater than every entry. Once such a fork activates the top
%% block carries that version, so without this it would have no entry at all.
protocols_with_signalled_fork(Version) ->
    Protocols = aec_hard_forks:protocols(),
    case application:get_env(aecore, fork, undefined) of
        #{version := Version, signalling_end_height := SigEndHeight} ->
            Protocols#{Version => SigEndHeight};
        _ ->
            Protocols
    end.

%% Only the protocol the top block runs under and any later one whose fork height
%% has not been reached: a client cannot build a transaction against a superseded
%% protocol, and the full table is six times the body on mainnet. Filtering on the
%% version rather than the height keeps the current entry present by construction,
%% including the signalled-fork one.
current_and_pending_protocols(CurrentVersion) ->
    Protocols = protocols_with_signalled_fork(CurrentVersion),
    lists:keysort(1, [ VH || {Vsn, _} = VH <- maps:to_list(Protocols),
                             Vsn >= CurrentVersion ]).

%% The version of the top header, not aec_hard_forks:protocol_effective_at_height/1,
%% which is documented as block-insertion only: inside a community-fork signalling
%% window it can name a different protocol than the block just accepted.
current_protocol_version() ->
    case aec_chain:dirty_top_header() of
        undefined -> aec_block_genesis:version();
        Header    -> aec_headers:version(Header)
    end.

%% Each consensus reports the interval that actually governs it: the mine rate is
%% the PoW difficulty retarget target, so reporting it on a Hyperchains node would
%% advertise an interval the chain does not have.
%%
%% Reads the same (mandatory) consensus config key aec_consensus_hc:child_block_time/0
%% reads, but directly: that accessor memoises into an ETS table created by the
%% calling process, here a short-lived cowboy request process.
block_interval_setting() ->
    case aec_consensus:get_consensus_type() of
        pow ->
            #{<<"expected_block_mine_rate">> => aec_governance:expected_block_mine_rate()};
        pos ->
            case aeu_env:user_config([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>,
                                      <<"child_block_time">>]) of
                {ok, BlockTime} when is_integer(BlockTime) ->
                    #{<<"child_block_time">> => BlockTime};
                _ ->
                    #{}
            end
    end.

%% Mirrors the override aec_governance:name_claim_bid_timeout/2 reads; resolved
%% once per request.
name_claim_bid_timeout_override() ->
    aeu_env:user_config_or_env([<<"mining">>, <<"name_claim_bid_timeout">>],
                               aecore, name_claim_bid_timeout, undefined).

%% Partition aetx:tx_types/0 explicitly: aec_governance:tx_base_gas/2 has no
%% catch-all, so a new type must go missing from the response - which
%% aehttp_integration_SUITE catches - rather than 500 every request. None of the
%% excluded types has a tx_base_gas/2 clause.
-define(NO_BASE_GAS_TX_TYPES, [channel_offchain_tx, channel_client_reconnect_tx, hc_vote_tx]).
-define(CONTRACT_TX_TYPES, [contract_create_tx, contract_call_tx, ga_attach_tx, ga_meta_tx]).

protocol_consensus_parameters(Protocol, EffectiveAtHeight, BidTimeoutOverride) ->
    PlainTxTypes = aetx:tx_types() -- (?CONTRACT_TX_TYPES ++ ?NO_BASE_GAS_TX_TYPES),
    ContractTxTypes = ?CONTRACT_TX_TYPES,
    %% aec_governance:tx_base_gas/3 accepts any ABI - it charges max gas for one
    %% it does not know - but a row per unknown ABI would document a fee for a
    %% transaction no protocol ever accepted. A new Sophia ABI goes here by hand.
    SophiaAbis = [?ABI_AEVM_SOPHIA_1, ?ABI_FATE_SOPHIA_1],
    OracleTxTypes = [oracle_register_tx, oracle_extend_tx, oracle_query_tx, oracle_response_tx],
    TxBaseGas = maps:from_list(
                  [ {aetx:type_to_swagger_name(Type), aec_governance:tx_base_gas(Type, Protocol)}
                    || Type <- PlainTxTypes ]),
    ContractTxBaseGas =
        [ #{<<"tx_type">>     => aetx:type_to_swagger_name(Type),
            <<"abi_version">> => Abi,
            <<"tx_base_gas">> => aec_governance:tx_base_gas(Type, Protocol, Abi)}
          || Type <- ContractTxTypes, Abi <- SophiaAbis ],
    StateGasPerBlock =
        maps:from_list(
          [ begin
                {Part, Whole} = aec_governance:state_gas_per_block(Type),
                {aetx:type_to_swagger_name(Type), #{<<"part">> => Part, <<"whole">> => Whole}}
            end
            || Type <- OracleTxTypes ]),
    MaxAuctionLength = aec_governance:name_max_length_starting_auction(),
    NameClaimFees = [ aettos_string(aec_governance:name_claim_fee_for_size(Length, Protocol))
                      || Length <- lists:seq(1, ?NAME_FEE_TABLE_MAX_LENGTH) ],
    NameAuctionTimeouts =
        [ #{<<"length">>        => Length,
            <<"bid_timeout">>   =>
                aec_governance:name_claim_bid_timeout_for_size(Length, Protocol, BidTimeoutOverride),
            <<"bid_extension">> =>
                aec_governance:name_claim_bid_extension_for_size(Length, Protocol, BidTimeoutOverride)}
          || Length <- lists:seq(1, MaxAuctionLength) ],
    Params =
        #{<<"version">>                          => Protocol,
          <<"effective_at_height">>              => EffectiveAtHeight,
          <<"minimum_gas_price">>                => aettos_string(aec_governance:minimum_gas_price(Protocol)),
          <<"gas_per_byte">>                     => aec_governance:byte_gas(),
          <<"store_byte_gas">>                   => aec_governance:store_byte_gas(),
          <<"tx_base_gas">>                      => TxBaseGas,
          <<"contract_tx_base_gas">>             => ContractTxBaseGas,
          <<"state_gas_per_block">>              => StateGasPerBlock,
          <<"name_claim_fees">>                  => NameClaimFees,
          <<"name_auction_timeouts">>            => NameAuctionTimeouts,
          <<"name_claim_bid_increment">>         => aec_governance:name_claim_bid_increment(),
          <<"name_max_length_starting_auction">> => MaxAuctionLength,
          <<"name_claim_max_expiration">>        => aec_governance:name_claim_max_expiration(Protocol),
          <<"name_registrars">>                  => aec_governance:name_registrars(Protocol),
          <<"name_preclaim_expiration">>         => aec_governance:name_preclaim_expiration(),
          <<"name_claim_preclaim_delta">>        => aec_governance:name_claim_preclaim_delta(),
          <<"name_protection_period">>           => aec_governance:name_protection_period(),
          <<"name_claim_locked_fee">>            => aettos_string(aec_governance:name_claim_locked_fee()),
          <<"allowed_contract_versions">>        => allowed_contract_versions(Protocol),
          <<"allowed_oracle_abi_versions">>      => allowed_oracle_abi_versions(Protocol)},
    Params1 = add_unless_unlimited(<<"name_pointers_max_count">>,
                                   aec_governance:name_pointers_max_count(Protocol), Params),
    add_unless_unlimited(<<"name_pointer_max_key_size">>,
                         aec_governance:name_pointer_max_key_size(Protocol), Params1).

%% Decimal string, not a JSON number: name fees reach ~2^69. See oas3.yaml.
aettos_string(Amount) when is_integer(Amount) ->
    integer_to_binary(Amount).

%% The limit dry-run enforces; also reported by GetNodeSettings.
dry_run_gas_limit() ->
    aeu_env:config_value([<<"http">>, <<"external">>, <<"gas_limit">>],
                         aehttp, [external, gas_limit], ?DEFAULT_GAS_LIMIT).

add_unless_unlimited(_Key, infinity, Params) -> Params;
add_unless_unlimited(Key, Value, Params) when is_integer(Value) ->
    maps:put(Key, Value, Params).

allowed_contract_versions(Protocol) ->
    [ #{<<"vm_version">> => Vm, <<"abi_version">> => Abi}
      || Vm  <- ?KNOWN_VM_VERSIONS,
         Abi <- [?ABI_AEVM_SOPHIA_1, ?ABI_SOLIDITY_1, ?ABI_FATE_SOPHIA_1],
         aect_contracts:is_legal_version_at_protocol(create, #{vm => Vm, abi => Abi}, Protocol) ].

allowed_oracle_abi_versions(Protocol) ->
    [ Abi || Abi <- [?ABI_NO_VM, ?ABI_AEVM_SOPHIA_1, ?ABI_SOLIDITY_1, ?ABI_FATE_SOPHIA_1],
             aect_contracts:is_legal_version_at_protocol(oracle_register, #{vm => ?VM_NO_VM, abi => Abi},
                                                         Protocol) ].

generation_rsp(error) ->
    {404, [], #{reason => <<"Block not found">>}};
generation_rsp({ok, #{ key_block := KeyBlock, micro_blocks := MicroBlocks }}) ->
    case aec_blocks:height(KeyBlock) of
        0 ->
            {200, [], encode_generation(KeyBlock, MicroBlocks, key)};
        _ ->
            PrevBlockHash = aec_blocks:prev_hash(KeyBlock),
            case prev_block_type(PrevBlockHash) of
                {ok, PrevBlockType} ->
                    {200, [], encode_generation(KeyBlock, MicroBlocks, PrevBlockType)};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end.

%% Only the block type of the previous block is needed to serialize a header
%% for a client, so read just the header - reading the full block would also
%% fetch and deserialize every transaction of a micro block.
prev_block_type(PrevBlockHash) ->
    case aec_chain:get_header(PrevBlockHash) of
        {ok, PrevHeader} -> {ok, aec_headers:type(PrevHeader)};
        error -> error
    end.

deserialize_transaction(Tx) ->
    try
        {ok, aetx_sign:deserialize_from_binary(Tx)}
    catch
        _:_ -> {error, broken_tx}
    end.

handle_get_name_entry(GetResult) ->
    case GetResult of
        {ok, #{id       := Id,
               ttl      := TTL,
               owner    := Owner,
               pointers := Pointers}} ->
            {200, [], #{<<"id">>       => aeser_api_encoder:encode(id_hash, Id),
                        <<"owner">>    => aeser_api_encoder:encode(account_pubkey, Owner),
                        <<"ttl">>      => TTL,
                        <<"pointers">> => [aens_pointer:serialize_for_client(P) || P <- Pointers]}};
        {error, name_not_found = Code} ->
            {404, [], #{reason => <<"Name not found">>,
                        error_code => atom_to_binary(Code, utf8)}};
        {error, name_revoked = Code} ->
            {404, [], #{reason => <<"Name revoked">>,
                        error_code => atom_to_binary(Code, utf8)}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>,
                        error_code => ReasonBin}}
    end.

%% Compute hash-rate
%%
%% Target is scientific notation, aeminer_pow:target_to_difficulty computes
%% Difficulty * K   - (for integer precision, where K is (1 bsl 24)
%%
%% One correct solution per blocktime (provided in ms) and 42 graphs per
%% solution explains the last bit of math.
target_to_hashrate(Target, aec_consensus_bitcoin_ng) ->
    case aeminer_pow:scientific_to_integer(Target) of
      0 -> 0;
      _ ->
          Difficulty = aeminer_pow:target_to_difficulty(Target) / (1 bsl 24),
          round((Difficulty * 42) / (aec_governance:expected_block_mine_rate() / 1000))
    end;
target_to_hashrate(_Target, _Consensus) ->
    0.

%%% Difficulty for hyperchains is the number of tokens staked, that is a large number, present
%%% it in microAE instead.
difficulty(Difficulty, Consensus) when Consensus =:= aec_consensus_hc ->
    Difficulty div 1_000_000_000_000;
difficulty(Difficulty, _Consensus) ->
    Difficulty.


get_default_network_name() ->
    get_default_network_name(aec_governance:get_network_id()).

get_default_network_name(<<"ae_mainnet">>) ->
    <<"Mainnet">>;
get_default_network_name(<<"ae_uat">>) ->
    <<"Testnet">>;
get_default_network_name(NetworkId) ->
    NetworkId.

uptime() ->
  {UptimeMs, _} = statistics(wall_clock),

  Ts = UptimeMs div 1000,
  Tms = UptimeMs rem 1000,

  Res =
      if Ts < 60 ->
           io_lib:format("~ps.~p", [Ts, Tms]);
         Ts < 60 * 60 ->
           io_lib:format("~pm:~ps.~p", [Ts div 60, Ts rem 60, Tms]);
         Ts < 60 * 60 * 24 ->
           io_lib:format("~ph:~pm:~ps", [Ts div (60 * 60), (Ts rem (60 * 60)) div 60, Ts rem 60]);
         true ->
           io_lib:format("~pd:~ph:~pm:~ps", [Ts div (60 * 60 * 24), (Ts rem (60 * 60 * 24)) div (60 * 60),
                                             (Ts rem (60 * 60)) div 60, Ts rem 60])
      end,

  iolist_to_binary(Res).
