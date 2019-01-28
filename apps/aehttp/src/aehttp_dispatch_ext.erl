-module(aehttp_dispatch_ext).

-export([forbidden/1]).
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
                        , get_contract_call_object_from_tx/2
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , verify_name/1
                        , ttl_decode/1
                        , poi_decode/1
                        , compute_contract_create_data/0
                        , compute_contract_call_data/0
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/2
                        , ok_response/1
                        , read_optional_param/3
                        , get_poi/3
                        , get_block_hash_optionally_by_hash_or_height/1
                        ]).

-compile({parse_transform, lager_transform}).

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).
-define(NO_Q, no_queue).

-define(TC(Expr, Msg), begin {Time, Res} = timer:tc(fun() -> Expr end), lager:debug("[~p] Msg = ~p", [Time, Msg]), Res end).

-spec forbidden( OperationID :: atom() ) -> boolean().
forbidden(_OpId) -> false.

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
    try aec_jobs_queues:run(Queue, F)
    catch
        error:{rejected, _} ->
            {503, [], #{reason => <<"Temporary overload">>}}
    end.

%% read transactions
queue('GetTopBlock')                            -> ?READ_Q;
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
queue('GetPendingAccountTransactionsByPubkey')  -> ?READ_Q;
queue('GetTransactionByHash')                   -> ?READ_Q;
queue('GetTransactionInfoByHash')               -> ?READ_Q;
queue('GetContract')                            -> ?READ_Q;
queue('GetContractCode')                        -> ?READ_Q;
queue('GetContractStore')                       -> ?READ_Q;
queue('GetContractPoI')                         -> ?READ_Q;
queue('GetOracleByPubkey')                      -> ?READ_Q;
queue('GetOracleQueriesByPubkey')               -> ?READ_Q;
queue('GetOracleQueryByPubkeyAndQueryId')       -> ?READ_Q;
queue('GetNameEntryByName')                     -> ?READ_Q;
queue('GetChannelByPubkey')                     -> ?READ_Q;
queue('GetPeerPubkey')                          -> ?READ_Q;
queue('GetStatus')                              -> ?READ_Q;
queue('GetContractCallFromTx')                  -> ?READ_Q;
queue('GetPeerKey')                             -> ?READ_Q;
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
                    case aec_chain:get_block(PrevBlockHash) of
                        {ok, PrevBlock} ->
                            PrevBlockType = aec_blocks:type(PrevBlock),
                            Header = aec_blocks:to_header(Block),
                            Header1 = aec_headers:serialize_for_client(Header, PrevBlockType),
                            Res =
                                case aec_headers:type(Header) of
                                    key   -> #{key_block => Header1};
                                    micro -> #{micro_block => Header1}
                                end,
                            {200, [], Res};
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
                _ ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case aec_chain:get_block(PrevBlockHash) of
                        {ok, PrevBlock} ->
                            PrevBlockType = aec_blocks:type(PrevBlock),
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
    EncodedHash = aehttp_api_encoder:encode(key_block_hash, Hash),
    {200, [], #{hash => EncodedHash}};

handle_request_('GetCurrentKeyBlockHeight', _, _Context) ->
    TopBlock = aec_chain:top_block(),
    Height = aec_blocks:height(TopBlock),
    {200, [], #{height => Height}};

handle_request_('GetPendingKeyBlock', _Req, _Context) ->
    case aec_conductor:get_key_block_candidate() of
        {ok, Block} ->
            PrevBlockHash = aec_blocks:prev_hash(Block),
            case aec_chain:get_block(PrevBlockHash) of
                {ok, PrevBlock} ->
                    PrevBlockType = aec_blocks:type(PrevBlock),
                    Header = aec_blocks:to_header(Block),
                    {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, beneficiary_not_configured} ->
            {400, [], #{reason => <<"Beneficiary not configured">>}};
        {error, _} ->
            {404, [], #{reason => <<"Block not found">>}}
    end;

handle_request_('GetKeyBlockByHash', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(key_block_hash, maps:get('hash', Params)) of
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
                                    case aec_chain:get_block(PrevBlockHash) of
                                        {ok, PrevBlock} ->
                                            PrevBlockType = aec_blocks:type(PrevBlock),
                                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                                        error ->
                                            {404, [], #{reason => <<"Block not found">>}}
                                    end
                            end;
                        false ->
                            {404, [], #{reason => <<"Block not fond">>}}
                    end;
                error ->
                    {404, [], #{reason => <<"Block not fond">>}}
            end
    end;

handle_request_('GetKeyBlockByHeight', Params, _Context) ->
    Height = maps:get(height, Params),
    case aec_chain:get_key_block_by_height(Height) of
        {ok, Block} ->
            Header = aec_blocks:to_header(Block),
            case aec_blocks:height(Block) of
                0 ->
                    {200, [], aec_headers:serialize_for_client(Header, key)};
                _ ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case aec_chain:get_block(PrevBlockHash) of
                        {ok, PrevBlock} ->
                            PrevBlockType = aec_blocks:type(PrevBlock),
                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end
            end;
        {error, _Rsn} ->
            {404, [], #{reason => <<"Block not found">>}}
    end;

handle_request_('GetMicroBlockHeaderByHash', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    PrevBlockHash = aec_blocks:prev_hash(Block),
                    case aec_chain:get_block(PrevBlockHash) of
                        {ok, PrevBlock} ->
                            PrevBlockType = aec_blocks:type(PrevBlock),
                            Header = aec_blocks:to_header(Block),
                            {200, [], aec_headers:serialize_for_client(Header, PrevBlockType)};
                        error ->
                            {404, [], #{reason => <<"Block not found">>}}
                    end;
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request_('GetMicroBlockTransactionsByHash', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    Header = aec_blocks:to_header(Block),
                    Txs = [ aetx_sign:serialize_for_client(Header, Tx)
                            || Tx <- aec_blocks:txs(Block)],
                    {200, [], #{transactions => Txs}};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request_('GetMicroBlockTransactionByHashAndIndex', Params, _Context) ->
    HashDec = aehttp_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)),
    IndexDec = maps:get(index, Params),
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
    case aehttp_api_encoder:safe_decode(micro_block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    {200, [], #{count => length(aec_blocks:txs(Block))}};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request_('GetCurrentGeneration', _, _Context) ->
    generation_rsp(aec_chain:get_current_generation());

handle_request_('GetGenerationByHash', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(key_block_hash, maps:get('hash', Params)) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aec_chain_state:hash_is_in_main_chain(Hash) of
                true  -> generation_rsp(aec_chain:get_generation_by_hash(Hash, forward));
                false -> {400, [], #{reason => <<"Hash not on main chain">>}}
            end
    end;
handle_request_('GetGenerationByHeight', Params, _Context) ->
    Height = maps:get('height', Params),
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {404, [], #{reason => <<"Chain too short">>}};
        {ok, Hash} -> generation_rsp(aec_chain:get_generation_by_hash(Hash, forward))
    end;

handle_request_('GetAccountByPubkey', Params, _Context) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aehttp_api_encoder:safe_decode({id_hash, AllowedTypes}, maps:get(pubkey, Params)) of
        {ok, Id} ->
            {_IdType, Pubkey} = aec_id:specialize(Id),
            case aec_chain:get_account(Pubkey) of
                {value, Account} ->
                    {200, [], aec_accounts:serialize_for_client(Account)};
                none ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request_('GetPendingAccountTransactionsByPubkey', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(account_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_account(Pubkey) of
                {value, _} ->
                    {ok, Txs0} = aec_tx_pool:peek(infinity, Pubkey),
                    Txs = [aetx_sign:serialize_for_client_pending(T) || T <- Txs0],
                    {200, [], #{transactions => Txs}};
                _ ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request_('GetTransactionByHash', Params, _Config) ->
    case aehttp_api_encoder:safe_decode(tx_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aec_chain:find_tx_with_location(Hash) of
                none ->
                    {404, [], #{<<"reason">> => <<"Transaction not found">>}};
                {mempool, Tx} ->
                    {200, [], aetx_sign:serialize_for_client_pending(Tx)};
                {BlockHash, Tx} ->
                    {ok, Header} = aec_chain:get_header(BlockHash),
                    {200, [], aetx_sign:serialize_for_client(Header, Tx)}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request_('GetTransactionInfoByHash', Params, _Config) ->
    ParseFuns = [read_required_params([hash]),
                 api_decode([{hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 get_contract_call_object_from_tx(tx, contract_call),
                 ok_response(
                    fun(#{contract_call := Call}) ->
                            aect_call:serialize_for_client(Call)
                    end)
                ],
    process_request(ParseFuns, Params);

handle_request_('PostTransaction', #{'Tx' := Tx}, _Context) ->
    case aehttp_api_encoder:safe_decode(transaction, maps:get(<<"tx">>, Tx)) of
        {ok, TxDec} ->
            case deserialize_transaction(TxDec) of
                {ok, SignedTx} ->
                    case aec_tx_pool:push(SignedTx) of
                        ok ->
                            Hash = aetx_sign:hash(SignedTx),
                            {200, [], #{<<"tx_hash">> => aehttp_api_encoder:encode(tx_hash, Hash)}};
                        {error, _} ->
                            {400, [], #{reason => <<"Invalid tx">>}}
                    end;
                {error, broken_tx} ->
                    {400, [], #{reason => <<"Invalid tx">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid api encoding">>}}
    end;

handle_request_('GetContract', Req, _Context) ->
    case aehttp_api_encoder:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Response = aect_contracts:serialize_for_client(Contract),
                    {200, [], Response}
            end
    end;

handle_request_('GetContractCode', Req, _Context) ->
    case aehttp_api_encoder:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Code = aect_contracts:code(Contract),
                    {200, [], #{ <<"bytecode">> => aehttp_api_encoder:encode(contract_bytearray, Code) }}
            end
    end;

handle_request_('GetContractStore', Req, _Context) ->
    case aehttp_api_encoder:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Response = [ #{<<"key">> => aeu_hex:hexstring_encode(K),
                                   <<"value">> => aehttp_api_encoder:encode(contract_bytearray, V)}
                               || {K, V} <- maps:to_list(aect_contracts_store:contents(aect_contracts:state(Contract))) ],
                    {200, [], #{ <<"store">> => Response }}
            end
    end;

handle_request_('GetContractPoI', Req, _Context) ->
    ParseFuns = [read_required_params([pubkey]),
                 api_decode([{pubkey, pubkey, contract_pubkey}]),
                 get_poi(contract, pubkey, poi),
                 ok_response(
                    fun(#{poi := PoI}) ->
                        #{poi => aehttp_api_encoder:encode(poi, aec_trees:serialize_poi(PoI))}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request_('GetOracleByPubkey', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_oracle(Pubkey) of
                {ok, Oracle} ->
                    {200, [], aeo_oracles:serialize_for_client(Oracle)};
                {error, _} ->
                    {404, [], #{reason => <<"Oracle not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request_('GetOracleQueriesByPubkey', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            Limit = case maps:get(limit, Params) of
                        N when N =/= undefined -> N;
                        undefined -> 20
                    end,
            FromQueryId = case maps:get(from, Params) of
                              Id when Id =/= undefined ->
                                  {ok, OracleQueryId} = aehttp_api_encoder:safe_decode(oracle_query_id, Id),
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
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request_('GetOracleQueryByPubkeyAndQueryId', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aehttp_api_encoder:safe_decode(oracle_query_id, maps:get('query-id', Params)) of
                {ok, QueryId} ->
                    case aec_chain:get_oracle_query(Pubkey, QueryId) of
                        {ok, Query} ->
                            {200, [], aeo_query:serialize_for_client(Query)};
                        {error, _} ->
                            {404, [], #{reason => <<"Query not found">>}}
                    end;
                {error, _} ->
                    {400, [], #{reason => <<"Invalid public key or query ID">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key or query ID">>}}
    end;

handle_request_('GetNameEntryByName', Params, _Context) ->
    Name = maps:get(name, Params),
    case aec_chain:name_entry(Name) of
        {ok, #{id       := Id,
               ttl      := TTL,
               pointers := Pointers}} ->
            {200, [], #{<<"id">>       => aehttp_api_encoder:encode(id_hash, Id),
                        <<"ttl">>      => TTL,
                        <<"pointers">> => [aens_pointer:serialize_for_client(P) || P <- Pointers]}};
        {error, name_not_found} ->
            {404, [], #{reason => <<"Name not found">>}};
        {error, name_revoked} ->
            {404, [], #{reason => <<"Name revoked">>}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request_('GetChannelByPubkey', Params, _Context) ->
    case aehttp_api_encoder:safe_decode(channel, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_channel(Pubkey) of
                {ok, Channel} ->
                    {200, [], aesc_channels:serialize_for_client(Channel)};
                {error, _} ->
                    {404, [], #{reason => <<"Channel not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request_('GetPeerPubkey', _Params, _Context) ->
    {ok, Pubkey} = aec_keys:peer_pubkey(),
    {200, [], #{pubkey => aehttp_api_encoder:encode(peer_pubkey, Pubkey)}};

handle_request_('GetStatus', _Params, _Context) ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    {ok, GenesisBlockHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
    Solutions = 0, %% TODO
    Difficulty = aec_blocks:difficulty(TopKeyBlock),
    Syncing = true, %% TODO
    Listening = true, %% TODO
    Protocols = maps:fold(fun(Vsn, Height, Acc) ->
                          [#{<<"version">> => Vsn, <<"effective_at_height">> => Height} | Acc]
                 end, [], aec_governance:protocols()),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = aec_peers:count(peers),
    PendingTxsCount = aec_tx_pool:size(),
    {200, [],
     #{<<"genesis_key_block_hash">>     => aehttp_api_encoder:encode(key_block_hash, GenesisBlockHash),
       <<"solutions">>                  => Solutions,
       <<"difficulty">>                 => Difficulty,
       <<"syncing">>                    => Syncing,
       <<"listening">>                  => Listening,
       <<"protocols">>                  => Protocols,
       <<"node_version">>               => NodeVersion,
       <<"node_revision">>              => NodeRevision,
       <<"peer_count">>                 => PeerCount,
       <<"pending_transactions_count">> => PendingTxsCount,
       <<"network_id">>                 => aec_governance:get_network_id()}};

handle_request_('GetContractCallFromTx', Req, _Context) ->
    ParseFuns = [read_required_params([tx_hash]),
                 api_decode([{tx_hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 get_contract_call_object_from_tx(tx, contract_call),
                 ok_response(
                    fun(#{contract_call := Call}) ->
                            aect_call:serialize_for_client(Call)
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request_('GetPeerKey', _Req, _Context) ->
    case aehttp_logic:peer_pubkey() of
        {ok, PeerKey} ->
            {200, [], #{pub_key => aehttp_api_encoder:encode(peer_pubkey, PeerKey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

generation_rsp(error) ->
    {404, [], #{reason => <<"Block not found">>}};
generation_rsp({ok, #{ key_block := KeyBlock, micro_blocks := MicroBlocks }}) ->
    case aec_blocks:height(KeyBlock) of
        0 ->
            {200, [], encode_generation(KeyBlock, MicroBlocks, key)};
        _ ->
            PrevBlockHash = aec_blocks:prev_hash(KeyBlock),
            case aec_chain:get_block(PrevBlockHash) of
                {ok, PrevBlock} ->
                    PrevBlockType = aec_blocks:type(PrevBlock),
                    {200, [], encode_generation(KeyBlock, MicroBlocks, PrevBlockType)};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end.

encode_generation(KeyBlock, MicroBlocks, PrevBlockType) ->
    Header = aec_blocks:to_header(KeyBlock),
    #{key_block => aec_headers:serialize_for_client(Header, PrevBlockType),
      micro_blocks => [begin
                           {ok, Hash} = aec_blocks:hash_internal_representation(M),
                           aehttp_api_encoder:encode(micro_block_hash, Hash)
                       end || M <- MicroBlocks]}.


deserialize_transaction(Tx) ->
    try
        {ok, aetx_sign:deserialize_from_binary(Tx)}
    catch
        _:_ -> {error, broken_tx}
    end.

