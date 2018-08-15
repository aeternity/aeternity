-module(aehttp_dispatch_ext).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ process_request/2
                        , read_required_params/1
                        , read_optional_params/1
                        , parse_map_to_atom_keys/0
                        , base58_decode/1
                        , hexstrings_decode/1
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
                        , parse_filter_param/2
                        , read_optional_param/3
                        , get_block/2
                        , get_block/3
                        , get_poi/3
                        , get_block_hash_optionally_by_hash_or_height/1
                        ]).

-compile({parse_transform, lager_transform}).

-spec handle_request(
        OperationID :: atom(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request('GetTopBlock', _, _Context) ->
    {ok, TopBlock} = aehttp_logic:get_top(),
    EncodedHash = aehttp_api_parser:encode(block_hash, TopBlock),
    EncodedHeader = aehttp_api_parser:encode(header, TopBlock),
    {200, [], maps:put(<<"hash">>, EncodedHash, EncodedHeader)};

handle_request('GetCurrentKeyBlock', Req, _Context) ->
    get_block(fun() -> aec_chain:top_key_block() end, Req);

handle_request('GetCurrentKeyBlockHash', _, _Context) ->
    Hash = aec_chain:top_key_block_hash(),
    EncodedHash = aec_base58c:encode(block_hash, Hash),
    {200, [], #{hash => EncodedHash}};

handle_request('GetCurrentKeyBlockHeight', _, _Context) ->
    TopBlock = aec_chain:top_block(),
    Height = aec_blocks:height(TopBlock),
    {200, [], #{height => Height}};

handle_request('GetPendingKeyBlock', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_pending/0, Req, false);

handle_request('GetKeyBlockByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Params)) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aehttp_logic:get_key_block_by_hash(Hash) end, Params)
    end;

handle_request('GetKeyBlockByHeight', Params, _Context) ->
    Height = maps:get(height, Params),
    get_block(fun() -> aehttp_logic:get_key_block_by_height(Height) end, Params);

handle_request('GetMicroBlockHeaderByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get(hash, Params)) of
        {ok, Hash} ->
            case aehttp_logic:get_micro_block_by_hash(Hash) of
                {ok, Block} ->
                    Resp = aehttp_api_parser:encode(header, Block),
                    {200, [], Resp#{hash => aec_base58c:encode(block_hash, Hash)}};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;

handle_request('GetMicroBlockTransactionsByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get(hash, Params)) of
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

handle_request('GetMicroBlockTransactionByHashAndIndex', Params, _Context) ->
    HashDec = aec_base58c:safe_decode(block_hash, maps:get(hash, Params)),
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


handle_request('GetMicroBlockTransactionsCountByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get(hash, Params)) of
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

handle_request('GetCurrentGeneration', _, _Context) ->
    get_generation(aec_chain:top_key_block_hash());

handle_request('GetGenerationByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Params)) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} -> get_generation(Hash)
    end;
handle_request('GetGenerationByHeight', Params, _Context) ->
    Height = maps:get('height', Params),
    case aec_chain_state:get_key_block_hash_at_height(Height) of
        error -> {404, [], #{reason => <<"Chain too short">>}};
        {ok, Hash} -> get_generation(Hash)
    end;

handle_request('GetAccountByPubkey', Params, _Context) ->
    AllowedTypes = [account_pubkey, contract_pubkey],
    case aec_base58c:safe_decode({id_hash, AllowedTypes}, maps:get(pubkey, Params)) of
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

handle_request('GetPendingAccountTransactionsByPubkey', Params, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get(pubkey, Params)) of
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

handle_request('GetTransactionByHash', Params, _Config) ->
    case aec_base58c:safe_decode(tx_hash, maps:get(hash, Params)) of
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

handle_request('GetTransactionInfoByHash', Params, _Config) ->
    ParseFuns = [read_required_params([hash]),
                 base58_decode([{hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 get_contract_call_object_from_tx(tx, contract_call),
                 ok_response(
                    fun(#{contract_call := Call}) ->
                            aect_call:serialize_for_client(Call)
                    end)
                ],
    process_request(ParseFuns, Params);

handle_request('PostTransaction', #{'Tx' := Tx}, _Context) ->
    case aehttp_api_parser:decode(tx, maps:get(<<"tx">>, Tx)) of
        {error, #{<<"tx">> := broken_tx}} ->
            {400, [], #{reason => <<"Invalid tx">>}};
        {error, _} ->
            {400, [], #{reason => <<"Invalid base58Check encoding">>}};
        {ok, SignedTx} ->
            ok = aec_tx_pool:push(SignedTx), %% TODO Add proper error handling
            Hash = aetx_sign:hash(SignedTx),
            {200, [], #{<<"tx_hash">> => aec_base58c:encode(tx_hash, Hash)}}
    end;

handle_request('GetContract', Req, _Context) ->
    case aec_base58c:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Response = aect_contracts:serialize_for_client(Contract),
                    {200, [], Response}
            end
    end;

handle_request('GetContractCode', Req, _Context) ->
    case aec_base58c:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Code = aect_contracts:code(Contract),
                    {200, [], #{ <<"bytecode">> => aeu_hex:hexstring_encode(Code) }}
            end
    end;

handle_request('GetContractStore', Req, _Context) ->
    case aec_base58c:safe_decode(contract_pubkey, maps:get(pubkey, Req)) of
        {error, _} -> {400, [], #{reason => <<"Invalid public key">>}};
        {ok, PubKey} ->
            case aec_chain:get_contract(PubKey) of
                {error, _} -> {404, [], #{reason => <<"Contract not found">>}};
                {ok, Contract} ->
                    Response = [ #{<<"key">> => aeu_hex:hexstring_encode(K),
                                   <<"value">> => aeu_hex:hexstring_encode(V)}
                               || {K, V} <- maps:to_list(aect_contracts:state(Contract)) ],
                    {200, [], #{ <<"store">> => Response }}
            end
    end;

handle_request('GetOracleByPubkey', Params, _Context) ->
    case aec_base58c:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_chain:get_oracle(Pubkey) of
                {ok, Oracle} ->
                    {200, [], aehttp_api_parser:encode(oracle, aeo_oracles:serialize_for_client(Oracle))};
                {error, _} ->
                    {404, [], #{reason => <<"Oracle not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;

handle_request('GetOracleQueriesByPubkey', Params, _Context) ->
    case aec_base58c:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            Limit = case maps:get(limit, Params) of
                        N when N =/= undefined -> N;
                        undefined -> 20
                    end,
            FromQueryId = case maps:get(from, Params) of
                              Id when Id =/= undefined ->
                                  {ok, OracleQueryId} = aec_base58c:safe_decode(oracle_query_id, Id),
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

handle_request('GetOracleQueryByPubkeyAndQueryId', Params, _Context) ->
    case aec_base58c:safe_decode(oracle_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aec_base58c:safe_decode(oracle_query_id, maps:get('query-id', Params)) of
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

handle_request('GetNameEntryByName', Params, _Context) ->
    Name = maps:get(name, Params),
    epoch_mining:info(">>>> ~p", [Name]),
    case aec_chain:name_entry(Name) of
        {ok, #{id       := Id,
               expires  := Expires,
               pointers := Pointers}} ->
            {200, [], #{<<"id">>       => aec_base58c:encode(id_hash, Id),
                        <<"expires">>  => Expires,
                        <<"pointers">> => [aens_pointer:serialize_for_client(P) || P <- Pointers]}};
        {error, name_not_found} ->
            {404, [], #{reason => <<"Name not found">>}};
        {error, name_revoked} ->
            {404, [], #{reason => <<"Name revoked">>}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('GetChannelByPubkey', Params, _Context) ->
    case aec_base58c:safe_decode(channel, maps:get(pubkey, Params)) of
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

handle_request('GetPeerPubkey', _Params, _Context) ->
    {ok, Pubkey} = aec_keys:peer_pubkey(),
    {200, [], #{pubkey => aec_base58c:encode(peer_pubkey, Pubkey)}};

handle_request('GetStatus', _Params, _Context) ->
    {ok, TopBlock} = aehttp_logic:get_top(),
    {ok, GenesisBlockHash} = aec_headers:hash_header(aec_block_genesis:genesis_header()),
    Solutions = 0, %% TODO
    Difficulty = aec_blocks:difficulty(TopBlock),
    Syncing = true, %% TODO
    Listening = true, %% TODO
    Protocols = maps:fold(fun(Vsn, Height, Acc) ->
                          [#{protocol => #{version => Vsn, effective_at_height => Height}} | Acc]
                 end, [], aec_governance:protocols()),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = length(aec_peers:get_random(all)),
    PendingTxsCount =
        case aec_tx_pool:size() of
            N when N =/= undefined -> N;
            undefined -> 0
        end,
    {200, [],
     #{<<"genesis-key-block-hash">>     => aec_base58c:encode(block_hash, GenesisBlockHash),
       <<"solutions">>                  => Solutions,
       <<"difficulty">>                 => Difficulty,
       <<"syncing">>                    => Syncing,
       <<"listening">>                  => Listening,
       <<"protocols">>                  => Protocols,
       <<"node-version">>               => NodeVersion,
       <<"node-revision">>              => NodeRevision,
       <<"peer-count">>                 => PeerCount,
       <<"pending-transactions-count">> => PendingTxsCount}};

handle_request('GetContractPoI', Req, _Context) ->
    ParseFuns = [read_required_params([contract]),
                 base58_decode([{contract, contract, contract_pubkey}]),
                 get_poi(contracts, contract, poi),
                 ok_response(
                    fun(#{poi := PoI}) ->
                        #{poi => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI))}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('GetContractCallFromTx', Req, _Context) ->
    ParseFuns = [read_required_params([tx_hash]),
                 base58_decode([{tx_hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 get_contract_call_object_from_tx(tx, contract_call),
                 ok_response(
                    fun(#{contract_call := Call}) ->
                            aect_call:serialize_for_client(Call)
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('GetPeerKey', _Req, _Context) ->
    case aehttp_logic:peer_pubkey() of
        {ok, PeerKey} ->
            {200, [], #{pub_key => aec_base58c:encode(peer_pubkey, PeerKey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

get_generation(Hash) ->
    case aec_chain:get_generation(Hash) of
        error -> {404, [], #{reason => <<"Block not found">>}};
        {ok, KeyBlock, MicroBlocks} ->
            Struct = #{
              key_block => aehttp_api_parser:encode_client_readable_key_block(KeyBlock),
              micro_blocks => [ aehttp_api_parser:encode(block_hash, Micro) || Micro <- MicroBlocks ]
            },
            {200, [], Struct}
    end.

