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
                        , parse_tx_encoding/1
                        , compute_contract_create_data/0
                        , compute_contract_call_data/0
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/3
                        , ok_response/1
                        , read_tx_encoding_param/1
                        , parse_filter_param/2
                        , read_optional_param/3
                        , get_block/3
                        , get_block/4
                        , get_poi/3
                        , get_block_hash_optionally_by_hash_or_height/1
                        ]).

-compile({parse_transform, lager_transform}).

-spec handle_request(
        OperationID :: atom(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request('GetTop', Req, Context) ->
    handle_request('GetTopBlock', Req, Context);

handle_request('GetTopBlock', _, _Context) ->
    {ok, TopBlock} = aehttp_logic:get_top(),
    EncodedHash = aehttp_api_parser:encode(block_hash, TopBlock),
    EncodedHeader = aehttp_api_parser:encode(header, TopBlock),
    {200, [], maps:put(<<"hash">>, EncodedHash, EncodedHeader)};

handle_request('GetCurrentKeyBlock', Req, _Context) ->
    get_block(fun() -> aec_chain:top_key_block() end, Req, json);

handle_request('GetCurrentKeyBlockHash', _, _Context) ->
    Hash = aec_chain:top_key_block_hash(),
    EncodedHash = aec_base58c:encode(block_hash, Hash),
    {200, [], #{hash => EncodedHash}};

handle_request('GetCurrentKeyBlockHeight', _, _Context) ->
    TopBlock = aec_chain:top_block(),
    Height = aec_blocks:height(TopBlock),
    {200, [], #{height => Height}};

handle_request('GetPendingKeyBlock', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_pending/0, Req, json, false);

handle_request('GetKeyBlockByHash', Params, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Params)) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aehttp_logic:get_key_block_by_hash(Hash) end, Params, json)
    end;

handle_request('GetKeyBlockByHeight', Params, _Context) ->
    Height = maps:get(height, Params),
    get_block(fun() -> aehttp_logic:get_key_block_by_height(Height) end, Params, json);

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
                    Txs = [ aetx_sign:serialize_for_client(json, Header, Tx)
                            || Tx <- aec_blocks:txs(Block)],
                    JsonTxs = #{data_schema => <<"JSONTx">>,
                                transactions => Txs},
                    {200, [], JsonTxs};
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
                            SingleTxJSON = aetx_sign:serialize_for_client(json, Header, Tx),
                            {200, [], SingleTxJSON};
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
    case aec_base58c:safe_decode(account_pubkey, maps:get(pubkey, Params)) of
        {ok, Pubkey} ->
            case aehttp_logic:get_account(Pubkey) of
                {ok, Account} ->
                    {200, [],
                     #{pubkey => aec_base58c:encode(account_pubkey, Pubkey),
                       balance => aec_accounts:balance(Account),
                       nonce => aec_accounts:nonce(Account)}};
                {error, _} ->
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
                    Txs = [aetx_sign:serialize_for_client_pending(json, T) || T <- Txs0],
                    JsonTxs = #{data_schema => <<"JSONTx">>,
                                transactions => Txs},
                    {200, [], JsonTxs};
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
                    JSONTx = aetx_sign:serialize_for_client_pending(json, Tx),
                    {200, [], JSONTx};
                {BlockHash, Tx} ->
                    {ok, Header} = aec_chain:get_header(BlockHash),
                    JSONTx = aetx_sign:serialize_for_client(json, Header, Tx),
                    {200, [], JSONTx}
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
    case aec_chain:name_entry(Name) of
        {ok, NameEntry} ->
            #{<<"name">>     := Name,
              <<"hash">>     := Hash,
              <<"name_ttl">> := NameTTL,
              <<"pointers">> := Pointers} = NameEntry,
            {200, [], #{name      => Name,
                        name_hash => aec_base58c:encode(name, Hash),
                        name_ttl  => NameTTL,
                        pointers  => Pointers}};
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

handle_request('GetBlockGenesis', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_genesis/0, Req, json);

handle_request('GetBlockLatest', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_latest/0, Req, json);

handle_request('GetKeyBlockByHeightObsolete', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block(fun() -> aehttp_logic:get_key_block_by_height(Height) end, Req, json);


handle_request('GetBlockByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aehttp_logic:get_block_by_hash(Hash) end, Req, json)
    end;

handle_request('GetHeaderByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aehttp_logic:get_block_by_hash(Hash) of
                {ok, Block} ->
                    {200, [], aehttp_api_parser:encode(header, Block)};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Header not found">>}}
            end
    end;

handle_request('GetKeyHeaderByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    case aehttp_logic:get_key_block_by_height(Height) of
        {ok, Block} ->
            Resp = aehttp_api_parser:encode(header, Block),
            lager:debug("Resp = ~p", [pp(Resp)]),
            {200, [], Resp};
        {error, chain_too_short} ->
            {400, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetTxs', _Req, _Context) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    lager:debug("GetTxs : ~p", [pp(Txs0)]),
    Txs = [aehttp_api_parser:encode(tx, T) || T <- Txs0],
    {200, [], Txs};

handle_request('PostBlock', Req, _Context) ->
    case aehttp_api_parser:decode(block, maps:get('Block', Req)) of
        {error, Reason} ->
            lager:info("Post block failed: ~p", [Reason]),
            {400, [], #{reason => <<"Block rejected">>}};
        {ok, Block} ->
            %% Only for logging
            Header = aec_blocks:to_header(Block),
            {ok, HH} = aec_headers:hash_header(Header),
            lager:debug("'PostBlock'; header hash: ~p", [HH]),
            case aec_conductor:post_block(Block) of
                ok -> {200, [], #{}};
                {error, Reason} ->
                    lager:info("Post block failed: ~p", [Reason]),
                    {400, [], #{reason => <<"Block rejected">>}}
            end
    end;

handle_request('PostTx', #{'Tx' := Tx} = Req, _Context) ->
    lager:debug("Got PostTx; Req = ~p", [pp(Req)]),
    case aehttp_api_parser:decode(tx, maps:get(<<"tx">>, Tx)) of
        {error, #{<<"tx">> := broken_tx}} ->
            {400, [], #{reason => <<"Invalid tx">>}};
        {error, _} ->
            {400, [], #{reason => <<"Invalid base58Check encoding">>}};
        {ok, SignedTx} ->
            lager:debug("deserialized: ~p", [pp(SignedTx)]),
            PushRes = aec_tx_pool:push(SignedTx),
            lager:debug("PushRes = ~p", [pp(PushRes)]),
            Hash = aetx_sign:hash(SignedTx),
            {200, [], #{<<"tx_hash">> => aec_base58c:encode(tx_hash, Hash)}}
    end;

handle_request('PostContractCreate', #{'ContractCreateData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner, code, vm_version, deposit,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{owner, owner, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(owner),
                 hexstrings_decode([code, call_data]),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        {CB, CTx} = aetx:specialize_callback(Tx),
                        ContractPubKey = CB:contract_pubkey(CTx),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_address =>
                              aec_base58c:encode(contract_pubkey, ContractPubKey)
                         }
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCreateCompute', #{'ContractCreateCompute' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner, code, vm_version, deposit,
                                       amount, gas, gas_price, fee,
                                       arguments]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{owner, owner, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(owner),
                 hexstrings_decode([code]),
                 compute_contract_create_data(),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        {CB, CTx} = aetx:specialize_callback(Tx),
                        ContractPubKey = CB:contract_pubkey(CTx),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_address =>
                              aec_base58c:encode(contract_pubkey, ContractPubKey)
                         }
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCall', #{'ContractCallData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller, contract, vm_version,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{caller, caller, {id_hash, [account_pubkey]}},
                                {contract, contract, {id_hash, [contract_pubkey]}}]),
                 get_nonce_from_account_id(caller),
                 get_contract_code(contract, contract_code),
                 hexstrings_decode([call_data]),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCallCompute', #{'ContractCallCompute' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller, contract, vm_version,
                                       amount, gas, gas_price, fee,
                                       function, arguments]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{caller, caller, {id_hash, [account_pubkey]}},
                                {contract, contract, {id_hash, [contract_pubkey]}}]),
                 get_nonce_from_account_id(caller),
                 get_contract_code(contract, contract_code),
                 compute_contract_call_data(),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleRegister', #{'OracleRegisterTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, {query_format, query_format},
                                       {response_format, response_format},
                                       query_fee, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(account_id),
                 ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_register_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleExtend', #{'OracleExtendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}}]),
                 get_nonce_from_account_id(oracle_id),
                 ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_extend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleQuery', #{'OracleQueryTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id, oracle_id, query,
                                       query_fee, fee, query_ttl, response_ttl]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                                {oracle_id, oracle_id, {id_hash, [oracle_pubkey]}}]),
                 get_nonce_from_account_id(sender_id),
                 ttl_decode(query_ttl),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_existence(oracle_id),
                 unsigned_tx_response(fun aeo_query_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleResponse', #{'OracleResponseTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, query_id, response, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}},
                                {query_id, query_id, oracle_query_id}]),
                 get_nonce_from_account_id(oracle_id),
                 verify_oracle_query_existence(oracle_id, query_id),
                 unsigned_tx_response(fun aeo_response_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNamePreclaim', #{'NamePreclaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, commitment, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account, account, {id_hash, [account_pubkey]}},
                                {commitment, commitment, {id_hash, [commitment]}}]),
                 get_nonce_from_account_id(account),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameClaim', #{'NameClaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name, name_salt, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account, account, {id_hash, [account_pubkey]}},
                                {name, name, name}]),
                 get_nonce_from_account_id(account),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameUpdate', #{'NameUpdateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, name_ttl,
                                       pointers, client_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account, account, {id_hash, [account_pubkey]}},
                                {name_hash, name_hash, {id_hash, [name]}}]),
                 nameservice_pointers_decode(pointers),
                 get_nonce_from_account_id(account),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameTransfer', #{'NameTransferTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, recipient_pubkey, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account, account, {id_hash, [account_pubkey]}},
                                {recipient_pubkey, recipient_account,
                                 {id_hash, [account_pubkey, name]}},
                                {name_hash, name_hash, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameRevoke', #{'NameRevokeTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account, account, {id_hash, [account_pubkey]}},
                                {name_hash, name_hash, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostSpend', #{'SpendTx' := Req}, _Context) ->
    AllowedRecipients = [account_pubkey, name, oracle_pubkey, contract_pubkey],
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender,
                                       {recipient_pubkey, recipient},
                                        amount, fee, payload]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{sender, sender, {id_hash, [account_pubkey]}},
                                {recipient, recipient,
                                 {id_hash, AllowedRecipients}}]),
                 get_nonce_from_account_id(sender),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelCreate', #{'ChannelCreateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([initiator, initiator_amount,
                                       state_hash,
                                       responder, responder_amount,
                                       push_amount, channel_reserve,
                                       lock_period, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{initiator, initiator, {id_hash, [account_pubkey]}},
                                {responder, responder, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}
                               ]),
                 get_nonce_from_account_id(initiator),
                 unsigned_tx_response(fun aesc_create_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelDeposit', #{'ChannelDepositTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from, from, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_deposit_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelWithdrawal', #{'ChannelWithdrawalTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, to,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {to, to, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_withdraw_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSnapshotSolo', #{'ChannelSnapshotSoloTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from,
                                       payload, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from, from, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(from),
                 unsigned_tx_response(fun aesc_snapshot_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);


handle_request('PostChannelCloseMutual', #{'ChannelCloseMutualTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}}]),
                 unsigned_tx_response(fun aesc_close_mutual_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelCloseSolo', #{'ChannelCloseSoloTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from, from, {id_hash, [account_pubkey]}},
                                {poi, poi, poi}]),
                 get_nonce_from_account_id(from),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_close_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSlash', #{'ChannelSlashTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from, from, {id_hash, [account_pubkey]}},
                                {poi, poi, poi}]),
                 get_nonce_from_account_id(from),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_slash_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSettle', #{'ChannelSettleTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from, from, {id_hash, [account_pubkey]}}]),
                 unsigned_tx_response(fun aesc_settle_tx:new/1)
                ],
    process_request(ParseFuns, Req);

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

handle_request('GetAccountBalance', Req, _Context) ->
    ParseFuns = [read_required_params([address]),
                 base58_decode([{address, address, [account_pubkey,
                                                    contract_pubkey]}]),
                 get_block_hash_optionally_by_hash_or_height(block_hash),
                 fun(_, #{address := Pubkey, block_hash := AtHash}) ->
                      case aehttp_logic:get_account_balance_at_hash(Pubkey, AtHash) of
                          {error, account_not_found} ->
                              {error, {404, [], #{reason => <<"Account not found">>}}};
                          {error, not_on_main_chain} ->
                              {error, {400, [], #{reason => <<"Block not on the main chain">>}}};
                          {ok, Balance} ->
                              {ok, {200, [], #{balance => Balance}}}
                      end
                end
                ],
    process_request(ParseFuns, Req);

handle_request('GetAccountPendingTransactions', Req, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get('account_pubkey', Req)) of
        {ok, AccountPubkey} ->
            case aec_chain:get_account(AccountPubkey) of
                {value, _} ->
                    {ok, Txs0} = aec_tx_pool:peek(infinity, AccountPubkey),
                    Txs = [aehttp_api_parser:encode(tx, T) || T <- Txs0],
                    {200, [], Txs};
                _ ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        _ ->
            {400, [], #{reason => <<"Invalid account hash">>}}
    end;

handle_request('GetAccountNonce', Req, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get('account_pubkey', Req)) of
        {ok, AccountPubkey} ->
            case aec_chain:get_account(AccountPubkey) of
                {value, Account} ->
                    {200, [], #{nonce => aec_accounts:nonce(Account)}};
                _ ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        _ ->
            {400, [], #{reason => <<"Invalid account hash">>}}
    end;

handle_request('GetCommitmentHash', Req, _Context) ->
    Name         = maps:get('name', Req),
    Salt         = maps:get('salt', Req),
    case aens:get_commitment_hash(Name, Salt) of
        {ok, CHash} ->
            EncodedCHash = aec_base58c:encode(commitment, CHash),
            {200, [], #{commitment => EncodedCHash}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

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

handle_request('GetName', Req, _Context) ->
    Name = maps:get('name', Req),
    case aec_chain:name_entry(Name) of
        {ok, NameEntry} ->
            #{<<"name">>     := Name,
              <<"hash">>     := Hash,
              <<"name_ttl">> := NameTTL,
              <<"pointers">> := Pointers} = NameEntry,
            {200, [], #{name      => Name,
                        name_hash => aec_base58c:encode(name, Hash),
                        name_ttl  => NameTTL,
                        pointers  => Pointers}};
        {error, name_not_found} ->
            {404, [], #{reason => <<"Name not found">>}};
        {error, name_revoked} ->
            {404, [], #{reason => <<"Name revoked">>}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('GetAccountsBalances', _Req, _Context) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"debug">>],
                                    aehttp, enable_debug_endpoints, false) of
        true ->
            {ok, AccountsBalances} = aehttp_logic:get_all_accounts_balances(),
            {200, [], #{accounts_balances =>
                        aehttp_api_parser:encode(account_balances,
                                                 AccountsBalances)}};
        false ->
            {403, [], #{reason => <<"Balances not enabled">>}}
    end;

handle_request('GetVersion', _Req, _Context) ->
    {ok, Version} = aehttp_logic:version(),
    {ok, Revision} = aehttp_logic:revision(),
    {ok, GenHash} = aehttp_logic:get_genesis_hash(),
    Resp = #{<<"version">> => Version,
             <<"revision">> => Revision,
             <<"genesis_hash">> => GenHash},
    {200, [], aehttp_api_parser:encode(node_version, Resp)};

handle_request('GetInfo', _Req, _Context) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"debug">>],
                                    aehttp, enable_debug_endpoints, false) of
        true ->
            {ok, TimeSummary} = aehttp_logic:get_top_blocks_time_summary(30),
            {200, [], #{last_30_blocks_time => TimeSummary}};
        false ->
            {403, [], #{reason => <<"Info not enabled">>}}
    end;

handle_request('CompileContract', Req, _Context) ->
    case Req of
        #{'Contract' :=
              #{ <<"code">> := Code
               , <<"options">> := Options }} ->
            %% TODO: Handle other languages
            case aehttp_logic:contract_compile(Code, Options) of
                 {ok, ByteCode} ->
                     {200, [], #{ bytecode => ByteCode}};
                 {error, ErrorMsg} ->
                     {403, [], #{reason => ErrorMsg}}
             end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('CallContract', Req, _Context) ->
    case Req of
        #{'ContractCallInput' :=
              #{ <<"abi">> := ABI
               , <<"code">> := Code
               , <<"function">> := Function
               , <<"arg">> := Argument }}  ->
            case aehttp_logic:contract_call(ABI, Code, Function, Argument) of
                {ok, Result} ->
                    {200, [], #{ out => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('EncodeCalldata', Req, _Context) ->
    case Req of
        #{'ContractCallInput' :=
              #{ <<"abi">>  := ABI
               , <<"code">> := Code
               , <<"function">> := Function
               , <<"arg">> := Argument }} ->
            %% TODO: Handle other languages
            case aehttp_logic:contract_encode_call_data(ABI, Code, Function, Argument) of
                {ok, Result} ->
                    {200, [], #{ calldata => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('DecodeData', Req, _Context) ->
    case Req of
        #{'SophiaBinaryData' :=
              #{ <<"sophia-type">>  := Type
               , <<"data">>  := Data
               }} ->
            case aehttp_logic:contract_decode_data(Type, Data) of
                {ok, Result} ->
                    {200, [], #{ data => Result}};
                {error, ErrorMsg} ->
                    {400, [], #{reason => ErrorMsg}}
            end
    end;

handle_request('GetTx', Req, _Context) ->
    ParseFuns = [read_required_params([tx_hash]),
                 read_optional_params([{tx_encoding, tx_encoding, message_pack}]),
                 parse_tx_encoding(tx_encoding),
                 base58_decode([{tx_hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 encode_transaction(tx, tx_encoding, encoded_tx),
                 ok_response(
                    fun(#{encoded_tx := #{tx := Tx, schema := Schema}}) ->
                        #{transaction => Tx,
                          data_schema => Schema}
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
              key_block => aehttp_api_parser:encode_client_readable_key_block(KeyBlock, json),
              micro_blocks => [ aehttp_api_parser:encode(block_hash, Micro) || Micro <- MicroBlocks ]
            },
            {200, [], Struct}
    end.

