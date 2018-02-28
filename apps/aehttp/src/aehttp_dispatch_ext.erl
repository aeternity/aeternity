-module(aehttp_dispatch_ext).

-export([handle_request/3]).
-export([cleanup_genesis/1,
         add_missing_to_genesis_block/1]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ process_request/2
                        , read_required_params/1
                        , read_optional_params/1
                        , parse_map_to_atom_keys/0
                        , base58_decode/1
                        , hexstrings_decode/1
                        , nameservice_pointers_decode/1
                        , get_nonce/1
                        , print_state/0
                        , get_contract_code/2
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , verify_name/1
                        , ttl_decode/1
                        , parse_tx_encoding/1
                        , compute_contract_call_data/0
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/3
                        , ok_response/1
                        ]).

-compile({parse_transform, lager_transform}).
-include_lib("aecore/include/common.hrl").

-spec handle_request(
        OperationID :: swagger_api:operation_id(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: #{}}.

handle_request('Ping', #{'Ping' := PingObj}, _Context) ->
    handle_ping(PingObj);

handle_request('GetTop', _, _Context) ->
    Header = aec_chain:top_header(),
    {ok, HH} = aec_headers:hash_header(Header),
    {ok, Top} = aec_headers:serialize_to_map(Header),
    Resp = cleanup_genesis(Top),
    {200, [], maps:put(hash, aec_base58c:encode(block_hash, HH), Resp)};

handle_request('GetBlockByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    case aec_chain:get_block_by_height(Height) of
        {ok, Block} ->
            %% swagger generated code expects the Resp to be proplist or map
            %% and always runs jsx:encode/1 on it - even if it is already
            %% encoded to a binary; that's why we use
            %% aec_blocks:serialize_to_map/1
            Resp = cleanup_genesis(aec_blocks:serialize_to_map(Block)),
            lager:debug("Resp = ~p", [pp(Resp)]),
            {200, [], Resp};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetBlockByHash' = _Method, Req, _Context) ->
    lager:debug("got ~p; Req = ~p", [_Method, pp(Req)]),
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aec_chain:get_block(Hash) of
                {ok, Block} ->
                    %% swagger generated code expects the Resp to be proplist
                    %% or map and always runs jsx:encode/1 on it - even if it
                    %% is already encoded to a binary; that's why we use
                    %% aec_blocks:serialize_to_map/1
                    lager:debug("Block = ~p", [pp(Block)]),
                    Resp =
                        cleanup_genesis(aec_blocks:serialize_to_map(Block)),
                    lager:debug("Resp = ~p", [pp(Resp)]),
                    {200, [], Resp};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request('GetHeaderByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aec_chain:get_header(Hash) of
                {ok, Header} ->
                    %% We serialize to a map because the client expects a
                    %% decoded JSON object as response.
                    lager:debug("Header = ~p", [pp(Header)]),
                    {ok, HH} = aec_headers:serialize_to_map(Header),
                    Resp = cleanup_genesis(HH),
                    lager:debug("Resp = ~p", [pp(Resp)]),
                    {200, [], Resp};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request('GetTxs', _Req, _Context) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    lager:debug("GetTxs : ~p", [pp(Txs0)]),
    Txs = [#{<<"tx">> => aec_base58c:encode(
                           transaction,
                           aetx_sign:serialize_to_binary(T))}
           || T <- Txs0],
    {200, [], Txs};

handle_request('PostBlock', Req, _Context) ->
    SerializedBlock = add_missing_to_genesis_block(maps:get('Block', Req)),
    {ok, Block} = aec_blocks:deserialize_from_map(SerializedBlock),

    %% Only for logging
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    lager:debug("'PostBlock'; header hash: ~p", [HH]),
    case aec_conductor:post_block(Block) of
        ok -> {200, [], #{}};
        {error, Reason} ->
            lager:error("Post block failed: ~p", [Reason]),
            {400, [], #{reason => <<"Block rejected">>}}
    end;

handle_request('PostTx', #{'Tx' := Tx} = Req, _Context) ->
    lager:debug("Got PostTx; Req = ~p", [pp(Req)]),
    case aec_base58c:safe_decode(transaction, maps:get(<<"tx">>, Tx)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid base58Check encoding">>}};
        {ok, DecodedTx} ->
            DeserializedTx =
                try {ok, aetx_sign:deserialize_from_binary(DecodedTx)}
                catch _:_ -> {error, broken_tx}
                end,
            case DeserializedTx of
                {error, broken_tx} ->
                    {400, [], #{reason => <<"Invalid tx">>}};
                {ok, SignedTx} ->
                    lager:debug("deserialized: ~p", [pp(SignedTx)]),
                    PushRes = aec_tx_pool:push(SignedTx, tx_received),
                    lager:debug("PushRes = ~p", [pp(PushRes)]),
                    {200, [], #{}}
            end
    end;

handle_request('PostContractCreate', #{'ContractCreateData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner, code, vm_version, deposit,
                                       amount, gas, gas_price, fee,
                                       call_data]),
                 base58_decode([{owner, owner, account_pubkey}]),
                 get_nonce(owner),
                 hexstrings_decode([code, call_data]),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        #{owner := Owner, nonce := Nonce} = Data,
                        ContractPubKey =
                            aect_contracts:compute_contract_pubkey(Owner, Nonce),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_address => ContractPubKey,
                          tx_hash => aec_base58c:encode(tx_hash, aetx:hash(Tx))}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCall', #{'ContractCallData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller, contract, vm_version,
                                       amount, gas, gas_price, fee,
                                       call_data]),
                 base58_decode([{caller, caller, account_pubkey}]),
                 get_nonce(caller),
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
                 base58_decode([{caller, caller, account_pubkey}]),
                 get_nonce(caller),
                 get_contract_code(contract, contract_code),
                 compute_contract_call_data(),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleRegister', #{'OracleRegisterTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, {query_format, query_spec},
                                       {response_format, response_spec},
                                       query_fee, fee, ttl]),
                 base58_decode([{account, account, account_pubkey}]),
                 get_nonce(account),
                 ttl_decode(ttl),
                 unsigned_tx_response(fun aeo_register_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleExtend', #{'OracleExtendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle, fee, ttl]),
                 base58_decode([{oracle, oracle, oracle_pubkey}]),
                 get_nonce(oracle),
                 ttl_decode(ttl),
                 unsigned_tx_response(fun aeo_extend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleQuery', #{'OracleQueryTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender, oracle_pubkey, query,
                                       query_fee, fee, query_ttl, response_ttl]),
                 base58_decode([{sender, sender, account_pubkey},
                               {oracle_pubkey, oracle, oracle_pubkey}]),
                 get_nonce(sender),
                 ttl_decode(query_ttl),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_existence(oracle),
                 unsigned_tx_response(fun aeo_query_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleResponse', #{'OracleResponseTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle, query_id,
                                       response, fee]),
                 base58_decode([{oracle, oracle, account_pubkey},
                               {query_id, query_id, oracle_query_id}]),
                 get_nonce(oracle),
                 verify_oracle_query_existence(oracle, query_id),
                 unsigned_tx_response(fun aeo_response_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNamePreclaim', #{'NamePreclaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, commitment, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {commitment, commitment, commitment}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameClaim', #{'NameClaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name, name_salt, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name, name, name}]),
                 get_nonce(account),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameUpdate', #{'NameUpdateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, name_ttl,
                                       pointers, ttl, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 nameservice_pointers_decode(pointers),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameTransfer', #{'NameTransferTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, recipient_pubkey,
                                       fee]),
                 base58_decode([{account, account, account_pubkey},
                                {recipient_pubkey, recipient_account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameRevoke', #{'NameRevokeTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostSpend', #{'SpendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender,
                                       {recipient_pubkey, recipient},
                                       amount, fee]),
                 base58_decode([{sender, sender, account_pubkey},
                                {recipient, recipient, account_pubkey}]),
                 get_nonce(sender),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('GetAccountBalance', Req, _Context) ->
    Decoded =
      case maps:get('pub_key', Req) of
          undefined ->
              {ok, PK} = aec_keys:pubkey(),
              {ok, PK};
          PK when is_binary(PK) ->
              aec_base58c:safe_decode(account_pubkey, PK)
      end,
    case Decoded of
        {error, _} ->
            {400, [], #{reason => <<"Invalid address">>}};
        {ok, Pubkey} when is_binary(Pubkey) ->
            case aec_chain:get_account(Pubkey) of
                {value, A} ->
                    {200, [], #{balance => aec_accounts:balance(A)}};
                none ->
                    {404, [], #{reason => <<"Account not found">>}}
            end
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
    case application:get_env(aehttp, enable_debug_endpoints, false) of
        true ->
            {ok, AccountsBalances} =
                aec_chain:all_accounts_balances_at_hash(
                  aec_chain:top_block_hash()),
            FormattedAccountsBalances =
                lists:foldl(
                  fun({Pubkey, Balance}, Acc) ->
                          [#{pub_key => aec_base58c:encode(
                                          account_pubkey, Pubkey),
                             balance => Balance} | Acc]
                  end, [], AccountsBalances),
            {200, [], #{accounts_balances => FormattedAccountsBalances}};
        false ->
            {403, [], #{reason => <<"Balances not enabled">>}}
    end;

handle_request('GetVersion', _Req, _Context) ->
    {200, [], #{version => aeu_info:get_version(),
                revision => aeu_info:get_revision(),
                genesis_hash => aec_base58c:encode(
                                  block_hash,
                                  aec_chain:genesis_hash())}};

handle_request('GetInfo', _Req, _Context) ->
    case application:get_env(aehttp, enable_debug_endpoints, false) of
        true ->
            TimeSummary0 = aec_chain:get_top_N_blocks_time_summary(30),
            TimeSummary =
                lists:foldl(
                  fun({Height, Ts, Delta, Difficulty}, Acc) ->
                          [#{height => Height,
                            time => Ts,
                            difficulty => Difficulty,
                            time_delta_to_parent => Delta} | Acc];
                    ({Height, Ts, Difficulty}, Acc) ->
                          [#{height => Height,
                            time => Ts,
                            difficulty => Difficulty} | Acc]
                  end, [], TimeSummary0),
            {200, [], #{last_30_blocks_time => lists:reverse(TimeSummary)}};
        false ->
            {403, [], #{reason => <<"Info not enabled">>}}
    end;

handle_request('CompileContract', Req, _Context) ->
    case Req of
        #{'Contract' :=
              #{ <<"code">> := Code
               , <<"options">> := Options }} ->
            %% TODO: Handle other languages
            case aect_ring:compile(Code, Options) of
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
            case aect_dispatch:call(ABI, Code, Function, Argument) of
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
            case aect_dispatch:encode_call_data(ABI, Code, Function, Argument) of
                {ok, Result} ->
                    {200, [], #{ calldata => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
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

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.


mk_num(undefined) ->
    undefined;
mk_num(I) when is_integer(I) ->
    I;
mk_num(B) when is_binary(B) ->
    try binary_to_integer(B)
    catch
	error:_ ->
	    undefined
    end.

empty_fields_in_genesis() ->
    [ <<"prev_hash">>,
      <<"pow">>,
      <<"txs_hash">>,
      <<"transactions">>].

% assuming no transactions in genesis block
% if this changes - both functions should be changes:
% empty_fields_in_genesis/0 and values_for_empty_fields_in_genesis/0
values_for_empty_fields_in_genesis() ->
    true = lists:member(<<"transactions">>, empty_fields_in_genesis()),
    #{<<"prev_hash">> => aec_base58c:encode(
                           block_hash, aec_block_genesis:prev_hash()),
      <<"pow">> => aec_headers:serialize_pow_evidence(aec_block_genesis:pow()),
      <<"txs_hash">> => aec_base58c:encode(
                          block_tx_hash, aec_block_genesis:txs_hash()),
      <<"transactions">> => aec_block_genesis:transactions()}.

%% to be used for both headers and blocks
cleanup_genesis(#{<<"height">> := 0} = Genesis) ->
    maps:without(empty_fields_in_genesis(), Genesis);
cleanup_genesis(Val) ->
    Val.

add_missing_to_genesis_block(#{<<"height">> := 0} = Block) ->
    maps:merge(Block, values_for_empty_fields_in_genesis());
add_missing_to_genesis_block(Val) ->
    Val.

handle_ping(#{<<"source">> := Src} = PingObj) ->
    %% Source only contains host and port
    %% If the http API version differs, then response ping will go
    %% to wrong endpoint and eventually timeout.
    IsBlocked = aec_peers:is_blocked(Src),
    case IsBlocked of
        false -> handle_ping_(Src, PingObj);
        true  ->
            %% invalid Source URIs are by definition blocked
            abort_sync(Src, 403, <<"Not allowed">>)
    end.

handle_ping_(Source, PingObj) ->
    LocalPingObj = aec_sync:local_ping_object(),
    case PingObj of
      #{<<"genesis_hash">> := EncRemoteGHash,
        <<"best_hash">> := EncRemoteTopHash,
        <<"share">> := Share} ->
            case {aec_base58c:safe_decode(block_hash, EncRemoteGHash),
                  aec_base58c:safe_decode(block_hash, EncRemoteTopHash)} of
                {{ok, RemoteGHash}, {ok, RemoteTopHash}} ->
                    RemoteObj = PingObj#{<<"genesis_hash">> => RemoteGHash,
                                         <<"best_hash">>  => RemoteTopHash},
                    lager:debug("ping received (~p): ~p", [Source, RemoteObj]),
                    case aec_sync:compare_ping_objects(
                           Source, LocalPingObj, RemoteObj) of
                        ok ->
                            aec_peers:update_last_seen(Source),
                            TheirPeers = maps:get(<<"peers">>, RemoteObj, []),
                            aec_peers:add_and_ping_peers(TheirPeers),
                            LocalGHash =  maps:get(<<"genesis_hash">>, LocalPingObj),
                            LocalTopHash =  maps:get(<<"best_hash">>, LocalPingObj),
                            Map = LocalPingObj#{<<"pong">> => <<"pong">>,
                                                <<"genesis_hash">> => aec_base58c:encode(block_hash,LocalGHash),
                                                <<"best_hash">> => aec_base58c:encode(block_hash,LocalTopHash)
                                               },
                            Res =
                                case mk_num(Share) of
                                    N when is_integer(N), N > 0 ->
                                        Peers = aec_peers:get_random(N, [Source|TheirPeers]),
                                        lager:debug("PeerUris = ~p~n", [Peers]),
                                        Map#{<<"peers">> => Peers};
                                    _ ->
                                        Map
                                end,
                            {200, [], Res};
                        {error, different_genesis_blocks} ->
                            abort_ping(Source)
                    end;
                _ ->
                    abort_ping(Source)
            end;
        _ ->
          %% violation of protocol
          abort_ping(Source)
    end.

abort_ping(Source) ->
    aec_peers:block_peer(Source),
    abort_sync(Source, 409, <<"Different genesis blocks">>).

abort_sync(Uri, Code, Reason) ->
    aec_events:publish(
      chain_sync,
      {sync_aborted, #{uri => Uri,
                       reason => Reason}}),
      {Code, [], #{reason => Reason}}.

