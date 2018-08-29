-module(aehttp_dispatch_int).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ parse_filter_param/2
                        , get_block/2
                        , get_block/3
                        , get_block_from_chain/1
                        , parse_map_to_atom_keys/0
                        , read_required_params/1
                        , read_optional_params/1
                        , base58_decode/1
                        , get_nonce_from_account_id/1
                        , get_contract_code/2
                        , compute_contract_create_data/0
                        , compute_contract_call_data/0
                        , verify_name/1
                        , nameservice_pointers_decode/1
                        , ttl_decode/1
                        , relative_ttl_decode/1
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , poi_decode/1
                        , hexstrings_decode/1
                        , unsigned_tx_response/1
                        , ok_response/1
                        , process_request/2
                        ]).

-spec handle_request(
        OperationID :: atom(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request('PostSpend', #{'SpendTx' := Req}, _Context) ->
    AllowedRecipients = [account_pubkey, name, oracle_pubkey, contract_pubkey],
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id,
                                       {recipient_id, recipient_id},
                                        amount, fee, payload]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                                {recipient_id, recipient_id,
                                 {id_hash, AllowedRecipients}}]),
                 get_nonce_from_account_id(sender_id),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCreate', #{'ContractCreateData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner_id, code, vm_version, deposit,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{owner_id, owner_id, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(owner_id),
                 hexstrings_decode([code, call_data]),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        {CB, CTx} = aetx:specialize_callback(Tx),
                        ContractPubKey = CB:contract_pubkey(CTx),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_id =>
                              aec_base58c:encode(contract_pubkey, ContractPubKey)
                         }
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCreateCompute', #{'ContractCreateCompute' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner_id, code, vm_version, deposit,
                                       amount, gas, gas_price, fee,
                                       arguments]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{owner_id, owner_id, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(owner_id),
                 hexstrings_decode([code]),
                 compute_contract_create_data(),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        {CB, CTx} = aetx:specialize_callback(Tx),
                        ContractPubKey = CB:contract_pubkey(CTx),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_id =>
                              aec_base58c:encode(contract_pubkey, ContractPubKey)
                         }
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCall', #{'ContractCallData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller_id, contract_id, vm_version,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{caller_id, caller_id, {id_hash, [account_pubkey]}},
                                {contract_id, contract_id, {id_hash, [contract_pubkey]}}]),
                 get_nonce_from_account_id(caller_id),
                 get_contract_code(contract_id, contract_code),
                 hexstrings_decode([call_data]),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCallCompute', #{'ContractCallCompute' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller_id, contract_id, vm_version,
                                       amount, gas, gas_price, fee,
                                       function, arguments]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{caller_id, caller_id, {id_hash, [account_pubkey]}},
                                {contract_id, contract_id, {id_hash, [contract_pubkey]}}]),
                 get_nonce_from_account_id(caller_id),
                 get_contract_code(contract_id, contract_code),
                 compute_contract_call_data(),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

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

handle_request('PostNamePreclaim', #{'NamePreclaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, commitment_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {commitment_id, commitment_id, {id_hash, [commitment]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameUpdate', #{'NameUpdateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, name_ttl,
                                       pointers, client_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 nameservice_pointers_decode(pointers),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameClaim', #{'NameClaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name, name_salt, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name, name, name}]),
                 get_nonce_from_account_id(account_id),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameTransfer', #{'NameTransferTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, recipient_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {recipient_id, recipient_id,
                                 {id_hash, [account_pubkey, name]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameRevoke', #{'NameRevokeTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelCreate', #{'ChannelCreateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([initiator_id, initiator_amount,
                                       state_hash,
                                       responder_id, responder_amount,
                                       push_amount, channel_reserve,
                                       lock_period, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{initiator_id, initiator_id, {id_hash, [account_pubkey]}},
                                {responder_id, responder_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}
                               ]),
                 get_nonce_from_account_id(initiator_id),
                 unsigned_tx_response(fun aesc_create_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelDeposit', #{'ChannelDepositTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_deposit_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelWithdraw', #{'ChannelWithdrawTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, to_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {to_id, to_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_withdraw_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSnapshotSolo', #{'ChannelSnapshotSoloTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(from_id),
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
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}},
                                {poi, poi, poi}]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_close_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSlash', #{'ChannelSlashTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}},
                                {poi, poi, poi}]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_slash_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostChannelSettle', #{'ChannelSettleTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 base58_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 unsigned_tx_response(fun aesc_settle_tx:new/1)
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

handle_request('PostOracleRespond', #{'OracleRespondTx' := Req}, _Context) ->
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

handle_request('GetNodePubkey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            %% TODO: rename pub_key to pubkey
            {200, [], #{pub_key => aec_base58c:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Public key not found">>}}
    end;

handle_request('GetCommitmentId', Req, _Context) ->
    Name         = maps:get('name', Req),
    Salt         = maps:get('salt', Req),
    case aens:get_commitment_hash(Name, Salt) of
        {ok, CHash} ->
            EncodedCHash = aec_base58c:encode(commitment, CHash),
            {200, [], #{commitment_id => EncodedCHash}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('GetPendingTransactions', _Req, _Context) ->
    {ok, Txs} = aec_tx_pool:peek(infinity),
    {200, [], #{transactions => [aetx_sign:serialize_for_client_pending(T) || T <- Txs]}};

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

