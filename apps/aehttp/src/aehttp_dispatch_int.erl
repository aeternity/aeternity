-module(aehttp_dispatch_int).

-export([forbidden/1]).
-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ parse_map_to_atom_keys/0
                        , read_required_params/1
                        , read_optional_params/1
                        , api_decode/1
                        , api_optional_decode/1
                        , api_conditional_decode/1
                        , get_nonce_from_account_id/1
                        , get_contract_code/2
                        , verify_name/1
                        , nameservice_pointers_decode/1
                        , ttl_decode/1
                        , relative_ttl_decode/1
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , poi_decode/1
                        , contract_bytearray_params_decode/1
                        , unsigned_tx_response/1
                        , ok_response/1
                        , process_request/2
                        , do_dry_run/0
                        , dry_run_results/1
                        ]).

-include("../../aecontract/include/aecontract.hrl").

-spec forbidden( OperationID :: atom() ) -> boolean().
forbidden(OpId) ->
    OpSpec = endpoints:operation(OpId),
    [ #{ tags := Tags } | _ ] = maps:values(OpSpec),
    case lists:member(<<"debug">>, Tags) of
        true -> not aehttp_app:enable_internal_debug_endpoints();
        false -> false
    end.

-spec handle_request(
        OperationID :: atom(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request(OperationID, Req, Context) ->
    try aec_jobs_queues:run(http_update,
                            fun() -> handle_request_(OperationID, Req, Context) end)
    catch
        error:{rejected, _} ->
            {503, [], #{reason => <<"Temporary overload">>}}
    end.

handle_request_('PostKeyBlock', #{'KeyBlock' := Data}, _Context) ->
    case aec_headers:deserialize_from_client(key, Data) of
        {ok, Header} ->
            KeyBlock = aec_blocks:new_key_from_header(Header),
            case aec_conductor:post_block(KeyBlock) of
                ok ->
                    {200, [], #{}};
                {error, _Rsn} ->
                    {400, [], #{reason => <<"Block rejected">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid block">>}}
    end;

handle_request_('PostSpend', #{'SpendTx' := Req}, _Context) ->
    AllowedRecipients = [account_pubkey, name, oracle_pubkey, contract_pubkey],
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id, recipient_id, amount, fee, payload]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                             {recipient_id, recipient_id, {id_hash, AllowedRecipients}}]),
                 api_optional_decode([{payload, bytearray}]),
                 get_nonce_from_account_id(sender_id),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostContractCreate', #{'ContractCreateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner_id, code, vm_version, abi_version, deposit,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{owner_id, owner_id, {id_hash, [account_pubkey]}}]),
                 get_nonce_from_account_id(owner_id),
                 contract_bytearray_params_decode([code, call_data]),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        {CB, CTx} = aetx:specialize_callback(Tx),
                        ContractPubKey = CB:contract_pubkey(CTx),
                        #{tx => aeser_api_encoder:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_id =>
                              aeser_api_encoder:encode(contract_pubkey, ContractPubKey)
                         }
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostContractCall', #{'ContractCallTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller_id, contract_id, abi_version,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{caller_id, caller_id, {id_hash, [account_pubkey]}},
                                {contract_id, contract_id, {id_hash, [contract_pubkey]}}]),
                 get_nonce_from_account_id(caller_id),
                 get_contract_code(contract_id, contract_code),
                 contract_bytearray_params_decode([call_data]),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('DryRunTxs', #{ 'DryRunInput' := Req }, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([txs]),
                 read_optional_params([{top, top, top}, {accounts, accounts, []}]),
                 do_dry_run()],
    process_request(ParseFuns, Req);

handle_request_('PostNamePreclaim', #{'NamePreclaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, commitment_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {commitment_id, commitment_id, {id_hash, [commitment]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostNameUpdate', #{'NameUpdateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, name_ttl,
                                       pointers, client_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 nameservice_pointers_decode(pointers),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostNameClaim', #{'NameClaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name, name_salt, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name, name, name}]),
                 get_nonce_from_account_id(account_id),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostNameTransfer', #{'NameTransferTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, recipient_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {recipient_id, recipient_id,
                                 {id_hash, [account_pubkey, name]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostNameRevoke', #{'NameRevokeTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelCreate', #{'ChannelCreateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([initiator_id, initiator_amount,
                                       state_hash,
                                       responder_id, responder_amount,
                                       push_amount, channel_reserve,
                                       lock_period, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'},
                                       {delegate_ids, delegate_ids, []}]),
                 api_decode([{initiator_id, initiator_id, {id_hash, [account_pubkey]}},
                              {responder_id, responder_id, {id_hash, [account_pubkey]}},
                              {state_hash, state_hash, state},
                              {delegate_ids, delegate_ids, {list, {id_hash, [account_pubkey]}}}
                              ]),
                 get_nonce_from_account_id(initiator_id),
                 unsigned_tx_response(fun aesc_create_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelDeposit', #{'ChannelDepositTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_deposit_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelWithdraw', #{'ChannelWithdrawTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, to_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {to_id, to_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 unsigned_tx_response(fun aesc_withdraw_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelSnapshotSolo', #{'ChannelSnapshotSoloTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {payload, payload, bytearray}]),
                 get_nonce_from_account_id(from_id),
                 unsigned_tx_response(fun aesc_snapshot_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelCloseMutual', #{'ChannelCloseMutualTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 unsigned_tx_response(fun aesc_close_mutual_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelCloseSolo', #{'ChannelCloseSoloTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {poi, poi, poi}, {payload, payload, bytearray}]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_close_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelSlash', #{'ChannelSlashTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {poi, poi, poi}, {payload, payload, bytearray}]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_slash_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostChannelSettle', #{'ChannelSettleTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 unsigned_tx_response(fun aesc_settle_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostOracleRegister', #{'OracleRegisterTx' := Req}, _Context) ->
    IsVmABI = fun(Data) -> maps:get(abi_version, Data, 0) == ?ABI_AEVM_SOPHIA_1 end,
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, query_format, response_format,
                                       query_fee, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}, {abi_version, abi_version, 0}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}}]),
                 api_conditional_decode([{query_format, {contract_bytearray, IsVmABI}},
                                         {response_format, {contract_bytearray, IsVmABI}}]),
                 get_nonce_from_account_id(account_id),
                 ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_register_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostOracleExtend', #{'OracleExtendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}}]),
                 get_nonce_from_account_id(oracle_id),
                 relative_ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_extend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostOracleQuery', #{'OracleQueryTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id, oracle_id, query,
                                       query_fee, fee, query_ttl, response_ttl]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                                {oracle_id, oracle_id, {id_hash, [oracle_pubkey]}}]),
                 get_nonce_from_account_id(sender_id),
                 api_optional_decode([{query, contract_bytearray}]),
                 ttl_decode(query_ttl),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_existence(oracle_id),
                 unsigned_tx_response(fun aeo_query_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('PostOracleRespond', #{'OracleRespondTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, query_id, response, response_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}},
                                {query_id, query_id, oracle_query_id}]),
                 api_optional_decode([{response, contract_bytearray}]),
                 get_nonce_from_account_id(oracle_id),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_query_existence(oracle_id, query_id),
                 unsigned_tx_response(fun aeo_response_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request_('GetNodeBeneficiary', _, _Context) ->
    case aec_conductor:get_beneficiary() of
        {ok, PubKey} ->
            {200, [], #{pub_key => aeser_api_encoder:encode(account_pubkey, PubKey)}};
        {error, Reason} ->
            {404, [], #{reason =>  atom_to_binary(Reason, utf8)}}
    end;

handle_request_('GetNodePubkey', _, _Context) ->
    case aec_keys:pubkey() of
        {ok, Pubkey} ->
            %% TODO: rename pub_key to pubkey
            {200, [], #{pub_key => aeser_api_encoder:encode(account_pubkey, Pubkey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Public key not found">>}}
    end;

handle_request_('GetCommitmentId', Req, _Context) ->
    Name         = maps:get('name', Req),
    Salt         = maps:get('salt', Req),
    case aens:get_commitment_hash(Name, Salt) of
        {ok, CHash} ->
            EncodedCHash = aeser_api_encoder:encode(commitment, CHash),
            {200, [], #{commitment_id => EncodedCHash}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request_('GetPendingTransactions', _Req, _Context) ->
    {ok, Txs} = aec_tx_pool:peek(infinity),
    {200, [], #{transactions => [aetx_sign:serialize_for_client_pending(T) || T <- Txs]}};

handle_request_('GetPeers', _Req, _Context) ->
    Peers = aehttp_logic:connected_peers(all),
    InboundPeers = aehttp_logic:connected_peers(inbound),
    OutboundPeers = aehttp_logic:connected_peers(outbound),
    Blocked = aehttp_logic:blocked_peers(),

    {200, [], #{peers => lists:map(fun aec_peers:encode_peer_address/1, Peers),
                inbound => lists:map(fun aec_peers:encode_peer_address/1, InboundPeers),
                outbound => lists:map(fun aec_peers:encode_peer_address/1, OutboundPeers),
                blocked => lists:map(fun aec_peers:encode_peer_address/1, Blocked)}};

handle_request_('GetTokenSupplyByHeight', Req, _Context) ->
    Height = maps:get(height, Req),
    case aec_chain:sum_tokens_at_height(Height) of
        {ok, Result} ->
            {200, [], Result};
        {error, chain_too_short} ->
            {400, [], #{reason => <<"Chain too short">>}}
    end;

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

