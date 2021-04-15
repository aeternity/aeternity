-module(aehttp_dispatch_int).

-export([forbidden/2]).
-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ parse_map_to_atom_keys/0
                        , read_required_params/1
                        , read_optional_params/1
                        , api_decode/1
                        , api_optional_decode/1
                        , api_conditional_decode/1
                        , api_str_to_int/1
                        , get_nonce_from_account_id/1
                        , get_contract_code/2
                        , verify_name/1
                        , nameservice_pointers_decode/1
                        , ttl_decode/1
                        , relative_ttl_decode/1
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , poi_decode/1
                        , delegates_decode/1
                        , contract_bytearray_params_decode/1
                        , unsigned_tx_response/1
                        , ok_response/1
                        , process_request/2
                        , do_dry_run/0
                        , dry_run_results/1
                        , decode_transaction/1
                        ]).

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).

-export([patterns/0]).

-include("../../aecontract/include/aecontract.hrl").

patterns() ->
    [{?MODULE, F, A, []} || {F, A} <- [ {handle_request, 3}
				      , {forbidden, 2}
				      , {ok_response, 1}
				      , {process_request, 2}
				      , {dry_run_results, 1}]].

-spec forbidden( Mod :: module(), OperationID :: atom() ) -> boolean().
forbidden(Mod, OpId) ->
    OpSpec = Mod:operation(OpId),
    [ #{ tags := Tags } | _ ] = maps:values(OpSpec),
    case lists:member(<<"debug">>, Tags) of
        true -> not aehttp_app:enable_internal_debug_endpoints();
        false -> false
    end.

queue('GetNetworkStatus') -> ?READ_Q;
queue(_)                  -> ?WRITE_Q.

-spec handle_request(
        OperationID :: atom(),
        Req :: map(),
        Context :: #{}
                   ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request(OperationID, Req, Context) ->
    try aec_jobs_queues:run(queue(OperationID),
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

handle_request_(OperationId, Req, _Context)
    when  OperationId =:= 'PostSpend';
          OperationId =:= 'PostContractCreate';
          OperationId =:= 'PostContractCall';
          OperationId =:= 'PostNamePreclaim';
          OperationId =:= 'PostNameUpdate';
          OperationId =:= 'PostNameClaim';
          OperationId =:= 'PostNameTransfer';
          OperationId =:= 'PostNameRevoke';
          OperationId =:= 'PostChannelCreate';
          OperationId =:= 'PostChannelDeposit';
          OperationId =:= 'PostChannelWithdraw';
          OperationId =:= 'PostChannelSnapshotSolo';
          OperationId =:= 'PostChannelSetDelegates';
          OperationId =:= 'PostChannelCloseMutual';
          OperationId =:= 'PostChannelCloseSolo';
          OperationId =:= 'PostChannelSlash';
          OperationId =:= 'PostChannelSettle';
          OperationId =:= 'PostOracleRegister';
          OperationId =:= 'PostOracleExtend';
          OperationId =:= 'PostOracleQuery';
          OperationId =:= 'PostOracleRespond' ->
    SwaggerName = tx_swagger_name_from_operation_id(OperationId),
    SwaggerNameBin = atom_to_binary(SwaggerName, utf8),
    TxType = aetx:swagger_name_to_type(SwaggerNameBin),
    TxCreateData = maps:get(SwaggerName, Req),
    produce_tx(TxType, TxCreateData);
handle_request_('DryRunTxs', #{ 'DryRunInput' := Req }, _Context) ->
    lager:debug("DryRunInput := ~p", [Req]),
    produce_tx(dry_run_txs, Req);
handle_request_('PostPayingFor', #{'PayingForTx' := Req}, _Context) ->
    ParseFuns = [ parse_map_to_atom_keys(),
                  read_required_params([payer_id, fee, tx]),
                  api_decode([{payer_id, payer_id, {id_hash, [account_pubkey]}}]),
                  get_nonce_from_account_id(payer_id),
                  api_str_to_int([nonce, fee]),
                  fun(#{tx := #{ tx := InnerTxReq, signatures := Sigs}}, State) ->
                      #{type := InnerTxSwaggerNameBin} = InnerTxReq,
                      InnerTxType = aetx:swagger_name_to_type(InnerTxSwaggerNameBin),
                      case produce_tx(InnerTxType, InnerTxReq) of
                          {200, [], #{tx := TxBin}} ->
                              {ok, Tx} =
                                  case aeser_api_encoder:safe_decode(transaction, TxBin) of
                                      {error, _} = E -> E;
                                      {ok, TxDec} ->
                                          try {ok, aetx:deserialize_from_binary(TxDec)}
                                          catch 
                                              _:_ -> error
                                          end
                                  end,
                              Signatures =
                                  lists:map(
                                      fun(S) ->
                                          {ok, B} = aeser_api_encoder:safe_decode(signature, S),
                                          B
                                      end, Sigs),
                              InnerTx = aetx_sign:new(Tx, Signatures),
                              {Env0, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
                              {account, PayerPubKey} = aeser_id:specialize(maps:get(payer_id, State)),
                              Env = aetx_env:set_payer(Env0, PayerPubKey),
                              case aetx_sign:verify_w_env(InnerTx, Trees, Env) of
                                  ok ->
                                      {ok, State#{tx => InnerTx}};
                                  {error, _} ->
                                      {error, {400, [], #{<<"reason">> => <<"Inner tx: invalid authentication">>}}}
                              end;
                          {error, {400, [],  #{<<"reason">> := R}}} ->
                              {error, {400, [], #{<<"reason">> => <<"Inner tx: ", R/binary>>}}}
                      end
                  end,
                  unsigned_tx_response(fun aec_paying_for_tx:new/1)
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
    Height = aehttp_helpers:to_int(maps:get(height, Req)),
    case aec_chain:sum_tokens_at_height(Height) of
        {ok, Result} ->
            {200, [], Result};
        {error, chain_too_short} ->
            {400, [], #{reason => <<"Chain too short">>}}
    end;

handle_request_('GetNetworkStatus', _Req, _Context) ->
    case aec_peer_analytics:enabled() of
        false ->
            {404, [], #{reason => <<"Network analytics disabled in node config">>}};
        true ->
            {200, [], aec_peer_analytics:get_stats_for_client()}
    end;

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

produce_tx(spend_tx, Req) ->
    AllowedRecipients = [account_pubkey, name, oracle_pubkey, contract_pubkey],
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id, recipient_id, amount, fee, payload]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                             {recipient_id, recipient_id, {id_hash, AllowedRecipients}}]),
                 api_optional_decode([{payload, bytearray}]),
                 api_str_to_int([amount, fee]),
                 get_nonce_from_account_id(sender_id),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(contract_create_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner_id, code, vm_version, abi_version, deposit,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{owner_id, owner_id, {id_hash, [account_pubkey]}}]),
                 api_str_to_int([vm_version, abi_version, deposit, amount,
                                 gas, gas_price, fee]),
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
produce_tx(contract_call_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller_id, contract_id, abi_version,
                                       amount, gas, gas_price, fee, call_data]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{caller_id, caller_id, {id_hash, [account_pubkey]}},
                                {contract_id, contract_id, {id_hash, [contract_pubkey]}}]),
                 api_str_to_int([abi_version, amount, gas, gas_price, fee, ttl]),
                 get_nonce_from_account_id(caller_id),
                 get_contract_code(contract_id, contract_code),
                 contract_bytearray_params_decode([call_data]),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(dry_run_txs, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([txs]),
                 read_optional_params([{top, top, top}, {accounts, accounts, []},
                                       {tx_events, tx_events, false}]),
                 do_dry_run()],
    process_request(ParseFuns, Req);
produce_tx(name_preclaim_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, commitment_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {commitment_id, commitment_id, {id_hash, [commitment]}}]),
                 api_str_to_int([abi_version, amount, gas, gas_price, fee, ttl]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(name_update_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, name_ttl,
                                       pointers, client_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 api_str_to_int([name_ttl, fee, ttl]),
                 nameservice_pointers_decode(pointers),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(name_claim_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name, name_salt, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}, {name_fee, name_fee, prelima}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name, name, name}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(account_id),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(name_transfer_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, recipient_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {recipient_id, recipient_id,
                                {id_hash, [account_pubkey, name]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(name_revoke_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, name_id, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}},
                                {name_id, name_id, {id_hash, [name]}}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(account_id),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_create_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([initiator_id, initiator_amount,
                                       state_hash,
                                       responder_id, responder_amount,
                                       push_amount, channel_reserve,
                                       lock_period, fee, delegate_ids]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{initiator_id, initiator_id, {id_hash, [account_pubkey]}},
                              {responder_id, responder_id, {id_hash, [account_pubkey]}},
                              {state_hash, state_hash, state}
                              ]),
                 api_str_to_int([initiator_amount, responder_amount,
                                 push_amount, channel_reserve, lock_period, fee, ttl]),
                 delegates_decode(delegate_ids),
                 get_nonce_from_account_id(initiator_id),
                 unsigned_tx_response(fun aesc_create_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_deposit_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 api_str_to_int([amount, fee, round, nonce, ttl]),
                 unsigned_tx_response(fun aesc_deposit_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_withdraw_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, to_id,
                                       amount, fee, state_hash, round, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {to_id, to_id, {id_hash, [account_pubkey]}},
                                {state_hash, state_hash, state}]),
                 api_str_to_int([amount, fee, round, nonce, ttl]),
                 unsigned_tx_response(fun aesc_withdraw_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_snapshot_solo_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {payload, payload, bytearray}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(from_id),
                 unsigned_tx_response(fun aesc_snapshot_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_set_delegates_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_delegate_ids,
                                       responder_delegate_ids,
                                       state_hash, round,
                                       payload, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {state_hash, state_hash, state},
                             {payload, payload, bytearray}]),
                 delegates_decode(initiator_delegate_ids),
                 delegates_decode(responder_delegate_ids),
                 api_str_to_int([fee, ttl, round]),
                 get_nonce_from_account_id(from_id),
                 unsigned_tx_response(fun aesc_set_delegates_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_close_mutual_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 api_str_to_int([initiator_amount_final,
                                 responder_amount_final, fee, ttl]),
                 unsigned_tx_response(fun aesc_close_mutual_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_close_solo_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {poi, poi, poi}, {payload, payload, bytearray}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_close_solo_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_slash_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       payload, poi, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                             {from_id, from_id, {id_hash, [account_pubkey]}},
                             {poi, poi, poi}, {payload, payload, bytearray}]),
                 api_str_to_int([fee, ttl]),
                 get_nonce_from_account_id(from_id),
                 poi_decode(poi),
                 unsigned_tx_response(fun aesc_slash_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(channel_settle_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([channel_id, from_id,
                                       initiator_amount_final,
                                       responder_amount_final,
                                       fee, nonce]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{channel_id, channel_id, {id_hash, [channel]}},
                                {from_id, from_id, {id_hash, [account_pubkey]}}]),
                 api_str_to_int([initiator_amount_final,
                                 responder_amount_final, nonce, fee, ttl]),
                 unsigned_tx_response(fun aesc_settle_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(oracle_register_tx, Req) ->
    IsVmABI = fun(Data) -> maps:get(abi_version, Data, 0) == ?ABI_AEVM_SOPHIA_1 end,
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account_id, query_format, response_format,
                                       query_fee, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}, {abi_version, abi_version, 0}]),
                 api_decode([{account_id, account_id, {id_hash, [account_pubkey]}}]),
                 api_conditional_decode([{query_format, {contract_bytearray, IsVmABI}},
                                         {response_format, {contract_bytearray, IsVmABI}}]),
                 api_str_to_int([oracle_fee, oracle_ttl, fee, ttl]),
                 get_nonce_from_account_id(account_id),
                 ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_register_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(oracle_extend_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, oracle_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}}]),
                 api_str_to_int([oracle_ttl, fee, ttl]),
                 get_nonce_from_account_id(oracle_id),
                 relative_ttl_decode(oracle_ttl),
                 unsigned_tx_response(fun aeo_extend_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(oracle_query_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender_id, oracle_id, query,
                                       query_fee, fee, query_ttl, response_ttl]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{sender_id, sender_id, {id_hash, [account_pubkey]}},
                             {oracle_id, oracle_id, {id_hash, [oracle_pubkey, name]}}]),
                 api_str_to_int([query_fee, query_ttl, response_ttl, fee, ttl]),
                 get_nonce_from_account_id(sender_id),
                 api_optional_decode([{query, contract_bytearray}]),
                 ttl_decode(query_ttl),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_existence(oracle_id),
                 unsigned_tx_response(fun aeo_query_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(oracle_response_tx, Req) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle_id, query_id, response, response_ttl, fee]),
                 read_optional_params([{ttl, ttl, '$no_value'}]),
                 api_decode([{oracle_id, oracle_id, {id_hash, [oracle_pubkey]}},
                                {query_id, query_id, oracle_query_id}]),
                 api_optional_decode([{response, contract_bytearray}]),
                 api_str_to_int([response_ttl, fee, ttl]),
                 get_nonce_from_account_id(oracle_id),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_query_existence(oracle_id, query_id),
                 unsigned_tx_response(fun aeo_response_tx:new/1)
                ],
    process_request(ParseFuns, Req);
produce_tx(paying_for_tx, _Req) ->
      %% this function is intended to produce unsigned transactions that can
      %% be wrapped in a paying for transaction. Please don't move paying for
      %% here as it would allow the creaton of recoursive paying for
      %% transactions
      err.

tx_swagger_name_from_operation_id('PostSpend') -> 'SpendTx';
tx_swagger_name_from_operation_id('PostContractCreate') -> 'ContractCreateTx';
tx_swagger_name_from_operation_id('PostContractCall') -> 'ContractCallTx';
tx_swagger_name_from_operation_id('PostNamePreclaim') -> 'NamePreclaimTx';
tx_swagger_name_from_operation_id('PostNameUpdate') -> 'NameUpdateTx';
tx_swagger_name_from_operation_id('PostNameClaim') -> 'NameClaimTx';
tx_swagger_name_from_operation_id('PostNameTransfer') -> 'NameTransferTx';
tx_swagger_name_from_operation_id('PostNameRevoke') -> 'NameRevokeTx';
tx_swagger_name_from_operation_id('PostChannelCreate') -> 'ChannelCreateTx';
tx_swagger_name_from_operation_id('PostChannelDeposit') -> 'ChannelDepositTx';
tx_swagger_name_from_operation_id('PostChannelWithdraw') -> 'ChannelWithdrawTx';
tx_swagger_name_from_operation_id('PostChannelSnapshotSolo') -> 'ChannelSnapshotSoloTx';
tx_swagger_name_from_operation_id('PostChannelSetDelegates') -> 'ChannelSetDelegatesTx';
tx_swagger_name_from_operation_id('PostChannelCloseMutual') -> 'ChannelCloseMutualTx';
tx_swagger_name_from_operation_id('PostChannelCloseSolo') -> 'ChannelCloseSoloTx';
tx_swagger_name_from_operation_id('PostChannelSlash') -> 'ChannelSlashTx';
tx_swagger_name_from_operation_id('PostChannelSettle') -> 'ChannelSettleTx';
tx_swagger_name_from_operation_id('PostOracleRegister') -> 'OracleRegisterTx';
tx_swagger_name_from_operation_id('PostOracleExtend') -> 'OracleExtendTx';
tx_swagger_name_from_operation_id('PostOracleQuery') -> 'OracleQueryTx';
tx_swagger_name_from_operation_id('PostOracleRespond') -> 'OracleRespondTx'.

