%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

%% API
-export([get_channel/2,
         accounts_in_poi/2,
         check_is_active/1,
         check_is_peer/2,
         check_round_greater_than_last/3,
         check_state_hash_size/1,
         deserialize_payload/1,
         check_solo_close_payload/8,
         is_payload_valid_at_protocol/2,
         check_slash_payload/8,
         check_solo_snapshot_payload/7,
         check_force_progress/3,
         process_solo_close/9,
         process_slash/9,
         process_force_progress/7,
         process_solo_snapshot/7,
         is_offchain_tx/1,
         verify_signatures_offchain/4,
         verify_signatures_offchain/5,
         verify_signatures_onchain/3,
         verify_signatures_onchain/4,
         count_authentications/1,
         channel_pubkey/1,
         censor_init_opts/1,
         censor_ws_req/1,
         tx_hash_to_contract_pubkey/1]).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-else.
-define(TEST_LOG(Format, Data), ok).
-endif.

-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").

-include("aesc_values.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec get_channel(aesc_channels:pubkey(), aec_trees:trees()) ->
                         {error, term()} | {ok, aesc_channels:channel()}.
get_channel(ChannelPubKey, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelPubKey, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            {ok, Ch}
    end.

-spec accounts_in_poi([aec_keys:pubkey()], aec_trees:poi()) -> {ok, [aec_accounts:account()]}|
                                                               {error, wrong_channel_peers}.
accounts_in_poi(Peers, PoI) ->
    Lookups = [aec_trees:lookup_poi(accounts, Pubkey, PoI) || Pubkey <- Peers],
    Accounts = [Acc || {ok, Acc} <- Lookups], % filter successful ones
    case length(Accounts) =:= length(Peers) of
        false -> {error, wrong_channel_peers};
        true ->
            {ok, Accounts}
    end.

-spec check_is_active(aesc_channels:channel()) -> ok | {error, channel_not_active}.
check_is_active(Channel) ->
    case aesc_channels:is_active(Channel) of
        true  -> ok;
        false -> {error, channel_not_active}
    end.

check_is_closing(Channel) ->
    case aesc_channels:is_solo_closing(Channel) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

-spec check_round_greater_than_last(aesc_channels:channel(),
                                    non_neg_integer(),
                                    solo_close | slash | force_progress |
                                    deposit | withdrawal
                                    )
    -> ok | {error, old_round | same_round}.
check_round_greater_than_last(Channel, Round, Type) ->
    ChannelRound = aesc_channels:round(Channel),
    MinRound =
        case aesc_channels:is_last_state_forced(Channel) of
            true when Type =:= force_progress ->
                %% last state is a force progress and an another force
                %% progress' round must be greater than the current one
                ChannelRound;
            true when Type =/= force_progress ->
                %% last state is a force progress and a co-signed transaction
                %% is posted; There might be a couple of subsequent forced
                %% progressed states.
                %% The co-signed state can overwrite the on-chain
                %% computed ones, ever since the first on-chain produced one
                aesc_channels:solo_round(Channel) - 1;
            false ->
                ChannelRound
        end,
    ?TEST_LOG("MinRound ~p, Round ~p", [MinRound, Round]),
    if MinRound < Round ->
            ok;
       MinRound == Round ->
            {error, same_round};
       true ->
            {error, old_round}
    end.

-spec check_is_peer(aec_keys:pubkey(), list(aec_keys:pubkey())) -> ok | {error, account_not_peer}.
check_is_peer(PubKey, Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> ok;
        false -> {error, account_not_peer}
    end.

-spec check_state_hash_size(binary()) -> boolean().
check_state_hash_size(Hash) ->
    byte_size(Hash) =:= aeser_api_encoder:byte_size_for_type(state).

%% From FORTUNA_PROTOCOL_VSN the payload (due to Generalized accounts) can
%% now be either a double signed aesc_offchain_tx, a single signed, single
%% (MetaTx-)wrapped aesc_offchain_tx or a double wrapped aesc_offchain_tx
-spec deserialize_payload(binary()) -> {ok, aetx_sign:signed_tx(), aesc_offchain_tx:tx()}
                                     | {ok, last_onchain}
                                     | {error, bad_offchain_state_type}
                                     | {error, payload_deserialization_failed}.
deserialize_payload(?EMPTY_PAYLOAD) ->
    {ok, last_onchain};
deserialize_payload(Payload) ->
    try
        SignedTx = aetx_sign:deserialize_from_binary(Payload),
        case unpack_payload(SignedTx) of
            {ok, PayloadTx} ->
                {ok, SignedTx, PayloadTx};
            Err = {error, _} ->
                Err
        end
    catch _:_ ->
            {error, payload_deserialization_failed}
    end.

unpack_payload(Tx) ->
    case aetx:specialize_type(aetx_sign:tx(Tx)) of
        {channel_offchain_tx, OffChainTx} -> {ok, OffChainTx};
        {ga_meta_tx, GAMetaTx}            -> unpack_payload(aega_meta_tx:tx(GAMetaTx));
        {_, _}                            -> {error, bad_offchain_state_type}
    end.

-spec is_offchain_tx(aetx_sign:signed_tx()) -> boolean().
is_offchain_tx(SignedTx) ->
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        {aesc_offchain_tx, _} -> true;
        _ -> false
    end.

-spec is_payload_valid_at_protocol(aec_hard_forks:protocol_vsn(), binary()) -> boolean().
is_payload_valid_at_protocol(Protocol, Payload) ->
    case aesc_utils:deserialize_payload(Payload) of
        {error, _}                  -> false;
        {ok, last_onchain}          -> true; %% using tx already on-chain
        {ok, SignedTx, _OffChainTx} ->
            aetx:valid_at_protocol(Protocol, aetx_sign:tx(SignedTx))
    end.

-define(REDACTED, "REDACTED").
-spec censor_init_opts(list() | map()) -> list() | map().
censor_init_opts(Params) ->
    ToCensor = [<<"existing_fsm_id">>],
    lists:foldl(fun censor_init_opt/2, Params, ToCensor).

censor_init_opt(ToCensor, #{} = Opts) ->
    case Opts of
        #{ToCensor := _} ->
            Opts#{ToCensor => ?REDACTED};
        _ ->
            Opts
    end;
censor_init_opt(ToCensor, [_|_] = Opts) ->
    lists:keyreplace(ToCensor, 1, Opts, {ToCensor, ?REDACTED}).

-spec censor_ws_req(map()) -> map().
censor_ws_req(Req) ->
    Req#{qs => ?REDACTED}.

%%%===================================================================
%%% Check payload for slash, solo close and snapshot
%%%===================================================================

-spec check_solo_close_payload(aec_keys:pubkey(), aec_keys:pubkey(),
                               non_neg_integer(), non_neg_integer(), binary(),
                               aec_trees:poi(), aec_trees:trees(),
                               aetx_env:env()) ->
    ok | {error, payload_deserialization_failed
               | bad_offchain_state_type
               | channel_does_not_exist
               | account_not_found
               | insufficient_funds
               | account_not_peer
               | channel_not_active
               | invalid_poi_hash_in_channel
               | bad_state_channel_pubkey
               | same_round
               | old_round
               | signature_check_failed
               | wrong_channel_peers
               | poi_amounts_change_channel_funds }.
check_solo_close_payload(ChannelPubKey, FromPubKey, Nonce, Fee, Payload,
                         PoI, Trees, Env) ->
    case get_vals([get_channel(ChannelPubKey, Trees),
                   deserialize_payload(Payload)]) of
        {error, _} = E -> E;
        {ok, [Channel, last_onchain]} ->
            Checks =
                [fun() -> check_account(FromPubKey, Trees, Nonce, Fee, Env) end,
                 fun() -> check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end,
                 fun() -> check_is_active(Channel) end,
                 fun() -> check_root_hash_in_channel(Channel, PoI) end,
                 fun() -> check_peers_and_amounts_in_poi(Channel, PoI) end
                ],
            aeu_validation:run(Checks);
        {ok, [Channel, {SignedState, PayloadTx}]} ->
            Checks =
                [ fun() -> check_account(FromPubKey, Trees, Nonce, Fee, Env) end,
                  fun() -> check_is_active(Channel) end,
                  fun() -> check_payload(Channel, PayloadTx, FromPubKey, SignedState,
                                          Trees, Env, solo_close) end,
                  fun() -> check_poi(Channel, PayloadTx, PoI) end
                ],
            aeu_validation:run(Checks)
    end.

-spec check_slash_payload(aec_keys:pubkey(), aec_keys:pubkey(),
                          non_neg_integer(), non_neg_integer(), binary(),
                          aec_trees:poi(), aec_trees:trees(),
                          aetx_env:env()) ->
    ok | {error, payload_deserialization_failed
               | slash_must_have_payload
               | bad_offchain_state_type
               | channel_does_not_exist
               | account_not_found
               | insufficient_funds
               | account_not_peer
               | channel_not_closing
               | invalid_poi_hash_in_channel
               | bad_state_channel_pubkey
               | same_round
               | old_round
               | signature_check_failed
               | wrong_channel_peers
               | poi_amounts_change_channel_funds }.
check_slash_payload(ChannelPubKey, FromPubKey, Nonce, Fee, Payload,
                    PoI, Trees, Env) ->
    case get_vals([get_channel(ChannelPubKey, Trees),
                   deserialize_payload(Payload)]) of
        {error, _} = E -> E;
        {ok, [_Channel, last_onchain]} ->
            {error, slash_must_have_payload};
        {ok, [Channel, {SignedState, PayloadTx}]} ->
            Checks =
                [ fun() -> check_account(FromPubKey, Trees, Nonce, Fee, Env) end,
                  fun() -> check_is_closing(Channel) end,
                  fun() -> check_payload(Channel, PayloadTx, FromPubKey, SignedState,
                                         Trees, Env, slash) end,
                  fun() -> check_poi(Channel, PayloadTx, PoI) end
                ],
            aeu_validation:run(Checks)
    end.

-spec check_force_progress(aesc_force_progress_tx:tx(), aec_trees:trees(),
                          aetx_env:env()) ->
    ok | {error, atom()}.
check_force_progress(Tx, Trees, Env) ->
    Payload = aesc_force_progress_tx:payload(Tx),
    OffChainTrees = aesc_force_progress_tx:offchain_trees(Tx),
    Protocol = aetx_env:consensus_version(Env),
    ?TEST_LOG("Checking force progress:\nTx: ~p,\nPayload: ~p,\nOffChainTrees: ~p,\nHeight: ~p",
              [Tx, Payload, OffChainTrees, aetx_env:height(Env)]),
    ChannelPubKey = aesc_force_progress_tx:channel_pubkey(Tx),
    FromPubKey = aesc_force_progress_tx:origin(Tx),
    Nonce = aesc_force_progress_tx:nonce(Tx),
    Fee = aesc_force_progress_tx:fee(Tx),
    [Update] = aesc_force_progress_tx:updates(Tx),
    NextRound = aesc_force_progress_tx:round(Tx),
    case get_vals([get_channel(ChannelPubKey, Trees),
                   deserialize_payload(Payload)]) of
        {error, _} = E -> E;
        {ok, [Channel, last_onchain]} ->
              ?TEST_LOG("Using last on-chain state", []),
              Round = aesc_channels:round(Channel),
              PayloadHash = aesc_channels:state_hash(Channel),
              Checks = [
                  fun() ->
                      check_force_progress_(PayloadHash, Round,
                              Channel, FromPubKey, Nonce, Fee, Update,
                              NextRound, OffChainTrees, Protocol, Trees, Env)
                  end],
              aeu_validation:run(Checks);

        {ok, [Channel, {SignedState, PayloadTx}]} ->
              Round = aesc_offchain_tx:round(PayloadTx),
              ?TEST_LOG("Using provided newer state with round ~p", [Round]),
              ?TEST_LOG("On-chain channel object ~p", [Channel]),
              ?TEST_LOG("Payload transaction ~p", [PayloadTx]),
              PayloadHash = aesc_offchain_tx:state_hash(PayloadTx),
              ?TEST_LOG("Payload hash ~p", [PayloadHash]),
              Checks = [
                  fun() -> check_payload(Channel, PayloadTx, FromPubKey,
                                         SignedState, Trees, Env, force_progress)
                  end,
                  fun() ->
                      check_force_progress_(PayloadHash, Round,
                              Channel, FromPubKey, Nonce, Fee, Update,
                              NextRound, OffChainTrees, Protocol, Trees, Env)
                  end],
              aeu_validation:run(Checks)
    end.

check_force_progress_(PayloadHash, PayloadRound,
                      Channel, FromPubKey, Nonce, Fee, Update,
                      NextRound, OffChainTrees, Protocol, Trees, Env) ->
    Checks =
        [ fun() ->
              case aesc_offchain_update:is_call(Update) of
                  true -> ok;
                  false -> {error, update_not_call}
              end
          end,
          fun() ->
              check_round_greater_than_last(Channel, NextRound,
                                            force_progress)
          end,
          fun() ->
              CallerPubKey = aesc_offchain_update:extract_caller(Update),
              case CallerPubKey =:= FromPubKey of
                  true -> ok;
                  false -> {error, not_caller}
              end
          end,
          fun() -> check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end,
          fun() ->
              case PayloadRound =:= NextRound - 1 of
                  true -> ok;
                  false -> {error, wrong_round}
              end
          end,
          fun() -> check_root_hash_of_trees(PayloadHash, OffChainTrees) end,
          fun() -> % check produced tree has the same root hash as the poi
              ContractPubkey = aesc_offchain_update:extract_contract_pubkey(Update),
              aeu_validation:run([
                  fun() ->
                      ContractTrees = aec_trees:contracts(OffChainTrees),
                      case aect_state_tree:lookup_contract(ContractPubkey,
                                                           ContractTrees) of
                          none -> {error, contract_missing};
                          {value, Contract} ->
                            ABIVersion = aesc_offchain_update:extract_abi_version(Update),
                            CTVersion = aect_contracts:ct_version(Contract),
                            Code = aect_contracts:code(Contract),
                            case check_abi_version(CTVersion, ABIVersion, Protocol) of
                                ok ->
                                    check_code_serialization(Code, CTVersion, Protocol);
                                Error -> Error
                            end
                      end
                  end])
          end,
          fun() ->
              {_Amount, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
              RequiredAmount = Fee + GasLimit * GasPrice,
              check_account(FromPubKey, Trees, Nonce, RequiredAmount, Env)
          end],
    Res = aeu_validation:run(Checks),
    ?TEST_LOG("check_force_progress result: ~p", [Res]),
    Res.

check_account(FromPK, Trees, Nonce, Amount, Env) ->
    Payer = establish_payer(FromPK, Env),
    check_account(FromPK, Payer, Trees, Nonce, Amount, Env).

check_account(FromPK, FromPK, Trees, Nonce, Amount, Env) ->
    aetx_utils:check_account(FromPK, Trees, Nonce, Amount, Env);
check_account(FromPK, Payer, Trees, Nonce, Amount, Env) ->
    case aetx_utils:check_account(FromPK, Trees, Nonce, 0, Env) of
        ok  -> aetx_utils:check_account(Payer, Trees, Amount);
        Err -> Err
    end.


check_code_serialization(Code, #{abi := ABI}, Protocol) ->
    case aeser_contract_code:deserialize(Code) of
        Deserialized ->
            case aect_contracts:is_legal_serialization_at_protocol(
                   ABI, maps:get(contract_vsn, Deserialized, 1), Protocol) of
                true ->
                    ok;
                false ->
                    {error, illegal_contract_compiler_version}
            end
    end.

check_abi_version(#{abi := ABI} = Version, ABI, Protocol) ->
    case aect_contracts:is_legal_version_at_protocol(call, Version, Protocol) of
        true -> ok;
        false -> {error, unknown_vm_version}
    end;
check_abi_version(_, _, _) ->
    {error, wrong_abi_version}.

-spec check_solo_snapshot_payload(aec_keys:pubkey(), aec_keys:pubkey(),
                               non_neg_integer(), non_neg_integer(), binary(),
                               aec_trees:trees(), aetx_env:env()) ->
    ok | {error, payload_deserialization_failed
               | snapshot_must_have_payload
               | bad_offchain_state_type
               | channel_does_not_exist
               | account_not_found
               | insufficient_funds
               | account_not_peer
               | channel_not_active
               | same_round
               | old_round
               | signature_check_failed }.
check_solo_snapshot_payload(ChannelId, FromPubKey, Nonce, Fee, Payload,
                            Trees, Env) ->
    case get_vals([aesc_utils:get_channel(ChannelId, Trees),
                   aesc_utils:deserialize_payload(Payload)]) of
        {error, _} = E -> E;
        {ok, [_Channel, last_onchain]} ->
            {error, snapshot_must_have_payload};
        {ok, [Channel, {SignedState, PayloadTx}]} ->
            ChannelId = aesc_channels:pubkey(Channel),
            Checks =
                [ fun() -> check_account(FromPubKey, Trees, Nonce, Fee, Env) end,
                  fun() -> check_is_active(Channel) end,
                  fun() -> check_payload(Channel, PayloadTx, FromPubKey, SignedState,
                                         Trees, Env, solo_snapshot) end
                ],
            aeu_validation:run(Checks)
    end.

check_poi(Channel, PayloadTx, PoI) ->
    Checks =
        [fun() -> check_root_hash_in_payload(PayloadTx, PoI) end,
         fun() -> check_peers_and_amounts_in_poi(Channel, PoI) end
        ],
    aeu_validation:run(Checks).

check_payload(Channel, PayloadTx, FromPubKey, SignedState, Trees, Env, Type) ->
    ChannelId = aesc_channels:id(Channel),
    Checks =
        [ fun() -> check_channel_id_in_payload(Channel, PayloadTx) end,
          fun() -> check_round_in_payload(Channel, PayloadTx, Type) end,
          fun() -> is_peer_or_delegate(ChannelId, FromPubKey, SignedState,
                                       Trees, Type, Env) end,
          fun() -> verify_signatures_offchain(Channel, SignedState, Trees, Env) end
        ],
    aeu_validation:run(Checks).

check_peers_and_amounts_in_poi(Channel, PoI) ->
    InitiatorPubKey   = aesc_channels:initiator_pubkey(Channel),
    ResponderPubKey   = aesc_channels:responder_pubkey(Channel),
    ChannelAmount     = aesc_channels:channel_amount(Channel),
    case aesc_utils:accounts_in_poi([InitiatorPubKey, ResponderPubKey], PoI) of
        {error, _} = Err -> Err;
        {ok, [PoIInitiatorAcc, PoIResponderAcc]} ->
            PoIInitiatorAmt = aec_accounts:balance(PoIInitiatorAcc),
            PoIResponderAmt = aec_accounts:balance(PoIResponderAcc),
            PoIAmount       = PoIInitiatorAmt + PoIResponderAmt,
            % do not create tokens but we can burn some
            % this allows closing a channel with some accounts not being
            % provided (contracts for example) and those can be force
            % progressed later on while the channel is still closing
            case ChannelAmount >= PoIAmount of
                true  -> ok;
                false -> {error, poi_amounts_change_channel_funds}
            end
    end.

is_peer_or_delegate(ChannelId, FromPubKey, SignedState, Trees, Type, Env) ->
    case is_peer(FromPubKey, SignedState, Trees) of
        ok -> ok;
        {error, account_not_peer} = E0 ->
            Protocol =
                aec_hard_forks:protocol_effective_at_height(aetx_env:height(Env)),
            case is_delegatable_tx_type(Type, Protocol) of
                true ->
                    case is_delegate(ChannelId, FromPubKey, Trees) of
                        ok -> ok;
                        {error, account_not_delegate} ->
                            {error, account_not_peer_or_delegate};
                        {error,_} = E ->
                            E
                    end;
                false ->
                    E0
            end
    end.

is_peer(FromPubKey, SignedState, Trees) ->
    Tx = aetx_sign:tx(SignedState),
    case signers(Tx, Trees) of
        {ok, Signers} ->
            case lists:member(FromPubKey, Signers) of
                true  -> ok;
                false -> {error, account_not_peer}
            end;
        {error, _Reason}=Err -> Err
    end.

signers(Tx, Trees) ->
    case aetx:specialize_type(Tx) of
        {channel_offchain_tx, _} -> aetx:signers(Tx, Trees);
        {ga_meta_tx, MetaTx}  ->
            signers(aetx_sign:tx(aega_meta_tx:tx(MetaTx)), Trees)
    end.

verify_signatures_offchain(Channel, SignedTx, Trees, Env) ->
    verify_signatures_offchain(Channel, SignedTx, Trees, Env, []).

verify_signatures_offchain(Channel, SignedTx, Trees, Env, CheckedSigners) ->
    VerifyOff     = fun(MTx, Ts, E) -> verify_signature_offchain(Channel, MTx, Ts, E) end,
    BasicCheckOff = fun(Signer, Ts) -> is_basic_check_offchain(Channel, Signer, Ts) end,
    verify_signatures(SignedTx, Trees, Env, CheckedSigners,
                      VerifyOff, BasicCheckOff).

is_basic_check_offchain(Channel, SignerPubkey, _Trees) ->
    SignerId = aeser_id:create(account, SignerPubkey),
    case aesc_channels:auth_for_id(SignerId, Channel) of
        {ok, basic}      -> ok;
        {ok, _}          -> {error, ga_using_signature_not_allowed};
        Err = {error, _} -> Err
    end.

verify_signature_offchain(Channel, MetaTx, Trees, Env) ->
    SignerId = aega_meta_tx:ga_id(MetaTx),
    ABIVersion = aega_meta_tx:abi_version(MetaTx),
    case aesc_channels:auth_for_id(SignerId, Channel) of
        {ok, {AuthFunHash, AuthContractId}} ->
            AuthData = aega_meta_tx:auth_data(MetaTx),
            case aeprimop:check_auth_data_function(ABIVersion, AuthData, AuthFunHash) of
                ok ->
                    verify_signature_offchain_(Channel, SignerId, AuthContractId,
                                               MetaTx, Trees, Env);
                Err = {error, _} ->
                    Err
            end;
        {ok, basic} ->
            {error, meta_tx_for_basic_account};
        Err = {error, _} ->
            Err
    end.

verify_signature_offchain_(Channel, SignerId, AuthContractId, MetaTx, Trees, Env) ->
    StoreKey = aesc_channels:auth_store_key(SignerId, Channel),
    verify_meta_tx(SignerId, StoreKey, AuthContractId, MetaTx, Trees, Env,
                   offchain).

verify_signature_onchain_(SignerId, AuthContractId, MetaTx, Trees, Env)
    when AuthContractId =/= undefined ->
    StoreKey = aeser_id:specialize(AuthContractId, contract),
    verify_meta_tx(SignerId, StoreKey, AuthContractId, MetaTx, Trees, Env,
                   onchain).

-spec verify_meta_tx(aeser_id:id(), binary(), aeser_id:id(),
                     aega_meta_tx:tx(), aec_trees:trees(), any(),
                     onchain | offchain) ->
  {ok, binary(), aetx_sign:signed_tx()} | {error, atom()}.
verify_meta_tx(SignerId, StoreKey, AuthContractId, MetaTx, Trees, Env, TxType)
    when StoreKey =/= undefined, AuthContractId =/= undefined ->
    Protocol = aetx_env:consensus_version(Env),
    Height = aetx_env:height(Env),
    {_, SignerPK} = aeser_id:specialize(SignerId),
    {_, AuthContractPK} = aeser_id:specialize(AuthContractId),
    Call = aect_call:new(SignerId, 0, AuthContractId, Height,
                         aega_meta_tx:gas_price(MetaTx)),
    CTree = aec_trees:contracts(Trees),
    case {aect_state_tree:lookup_contract(AuthContractPK, CTree, [no_store]),
          aect_state_tree:read_contract_store(StoreKey, CTree)} of
        {{value, Contract}, {ok, Store}} ->
            CallDef = #{ caller      => SignerPK
                       , contract    => AuthContractPK
                       , gas         => aega_meta_tx:gas_limit(MetaTx, Height, Protocol)
                       , gas_price   => aega_meta_tx:gas_price(MetaTx)
                       , call_data   => aega_meta_tx:auth_data(MetaTx)
                       , amount      => 0
                       , call_stack  => []
                       , code        => aect_contracts:code(Contract)
                       , store       => Store
                       , call        => Call
                       , trees       => Trees
                       , tx_env      => set_auth_tx_hash(aega_meta_tx:tx(MetaTx), Env, TxType)
                       , off_chain   => false
                       , origin      => SignerPK
                       , creator     => aect_contracts:owner_pubkey(Contract)
                       },
            CTVersion = aect_contracts:ct_version(Contract),
            {Call1, _Trees1, _Env1} = aect_dispatch:run(CTVersion, CallDef),
            case check_auth_result(CTVersion, Call1) of
                ok               -> {ok, SignerPK, aega_meta_tx:tx(MetaTx)};
                Err = {error, _} -> Err
            end;
        {none, _} ->
            {error, signature_verification_failed_no_contract};
        {_, {error, _}} ->
            {error, signature_verification_failed_no_state}
    end.

verify_signature_onchain(MetaTx, Trees, Env) ->
    SignerPubkey = aega_meta_tx:ga_pubkey(MetaTx),
    AbiVersion = aega_meta_tx:abi_version(MetaTx),
    AccountsTrees = aec_trees:accounts(Trees),
    Account = aec_accounts_trees:get(SignerPubkey, AccountsTrees),
    case aec_accounts:type(Account) of
        generalized ->
            AuthFunHash0 = aec_accounts:ga_auth_fun(Account),
            {AuthFunHash, GetFunHashFun} =
                case AbiVersion of
                    ?ABI_FATE_SOPHIA_1 ->
                        <<AuthFunHash1:4/binary, _:28/binary>> = AuthFunHash0,
                        {AuthFunHash1,
                         fun aeb_fate_abi:get_function_hash_from_calldata/1};
                    ?ABI_AEVM_SOPHIA_1 ->
                        {AuthFunHash0,
                         fun aeb_aevm_abi:get_function_hash_from_calldata/1}
                end,
            AuthContractId = aec_accounts:ga_contract(Account),
            case GetFunHashFun(aega_meta_tx:auth_data(MetaTx)) of
                {ok, AuthFunHash} ->
                    verify_signature_onchain_(aeser_id:create(account,
                                                              SignerPubkey),
                                              AuthContractId,
                                              MetaTx, Trees, Env);
                {ok, _OtherHash}  -> {error, wrong_auth_function};
                _Other            -> {error, bad_auth_data}
            end;
        basic ->
            {error, meta_tx_for_basic_account}
    end.


check_auth_result(#{ abi := ABIVersion }, Call) ->
    case aect_call:return_type(Call) of
        ok ->
            case aeprimop:decode_auth_call_result(ABIVersion, aect_call:return_value(Call)) of
                {ok, true} -> ok;
                _          -> {error, signature_verification_failed_authenticate_false}
            end;
        _  ->
            {error, signature_verification_failed_contract_error}
    end.

set_auth_tx_hash(STx, Env, TxType) ->
    Tx =
        case TxType of
            onchain -> aetx_sign:tx(STx);
            offchain -> aetx_sign:innermost_tx(STx)
        end,
    BinForNetwork = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    aetx_env:set_ga_tx_hash(Env, aec_hash:hash(tx, BinForNetwork)).

is_delegatable_tx_type(Type, Protocol) ->
    lists:member(Type, delegatable_tx_types(Protocol)).

delegatable_tx_types(Protocol) when Protocol < ?IRIS_PROTOCOL_VSN ->
    [slash];
delegatable_tx_types(_PostIrisProtocol) ->
    %%[slash, solo_snapshot, force_progress].
    [slash, solo_snapshot].

-spec verify_signatures_onchain(aetx_sign:signed_tx(), aec_trees:trees(),
                                aetx_env:env()) -> ok | {error, atom()}.
verify_signatures_onchain(SignedTx, Trees, Env) ->
    verify_signatures_onchain(SignedTx, Trees, Env, []).

verify_signatures_onchain(SignedTx, Trees, Env, CheckedSigners) ->
    verify_signatures(SignedTx, Trees, Env, CheckedSigners,
                      fun verify_signature_onchain/3,
                      fun is_basic_check_onchain/2).

is_basic_check_onchain(SignerPubkey, Trees) ->
    case aec_accounts_trees:lookup(SignerPubkey, aec_trees:accounts(Trees)) of
        none -> {error, account_not_in_trees};
        {value, Account} ->
            case aec_accounts:type(Account) of
                basic       -> ok;
                generalized -> {error, ga_using_signature_not_allowed}
            end
    end.

-spec verify_signatures(aetx_sign:signed_tx(), aec_trees:trees(),
                        aetx_env:env(), list(), fun(), fun()) -> ok | {error, atom()}.
verify_signatures(SignedTx, Trees, Env, CheckedSigners, VerifyAuthFun, CheckBasicFun) ->
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_type(Tx) of
        {ga_meta_tx, GAMetaTx} ->
            case VerifyAuthFun(GAMetaTx, Trees, Env) of
                {ok, Signer, InnerTx} ->
                    verify_signatures(InnerTx, Trees, Env,
                                      [Signer | CheckedSigners],
                                      VerifyAuthFun, CheckBasicFun);
                Err = {error, _} ->
                    Err
            end;
        {_, _} -> % most inner tx
            {ok, Signers} = aetx:signers(Tx, Trees),
            BasicSigners  = Signers -- CheckedSigners,
            Protocol      = aetx_env:consensus_version(Env),
            case is_basic_signers(CheckBasicFun, BasicSigners, Trees, Protocol) of
                ok ->
                    aetx_sign:verify_half_signed(BasicSigners, SignedTx, Protocol);
                Err = {error, _} ->
                    Err
            end
    end.

is_basic_signers(_, _, _, Protocol) when Protocol < ?LIMA_PROTOCOL_VSN ->
    ok;
is_basic_signers(CheckFun, Signers, Trees, _Protocol) ->
    is_basic_signers(CheckFun, Signers, Trees).

is_basic_signers(_, [], _) -> ok;
is_basic_signers(CheckFun, [Signer | Signers], Trees) ->
    case CheckFun(Signer, Trees) of
        ok               -> is_basic_signers(CheckFun, Signers, Trees);
        Err = {error, _} -> Err
    end.

-spec is_delegate(aesc_channels:id(), aec_keys:pubkey(),
                  aec_trees:trees())
                  -> ok | {error, atom()}.
is_delegate(ChannelId, FromPubKey, Trees) ->
    ChannelPubKey = aeser_id:specialize(ChannelId, channel),
    with_channel(fun(Channel) ->
                         is_delegate_(Channel, FromPubKey)
                 end, ChannelPubKey, Trees).

is_delegate_(Channel, FromPubKey) ->
    %% TODO!!!!!
    case lists:member(FromPubKey, aesc_channels:delegate_pubkeys(Channel,
                                                                 initiator)) of
        true ->
            ok;
        false ->
            {error, account_not_delegate}
    end.

with_channel(F, ChannelPubKey, Trees) ->
    case get_channel(ChannelPubKey, Trees) of
        {ok, Channel}  -> F(Channel);
        {error, _} = E -> E
    end.

check_channel_id_in_payload(Channel, PayloadTx) ->
    case aesc_channels:pubkey(Channel) =:= aesc_offchain_tx:channel_pubkey(PayloadTx) of
        false -> {error, bad_state_channel_pubkey};
        true -> ok
    end.

check_round_in_payload(Channel, PayloadTx, Type) ->
    check_round_greater_than_last(Channel, aesc_offchain_tx:round(PayloadTx),
                                  Type).

check_root_hash_in_payload(PayloadTx, PoI) ->
    ChannelStateHash = aesc_offchain_tx:state_hash(PayloadTx),
    check_root_hash_of_poi(ChannelStateHash, aec_trees:poi_hash(PoI)).

check_root_hash_of_trees(StateHash, OffChainTrees) ->
    check_root_hash_of_poi(StateHash, aec_trees:hash(OffChainTrees)).

check_root_hash_of_poi(StateHash, OffChainHash) ->
    ?TEST_LOG("Off-chain trees hash ~p", [OffChainHash]),
    case StateHash =:= OffChainHash of
        true -> ok;
        false -> {error, invalid_poi_hash}
    end.

check_root_hash_in_channel(Channel, PoI) ->
    ChannelStateHash = aesc_channels:state_hash(Channel),
    PoIHash = aec_trees:poi_hash(PoI),
    ?TEST_LOG("On-chain stored hash ~p", [ChannelStateHash]),
    case ChannelStateHash =:= PoIHash of
        true -> ok;
        false -> {error, invalid_poi_hash_in_channel}
    end.

establish_payer(NormalPayer, TxEnv) ->
    case aetx_env:payer(TxEnv) of
        undefined -> NormalPayer;
        X         -> X
    end.

%%%===================================================================
%%% Process payload for slash and solo close
%%%===================================================================

process_solo_close(ChannelPubKey, FromPubKey, Nonce, Fee,
                   Payload, PoI, Height, Trees, Env) ->
    add_event(
      process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                               Payload, PoI, Height, Trees, Env),
      ChannelPubKey, Env).


process_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
              Payload, PoI, Height, Trees, Env) ->
    add_event(
      process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                               Payload, PoI, Height, Trees, Env),
      ChannelPubKey, Env).

process_solo_snapshot(ChannelPubKey, FromPubKey, Nonce, Fee, Payload, Trees, Env) ->
    ChannelsTree0 = aec_trees:channels(Trees),
    Channel0 = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    {ok, _SignedOffchainTx, PayloadTx} = deserialize_payload(Payload),
    Channel = aesc_channels:snapshot_solo(Channel0, PayloadTx),
    Trees1 = set_channel(Channel, Trees),
    Trees2 = spend(FromPubKey, Fee, Nonce, Trees1, Env),
    add_event(Trees2, ChannelPubKey, Env).

process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                         Payload, PoI, Height, Trees, Env) ->
    Trees1        = spend(FromPubKey, Fee, Nonce, Trees, Env),
    ChannelsTree0 = aec_trees:channels(Trees1),

    Channel0 = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    Channel1 =
        case aesc_utils:deserialize_payload(Payload) of
            {ok, _SignedTx, PayloadTx} ->
                aesc_channels:close_solo_with_payload(Channel0, PayloadTx, PoI, Height);
            {ok, last_onchain} ->
                aesc_channels:close_solo_last_onchain(Channel0, PoI, Height)
        end,
    _Trees2 = set_channel(Channel1, Trees1).

process_force_progress(Tx, OffChainTrees, Payload, TxHash, Height, Trees, Env) ->
    ?TEST_LOG("process_force_progress begin", []),
    ChannelPubKey = aesc_force_progress_tx:channel_pubkey(Tx),
    FromPubKey = aesc_force_progress_tx:origin(Tx),
    Nonce = aesc_force_progress_tx:nonce(Tx),
    Fee = aesc_force_progress_tx:fee(Tx),
    [Update] = aesc_force_progress_tx:updates(Tx),
    NextRound = aesc_force_progress_tx:round(Tx),
    ?TEST_LOG("Next channel round will be ~p", [NextRound]),
    ExpectedHash = aesc_force_progress_tx:state_hash(Tx),
    {ok, Channel} = get_channel(ChannelPubKey, Trees),
    {ContractPubkey, Caller} = aesc_offchain_update:extract_call(Update),
    %% use in gas payment
    Reserve = aesc_channels:channel_reserve(Channel),
    PrunedOffChainTrees = aect_call_state_tree:prune_without_backend(OffChainTrees),
    NewOffChainTrees =
        try aesc_offchain_update:apply_on_trees(Update,
                                                PrunedOffChainTrees,
                                                Trees, Env,
                                                NextRound, Reserve)
        catch error:{off_chain_update_error, _} ->
          {_Amount, GasPrice, GasLimit} = aesc_offchain_update:extract_amounts(Update),
            CallsTrees =
                aect_channel_contract:insert_failed_call(ContractPubkey,
                                                         Caller,
                                                         NextRound,
                                                         GasPrice, GasLimit,
                                                         % prune old calls
                                                         aect_call_state_tree:empty()),
            aec_trees:set_calls(PrunedOffChainTrees, CallsTrees)
        end,

    {ok, Call} = aect_channel_contract:get_call(ContractPubkey,
                                                Caller,
                                                NextRound,
                                                aec_trees:calls(NewOffChainTrees)),
    ?TEST_LOG("Forced progress call: ~p", [Call]),
    % check hash
    ComputedHash = aec_trees:hash(NewOffChainTrees),

    Accs = aec_trees:accounts(NewOffChainTrees),
    GetBalance =
        fun(Pubkey) ->
            Acc = aec_accounts_trees:get(Pubkey, Accs),
            aec_accounts:balance(Acc)
        end,


    % consume gas from sender
    Trees1 = consume_gas_and_fee(Call, Fee, FromPubKey, Nonce, Trees, Env),

    % add a receipt call in the calls state tree
    Trees2 = add_call(Call, TxHash, Trees1, Env),

    ?TEST_LOG("Expected hash ~p", [ExpectedHash]),
    ?TEST_LOG("Computed hash ~p", [ComputedHash]),
    BalancesMatch =
        case aesc_channels:is_active(Channel) of
            true ->
                ?TEST_LOG("Channel is NOT closing, balances are not taken into account", []),
                true;
            false ->
                amounts_do_not_exceed_total_balance(NewOffChainTrees,
                                                    Channel)
        end,
    HashesMatch = ExpectedHash =:= ComputedHash,
    ?TEST_LOG("Matches: hashes ~p, balances ~p", [HashesMatch, BalancesMatch]),
    Trees3 =
        case HashesMatch andalso BalancesMatch of
            true ->
                ?TEST_LOG("Expected and computed hash MATCH. Balances does not exceed on-chain total balance. Channel object is being updated", []),
                % update channel obj
                InitiatorBalance = GetBalance(aesc_channels:initiator_pubkey(Channel)),
                ResponderBalance = GetBalance(aesc_channels:responder_pubkey(Channel)),
                ChannelForceProgressFun =
                    case Payload of
                        ?EMPTY_PAYLOAD ->
                            fun aesc_channels:force_progress_last_onchain/6;
                        _ ->
                            fun aesc_channels:force_progress_with_payload/6
                    end,
                Channel1 = ChannelForceProgressFun(Channel, ExpectedHash,
                                                   NextRound,
                                                   InitiatorBalance,
                                                   ResponderBalance,
                                                   Height),
                _Trees = set_channel(Channel1, Trees2);
            false ->
                ?TEST_LOG("Expected and computed values DO NOT MATCH. Channel object is NOT being updated", []),
                Trees2
        end,
    add_event(Trees3, ChannelPubKey, Env).


get_vals(List) ->
    R =
        lists:foldl(
            fun(_, {error, _} = Err) -> Err;
              ({error, _} = Err, _) -> Err;
              ({ok, Val}, Accum) -> [Val | Accum];
              ({ok, Val1, Val2}, Accum) -> [{Val1, Val2} | Accum]
            end,
            [],
            List),
    case R of
        {error, _} = Err -> Err;
        L when is_list(L) -> {ok, lists:reverse(L)}
    end.

amounts_do_not_exceed_total_balance(OffChainTrees, Channel) ->
    AccountsTree = aec_trees:accounts(OffChainTrees),
    GetBalance =
        fun(Pubkey) ->
            Acc = aec_accounts_trees:get(Pubkey, AccountsTree), % must be present
            B = aec_accounts:balance(Acc),
            ?TEST_LOG("Participant balance ~p", [B]),
            B
        end,
    AllBalances = lists:sum([GetBalance(K) ||
                             K <- [aesc_channels:initiator_pubkey(Channel),
                                   aesc_channels:responder_pubkey(Channel)]]),
    ChannelAmount = aesc_channels:channel_amount(Channel),
    ?TEST_LOG("AllBalances ~p, ChannelAmount ~p", [AllBalances, ChannelAmount]),
    AllBalances =< ChannelAmount.

spend(From, Amount, Nonce, Trees, Env) ->
    Payer = establish_payer(From, Env),
    spend_(From, Payer, Amount, Nonce, Trees).

spend_(From, From, Amount, Nonce, Trees) ->
    ATree0 = aec_trees:accounts(Trees),
    Acc0 = aec_accounts_trees:get(From, ATree0),
    {ok, Acc} = aec_accounts:spend(Acc0, Amount, Nonce),
    ATree1 = aec_accounts_trees:enter(Acc, ATree0),
    aec_trees:set_accounts(Trees, ATree1);
spend_(From, Payer, Amount, Nonce, Trees) ->
    ATree0 = aec_trees:accounts(Trees),
    AccF0 = aec_accounts_trees:get(From, ATree0),
    AccF1 = aec_accounts:set_nonce(AccF0, Nonce),
    AccP0 = aec_accounts_trees:get(Payer, ATree0),
    {ok, AccP1} = aec_accounts:spend_without_nonce_bump(AccP0, Amount),
    ATree1 = aec_accounts_trees:enter(AccF1, ATree0),
    ATree2 = aec_accounts_trees:enter(AccP1, ATree1),
    aec_trees:set_accounts(Trees, ATree2).

-spec consume_gas_and_fee(aect_call:call(),
                          integer(),
                          aec_keys:pubkey(),
                          non_neg_integer(),
                          aec_trees:trees(),
                          aetx_env:env()) -> aec_trees:trees().
consume_gas_and_fee(Call, Fee, From, Nonce, Trees, Env) ->
    UsedAmount = aect_call:gas_used(Call) * aect_call:gas_price(Call),
    spend(From, UsedAmount + Fee, Nonce, Trees, Env).

set_channel(Channel, Trees) ->
    ChannelsTree0 = aec_trees:channels(Trees),
    ChannelsTree1 = aesc_state_tree:enter(Channel, ChannelsTree0),
    aec_trees:set_channels(Trees, ChannelsTree1).

-spec tx_hash_to_contract_pubkey(binary()) -> binary().
tx_hash_to_contract_pubkey(TxHash) ->
    ByteSize = aeser_api_encoder:byte_size_for_type(contract_pubkey),
    case TxHash of
        <<_:ByteSize/binary>> -> TxHash;
        <<H:ByteSize/binary,_>> -> H;
        Short when byte_size(Short) < ByteSize ->
            BytesToPad = ByteSize - byte_size(Short),
            <<Short/binary, 0:BytesToPad/unit:8>>
    end.

add_call(Call0, TxHash, Trees, Env) ->
    ContractPubkey = tx_hash_to_contract_pubkey(TxHash),
    Call1          = aect_call:set_contract(ContractPubkey, Call0),
    Caller         = aect_call:caller_pubkey(Call1),
    NewId =
        case aetx_env:ga_nonce(Env, Caller) of
            {value, Nonce} ->
                aect_call:ga_id(Nonce, ContractPubkey);
            none ->
                aect_call:id(Caller, aect_call:caller_nonce(Call1), ContractPubkey)
        end,

    Call = aect_call:set_id(NewId, Call1),
    aect_utils:insert_call_in_trees(Call, Trees).

-spec add_event(aec_trees:trees(), binary(), aetx_env:env()) ->
                       {ok, aec_trees:trees(), aetx_env:env()}.
add_event(Trees, ChannelPubKey, Env) ->
    {ok, Trees, aetx_env:tx_event(channel, ChannelPubKey, Env)}.

%% This is meant to be a faster way of detecting how many signatures have been
%% added to the Tx. Plain signatures are 'usorted' (no duplicates), so we can
%% simply check the length. With GAs, it's trickier, and the same account
%% can sign multiple times. We maintain an ordset of GA pubkeys, and assume
%% that (GA sigs) and (plain sigs) are disjunct sets. They should be, but
%% a client could violate the rules and sign both plainly and with GA. This
%% will fail validation anyway, but we don't detect that here -- too costly
%%.
count_authentications(SignedTx) ->
    count_authentications(SignedTx, ordsets:new()).

count_authentications(SignedTx, GAs) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
        {aega_meta_tx, InnerSignedTx} ->
            count_authentications(aega_meta_tx:tx(InnerSignedTx),
                                  ordsets:add_element(
                                    aega_meta_tx:ga_pubkey(InnerSignedTx),
                                    GAs));
        {_, _} -> % most inner tx
            ordsets:size(GAs) + length(aetx_sign:signatures(SignedTx))
    end.

-spec channel_pubkey(aetx_sign:signed_tx()) -> {ok, aec_keys:pubkey()} |
                                               {error, not_channel_tx}.
channel_pubkey(SignedTx) ->
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
        %% initial channel id depends on the auth
        {aesc_create_tx, Txi} ->
            Initiator = aesc_create_tx:initiator_pubkey(Txi),
            Responder = aesc_create_tx:responder_pubkey(Txi),
            case channel_create_nonce_or_auth_id(SignedTx) of
                {ok, InitiatorAuthId} ->
                    PK = aesc_channels:pubkey(Initiator,
                                              InitiatorAuthId,
                                              Responder),
                    {ok, PK};
                {error, _} = Err ->
                    Err
            end;
        %% Likely only channel txs have a channel_id/1 callback, so prepare for
        %% 'undef' exceptions.
        {Mod, Txi} ->
            try {ok, Mod:channel_pubkey(Txi)}
            catch error:_ -> {error, not_channel_tx}
            end
    end.

channel_create_nonce_or_auth_id(SignedTx) ->
    {aesc_create_tx, Txi} =
        aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    Initiator = aesc_create_tx:initiator_pubkey(Txi),
    channel_create_nonce_or_auth_id(SignedTx, Initiator).


%% since there might be a couple of nested meta transactions by the same
%% signer - we need the innermost one as it is is required for channel_id
%% computation
channel_create_nonce_or_auth_id(SignedTx, Initiator) ->
    case channel_create_nonce_or_auth_id(SignedTx, Initiator, not_found) of
        not_found -> {error, missing_authentication};
        V -> {ok, V}
    end.

channel_create_nonce_or_auth_id(SignedTx, Initiator, OldValueFound) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of
        {aega_meta_tx, MetaTx} ->
            NewValueFound =
            case aega_meta_tx:ga_pubkey(MetaTx) =:= Initiator of
                true -> aega_meta_tx:auth_id(MetaTx);
                false -> OldValueFound
            end,
            channel_create_nonce_or_auth_id(aega_meta_tx:tx(MetaTx),
                                            Initiator, NewValueFound);
        {Mod, Tx} -> % most inner tx
            case Mod:nonce(Tx) of
                0 -> OldValueFound; % GA
                V when OldValueFound =:= not_found -> V
            end
    end.
