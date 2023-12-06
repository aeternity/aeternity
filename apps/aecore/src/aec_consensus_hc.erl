%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc Consensus module for HyperChains consensus
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_hc).

-export([]).

-behavior(aec_consensus).


-define(TAG, 1336). %% TODO: remove this
-define(SIGNATURE_SIZE, 16).
-define(ETS_CACHE_TABLE, ?MODULE).
-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).

%% API
-export([ can_be_turned_off/0
        , assert_config/1
        , start/2
        , stop/0
        , is_providing_extra_http_endpoints/0
        , client_request/1
        %% Deserialization
        , extra_from_header/1
        %% Building the Insertion Ctx
        , recent_cache_n/0
        , recent_cache_trim_key_header/1
        %% Target adjustment when creating keyblocks
        , keyblocks_for_target_calc/0
        , keyblock_create_adjust_target/2
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        , dirty_validate_header_pre_conductor/1
        , dirty_validate_key_hash_at_height/2
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/2
        , state_pre_transform_key_node/3
        , state_pre_transform_micro_node/2
        %% Block rewards
        , state_grant_reward/4
        %% PoGF
        , pogf_detected/2
        %% Genesis block
        , genesis_transform_trees/2
        , genesis_raw_header/0
        , genesis_difficulty/0
        %% Keyblock sealing
        , key_header_for_sealing/1
        , validate_key_header_seal/2
        , generate_key_header_seal/5
        , set_key_block_seal/2
        , nonce_for_sealing/1
        , next_nonce_for_sealing/2
        , trim_sealing_nonce/2
        %% Block target and difficulty
        , default_target/0
        , assert_key_target_range/1
        , key_header_difficulty/1
        %% rewards and signing
        , beneficiary/0
        , next_beneficiary/0
        , allow_lazy_leader/0
        , pick_lazy_leader/1
        , get_sign_module/0
        , get_type/0
        , get_block_producer_configs/0
        , is_leader_valid/4
        ]).

%% HC specific API
-export([ parent_chain_validators/2
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include("aec_consensus.hrl").

can_be_turned_off() -> false.
assert_config(_Config) -> ok.

start(Config, #{block_production := BlockProduction}) ->
    #{<<"stakers">> := StakersEncoded,
      <<"parent_chain">> :=
        #{  <<"start_height">> := StartHeight,
            <<"confirmations">> := Confirmations,
            <<"consensus">> :=
                #{  <<"type">> := PCType,
                    <<"network_id">> := NetworkId,
                    <<"spend_address">> := PCSpendAddress,
                    <<"fee">> := Fee,
                    <<"amount">> := Amount
                 },
            <<"polling">> :=
                #{  <<"fetch_interval">> := FetchInterval,
                    <<"nodes">> := Nodes0
                 } = Polling,
            <<"producing_commitments">> := ProducingCommitments
          },
     <<"lazy_leader_trigger_time">> := _TimeTillDeclaringLazy
     } = Config,
    %% assert the boolean type
    case ProducingCommitments of
        true -> ok;
        false -> ok
    end,
    CacheSize = maps:get(<<"cache_size">>, Polling, 200),
    ParentHosts =
        lists:map(
            fun(#{<<"host">> := Host,
                  <<"port">> := Port,
                  <<"user">> := User,
                  <<"password">> := Pass
                }) ->
                #{host => Host,
                  port => Port,
                  user => User,
                  password => Pass}
            end,
            Nodes0),
    {ParentConnMod, PCSpendPubkey, HCPCPairs, SignModule} =
        case PCType of
            <<"AE2AE">> -> start_ae(StakersEncoded, PCSpendAddress);
            <<"AE2BTC">> -> start_btc(StakersEncoded, PCSpendAddress, aehttpc_btc);
            <<"AE2DOGE">> -> start_btc(StakersEncoded, PCSpendAddress, aehttpc_doge)
        end,
    start_dependency(aec_parent_connector, [ParentConnMod, FetchInterval,
                                            ParentHosts, NetworkId,
                                            SignModule, HCPCPairs, PCSpendPubkey, Fee, Amount]),
    start_dependency(aec_parent_chain_cache, [StartHeight, CacheSize,
                                              Confirmations, BlockProduction,
                                             ProducingCommitments]),
    ok.

start_btc(StakersEncoded, PCSpendAddress, ParentConnMod) ->
    Stakers =
        lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := EncodedPubkey,
                                                 <<"priv">> := EncodedPrivkey}
                 }) ->
                 {HCPubkey, HCPrivkey} = validate_keypair(EncodedPubkey, EncodedPrivkey),
                 {HCPubkey, HCPrivkey}
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    start_dependency(aec_preset_keys, [StakersMap]),
    HCPCPairs = lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := EncodedPubkey},
                  <<"parent_chain_account">> := #{<<"pub">> := BTCPubkey}
                 }) ->
                 {ok, HCPubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                                EncodedPubkey),
                 {HCPubkey, BTCPubkey}
            end,
            StakersEncoded),
    SignModule = undefined,
    {ParentConnMod, PCSpendAddress, HCPCPairs, SignModule}.

start_ae(StakersEncoded, PCSpendAddress) ->
    Stakers =
        lists:flatmap(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := HCEncodedPubkey,
                                                 <<"priv">> := HCEncodedPrivkey},
                  <<"parent_chain_account">> := #{<<"pub">> := PCEncodedPubkey,
                                                  <<"priv">> := PCEncodedPrivkey}
                 }) ->
                {HCPubkey, HCPrivkey} = validate_keypair(HCEncodedPubkey, HCEncodedPrivkey),
                {PCPubkey, PCPrivkey} = validate_keypair(PCEncodedPubkey, PCEncodedPrivkey),
                [{HCPubkey, HCPrivkey}, {PCPubkey, PCPrivkey}]
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    %% TODO: ditch this after we move beyond OTP24
    _Mod = aec_preset_keys,
    start_dependency(aec_preset_keys, [StakersMap]),
    HCPCPairs = lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := HCEncodedPubkey},
                  <<"parent_chain_account">> := #{<<"pub">> := PCEncodedPubkey}
                 }) ->
                 {ok, HCPubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                 HCEncodedPubkey),
                 {HCPubkey, PCEncodedPubkey}
            end,
            StakersEncoded),
    ParentConnMod = aehttpc_aeternity,
    SignModule = get_sign_module(),
    {ok, PCSpendPubkey} = aeser_api_encoder:safe_decode(account_pubkey, PCSpendAddress),
    {ParentConnMod, PCSpendPubkey, HCPCPairs, SignModule}.

validate_keypair(EncodedPubkey, EncodedPrivkey) ->
    {ok, Pubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                 EncodedPubkey),
    Privkey = aeu_hex:hex_to_bin(EncodedPrivkey),
    case aec_keys:check_sign_keys(Pubkey, Privkey) of
        true -> pass;
        false -> throw({error, invalid_staker_pair, {EncodedPubkey, EncodedPrivkey}})
    end,
    {Pubkey, Privkey}.

start_dependency(Mod, Args) ->
    %% TODO: ditch this after we move beyond OTP24
    OldSpec =
        {Mod, {Mod, start_link, Args}, permanent, 3000, worker, [Mod]},
    {ok, _} = aec_consensus_sup:start_child(OldSpec).

stop() ->
    aec_preset_keys:stop(),
    aec_parent_connector:stop(),
    aec_parent_chain_cache:stop(),
    ok.

is_providing_extra_http_endpoints() -> false.

client_request(_) -> error(unsupported).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_key_header(_) -> ok.

keyblocks_for_target_calc() -> 0.
keyblock_create_adjust_target(Block0, []) ->
    {ok, Stake} = aeu_ets_cache:lookup(?ETS_CACHE_TABLE, added_stake),
    Block = aec_blocks:set_target(Block0, aeminer_pow:integer_to_scientific(Stake)),
    {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_header_pre_conductor(_) -> ok.
dirty_validate_key_hash_at_height(_, _) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.
dirty_validate_micro_node_with_ctx(_Node, _Block, _Ctx) -> ok.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, Trees) -> Trees.

state_pre_transform_key_node(Node, PrevNode, Trees) ->
    PrevHeader = aec_block_insertion:node_header(PrevNode),
    Beneficiary = aec_block_insertion:node_beneficiary(Node),
    {ok, PrevHash} = aec_headers:hash_header(PrevHeader),
    Height = aec_block_insertion:node_height(Node),
    case Height > 0 of
        true ->
            {TxEnv, _} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, PrevHash),
            %% TODO: discuss which is the correct height to pass: the new or the
            %% previous one. At this point since there is no key block hash yet, it
            %% makes sense to base the tx call on the previous height altogether
            PCHeight = pc_height(Height),
            case aec_parent_chain_cache:get_block_by_height(PCHeight) of
                {error, not_in_cache} ->
                    aec_conductor:throw_error(parent_chain_block_not_synced);
                {ok, Block} ->
                    Entropy = aec_parent_chain_block:hash(Block),
                    CommitmentsSophia = encode_commitments(Block),
                    NetworkId = aec_parent_chain_block:encode_network_id(aec_governance:get_network_id()),
                    {ok, CD} = aeb_fate_abi:create_calldata("elect",
                                                            [aefa_fate_code:encode_arg({string, Entropy}),
                                                             CommitmentsSophia,
                                                             aefa_fate_code:encode_arg({bytes, NetworkId})
                                                            ]),
                    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
                    try call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "elect", 0) of
                        {ok, Trees1, Call} ->
                            case aeb_fate_encoding:deserialize(aect_call:return_value(Call)) of
                                {tuple, {{address, Beneficiary}, AddedStake}} -> %% same beneficiary!
                                    cache(Beneficiary, AddedStake),
                                    Trees1;
                                {tuple, {{address, _OtherBeneficiary}, _AddedStake}} -> %% lazy leader
                                    elect_lazy_leader(Beneficiary, TxEnv, Trees) %% initial trees!!!
                            end;
                        {error, What} ->
                            lager:info("Consensus contract failed with ~p", [What]),
                            elect_lazy_leader(Beneficiary, TxEnv, Trees) %% initial trees!!!
                    catch error:{consensus_call_failed, {error, Why}} ->
                            lager:info("Consensus contract failed with ~p", [Why]),
                            elect_lazy_leader(Beneficiary, TxEnv, Trees) %% initial trees!!!
                    end
            end;
        false -> Trees %% do not elect leader for genesis
    end.

cache(Leader, AddedStake) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, current_leader, Leader),
    aeu_ets_cache:put(?ETS_CACHE_TABLE, added_stake, AddedStake),
    ok.

state_pre_transform_micro_node(_Node, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Node, Trees, Amount) ->
    {ok, CD} = aeb_fate_abi:create_calldata(
                 "reward", [aefa_fate_code:encode_arg({address, Beneficiary})]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract(?REWARDS_CONTRACT,
           Node, Trees, CallData,
           ["reward(", aeser_api_encoder:encode(account_pubkey, Beneficiary), ")"], Amount) of
        {ok, Trees1, _} -> Trees1;
        {error, What} ->
            error({failed_to_reward_leader, What}) %% maybe a softer approach than crash and burn?
    end.


%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees0, #{}) ->
    NetworkId = aec_governance:get_network_id(),
    GenesisProtocol = genesis_protocol_version(),
    {ok, #{ <<"contracts">> := Contracts
          , <<"calls">> := Calls }} =
        aec_fork_block_settings:hc_seed_contracts(GenesisProtocol, NetworkId),
    GenesisHeader = genesis_raw_header(),
    {ok, GenesisHash} = aec_headers:hash_header(GenesisHeader),
    TxEnv = aetx_env:tx_env_from_key_header(GenesisHeader,
                                            GenesisHash,
                                            aec_headers:time_in_msecs(GenesisHeader),
                                            aec_headers:prev_hash(GenesisHeader)),
    Trees1 = create_contracts(Contracts, TxEnv, Trees0),
    Trees = call_contracts(Calls, TxEnv, Trees1),
    aect_call_state_tree:prune(0, Trees).

genesis_raw_header() ->
    GenesisProtocol = genesis_protocol_version(),
    GenesisStartTime = genesis_start_time(),
    aec_headers:new_key_header(
        0,
        aec_governance:contributors_messages_hash(),
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
        <<0:32/unit:8>>,
        <<0:?MINER_PUB_BYTES/unit:8>>,
        <<0:?BENEFICIARY_PUB_BYTES/unit:8>>,
        ?TAG,
        no_value,
        0,
        GenesisStartTime,
        default,
        GenesisProtocol).

genesis_difficulty() -> 0.

key_header_for_sealing(Header0) ->
    Header = aec_headers:set_key_seal(Header0, no_value),
    aec_headers:serialize_to_binary(Header).

validate_key_header_seal(Header, _Protocol) ->
    Seal = aec_headers:key_seal(Header),
    {SignaturePart, Padding} = lists:split(?SIGNATURE_SIZE, Seal),
    Signature = << <<E:32>> || E <- SignaturePart >>,
    Validators = [ fun seal_correct_padding/3
                 , fun seal_correct_signature/3
                 ],
    case aeu_validation:run(Validators, [Header, Signature, Padding]) of
        ok -> ok;
        {error, signature_verification_failed} = Err ->
            case aec_headers:difficulty(Header) of
                0 -> ok; %% TODO: is this safe!?
                _ -> Err
            end;
        {error, _} = Err -> Err
    end.

seal_correct_padding(_Header, _Signature, Padding) ->
    PaddingSize = seal_padding_size(),
    ExpectedPadding = lists:duplicate(PaddingSize, 0),
    ExpectedPaddingLazy = [1 | lists:duplicate(PaddingSize - 1, 0)],
    case Padding =:= ExpectedPadding orelse Padding =:= ExpectedPaddingLazy of
        true -> ok;
        false -> {error, {erroneous_seal, Padding, ExpectedPadding, ExpectedPaddingLazy}}
    end.

seal_correct_signature(Header, Signature, _Padding) ->
    Leader = aec_headers:miner(Header),
    Bin = aec_headers:serialize_to_signature_binary(Header),
    case enacl:sign_verify_detached(Signature, Bin, Leader) of
        true  -> ok;
        false ->
            {error, signature_verification_failed}
    end.

generate_key_header_seal(_, Candidate, _PCHeight, #{expected_key_block_rate := _Expected} = _Config, _) ->
    Leader = aec_headers:beneficiary(Candidate),
    SignModule = get_sign_module(),
    case SignModule:set_candidate(Leader) of
        {error, key_not_found} ->
            timer:sleep(1000),
            {continue_mining, {error, no_solution} };
        ok ->
            {ok, Signature} = SignModule:produce_key_header_signature(Candidate, Leader),
            %% the signature is 64 bytes. The seal is 168 bytes. We add 104 bytes at
            %% the end of the signature
            PaddingSize = seal_padding_size(),
            Padding = << <<E:32>> || E <- lists:duplicate(PaddingSize, 0)>>,
            Seal = aec_headers:deserialize_pow_evidence_from_binary(<<Signature/binary, Padding/binary>>),
            {continue_mining, {ok, Seal}}
    end.


set_key_block_seal(KeyBlock, Seal) ->
    aec_blocks:set_key_seal(KeyBlock, Seal).

nonce_for_sealing(Header) ->
    Height = aec_headers:height(Header),
    PCHeight = pc_height(Height),
    PCHeight.

next_nonce_for_sealing(PCHeight, _) ->
    PCHeight.

trim_sealing_nonce(PCHeight, _) ->
    PCHeight.

default_target() ->
    -1. %% this is an impossible value will be rewritten later on

assert_key_target_range(_) ->
    ok.

key_header_difficulty(H) ->
    Target = aec_headers:target(H),
    aeminer_pow:scientific_to_integer(Target).

%% This is initial height; if neeeded shall be reinit at fork height
election_contract_pubkey() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      election_contract_pubkey,
      fun() ->
              {ok, EncContractId} =
                  aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                       <<"0">>,
                                       <<"config">>, <<"election_contract">>]),
              {ok, Pubkey}   = aeser_api_encoder:safe_decode(contract_pubkey,
                                                             EncContractId),
              Pubkey
      end).

rewards_contract_pubkey() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      rewards_contract_pubkey,
      fun() ->
              {ok, EncContractId} =
                  aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                       <<"0">>,
                                       <<"config">>, <<"rewards_contract">>]),
              {ok, Pubkey}   = aeser_api_encoder:safe_decode(contract_pubkey,
                                                             EncContractId),
              Pubkey
      end).

pc_start_height() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      pc_start_height,
      fun() ->
              {ok, H} =
                  aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                       <<"0">>,
                                       <<"config">>, <<"parent_chain">>,
                                       <<"start_height">>]),
              H
      end).

genesis_start_time() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      genesis_start_time,
      fun() ->
            case aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                      <<"0">>,
                                      <<"config">>,
                                      <<"genesis_start_time">>]) of
                {ok, Timestamp} -> Timestamp;
                undefined -> 0
            end
      end).


%% This is the contract owner, calls shall be only available via protocol
contract_owner() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      contract_owner,
      fun() ->
              {ok, EncOwner} =
                  aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                       <<"0">>,
                                       <<"config">>, <<"contract_owner">>]),
              {ok, Pubkey}   = aeser_api_encoder:safe_decode(account_pubkey,
                                                             EncOwner),
              Pubkey
      end).

%% TODO: do we need this in HC?
expected_key_block_rate() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      key_block_rate,
      fun() ->
              {ok, ExpectedRate} =
                  aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                       <<"0">>,
                                       <<"config">>, <<"expected_key_block_rate">>]),
              ExpectedRate
      end).

genesis_protocol_version() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      genesis_protocol_version,
      fun() ->
            hd(lists:sort(maps:keys(aec_hard_forks:protocols())))
      end).

log_consensus_call(TxEnv, FunName, EncodedCallData, Amount) ->
    Height = aetx_env:height(TxEnv),
    lager:debug("Height ~p, calling ~s with amount ~p aettos, encoded ~p",
                [Height, FunName, Amount, EncodedCallData]),
    ok.

call_consensus_contract(Contract, Node, Trees, EncodedCallData, Keyword, Amount) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(
              Header, aec_block_insertion:node_hash(Node),
              aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    call_consensus_contract_(Contract, TxEnv, Trees, EncodedCallData, Keyword, Amount).

call_consensus_contract_(ContractType, TxEnv, Trees, EncodedCallData, Keyword, Amount) ->
    log_consensus_call(TxEnv, Keyword, EncodedCallData, Amount),
    ContractPubkey =
        case ContractType of
            ?ELECTION_CONTRACT -> election_contract_pubkey();
            ?REWARDS_CONTRACT -> rewards_contract_pubkey();
            ?STAKING_CONTRACT -> rewards_contract_pubkey()
        end,
    OwnerPubkey = contract_owner(),
    Contract = aect_state_tree:get_contract(ContractPubkey,
                                            aec_trees:contracts(Trees)),
    OwnerAcc = aec_accounts_trees:get(OwnerPubkey,
                                            aec_trees:accounts(Trees)),
    Fee = 5000000000000000000, %% TODO: fine tune this
    Gas = 5000000000000000000, %% TODO: fine tune this
    GasPrice = 50000000000, %% TODO: fine tune this
    {ok, OwnerAcc1} = aec_accounts:earn(OwnerAcc, Fee + Gas * GasPrice),
    Trees1 = aec_trees:set_accounts(Trees, aec_accounts_trees:enter(OwnerAcc1,
                                                                    aec_trees:accounts(Trees))),
    {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCallData),
    CallSpec = #{ caller_id   => aeser_id:create(account, OwnerPubkey),
                  nonce       => aec_accounts:nonce(OwnerAcc) + 1,
                  contract_id => aeser_id:create(contract, ContractPubkey),
                  abi_version => aect_contracts:abi_version(Contract), %% TODO: maybe get the ABI from the config?
                  fee         => Fee,
                  amount      => Amount,
                  gas         => Gas,
                  gas_price   => GasPrice,
                  call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(CallSpec),
    case aetx:process(Tx, Trees1, TxEnv) of
        {ok, Trees2, _} ->
            Calls = aec_trees:calls(Trees2),
            {contract_call_tx, CallTx} = aetx:specialize_type(Tx),
            CallId = aect_call_tx:call_id(CallTx),
            Call = aect_call_state_tree:get_call(ContractPubkey, CallId,
                                                 Calls),
            Height = aetx_env:height(TxEnv),
            case aect_call:return_type(Call) of
                ok -> pass;
                revert ->
                    lager:debug("consensus contract call failed ~s~n", [aect_call:return_value(Call)]),
                    error({consensus_call_failed, {error, aeb_fate_encoding:deserialize(aect_call:return_value(Call))}});
                error ->
                    error({consensus_call_failed, {error, aeb_fate_encoding:deserialize(aect_call:return_value(Call))}})
            end,
            %% prune the call being produced. If not done, the fees for it
            %% would be redistributed to the corresponding leaders
            {ok, aect_call_state_tree:prune(Height, Trees2), Call};
        {error, _What} = Err ->
            Err
    end.

beneficiary() ->
    beneficiary_().

beneficiary_() ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    beneficiary_(TxEnv, Trees).

beneficiary_(TxEnv, Trees) ->
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "leader", 0) of
        {ok, _Trees1, Call} ->
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            {ok, Leader};
        {error, What} ->
            %% maybe a softer approach than crash and burn?
            error({failed_to_elect_new_leader, What})
    end.

next_beneficiary() ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    Height0 = aetx_env:height(TxEnv),
    NextHeight = Height0 + 1,
    PCHeight = pc_height(NextHeight),
    case aec_parent_chain_cache:get_block_by_height(PCHeight) of
        {ok, Block} ->
            Entropy = aec_parent_chain_block:hash(Block),
            CommitmentsSophia = encode_commitments(Block),
            NetworkId = aec_parent_chain_block:encode_network_id(aec_governance:get_network_id()),
            {ok, CD} = aeb_fate_abi:create_calldata("elect_next",
                                                    [aefa_fate_code:encode_arg({string, Entropy}),
                                                     CommitmentsSophia,
                                                     aefa_fate_code:encode_arg({bytes, NetworkId})
                                                    ]),
            CallData = aeser_api_encoder:encode(contract_bytearray, CD),
            try call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "elect_next", 0) of
                {ok, _Trees1, Call} ->
                    {tuple, {{address, Leader}, _Stake}}  = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                    SignModule = get_sign_module(),
                    case SignModule:set_candidate(Leader) of
                        {error, key_not_found} ->
                            timer:sleep(1000),
                            {error, not_leader};
                        ok ->
                            {ok, Leader}
                    end;
                {error, _What} ->
                    timer:sleep(1000),
                    {error, not_leader}
            catch error:{consensus_call_failed, {error, _What}} ->
                    timer:sleep(1000),
                    {error, not_leader}
            end;
        {error, _Err} ->
            lager:debug("Unable to pick the next leader for height ~p, parent height ~p; reason is ~p",
                        [NextHeight, PCHeight, _Err]),
            timer:sleep(1000),
            {error, not_in_cache}
    end.

lazy_leader_time_delta() ->
    {ok, Interval} =
        aeu_env:user_config([<<"chain">>, <<"consensus">>,
                            <<"0">>,
                            <<"config">>,
                            <<"lazy_leader_trigger_time">>]),
    Interval.

allow_lazy_leader() ->
    Height = aec_chain:top_height(),
    PCHeight = pc_height(Height),
    case aec_parent_chain_cache:get_block_by_height(PCHeight) of
        {error, not_in_cache} ->
            false;
        {ok, _} ->
            {true, lazy_leader_time_delta()}
    end.

pick_lazy_leader(TopHash) ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, TopHash),
    case parent_chain_validators(TxEnv, Trees) of
        {ok, AllStakers} ->
            SignModule = get_sign_module(),
            LocalStakers = lists:filter(fun SignModule:is_key_present/1, AllStakers),
            Staker = lists:nth(rand:uniform(length(LocalStakers)), LocalStakers),
            SignModule:set_candidate(Staker),
            {ok, Staker};
        _ ->
            timer:sleep(1000),
            error
    end.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

get_block_producer_configs() -> [{instance_not_used,
                                  #{expected_key_block_rate => expected_key_block_rate()}}].

is_leader_valid(Node, Trees, TxEnv, PrevNode) ->
    Header = aec_block_insertion:node_header(Node),
    PrevHeader = aec_block_insertion:node_header(PrevNode),
    TimeDelta = aec_headers:time_in_msecs(Header) - aec_headers:time_in_msecs(PrevHeader),
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case TimeDelta > lazy_leader_time_delta() of
         true -> true;
         false ->
            case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "leader", 0) of
                {ok, _Trees1, Call} ->
                    {address, ExpectedLeader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                    Leader = aec_headers:miner(Header),
                    Target = aec_headers:target(Header),
                    IsDefaultT = Target =:= default_target(),
                    case ExpectedLeader =:= Leader of
                        true when IsDefaultT -> true;
                        true ->
                            {ok, CD2} = aeb_fate_abi:create_calldata("added_stake", []),
                            CallData2 = aeser_api_encoder:encode(contract_bytearray, CD2),
                            {ok, _, Call2} = call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData2, "added_stake", 0),
                            AddedStake = aeb_fate_encoding:deserialize(aect_call:return_value(Call2)),
                            ExpectedTarget = aeminer_pow:integer_to_scientific(AddedStake),
                            ExpectedTarget =:= Target;
                        false -> false
                    end;
                {error, What} ->
                    lager:info("Block validation failed with a reason ~p", [What]),
                    false
            end
    end.

parent_chain_validators(TxEnv, Trees) ->
    %% TODO: cache this
    %% this could be cached: we only need to track contract call events for
    %% validator creation and going online and offline
    {ok, CD} = aeb_fate_abi:create_calldata("sorted_validators", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?STAKING_CONTRACT, TxEnv, Trees, CallData,
                                  "sorted_validators", 0) of
        {ok, _Trees1, Call} ->
            SortedValidators =
                lists:map(
                    fun({tuple, {{address, Address}, _Amt}}) -> Address end,
                    aeb_fate_encoding:deserialize(aect_call:return_value(Call))),
            {ok, SortedValidators};
        {error, What} ->
            lager:warning("Unable to fetch validators from the contract because of ~p", [What]),
            error
    end.

create_contracts([], _TxEnv, Trees) -> Trees;
create_contracts([Contract | Tail], TxEnv, TreesAccum) ->
    %% TODO: make GasLimit and GasPrice configurable
    GasLimit = 10000000,
    GasPrice = 10000000000,
    #{ <<"amount">> := Amount
      , <<"vm_version">> := VM
      , <<"abi_version">> := ABI
      , <<"nonce">> := Nonce
      , <<"code">> := EncodedCode
      , <<"call_data">> := EncodedCallData
      , <<"owner_pubkey">> := EncodedOwner
      , <<"pubkey">> := EncodedPubkey } = Contract,
    {ok, Pubkey}   = aeser_api_encoder:safe_decode(contract_pubkey, EncodedPubkey),
    {ok, Owner}    = aeser_api_encoder:safe_decode(account_pubkey, EncodedOwner),
    {ok, Code}     = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCode),
    {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCallData),
    TxSpec = #{owner_id    => aeser_id:create(account, Owner),
              nonce       => Nonce,
              code        => Code,
              vm_version  => VM,
              abi_version => ABI,
              deposit     => 0,
              amount      => Amount,
              gas         => GasLimit,
              gas_price   => GasPrice,
              call_data   => CallData,
              fee         => 1000000000000000}, %% Overshoot the size of the actual fee
    {ok, DummyTx} = aect_create_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_create_tx:new(TxSpec#{fee => MinFee}),
    %% Make sure the transaction will give the expected pubkey.
    case aect_contracts:compute_contract_pubkey(Owner, Nonce) of
                    Pubkey -> Tx;
                    Other          -> error({unexpected_pubkey, Other, Pubkey})
                end,
    TreesAccum1 = aec_block_fork:prepare_contract_owner([Tx], TxEnv, TreesAccum),
    {_, TreesAccum2} = aec_block_fork:apply_contract_create_tx(Tx, TreesAccum1, TxEnv),
    create_contracts(Tail, TxEnv, TreesAccum2).

call_contracts([], _TxEnv, Trees) -> Trees;
call_contracts([Call | Tail], TxEnv, TreesAccum) ->
    #{  <<"caller">>          := ECaller
      , <<"nonce">>           := Nonce
      , <<"contract_pubkey">> := EContractPubkey
      , <<"abi_version">>     := ABI
      , <<"fee">>             := Fee
      , <<"amount">>          := Amount
      , <<"gas">>             := Gas
      , <<"gas_price">>       := GasPrice1
      , <<"call_data">>       := ECallData
      } = Call,
    {ok, Caller} = aeser_api_encoder:safe_decode(account_pubkey, ECaller),
    {ok, ContractPubkey}   = aeser_api_encoder:safe_decode(contract_pubkey, EContractPubkey),
    {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, ECallData),
    TxSpec = #{ caller_id    => aeser_id:create(account, Caller),
                contract_id  => aeser_id:create(contract, ContractPubkey),
                nonce        => Nonce,
                abi_version  => ABI,
                amount       => Amount,
                gas          => Gas,
                gas_price    => GasPrice1,
                call_data    => CallData,
                fee          => Fee},
    {ok, DummyTx} = aect_call_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_call_tx:new(TxSpec#{fee => MinFee}),
    {_, TreesAccum1} = aec_block_fork:apply_contract_call_tx(Tx, TreesAccum, TxEnv),
    call_contracts(Tail, TxEnv, TreesAccum1).

seal_padding_size() ->
    ?KEY_SEAL_SIZE - ?SIGNATURE_SIZE.

pc_height(ChildHeight) ->
    ChildHeight + pc_start_height().%% child starts pinning from height 1, not genesis

encode_commitments(Block) ->
    {ok, Commitments} = aec_parent_chain_block:commitments(Block),
    Commitments1 =
        lists:map(
            fun({Signature, StakerHash, TopKeyHash}) ->
                Sig = aefa_fate_code:encode_arg({signature, Signature}),
                Staker = aefa_fate_code:encode_arg({bytes, StakerHash}),
                TopHash = aefa_fate_code:encode_arg({bytes, TopKeyHash}),
                aeb_fate_data:make_tuple({Sig, Staker, TopHash})
            end,
            Commitments),
    aeb_fate_data:make_list(Commitments1).

elect_lazy_leader(Beneficiary, TxEnv, Trees) ->
    {ok, CDLazy} = aeb_fate_abi:create_calldata("elect_after_lazy_leader",
                                                [aefa_fate_code:encode_arg({address, Beneficiary})]),
    CallDataLazy = aeser_api_encoder:encode(contract_bytearray, CDLazy),
    case call_consensus_contract_(?ELECTION_CONTRACT,
                                TxEnv, Trees,
                                CallDataLazy, "elect_after_lazy_leader", 0) of
        {ok, Trees2, Call2} ->
            case aeb_fate_encoding:deserialize(aect_call:return_value(Call2)) of
                {tuple, {{address, Beneficiary}, AddedStake}} -> %% same beneficiary!
                    cache(Beneficiary, AddedStake),
                    Trees2;
                What ->
                    %% maybe a softer approach than crash and burn?
                    aec_conductor:throw_error({failed_to_elect_new_leader, What})
            end;
        {error, What} ->
            aec_conductor:throw_error({failed_to_elect_new_leader, What})
    end.

