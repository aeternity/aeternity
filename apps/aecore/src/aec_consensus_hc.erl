%%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2022, Aeternity
%%% @doc Consensus module for HyperChains consensus
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_hc).

-export([]).

-behavior(aec_consensus).


-define(SIGNATURE_SIZE, 16).
-define(ETS_CACHE_TABLE, ?MODULE).
-define(ELECTION_CONTRACT, election).
-define(STAKING_CONTRACT, staking).
-define(REWARDS_CONTRACT, rewards).
-define(FIRST_TIMESTAMPED_BLOCK, 2).

%% API
-export([ can_be_turned_off/0
        , assert_config/1
        , start/2
        , stop/0
        , is_providing_extra_http_endpoints/0
        , is_block_producer/0
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
        %% Block structure
        , key_block_height_relative_previous_block/2
        , micro_block_height_relative_previous_block/2
        %% State transition
        , state_pre_transform_key_node_consensus_switch/2
        , state_pre_transform_key_node/3
        , state_pre_transform_micro_node/3
        %% Block rewards
        , state_grant_reward/5
        , state_grant_rewards/5
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
        , leader_for_height/1
        , fixed_coinbase/0
        %% contract access
        , call_consensus_contract_result/5
        , create_consensus_call_contract_transaction/6
        , entropy_height/1
        , get_entropy_hash/1
        , get_contract_pubkey/1
        %% voting
        , vote_results/1
        , vote_results/0
        %% POS
        , next_producer/0
        ]).

-ifdef(TEST).

% -export([entropy/4]).

-endif.

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include("aec_consensus.hrl").

can_be_turned_off() -> false.
assert_config(_Config) -> ok.

start(Config, _) ->
    StakersConfig = maps:get(<<"stakers">>, Config, []),
    PinnersConfig  = maps:get(<<"pinners">>, Config, []),
    PCConfig      = maps:get(<<"parent_chain">>, Config),

    PCFinality      = maps:get(<<"finality">>, PCConfig, 5),
    StartHeight     = maps:get(<<"start_height">>, PCConfig, 0),
    ConsensusConfig = maps:get(<<"consensus">>, PCConfig, #{}),
    PollingConfig   = maps:get(<<"polling">>, PCConfig, #{}),

    PCType         = maps:get(<<"type">>, ConsensusConfig, <<"AE2AE">>),
    NetworkId      = maps:get(<<"network_id">>, ConsensusConfig, <<"ae_mainnet">>),

    FetchInterval = maps:get(<<"fetch_interval">>, PollingConfig, 500),
    RetryInterval = maps:get(<<"retry_interval">>, PollingConfig, 1000),
    CacheSize     = maps:get(<<"cache_size">>, PollingConfig, 200),
    Nodes         = maps:get(<<"nodes">>, PollingConfig, []),
    ParentHosts   = lists:map(fun aehttpc:parse_node_url/1, Nodes),


    {ParentConnMod, SignModule, HCPCMap} =
        case PCType of
            <<"AE2AE">>   -> start_ae(StakersConfig, PinnersConfig);
            <<"AE2BTC">>  -> start_btc(StakersConfig,PinnersConfig, aehttpc_btc);
            <<"AE2DOGE">> -> start_btc(StakersConfig, PinnersConfig, aehttpc_doge)
        end,

    start_dependency(aec_parent_connector, [ParentConnMod, FetchInterval, ParentHosts, NetworkId,
                                            SignModule, HCPCMap]),
    start_dependency(aec_parent_chain_cache, [StartHeight, RetryInterval,
                                              fun target_parent_heights/1, %% prefetch the next parent block
                                              CacheSize, PCFinality]),
    start_dependency(aec_pinning_agent, [get_contract_pubkey(?ELECTION_CONTRACT), default_pinning_behavior(), SignModule]),
    start_dependency(aec_hc_penalty_service, [child_epoch_length()*2]), % we only cache the last two epochs of Blocks...
    start_dependency(aec_penalty_agent, [get_contract_pubkey(?ELECTION_CONTRACT)]),
    ok.

start_btc(StakersEncoded, PinnersEncoded, ParentConnMod) ->
    Stakers =
        lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := EncodedPubkey,
                                                 <<"priv">> := EncodedPrivkey}
                 }) ->
                 {HCPubkey, HCPrivkey} = validate_keypair(EncodedPubkey, EncodedPrivkey),
                 {HCPubkey, HCPrivkey}
            end,
            StakersEncoded),
    Pinners = lists:flatmap(
        fun(#{<<"parent_chain_account">> := #{<<"pub">> := EncodedPubkey,
                                                <<"priv">> := EncodedPrivkey}
                }) ->
                {PCPubkey, PCPrivkey} = validate_keypair(EncodedPubkey, EncodedPrivkey),
                [{PCPubkey, PCPrivkey}]
        end,
        PinnersEncoded),
    StakersMap = maps:from_list(lists:append(Stakers, Pinners)),
    HCPC = lists:map(
        fun(#{<<"parent_chain_account">> := #{<<"pub">> := ParentPubEnc,
                                             <<"owner">> := OwnerPubEnc}
             }) ->
            {ok, ParentPub} = aeser_api_encoder:safe_decode(account_pubkey, ParentPubEnc),
            {ok, OwnerPub} = aeser_api_encoder:safe_decode(account_pubkey, OwnerPubEnc),
            {OwnerPub, ParentPub}
        end,
        PinnersEncoded),
    HCPCMap = maps:from_list(HCPC),
    start_dependency(aec_preset_keys, [StakersMap]),
    start_aec_eoe_vote(StakersMap),
    SignModule = undefined,
    {ParentConnMod, SignModule, HCPCMap}.

start_ae(StakersEncoded, PinnersEncoded) ->
    Stakers =
        lists:flatmap(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := HCEncodedPubkey,
                                                 <<"priv">> := HCEncodedPrivkey}
                 }) ->
                {HCPubkey, HCPrivkey} = validate_keypair(HCEncodedPubkey, HCEncodedPrivkey),
                [{HCPubkey, HCPrivkey}]
            end,
            StakersEncoded),
    Pinners = lists:flatmap(
            fun(#{<<"parent_chain_account">> := #{<<"pub">> := EncodedPubkey,
                                                 <<"priv">> := EncodedPrivkey}
                 }) ->
                 {PCPubkey, PCPrivkey} = validate_keypair(EncodedPubkey, EncodedPrivkey),
                 [{PCPubkey, PCPrivkey}]
            end,
            PinnersEncoded),
    StakersMap = maps:from_list(lists:append(Stakers, Pinners)),
    lager:debug("Stakers: ~p", [StakersMap]),
    start_dependency(aec_preset_keys, [StakersMap]),
    HCPC = lists:map(
            fun(#{<<"parent_chain_account">> := #{<<"pub">> := ParentPubEnc,
                                                 <<"owner">> := OwnerPubEnc}
                 }) ->
                {ok, ParentPub} = aeser_api_encoder:safe_decode(account_pubkey, ParentPubEnc),
                {ok, OwnerPub} = aeser_api_encoder:safe_decode(account_pubkey, OwnerPubEnc),
                {OwnerPub, ParentPub}
            end,
            PinnersEncoded),
    HCPCMap = maps:from_list(HCPC),
    lager:debug("Pinners: ~p", [HCPCMap]),
    start_aec_eoe_vote(StakersMap),
    ParentConnMod = aehttpc_aeternity,
    SignModule = get_sign_module(),
    {ParentConnMod, SignModule, HCPCMap}.

start_aec_eoe_vote(StakersMap) ->
    start_dependency(aec_eoe_vote, [StakersMap, child_block_time()]).

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
    aec_pinning_agent:stop(),
    aec_hc_penalty_service:stop(),
    ok.

is_providing_extra_http_endpoints() -> false.

client_request(_) -> error(unsupported).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_key_header(_) -> ok.

keyblocks_for_target_calc() -> 0.
keyblock_create_adjust_target(Block, []) -> {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_header_pre_conductor(_) -> ok.
dirty_validate_key_hash_at_height(_, _) -> ok.
dirty_validate_key_node_with_ctx(Node, Block, Ctx) ->
    Validators = [ fun ctx_validate_key_target/3
                 ],
    aeu_validation:run(Validators, [Node, Block, Ctx]).

ctx_validate_key_target(Node, _Block, Ctx) ->
    NodeTarget = aec_block_insertion:node_target(Node),
    NodeHeader = aec_block_insertion:node_header(Node),
    PrevNode = aec_block_insertion:ctx_prev_key(Ctx),
    PrevNodeTarget = aec_block_insertion:node_target(PrevNode),
    case aec_headers:is_eoe(NodeHeader) of
        true ->
            check_eoe(Node, NodeTarget);
        false ->
            case aec_headers:is_hole(NodeHeader) of
                true when NodeTarget == PrevNodeTarget -> ok;
                false when NodeTarget == PrevNodeTarget + 1 -> ok;
                 _ -> {error, wrong_number_of_non_holes}
            end
    end.

check_eoe(Node, NodeTarget) ->
    Header = aec_block_insertion:node_header(Node),
    Height = aec_headers:height(Header),
    case NodeTarget == Height of
        true ->
            Env = aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(Node),
                                                  aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
            case aec_chain:get_block_state(aec_block_insertion:node_prev_hash(Node)) of
                {ok, Trees} ->
                    PrevHash = aec_headers:prev_key_hash(Header),
                    Validator = aec_headers:miner(Header),
                    case aec_chain_hc:finalize_info({Env, Trees}) of
                        #{fork := PrevHash, producer := Validator} ->
                            ok;
                        Result ->
                            lager:warning("Election contract finalize info ~p didn't match hash ~p and producer ~p", [Result, PrevHash, Validator]),
                            {error, eoe_invalid}
                    end;
                _ ->
                    {error, eoe_invalid}
            end;
        false ->
            {error, eoe_invalid}
    end.

dirty_validate_micro_node_with_ctx(Node, Block, Ctx) ->
    Validators = [ fun ctx_validate_micro_block_time/3
                 , fun ctx_validate_micro_signature/3
                 ],
    aeu_validation:run(Validators, [Node, Block, Ctx]).

ctx_validate_micro_block_time(Node, _Block, Ctx) ->
    PrevNode = aec_block_insertion:ctx_prev(Ctx),
    case aec_block_insertion:node_time(Node) > aec_block_insertion:node_time(PrevNode) of
        true  -> ok;
        false -> {error, micro_block_time_too_low}
    end.

ctx_validate_micro_signature(Node, _Block, _Ctx) ->
    Height = aec_block_insertion:node_height(Node),
    case leader_for_timeslot(Height, {hash, aec_block_insertion:node_prev_hash(Node)}) of
        {ok, Leader} ->
            case aeu_sig:verify(aec_block_insertion:node_header(Node), Leader) of
                ok         -> ok;
                {error, _} -> {error, signature_verification_failed}
            end;
        {error, _} ->
            {error, signature_verification_failed}
    end.

%% ------------------------------------------------------------------------
%% -- Block structure
%% ------------------------------------------------------------------------
key_block_height_relative_previous_block(key, KeyHeight) ->
    KeyHeight + 1;
key_block_height_relative_previous_block(micro, MicroHeight) ->
    MicroHeight.

micro_block_height_relative_previous_block(key, KeyHeight) ->
    KeyHeight + 1;
micro_block_height_relative_previous_block(micro, MicroHeight) ->
    MicroHeight.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, Trees) -> Trees.

%% only called for key-blocks - this is the call where we set epoch and leader
-spec state_pre_transform_key_node(_Node, _PrevNode, aec_trees:trees()) -> {aec_trees:trees(), aetx_env:events()} | no_return().
state_pre_transform_key_node(Node, PrevNode, Trees) ->
    Height = aec_block_insertion:node_height(Node),
    state_pre_transform_node(key, Height, PrevNode, Trees).

-spec state_pre_transform_micro_node(non_neg_integer(), _PrevNode, aec_trees:trees()) -> aec_trees:trees() | no_return().
state_pre_transform_micro_node(Height, PrevNode, Trees) ->
    {T, _} = state_pre_transform_node(micro, Height, PrevNode, Trees),
    T.

-spec state_pre_transform_node(_Type :: atom(), Height :: non_neg_integer(), _PrevNode :: any(), Trees :: aec_trees:trees())
        -> {aec_trees:trees(), aetx_env:events()}.
state_pre_transform_node(_Type, Height, _PrevNode, Trees) when Height < 1 ->
    %% No leader for genesis
    {Trees, []};
state_pre_transform_node(Type, Height, PrevNode, Trees) ->
    PrevHeader = aec_block_insertion:node_header(PrevNode),
    {ok, PrevHash} = aec_headers:hash_header(PrevHeader),
    {TxEnv0, _} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, PrevHash),
    TxEnv = aetx_env:set_height(TxEnv0, Height),

    {ok, #{epoch := Epoch, first := EpochFirst, last := EpochLast} = EpochInfo} =
        aec_chain_hc:epoch_info({TxEnv, Trees}),

    lager:debug("------ STATE_PRE_TRANSFORM_NODE: height=~w epoch=~w ep_first=~w ep_last=~w",
        [Height, Epoch, EpochFirst, EpochLast]),

    case Type of
        key ->
            case Height >= EpochFirst andalso Height =< EpochLast of
                true ->
                    {ok, Leader} = leader_for_timeslot(Height, {TxEnv, Trees}),
                    step_key(Height, EpochInfo, {TxEnv, Trees}, Leader);
                false ->
                    %% We're now out of the last started epoch and the chain has halted
                    lager:warning("Epoch ~w has ended, but we're still ticking timeslots height=~w", [Epoch, Height]),
                    %% TODO: fix timings, prevent busy loop
                    timer:sleep(child_block_time() div 4),
                    {Trees, []}
            end;
        micro ->
            {ok, Leader} = leader_for_timeslot(Height, {TxEnv, Trees}),
            lager:debug("micro_calls: ~p", [aec_trees:calls(Trees)]),
            {step_micro(TxEnv, Trees, Leader), []}
    end.

-spec step_key(Height :: non_neg_integer(), EpochInfo :: aec_chain_hc:epoch_info(),
    {aetx_env:env(), aec_trees:trees()}, Leader :: binary()) -> {aec_trees:trees(), aetx_env:events()}.
step_key(Height, #{epoch := Epoch, first := EpochFirst, last := EpochLast} = EpochInfo,
         {TxEnv, Trees}, Leader) ->
    EpochFirstNonGenesis = erlang:min(?FIRST_TIMESTAMPED_BLOCK, EpochFirst),

    %% Ignore actual chain height, some blocks may be missing, check whether it is the time for a new epoch
    if Height =:= EpochFirstNonGenesis ->
        %% cache the current epoch start time
        EpochStartTime = aetx_env:time_in_msecs(TxEnv),
        lager:debug("Caching epoch_start_time=~w height=~w\n",[EpochStartTime, Height]),
        cache_child_epoch_info(Epoch, Height, EpochStartTime);
    true ->
        ok
    end,
    %% note that EpochFirst and EpochLast could be the same, not exclusive
    if Height =:= EpochLast ->
        {Trees1, CarryOverFlag, Events} = handle_pinning(TxEnv, Trees, EpochInfo, Leader),
        case get_entropy_hash(Epoch + 2) of
            {ok, Seed} ->
                lager:debug("End of epoch=~w, calling step_eoe(seed=~w)", [Epoch, Seed]),
                cache_validators_for_epoch({TxEnv, Trees1}, Seed, Epoch + 2),
                Trees2 = step_eoe(TxEnv, Trees1, Leader, Seed, -1, CarryOverFlag),
                {ok, NextEpochInfo} = aec_chain_hc:epoch_info({TxEnv, Trees2}),
                %% start_default_pinning_process(TxEnv, Trees2, Height, NextEpochInfo),
                {Trees2, Events ++ [{new_epoch, NextEpochInfo}]};
            {error, _} ->
                lager:debug("Entropy hash for height ~p is not in cache, attempting to resync", [Height]),
                %% Fail the keyblock production flow, attempt to resync
                timer:sleep(child_block_time() div 4),
                aec_conductor:throw_error(parent_chain_not_synced)
        end;
    true ->
        step(TxEnv, Trees, Leader)
    end.

create_consensus_call_contract_transaction(ContractType, OwnerPubkey, Trees, EncodedCallData, Amount, NonceOffset) ->
    ContractPubkey = get_contract_pubkey(ContractType),
    Contract = aect_state_tree:get_contract(ContractPubkey,
                                            aec_trees:contracts(Trees)),
    OwnerAcc = aec_accounts_trees:get(OwnerPubkey,
                                            aec_trees:accounts(Trees)),
    Fee = 1859800000000, %% TODO: fine tune this
    Gas = 600000, %% TODO: fine tune this
    GasPrice = 1000000, %% TODO: fine tune this

    {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCallData),
    CallSpec = #{ caller_id   => aeser_id:create(account, OwnerPubkey),
                  nonce       => aec_accounts:nonce(OwnerAcc) + NonceOffset,
                  contract_id => aeser_id:create(contract, ContractPubkey),
                  abi_version => aect_contracts:abi_version(Contract), %% TODO: maybe get the ABI from the config?
                  fee         => Fee,
                  amount      => Amount,
                  gas         => Gas,
                  gas_price   => GasPrice,
                  call_data   => CallData},
    aect_call_tx:new(CallSpec).

vote_results(Trees) ->
    aec_eoe_vote:get_finalize_transactions(Trees).

vote_results() ->
    case aec_chain:get_top_state() of
        {ok, Trees} ->
            vote_results(Trees);
        Error ->
            [{error, Error}]
    end.


cache_child_epoch_info(Epoch, Height, StartTime) ->
    %% if the leader is running on the same node, and the current process
    %% is not the leader, then the the epoch info should already be cached
    case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, child_epoch_start) of
        {ok, {E, _}} when E >= Epoch ->
            ok;  % should already be in cache
        {ok, {E, _}} when Epoch =/= E + 1 ->
            error({skipped_epoch, E, Epoch});
        Other ->
            Es = case Other of
                     {ok, {_, Es0}} when length(Es0) < 5 -> Es0;
                     {ok, {_, Es0}} -> lists:droplast(Es0);
                     error -> []
                 end,
            %% here, the current process must be the leader and is the
            %% only one who writes to this key, so there is no race
            aeu_ets_cache:put(?ETS_CACHE_TABLE, child_epoch_start,
                              {Epoch, [{Epoch, Height, StartTime} | Es]}),
            ok
    end.

%% -------------------------------------------------------------------
%% Block rewards

%% Not used for HC
state_grant_reward(_Beneficiary, _Node, _Delay, Trees, _Amount) ->
    Trees.

state_grant_rewards(Node, Delay, Trees, {Fees, BlockReward}, [_Node1, _Node2, _Node3] = Nodes) ->
    [ShareBefore, Share, ShareAfter] = fee_distribution(),
    Rewards = [(ShareBefore * Fees) div 100,
               BlockReward + (Share * Fees) div 100,
               (ShareAfter * Fees) div 100],

    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(
              Header, aec_block_insertion:node_hash(Node),
              aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    Height = aec_block_insertion:node_height(Node) - Delay,

    state_grant_reward_(TxEnv, Trees, Height, lists:zip(Nodes, Rewards), 0).

is_hole(Node) ->
    aec_headers:is_hole(aec_block_insertion:node_header(Node)).

state_grant_reward_(_TxEnv, Trees, Height, [], HoleRewards) ->
    lager:info("Burning ~p tokens, because of holes, while distributing rewards for height ~p",
               [HoleRewards, Height]),
    Trees;
state_grant_reward_(TxEnv, Trees, Height, [{Node, Amount} | Rewards], HoleRewards) ->
    %% No rewards to genesis block or holes...
    case is_hole(Node) orelse aec_block_insertion:node_height(Node) == 0 of
        true ->
            state_grant_reward_(TxEnv, Trees, Height, Rewards, HoleRewards + Amount);
        false ->
            Beneficiary = aec_block_insertion:node_beneficiary(Node),
            Trees1 = add_reward(TxEnv, Trees, Height, Beneficiary, Amount),
            state_grant_reward_(TxEnv, Trees1, Height, Rewards, HoleRewards)
    end.

add_reward(TxEnv, Trees, Height, Beneficiary, Amount) ->
    {ok, CD} = aeb_fate_abi:create_calldata("add_reward", [aefa_fate_code:encode_arg({integer, Height}),
                                                           aefa_fate_code:encode_arg({address, Beneficiary})]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    Tag = ["add_reward(value = ", integer_to_list(Amount), ", ", integer_to_list(Height), ", ",
           aeser_api_encoder:encode(account_pubkey, Beneficiary), ")"],
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, Tag, Amount) of
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
    GenesisProtocol = genesis_protocol_version(),
    #{ <<"contracts">> := Contracts
     , <<"calls">> := Calls } =
        aec_fork_block_settings:contracts(GenesisProtocol),
    GenesisHeader = genesis_raw_header(),
    {ok, GenesisHash} = aec_headers:hash_header(GenesisHeader),
    TxEnv = aetx_env:tx_env_from_key_header(GenesisHeader,
                                            GenesisHash,
                                            aec_headers:time_in_msecs(GenesisHeader),
                                            aec_headers:prev_hash(GenesisHeader)),
    Trees1 = create_contracts(Contracts, TxEnv, Trees0),
    Trees2 = call_contracts(Calls, TxEnv, Trees1),
    Trees3 = initialize_validators(TxEnv, Trees2, initial_validators()),
    Trees4 = init_epochs(TxEnv, Trees3, child_epoch_length(), pinning_reward_value()),
    aect_call_state_tree:prune(0, Trees4).

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
        0, %% Initial target`
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

generate_key_header_seal(_, Candidate, _PCHeight, _Config, _) ->
    Leader = aec_headers:beneficiary(Candidate),
    SignModule = get_sign_module(),
    case SignModule:set_candidate(Leader) of
        {error, key_not_found} ->
            timer:sleep(1000),
            {continue_mining, {error, no_solution}};
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
    ChildHeight = aec_headers:height(Header),
    {ok, ChildEpoch} = aec_chain_hc:epoch(ChildHeight - 1),
    ChildEpoch * parent_epoch_length() + pc_start_height().

next_nonce_for_sealing(PCHeight, _) ->
    PCHeight.

trim_sealing_nonce(PCHeight, _) ->
    PCHeight.

default_target() ->
    -1. %% this is an impossible value will be rewritten later on

assert_key_target_range(_) ->
    ok.

key_header_difficulty(H) ->
    aec_headers:target(H).

%% This is initial height; if neeeded shall be reinit at fork height
election_contract_pubkey() ->
    Fun = fun() ->
              EncContractId = get_consensus_config_key([<<"election_contract">>]),
              case aeser_api_encoder:safe_decode(contract_pubkey, EncContractId) of
                  {ok, Pubkey} -> Pubkey;
                  _ -> erlang:error({contract_owner_not_valid_contract, EncContractId})
              end
          end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, election_contract_pubkey, Fun).

rewards_contract_pubkey() ->
    Fun = fun() ->
              EncContractId = get_consensus_config_key([<<"rewards_contract">>]),
              case aeser_api_encoder:safe_decode(contract_pubkey, EncContractId) of
                  {ok, Pubkey} -> Pubkey;
                  _ -> erlang:error({contract_owner_not_valid_contract, EncContractId})
              end
          end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, rewards_contract_pubkey, Fun).

staking_contract_pubkey() ->
    Fun = fun() ->
              EncContractId = get_consensus_config_key([<<"staking_contract">>]),
              case aeser_api_encoder:safe_decode(contract_pubkey, EncContractId) of
                  {ok, Pubkey} -> Pubkey;
                  _ -> erlang:error({contract_owner_not_valid_contract, EncContractId})
              end
          end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, staking_contract_pubkey, Fun).

pc_start_height() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"start_height">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, pc_start_height, Fun).

parent_finality() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"finality">>], 5) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, finality, Fun).

genesis_start_time() ->
    Fun = fun() -> get_consensus_config_key([<<"genesis_start_time">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, genesis_start_time, Fun).

parent_epoch_length() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"parent_epoch_length">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, parent_epoch_length, Fun).

parent_pin_sync_margin() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_pin_sync_margin">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, parent_pin_sync_margin, Fun).

child_epoch_length() ->
    Fun = fun() -> get_consensus_config_key([<<"child_epoch_length">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_epoch_length, Fun).

pinning_reward_value() ->
    Fun = fun() -> get_consensus_config_key([<<"pinning_reward_value">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, pinning_reward_value, Fun).

default_pinning_behavior() ->
    Fun = fun() -> get_consensus_config_key([<<"default_pinning_behavior">>], false) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, default_pinning_behavior, Fun).

fixed_coinbase() ->
    Fun = fun() -> get_consensus_config_key([<<"fixed_coinbase">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, fixed_coinbase, Fun).

initial_validators() ->
    Fun = fun() ->
            lists:map(
                fun validator_config/1,
                get_consensus_config_key([<<"initial_validators">>], []))
    end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, initial_validators, Fun).

validator_config(Validator) ->
    EncOwner = maps:get(<<"owner">>, Validator),
    EncSignKey = maps:get(<<"sign_key">>, Validator, EncOwner),
    EncCaller = maps:get(<<"caller">>, Validator, EncOwner),

    {ok, Owner} = aeser_api_encoder:safe_decode(account_pubkey, EncOwner),
    {ok, SignKey} = aeser_api_encoder:safe_decode(account_pubkey, EncSignKey),
    {ok, Caller} = aeser_api_encoder:safe_decode(account_pubkey, EncCaller),

    #{ <<"owner">>        => Owner
     , <<"sign_key">>     => SignKey
     , <<"caller">>       => Caller
     , <<"stake">>        => maps:get(<<"stake">>, Validator, 0)
     , <<"restake">>      => maps:get(<<"restake">>, Validator, false)}.

fee_distribution() ->
    Fun = fun() -> get_consensus_config_key([<<"fee_distribution">>], [0, 100, 0]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, fee_distribution, Fun).

%acceptable_sync_offset() ->
%    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"acceptable_sync_offset">>], 60000) end,
%    aeu_ets_cache:get(?ETS_CACHE_TABLE, acceptable_sync_offset, Fun).

init_epochs(TxEnv, Trees, InitialEpochLength, BasePinReward) ->
    {ok, CD} = aeb_fate_abi:create_calldata("init_epochs", [InitialEpochLength, BasePinReward]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "init_epochs", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Calling init_epochs failed with ~p", [What]),
            aec_conductor:throw_error(init_epochs_failed)
    end.

-spec step(aetx_env:env(), aec_trees:trees(), binary()) -> {aec_trees:trees(), aetx_env:events()}.
step(TxEnv, Trees, Leader) ->
    {ok, CD} = aeb_fate_abi:create_calldata("step", [{address, Leader}]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "step", 0) of
        {ok, Trees1, _Call} ->
            {Trees1, []};
        {error, What} ->
            lager:info("Calling step failed with ~p", [What]),
            aec_conductor:throw_error(step_failed)
    end.

step_eoe(TxEnv, Trees, Leader, Seed, BasePinReward, CarryOver) ->
    {ok, CD} = aeb_fate_abi:create_calldata("step_eoe", [{address, Leader}, {bytes, Seed}, BasePinReward, CarryOver]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "step_eoe", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Calling step_eoe failed with ~p", [What]),
            aec_conductor:throw_error(step_eoe_failed)
    end.

%% Set the leader in case there is a micro block to be produced
%% If not, leader is anyway set when creating key block
-spec step_micro(aetx_env:env(), aec_trees:trees(), binary()) -> aec_trees:trees().
step_micro(TxEnv, Trees, Leader) ->
    {ok, CD} = aeb_fate_abi:create_calldata("step_micro", [{address, Leader}]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "step_micro", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Calling step_micro failed with ~p", [What]),
            aec_conductor:throw_error(step_micro_failed)
    end.

child_block_time() ->
    Fun = fun() -> get_consensus_config_key([<<"child_block_time">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_block_time, Fun).

child_block_production_time() ->
    Fun = fun() -> get_consensus_config_key([<<"child_block_production_time">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_block_production_time, Fun).

%% This is the contract owner, calls shall be only available via protocol
contract_owner() ->
    Fun = fun() ->
              EncOwner = get_consensus_config_key([<<"contract_owner">>]),
              case aeser_api_encoder:safe_decode(account_pubkey, EncOwner) of
                  {ok, Pubkey} -> Pubkey;
                  _ -> erlang:error({contract_owner_not_valid_account, EncOwner})
              end
          end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, contract_owner, Fun).

genesis_protocol_version() ->
    aeu_ets_cache:get(
      ?ETS_CACHE_TABLE,
      genesis_protocol_version,
      fun() ->
            hd(lists:sort(maps:keys(aec_hard_forks:protocols())))
      end).

get_consensus_config_key(Keys) ->
    case aeu_env:user_config([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>] ++ Keys) of
      {ok, Value} -> Value;
      undefined   -> erlang:error({missing_mandatory_chain_consensus_config_key, Keys})
    end.

get_consensus_config_key(Keys, Default) ->
    case aeu_env:user_config([<<"chain">>, <<"consensus">>, <<"0">>, <<"config">>] ++ Keys) of
      {ok, Value} -> Value;
      undefined   -> Default
    end.

log_consensus_call(TxEnv, FunName, EncodedCallData, Amount) ->
    Height = aetx_env:height(TxEnv),
    lager:debug("calling ~s() at height=~p amount=~p aettos, encoded ~p", [FunName, Height, Amount, EncodedCallData]),
    ok.

call_consensus_contract_result(ContractType, TxEnv, Trees, ContractFun, Args) ->
    {ok, CD} = aeb_fate_abi:create_calldata(ContractFun, Args),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    {ok, _, Call} = call_consensus_contract_(ContractType, TxEnv, Trees, CallData, ContractFun, 0),
    Result = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
    % lager:debug("Call result ~p", [Result]),
    {ok, Result}.

-spec get_contract_pubkey(election | rewards | staking) -> binary().
get_contract_pubkey(ContractType) ->
    case ContractType of
        ?ELECTION_CONTRACT -> election_contract_pubkey();
        ?REWARDS_CONTRACT -> rewards_contract_pubkey();
        ?STAKING_CONTRACT -> staking_contract_pubkey()
    end.

call_consensus_contract_(ContractType, TxEnv, Trees, EncodedCallData, Keyword, Amount) ->
    log_consensus_call(TxEnv, Keyword, EncodedCallData, Amount),
    ContractPubkey = get_contract_pubkey(ContractType),
    OwnerPubkey = contract_owner(),
    Contract = aect_state_tree:get_contract(ContractPubkey,
                                            aec_trees:contracts(Trees)),
    OwnerAcc = aec_accounts_trees:get(OwnerPubkey,
                                            aec_trees:accounts(Trees)),
    Fee = 5_000000000_000000000, %% TODO: fine tune this
    Gas = 5_000000000_000000000, %% TODO: fine tune this
    GasPrice = 50_000000000, %% TODO: fine tune this
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
    try aetx:process(Tx, Trees1, TxEnv) of
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
                    Err = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
                    lager:debug("consensus contract call failed ~s~n", [Err]),
                    error({consensus_call_failed, {error, Err}});
                error ->
                    lager:debug("consensus contract call failed: error~n", []),
                    error({consensus_call_failed, {error, aeb_fate_encoding:deserialize(aect_call:return_value(Call))}})
            end,
            %% prune the call being produced. If not done, the fees for it
            %% would be redistributed to the corresponding leaders
            {ok, aect_call_state_tree:prune(Height, Trees2), Call};
        {error, _What} = Err ->
            Err
    catch Err:Reason:St ->
        lager:error("Consensus contract call to ~p crashed(~p): ~p", [Keyword, Err, Reason]),
        lager:info("Crash stacktrace: ~p", [St]),
        error({consensus_call_crashed, {error, Reason}})
    end.

beneficiary() ->
    % Height = aec_chain:top_height(),
    #{slot := Slot} = get_slot_info(),
    leader_for_height(Slot).

next_beneficiary() ->
    {error, not_applicable}.

next_producer() ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    TopHeight = aec_blocks:height(TopKeyBlock),
    RunEnv = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    lager:debug("- - - - - Next beneficiary: height=~w", [TopHeight]),
    BlockTime = child_block_time(),
    case get_slot_info(TopHeight, RunEnv) of
        #{slot := TopHeight, now := Now, t0 := T0} ->
            %% We already got a block for the current Slot
            DeltaT = BlockTime - (Now - T0) rem BlockTime,
            {wait, slot_filled, DeltaT};
        #{slot := Slot} = SlotInfo ->
            case leader_for_timeslot(Slot, RunEnv) of
                {ok, Leader} ->
                    activate_next_leader(Leader, TopHeight, SlotInfo, RunEnv);
                {error, _} ->
                    {error, not_in_cache, BlockTime div 5}
            end
    end.

activate_next_leader(Leader, TopHeight, SlotInfo = #{slot := Slot}, RunEnv) ->
    EpochInfo =
        case maps:get(epoch_info, SlotInfo, undefined) of
            undefined ->
                {ok, EI} = aec_chain_hc:epoch_info(RunEnv),
                EI;
            EI ->
                EI
        end,

    case maps:get(last, EpochInfo) of
        Slot ->
            {TxEnv, _} = RunEnv,
            Hash = aetx_env:key_hash(TxEnv),
            #{epoch := Epoch, seed := Seed, validators := Validators, length := Length} = EpochInfo,
            aec_eoe_vote:negotiate(Epoch, Slot, Hash, Leader, Validators, Seed, Length);
        _ ->
            ok
    end,

    #{now := Now, t0 := T0} = SlotInfo,
    BlockTime = child_block_time(),
    SignModule = get_sign_module(),
    DeltaT = BlockTime - (Now - T0) rem BlockTime,
    case SignModule:set_candidate(Leader) of
        {error, key_not_found} ->
            lager:debug("Node is not producer for ~p", [Slot]),
            {wait, not_producer, DeltaT};
        ok ->
            BlockProdTime = child_block_production_time(),
            lager:debug("Node is producer for block ~p", [Slot]),
            case TopHeight + 1 == Slot of
                true ->
                    DeltaT1 = DeltaT - BlockProdTime,
                    {ok, {leader, Leader}, DeltaT1};
                false ->
                    %% TODO: Possibly start even earlier to have time for holes...
                    DeltaT1 = DeltaT - BlockProdTime,
                    Holes = Slot - (TopHeight + 1),
                    lager:debug("~p blocks missing, need HOLES!!", [Holes]),
                    {ok, {leader_hole, Leader, Holes}, DeltaT1}
            end
    end.

-spec get_slot_info() -> map().
get_slot_info() ->
    %% Assumes that the next epoch could be started
    TopHash = aec_chain:top_block_hash(), % TODO: check not 'undefined'
    PrevHash = aec_chain:prev_hash_from_hash(TopHash),
    RunEnv = {TxEnv, _} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, PrevHash),
    get_slot_info(aetx_env:height(TxEnv) + 1, RunEnv).

-spec get_slot_info(TopHeight :: pos_integer(), RunEnv :: aec_chain_hc:run_env()) -> map().
get_slot_info(0, _RunEnv) ->
    Now = aeu_time:now_in_msecs(),
    #{epoch => 1, slot => 1, t0 => Now, delta => 0, now => Now};
get_slot_info(_TopHeight, RunEnv) ->
    {ok, EpochInfo = #{epoch := Epoch, first := First}} = aec_chain_hc:epoch_info(RunEnv),
    BlockTime     = child_block_time(),
    Now = aeu_time:now_in_msecs(),
    T0  = get_t0(Epoch, First),

    ElapsedBlocks = (Now - T0) div BlockTime,
    Delta = if Epoch == 1 -> ElapsedBlocks + 1;
               true       -> ElapsedBlocks
            end,
    Slot = First + Delta,
    #{epoch => Epoch, slot => Slot, t0 => T0, delta => Delta,
      now => Now, epoch_info => EpochInfo}.

get_t0(1, _) ->
    {ok, Block} = aec_chain:get_key_block_by_height(1),
    aec_blocks:time_in_msecs(Block);
get_t0(_Epoch, Height) ->
    {ok, Block} = aec_chain:get_key_block_by_height(Height - 1),
    aec_blocks:time_in_msecs(Block).

get_seed(#{seed := undefined, epoch := Epoch}) when Epoch =< 2 ->
    get_entropy_hash(Epoch);
get_seed(#{seed := Hash}) when is_binary(Hash) ->
    {ok, Hash}.

get_entropy_hash(ChildEpoch) ->
    EntropyHeight = entropy_height(ChildEpoch),
    lager:debug("Entropy for epoch ~p from PC block at height ~p", [ChildEpoch, EntropyHeight]),
    case aec_parent_chain_cache:get_block_by_height(EntropyHeight, 1000) of
        {ok, Block} ->
            aec_eoe_vote:add_parent_block(ChildEpoch, Block),
            {ok, aec_parent_chain_block:hash(Block)};
        {error, _Err} ->
            lager:debug("Unable to pick the next leader for epoch ~p, parent height ~p; reason is ~p",
                        [ChildEpoch, EntropyHeight, _Err]),
            {error, not_in_cache}
    end.

is_block_producer() ->
    aeu_ets_cache:get(?ETS_CACHE_TABLE, is_block_producer, fun is_block_producer_/0).

is_block_producer_() ->
    TopHeader = aec_chain:top_header(),
    Config = aec_consensus:get_consensus_config_at_height(aec_headers:height(TopHeader)),
    StakersConfig = maps:get(<<"stakers">>, Config, []),

    StakersConfig /= [].

%% See whitepaper for details, but corresponding parent epoch is max(0, n-3)
entropy_height(ChildEpoch) ->
    max(0, (ChildEpoch - 3)) * parent_epoch_length() + pc_start_height().

%% -- Validator schedule (cached) -----------------------------------------

%% This function takes height values or timeslots. Timeslots are based on the current clock and epoch start time.
-spec leader_for_height(Height :: non_neg_integer()) -> {ok, binary()} | {error, atom()}.
leader_for_height(Height) ->
    case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, validator_schedule) of
        error ->
            {error, not_in_cache};
        {ok, #{schedule := Schedule}} ->
            case maps:get(Height, Schedule, undefined) of
                undefined -> {error, not_in_cache};
                Leader -> {ok, Leader}
            end
    end.

get_cached_schedule(Height) ->
    case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, validator_schedule) of
        error -> error;
        {ok, #{schedule := Schedule}} ->
            [ P || {H, P} <- maps:to_list(Schedule), H >= Height ]
    end.

%% Timeslot is the same as the height for already produced blocks. Timeslot can be ahead of the height
%% if the block is not produced in time, and the time continues forward.
%% Second parameter contains run environment for computing a validator schedule, if needed.
-spec leader_for_timeslot(Timeslot :: pos_integer(), RunEnv :: aec_chain_hc:run_env()) -> {ok, binary()} | {error, atom()}.
leader_for_timeslot(Timeslot, RunEnv) ->
    %% TODO: Special case: When the end of epoch is not happening, the timeslots will spill into "next epoch" without starting the next epoch
    case leader_for_height(Timeslot) of
        {ok, Leader} ->
            lager:debug("Leader for timeslot=~w is ~w", [Timeslot, Leader]),
            {ok, Leader};
        {error, _Rsn} ->
            lager:debug("Unknown leader (reason=~w) for timeslot=~w, computing", [_Rsn, Timeslot]),
            {ok, EpochInfo = #{epoch := Epoch, last := EpochLast}} = aec_chain_hc:epoch_info(RunEnv),
            cache_validators_for_epoch_info(RunEnv, EpochInfo),
            cache_validators_for_epoch(RunEnv, Epoch + 1),
            case Timeslot > EpochLast of
                true ->
                    {error, epoch_did_not_end};
                false ->
                    %% Try once more
                    leader_for_height(Timeslot)
            end
    end.

cache_validators_for_epoch(RunEnv, Epoch) ->
    case epoch_info_for_epoch(RunEnv, Epoch) of
        {ok, EpochInfo} -> cache_validators_for_epoch_info(RunEnv, EpochInfo);
        _Err            -> error
    end.

cache_validators_for_epoch(RunEnv, Hash, Epoch) ->
    case epoch_info_for_epoch(RunEnv, Epoch) of
        {ok, EpochInfo} -> cache_validators_for_epoch_info_(RunEnv, Hash, EpochInfo);
        _Err            -> error
    end.

epoch_info_for_epoch(RunEnv, Epoch) ->
    try
        aec_chain_hc:epoch_info_for_epoch(RunEnv, Epoch)
    catch _:_ ->
        %% Not sure this is possible...
        error
    end.


cache_validators_for_epoch_info(RunEnv, EpochInfo) ->
    case get_seed(EpochInfo) of
        {ok, Seed} -> cache_validators_for_epoch_info_(RunEnv, Seed, EpochInfo);
        {error, _} -> error
    end.

-define(MAX_VALIDATOR_CACHE_SIZE, 5).

cache_validators_for_epoch_info_(RunEnv, Hash, EpochInfo) ->
    #{ first      := First
     , epoch      := Epoch
     , length     := Length
     , validators := Validators} = EpochInfo,
    %% Check if it is already in cache to avoid extra work
    case leader_for_height(First) of
        {error, _} ->
            {ok, RawSchedule} = aec_chain_hc:validator_schedule(RunEnv, Hash, Validators, Length),
            Schedule = maps:from_list(enumerate(First, RawSchedule)),

            Cache = aeu_ets_cache:lookup(?ETS_CACHE_TABLE, validator_schedule, #{}),

            Cache1 = Cache#{epochs => maps:get(epochs, Cache, []) ++ [Epoch],
                            epoch_infos => maps:put(Epoch, {First, First + Length - 1}, maps:get(epoch_infos, Cache, #{})),
                            schedule => maps:merge(maps:get(schedule, Cache, #{}), Schedule)},

            Cache2 =
                case length(maps:get(epochs, Cache1)) > ?MAX_VALIDATOR_CACHE_SIZE of
                    true  -> gc_validator_cache(Cache1);
                    false -> Cache1
                end,

            aeu_ets_cache:put(?ETS_CACHE_TABLE, validator_schedule, Cache2);
        {ok, _Leader} ->
            %% Already in cache
            ok
    end.

gc_validator_cache(C0 = #{epochs := [GC | Rest], epoch_infos := EIs, schedule := Schedule}) ->
    C1 = C0#{epochs => Rest},
    case maps:find(GC, EIs) of
        error -> C1;
        {ok, {First, Last}} ->
            Schedule1 = maps:without(lists:seq(First, Last), Schedule),
            C1#{epoch_infos := maps:remove(GC, EIs),
                schedule    := Schedule1}
    end.

%% support OTP24 without lists:enumerate
enumerate(From, List) when is_integer(From) ->
    lists:zip(lists:seq(From, From + length(List) - 1), List).

allow_lazy_leader() ->
    false.

pick_lazy_leader(_TopHash) ->
    error.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

get_block_producer_configs() -> [{instance_not_used,
                                  #{child_block_time => child_block_time()}}].

is_leader_valid(Node, _Trees, _TxEnv, _PrevNode) ->
    NodeHeader = aec_block_insertion:node_header(Node),
    NodeHeight = aec_headers:height(NodeHeader),
    NodeProducer = aec_headers:miner(NodeHeader),
    case aec_headers:is_hole(NodeHeader) of
        false -> is_leader_valid_for_block(NodeHeight, NodeProducer);
        true -> is_leader_valid_for_hole(NodeHeight, NodeProducer)
end.

is_leader_valid_for_block(Height, Producer) ->
    %% A non-hole block can only be mined by the current leader
    case leader_for_height(Height) of
        {ok, ExpectedProducer} ->
            Valid = Producer =:= ExpectedProducer,
            lager:debug("non-hole block, valid=~w height=~w produced_by=~w expected_producer=~w",
                [Valid, Height, Producer, case Valid of true -> true; false -> ExpectedProducer end]),
            Valid;
        %% Fix this to have stake as target validated here also?
        {error, _Rsn} ->
            %% This really should not happen, we just got through state_pre_transformation_key_node
            lager:debug("(Impossible) No leader known (reason=~w) for height=~w, produced_by=~w", [_Rsn, Height, Producer]),
            aec_conductor:throw_error(parent_chain_block_not_synced)
    end.

is_leader_valid_for_hole(Height, Producer) ->
    %% A hole block can be mined by any node on the schedule, which is NOT the current leader.
    %% But we also accept other nodes' hole blocks
    case leader_for_height(Height) of
        {ok, LeaderAtHeight} ->
            Schedule = get_cached_schedule(Height),
            Valid = % LeaderAtHeight /= Producer andalso
                lists:member(Producer, Schedule),
            lager:debug("HOLE block, valid=~w produced_by=~w leader_at_height=~w", [Valid, Producer, LeaderAtHeight]),
            Valid;
        {error, _Rsn} ->
            lager:debug("(Impossible) No leader known (reason=~w) for a hole block, produced_by=~w height=~w", [_Rsn, Producer, Height]),
            aec_conductor:throw_error(no_leader_known_for_hole_block)
    end.

handle_pinning(TxEnv, Trees, EpochInfo, Leader) ->
    case validate_pin(TxEnv, Trees, EpochInfo) of
        pin_missing ->
            lager:debug("PINNING: no proof posted"),
            {Trees, true, [{pin, {no_proof_posted}}]};
        pin_correct ->
            {Trees1, Events} = add_pin_reward(Trees, TxEnv, Leader, EpochInfo),
            lager:debug("PINNING: pin validated"),
            {Trees1, false, Events};
        {pin_validation_fail, Msg} ->
            lager:warning("PINNING: Incorrect proof posted: ~p", [Msg]),
            {Trees, true, [{pin, {incorrect_proof_posted}}]}
    end.

validate_pin(TxEnv, Trees, CurEpochInfo) ->
    case aec_chain_hc:pin_info({TxEnv, Trees}) of
        undefined -> pin_missing;
        {bytes, EncTxHash} ->
            try
                #{epoch := CurEpoch} = CurEpochInfo,
                {ok, #{epoch := PinEpoch, height := PinHeight, block_hash := PinHash, pc_height := PcHeight}} =
                    aec_parent_connector:get_pin_by_tx_hash(EncTxHash),
                    case {ok, PinHash} =:= aec_chain_state:get_key_block_hash_at_height(PinHeight) of
                        true ->
                            {PcMin, PcMax} = get_pcpinheights_from_pinepoch(PinEpoch),
                            if
                                PcHeight >= PcMin andalso PcHeight =< PcMax ->
                                    pin_correct;
                                true ->
                                    {pin_validation_fail, "PC Pin not in sync"}
                            end;
                    false -> {pin_validation_fail, "Incorrect Hash pinned to parent chain"}
                end
            catch
                Type:Err ->
                    {pin_validation_fail, Err}
            end
    end.

% returns the "ideal"/in-sync PC start and end heights of the epoch where the pin from PinEpoch needs to be (i.e. the next PC epoc),
% with respect to finality and "margin of sync"
get_pcpinheights_from_pinepoch(PinEpoch) ->
    PcEpochLength = parent_epoch_length(),
    BaseStart = pc_start_height() + (PinEpoch * PcEpochLength),
    SyncMargin = parent_pin_sync_margin(),
    PCFinality = parent_finality(),
    {BaseStart - SyncMargin + PCFinality, BaseStart + (PcEpochLength - 1) + SyncMargin + PCFinality}.


add_pin_reward(Trees, TxEnv, Leader, #{epoch := CurEpoch, last := Last}) ->
    #{cur_pin_reward := Reward} = aec_chain_hc:pin_reward_info({TxEnv, Trees}),
    Event = {pin, {pin_accepted, #{reward => Reward, recipient => Leader, epoch => CurEpoch, height => Last}}},
    Trees1 = add_reward(TxEnv, Trees, Last, Leader, Reward),
    {Trees1, [Event]}.

create_contracts([], _TxEnv, Trees) -> Trees;
create_contracts([Contract | Tail], TxEnv, TreesAccum) ->
    %% TODO: make GasLimit and GasPrice configurable
    GasLimit = 10_000_000,
    GasPrice = 10_000_000_000,
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
              fee         => 1_000_000_000_000_000}, %% Overshoot the size of the actual fee
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

initialize_validators(_TxEnv, Trees, []) -> Trees;
initialize_validators(TxEnv, Trees, [Validator | Tail]) ->
    #{ <<"owner">>        := Owner
     , <<"sign_key">>     := SignKey
     , <<"caller">>       := Caller
     , <<"stake">>        := Stake
     , <<"restake">>      := Restake} = Validator,

    lager:info("Initializing validators: Owner, SignKey, Caller ~p ~p ~p", [Owner, SignKey, Caller]),
    Args = [aefa_fate_code:encode_arg({address, Owner}),
        aefa_fate_code:encode_arg({address, SignKey}),
        aefa_fate_code:encode_arg({bool, Restake})],
    {ok, CallData} = aeb_fate_abi:create_calldata("new_validator", Args),
    CallerAcc = aec_accounts_trees:get(Caller, aec_trees:accounts(Trees)),
    ContractPubkey = staking_contract_pubkey(),
    Contract = aect_state_tree:get_contract(ContractPubkey, aec_trees:contracts(Trees)),
    ABIVersion = aect_contracts:abi_version(Contract),
    TxSpec = #{ caller_id    => aeser_id:create(account, Caller),
                contract_id  => aeser_id:create(contract, ContractPubkey),
                nonce        => aec_accounts:nonce(CallerAcc) + 1,
                abi_version  => ABIVersion,
                amount       => Stake,
                gas          => 1000000,
                gas_price    => 1000000000,
                fee          => 1000000000000000,
                call_data    => CallData},
    {ok, DummyTx} = aect_call_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_call_tx:new(TxSpec#{fee => MinFee}),
    {_, Trees1} = aec_block_fork:apply_contract_call_tx(Tx, Trees, TxEnv),
    initialize_validators(TxEnv, Trees1, Tail).

seal_padding_size() ->
    ?KEY_SEAL_SIZE - ?SIGNATURE_SIZE.

target_parent_heights(ChildHeight) ->
    {ok, EpochNum} = aec_chain_hc:epoch(ChildHeight),
    lager:debug("ChildHeight ~p, target_parent_heights called with child epoch ~p",
                [ChildHeight, EpochNum]),
    ParentHeightStart = (EpochNum - 2) * parent_epoch_length() + pc_start_height(),
    ParentHeightEnd = (EpochNum + 2) * parent_epoch_length() + pc_start_height(),
    [ParentHeightStart, ParentHeightEnd].

