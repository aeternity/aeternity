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
        , leader_for_height/1
        , leader_for_height/2
        %% contract access
        , call_consensus_contract_result/5
        , entropy_height/1
        , get_entropy_hash/1
        , get_contract_pubkey/1
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
    PCConfig      = maps:get(<<"parent_chain">>, Config),

    Confirmations   = maps:get(<<"confirmations">>, PCConfig, 6),
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


    {ParentConnMod, SignModule} =
        case PCType of
            <<"AE2AE">>   -> start_ae(StakersConfig);
            <<"AE2BTC">>  -> start_btc(StakersConfig, aehttpc_btc);
            <<"AE2DOGE">> -> start_btc(StakersConfig, aehttpc_doge)
        end,

    start_dependency(aec_parent_connector, [ParentConnMod, FetchInterval, ParentHosts, NetworkId,
                                            SignModule, []]),
    start_dependency(aec_parent_chain_cache, [StartHeight, RetryInterval,
                                              fun target_parent_heights/1, %% prefetch the next parent block
                                              CacheSize, Confirmations]),
    ok.

start_btc(StakersEncoded, ParentConnMod) ->
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
    SignModule = undefined,
    {ParentConnMod, SignModule}.

start_ae(StakersEncoded) ->
    Stakers =
        lists:flatmap(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := HCEncodedPubkey,
                                                 <<"priv">> := HCEncodedPrivkey}
                 }) ->
                {HCPubkey, HCPrivkey} = validate_keypair(HCEncodedPubkey, HCEncodedPrivkey),
                [{HCPubkey, HCPrivkey}]
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    start_dependency(aec_preset_keys, [StakersMap]),
    ParentConnMod = aehttpc_aeternity,
    SignModule = get_sign_module(),
    {ParentConnMod, SignModule}.

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
    Stake = 0,
    %% {ok, Stake} = aeu_ets_cache:lookup(?ETS_CACHE_TABLE, added_stake),
    Block = aec_blocks:set_target(Block0, aeminer_pow:integer_to_scientific(Stake)),
    {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_header_pre_conductor(_) -> ok.
dirty_validate_key_hash_at_height(_, _) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.

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
    case leader_for_height(Height, {hash, aec_block_insertion:node_prev_hash(Node)}) of
        {ok, Leader} ->
            case aeu_sig:verify(aec_block_insertion:node_header(Node), Leader) of
                ok         -> ok;
                {error, _} -> {error, signature_verification_failed}
            end;
        error ->
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

%% only called for key-blocks - this is the call where we set epoch and
%% leader
state_pre_transform_key_node(Node, PrevNode, Trees) ->
    PrevHeader = aec_block_insertion:node_header(PrevNode),
    {ok, PrevHash} = aec_headers:hash_header(PrevHeader),
    Height = aec_block_insertion:node_height(Node),
    %% TODO: maybe call init_epochs for height 0?
    case Height > 0 of
        true ->
            {TxEnv0, _} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, PrevHash),
            TxEnv = aetx_env:set_height(TxEnv0, Height),
            {ok, #{epoch := Epoch} = EpochInfo} = aec_chain_hc:epoch_info({TxEnv, Trees}),
            {ok, Leader} = leader_for_height(Height, {TxEnv, Trees}),
            case Height == maps:get(last, EpochInfo) of
                true ->
                    {ok, Seed} = get_entropy_hash(Epoch + 1),
                    cache_validators_for_epoch({TxEnv, Trees}, Seed, Epoch + 2),
                    step_eoe(TxEnv, Trees, Leader, Seed, 0);
                false ->
                    step(TxEnv, Trees, Leader)
            end;
        false ->
            %% No leader for genesis
            Trees
    end.

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
    Trees3 = init_epochs(TxEnv, Trees2, child_epoch_length()),
    aect_call_state_tree:prune(0, Trees3).

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
    Target = aec_headers:target(H),
    aeminer_pow:scientific_to_integer(Target).

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

%pc_finality() ->
%    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"finality">>], 0) end,
%    aeu_ets_cache:get(?ETS_CACHE_TABLE, finality, Fun).

genesis_start_time() ->
    Fun = fun() -> get_consensus_config_key([<<"genesis_start_time">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, genesis_start_time, Fun).

parent_epoch_length() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"parent_epoch_length">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, parent_epoch_length, Fun).

child_epoch_length() ->
    Fun = fun() -> get_consensus_config_key([<<"child_epoch_length">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_epoch_length, Fun).

%acceptable_sync_offset() ->
%    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"acceptable_sync_offset">>], 60000) end,
%    aeu_ets_cache:get(?ETS_CACHE_TABLE, acceptable_sync_offset, Fun).

init_epochs(TxEnv, Trees, InitialEpochLength) ->
    {ok, CD} = aeb_fate_abi:create_calldata("init_epochs", [InitialEpochLength]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "init_epochs", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Calling init_epochs failed with ~p", [What]),
            aec_conductor:throw_error(init_epochs_failed)
    end.

step(TxEnv, Trees, Leader) ->
    {ok, CD} = aeb_fate_abi:create_calldata("step", [{address, Leader}]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "step", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Calling step failed with ~p", [What]),
            aec_conductor:throw_error(step_failed)
    end.

step_eoe(TxEnv, Trees, Leader, Seed, Adjust) ->
    {ok, CD} = aeb_fate_abi:create_calldata("step_eoe", [{address, Leader}, {bytes, Seed}, Adjust]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "step_eoe", 0) of
        {ok, Trees1, _Call} ->
            lager:debug("StepEOE successful", []),
            Trees1;
        {error, What} ->
            lager:info("Calling step_eoe failed with ~p", [What]),
            aec_conductor:throw_error(step_eoe_failed)
    end.

child_block_time() ->
    Fun = fun() -> get_consensus_config_key([<<"child_block_time">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_block_time, Fun).

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
    lager:debug("Height ~p, calling ~s with amount ~p aettos, encoded ~p",
                [Height, FunName, Amount, EncodedCallData]),
    ok.

call_consensus_contract(Contract, Node, Trees, EncodedCallData, Keyword, Amount) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(
              Header, aec_block_insertion:node_hash(Node),
              aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    call_consensus_contract_(Contract, TxEnv, Trees, EncodedCallData, Keyword, Amount).

call_consensus_contract_result(ContractType, TxEnv, Trees, ContractFun, Args) ->
    {ok, CD} = aeb_fate_abi:create_calldata(ContractFun, Args),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    {ok, _, Call} = call_consensus_contract_(ContractType, TxEnv, Trees, CallData, ContractFun, 0),
    Result = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
    lager:debug("Call result ~p", [Result]),
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
    Height = aec_chain:top_height(),
    leader_for_height(Height).

next_beneficiary() ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    ChildBlockTime = child_block_time(),
    case aeu_time:now_in_msecs() - aetx_env:time_in_msecs(TxEnv) of
        T when T >= ChildBlockTime ->
            next_beneficiary(TxEnv, Trees);
        T ->
            lager:debug("Not time for next block yet; sleeping ~p ms", [ChildBlockTime - T + 1]),
            timer:sleep(ChildBlockTime - T + 1),
            next_beneficiary()
    end.

%% Called as part of deciding who will produce block at Height `height(TxEnv) + 1`
next_beneficiary(TxEnv, Trees) ->
    ChildHeight = aetx_env:height(TxEnv),
    case leader_for_height(ChildHeight + 1, {TxEnv, Trees}) of
        {ok, Leader} ->
            SignModule = get_sign_module(),
            case SignModule:set_candidate(Leader) of
                 {error, key_not_found} ->
                     lager:debug("node shall not produce (leader ~p)", [Leader]),
                     timer:sleep(200),
                     {error, not_leader};
                 ok ->
                     lager:debug("node is producer (leader ~p)", [Leader]),
                     {ok, Leader}
            end;
        error ->
            {error, not_in_cache}
    end.

get_seed(#{seed := undefined, epoch := Epoch}) when Epoch =< 2 ->
    get_entropy_hash(Epoch);
get_seed(#{seed := Hash}) when is_binary(Hash) ->
    {ok, Hash}.

get_entropy_hash(ChildEpoch) ->
    EntropyHeight = entropy_height(ChildEpoch),
    lager:debug("Entropy from PC block at height ~p", [EntropyHeight]),
    case aec_parent_chain_cache:get_block_by_height(EntropyHeight, 1000) of
        {ok, Block} ->
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

%% We start at parent epoch 1
%% We take first hash of parent epoch (hence -1)
entropy_height(ChildEpoch) ->
    (max(1, (ChildEpoch - 2)) - 1) * parent_epoch_length() + pc_start_height().

%% -- Validator schedule (cached) -----------------------------------------

%% Second parameter contains run environment for computing a validator
%% schedule, if needed.
leader_for_height(Height, RunEnv) ->
    case leader_for_height(Height) of
        error ->
            lager:debug("Unknown leader for height = ~p, computing", [Height]),
            {ok, EpochInfo = #{epoch := Epoch}} = aec_chain_hc:epoch_info(RunEnv),
            cache_validators_for_epoch_info(RunEnv, EpochInfo),
            cache_validators_for_epoch(RunEnv, Epoch + 1),
            leader_for_height(Height);
        {ok, Leader} ->
            {ok, Leader}
    end.

cache_validators_for_epoch(RunEnv, Epoch) ->
    case aec_chain_hc:epoch_info_for_epoch(RunEnv, Epoch) of
        {ok, EpochInfo} -> cache_validators_for_epoch_info(RunEnv, EpochInfo);
        _Err            -> error
    end.

cache_validators_for_epoch(RunEnv, Hash, Epoch) ->
    case aec_chain_hc:epoch_info_for_epoch(RunEnv, Epoch) of
        {ok, EpochInfo} -> cache_validators_for_epoch_info_(RunEnv, Hash, EpochInfo);
        _Err            -> error
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
        error ->
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


leader_for_height(Height) ->
    case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, validator_schedule) of
        error ->
            error;
        {ok, #{schedule := Schedule}} ->
            maps:find(Height, Schedule)
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

%% is_leader_valid is called (soon) after `state_pre_transformation_key_node`
%% only called for key-blocks - Height and Epoch should be up to date
is_leader_valid(Node, _Trees, TxEnv, _PrevNode) ->
    Height = aetx_env:height(TxEnv),
    case leader_for_height(Height) of
        {ok, ExpectedLeader} ->
            Header = aec_block_insertion:node_header(Node),
            Leader = aec_headers:miner(Header),
            Leader == ExpectedLeader;
            %% Fix this to have stake as target validated here also?
        _ ->
            %% This really should not happen, we just got through
            %% state_pre_transformation_key_node
            lager:debug("(Impossible) No leader known for height = ~p", [Height]),
            aec_conductor:throw_error(parent_chain_block_not_synced)
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

target_parent_heights(ChildHeight) ->
    {ok, EpochNum} = aec_chain_hc:epoch(ChildHeight),
    lager:debug("ChildHeight ~p, target_parent_heights called with child epoch ~p",
                [ChildHeight, EpochNum]),
    %%TODO this computation is wrong in the long run... it assumes all child epochs to be of equal length
    ParentHeightStart = (EpochNum - 2) * parent_epoch_length() + pc_start_height(),
    ParentHeightEnd = (EpochNum + 2) * parent_epoch_length() + pc_start_height(),
    [ParentHeightStart, ParentHeightEnd].

