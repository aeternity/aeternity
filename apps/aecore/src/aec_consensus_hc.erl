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
        , check_parent_generation_time/3
        %% rewards and signing
        , beneficiary/0
        , next_beneficiary/0
        , allow_lazy_leader/0
        , pick_lazy_leader/1
        , get_sign_module/0
        , get_type/0
        , get_block_producer_configs/0
        , is_leader_valid/4
        %% contract access
        , call_consensus_contract_result/5
        ]).

-ifdef(TEST).

% -export([entropy/4]).

-endif.

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include("aec_consensus.hrl").

can_be_turned_off() -> false.
assert_config(_Config) -> ok.

start(Config, #{block_production := BlockProduction}) ->
    #{ <<"stakers">>      := StakersConfig,
       <<"parent_chain">> := PCConfig      } = Config,

    StakersConfig = maps:get(<<"stakers">>, Config, []),
    PCConfig      = maps:get(<<"parent_chain">>, Config),

    Confirmations        = maps:get(<<"confirmations">>, PCConfig, 6),
    StartHeight          = maps:get(<<"start_height">>, PCConfig, 0),
    ConsensusConfig      = maps:get(<<"consensus">>, PCConfig, #{}),
    PollingConfig        = maps:get(<<"polling">>, PCConfig, #{}),

    PCType         = maps:get(<<"type">>, ConsensusConfig, <<"AE2AE">>),
    NetworkId      = maps:get(<<"network_id">>, ConsensusConfig, <<"ae_mainnet">>),
    PCSpendAddress = maps:get(<<"spend_address">>, ConsensusConfig, <<"">>),

    FetchInterval  = maps:get(<<"fetch_interval">>, PollingConfig, 500),
    RetryInterval  = maps:get(<<"retry_interval">>, PollingConfig, 1000),
    CacheSize      = maps:get(<<"cache_size">>, PollingConfig, 200),
    Nodes          = maps:get(<<"nodes">>, PollingConfig, []),
    ParentHosts    = lists:map(fun aehttpc:parse_node_url/1, Nodes),


    {ParentConnMod, _PCSpendPubkey, _HCs, SignModule} =
        case PCType of
            <<"AE2AE">> -> start_ae(StakersConfig, PCSpendAddress);
            <<"AE2BTC">> -> start_btc(StakersConfig, PCSpendAddress, aehttpc_btc);
            <<"AE2DOGE">> -> start_btc(StakersConfig, PCSpendAddress, aehttpc_doge)
        end,

    Hash2IntFun = fun ParentConnMod:hash_to_integer/1,
    aeu_ets_cache:put(?ETS_CACHE_TABLE, hash_to_int, Hash2IntFun),

    start_dependency(aec_parent_connector, [ParentConnMod, FetchInterval, ParentHosts, NetworkId,
                                            SignModule, []]),
    start_dependency(aec_parent_chain_cache, [StartHeight, RetryInterval, fun target_parent_heights/1, %% prefetch the next parent block
                                              CacheSize, Confirmations,
                                              BlockProduction]),
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
                                                 <<"priv">> := HCEncodedPrivkey}
                  %% <<"parent_chain_account">> := #{<<"pub">> := PCEncodedPubkey,
                  %%                                 <<"priv">> := PCEncodedPrivkey}
                 }) ->
                {HCPubkey, HCPrivkey} = validate_keypair(HCEncodedPubkey, HCEncodedPrivkey),
                %% {PCPubkey, PCPrivkey} = validate_keypair(PCEncodedPubkey, PCEncodedPrivkey),
                [{HCPubkey, HCPrivkey}]
                %% [{HCPubkey, HCPrivkey}, {PCPubkey, PCPrivkey}]
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    %% TODO: ditch this after we move beyond OTP24
    _Mod = aec_preset_keys,
    start_dependency(aec_preset_keys, [StakersMap]),
    HCPCPairs = lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := HCEncodedPubkey}
                  %% <<"parent_chain_account">> := #{<<"pub">> := PCEncodedPubkey}
                 }) ->
                 {ok, HCPubkey} = aeser_api_encoder:safe_decode(account_pubkey, HCEncodedPubkey),
                 %% {HCPubkey, PCEncodedPubkey}
                 HCPubkey
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
    Stake = 0,
    %% {ok, Stake} = aeu_ets_cache:lookup(?ETS_CACHE_TABLE, added_stake),
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
    {ok, PrevHash} = aec_headers:hash_header(PrevHeader),
    Height = aec_block_insertion:node_height(Node),
    case Height > 0 of
        true ->
            {TxEnv, _} = aetx_env:tx_env_and_trees_from_hash(aetx_transaction, PrevHash),
            tick(TxEnv, Trees);
        false -> Trees %% do not elect leader for genesis
    end.

%cache(Leader, AddedStake) ->
%    aeu_ets_cache:put(?ETS_CACHE_TABLE, current_leader, Leader),
%    aeu_ets_cache:put(?ETS_CACHE_TABLE, added_stake, AddedStake),
%    ok.

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
    aect_call_state_tree:prune(0, Trees2).

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
    ChildEpoch * parent_generation() + pc_start_height().

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

pc_start_height() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"start_height">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, pc_start_height, Fun).

%pc_finality() ->
%    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"finality">>], 0) end,
%    aeu_ets_cache:get(?ETS_CACHE_TABLE, finality, Fun).

genesis_start_time() ->
    Fun = fun() -> get_consensus_config_key([<<"genesis_start_time">>], 0) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, genesis_start_time, Fun).

parent_generation() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"parent_generation">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, parent_generation, Fun).

acceptable_sync_offset() ->
    Fun = fun() -> get_consensus_config_key([<<"parent_chain">>, <<"acceptable_sync_offset">>], 60000) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, acceptable_sync_offset, Fun).


get_child_epoch_length(TxEnv, Trees) ->
    {ok, Result} = call_consensus_contract_result(?ELECTION_CONTRACT, TxEnv, Trees, "epoch_length", []),
    Result.

get_child_epoch(TxEnv, Trees) ->
    {ok, Result} = call_consensus_contract_result(?ELECTION_CONTRACT, TxEnv, Trees, "epoch", []),
    Result.

%get_blocks_upto_child_epoch(TxEnv, Trees) ->
%    {ok, NrBlocks} = call_consensus_contract_result(?ELECTION_CONTRACT, TxEnv, Trees, "blocks_to_fill_epoch", []),
%    NrBlocks.

set_child_epoch_length(Length, TxEnv, Trees) ->
    {ok, CD} = aeb_fate_abi:create_calldata("set_next_epoch_length",
                                            [
                                            aefa_fate_code:encode_arg({integer, Length})
                                            ]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    try call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "set_next_epoch_length", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Retrieving epoch length failed with ~p", [What]),
            aec_conductor:throw_error(set_epoch_length_failed)
    catch error:{consensus_call_failed, {error, Why}} ->
            lager:info("Retrieving epoch length failed with ~p", [Why]),
            aec_conductor:throw_error(set_epoch_length_failed);
          error:{consensus_call_crashed, {error, _Why}} ->
            aec_conductor:throw_error(set_epoch_length_call_crashed)
    end.

tick(TxEnv, Trees) ->
    {ok, CD} = aeb_fate_abi:create_calldata("tick", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "tick", 0) of
        {ok, Trees1, _Call} ->
            Trees1;
        {error, What} ->
            lager:info("Retrieving epoch length failed with ~p", [What]),
            aec_conductor:throw_error(set_epoch_length_failed)
    end.


child_block_time() ->
    Fun = fun() -> get_consensus_config_key([<<"child_block_time">>]) end,
    aeu_ets_cache:get(?ETS_CACHE_TABLE, child_block_time, Fun).

parent_generation_block_time() ->
    aeu_ets_cache:lookup(?ETS_CACHE_TABLE, parent_generation_block_time).

parent_generation_block_time(Time) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, parent_generation_block_time, Time).

check_parent_generation_time(Block, TxEnv, Trees) ->
    PCTime1 = aec_parent_chain_block:time(Block),
    case parent_generation_block_time() of
        error ->
            %% This is the first epoch
            parent_generation_block_time(PCTime1),
            Trees;
        {ok, PCTime1} ->
            % Already been checked
            Trees;
        {ok, PCTime0} ->
            OldEpochLength = get_child_epoch_length(TxEnv, Trees),
            ExpectedEpochDuration = child_block_time() * OldEpochLength,
            PCGenerationDuration = PCTime1 - PCTime0,
            AcceptableOffset = acceptable_sync_offset(),
            case abs(ExpectedEpochDuration - PCGenerationDuration) of
                DurationDiff when DurationDiff > AcceptableOffset ->
                    NewEpochLength = ceil(PCGenerationDuration / child_block_time()),
                    lager:info("Adjusted child epoch length from ~p to ~p because of a difference in the parent chain of ~p", [OldEpochLength, NewEpochLength, DurationDiff]),
                    set_child_epoch_length(NewEpochLength, TxEnv, Trees);
                _ ->
                    Trees
            end,
            parent_generation_block_time(PCTime1),
            Trees
    end.

%entropy(Block, Height, TxEnv, Trees) ->
%    case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, seed) of
%        {ok, {_OldState, Seed, Height, _OldPCHeight}} ->
%            {Seed, Trees};
%        _Other ->
%            PCHeight = aec_parent_chain_block:height(Block),
%            Hash = aec_parent_chain_block:hash(Block),
%            EpochLength = get_child_epoch_length(TxEnv, Trees),
%            BlocksLeft = get_blocks_upto_child_epoch(TxEnv, Trees),
%            %% TODO possibly set new entropy at last block in epoch, i.e. BlocksLeft == 0
%            case BlocksLeft == EpochLength of
%                true ->
%                    case set_new_entropy(Hash, Height, PCHeight) of
%                        {_SeedState, _OldSeed, _OldHeight, SeedHeight} when PCHeight > SeedHeight ->
%                            aec_conductor:throw_error(parent_chain_finality_not_reached);
%                        _ ->
%                            Trees1 = check_parent_generation_time(Block, TxEnv, Trees),
%                            {Hash, Trees1}
%                    end;
%                _ ->
%                    {next_entropy(Hash, Height, PCHeight), Trees}
%            end
%    end.

%set_new_entropy(Hash, Height, PCHeight) ->
%    case aec_parent_chain_cache:get_block_by_height(PCHeight + pc_finality()) of
%        {error, not_in_cache} ->
%            case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, seed) of
%                error ->
%                    lager:error("Parent finality not reached for height ~p", [PCHeight]),
%                    aec_conductor:throw_error(parent_chain_block_not_synced);
%                {ok, {_OldState, _OldSeed, _OldHeight, _OldPCHeight} = Old} ->
%                    lager:warning("Parent finality not reached for parent height ~p", [PCHeight]),
%                    %% generate a block with the previous epoch, next block will check the epoch again.
%                    Old
%            end;
%        {ok, _Block} ->
%            lager:info("New epoch at parent height ~p", [PCHeight]),
%            Seed = hash_to_int(Hash),
%            State = rand:seed_s(exsss, Seed),
%            Result = {State, Hash, Height, PCHeight},
%            aeu_ets_cache:put(?ETS_CACHE_TABLE, seed, Result),
%            Result
%    end.
%
%next_entropy(Hash, Height, PCHeight) ->
%    {NewState, _Seed, _Height, NewPCHeight} =
%        case aeu_ets_cache:lookup(?ETS_CACHE_TABLE, seed) of
%            {ok, {_State, _OldSeed, _OldHeight, PCHeight} = Result} ->
%                Result;
%            _ ->
%                lager:info("Trying to set entropy for parent height ~p", [PCHeight]),
%                set_new_entropy(Hash, Height, PCHeight)
%        end,
%    next_entropy_from_seed(NewState, Height, NewPCHeight).

%next_entropy_from_seed(State, Height, PCHeight) ->
%    {HashInt, State2} = rand:bytes_s(256, State),
%    Seed = list_to_bitstring(base58:binary_to_base58(HashInt)),
%    aeu_ets_cache:put(?ETS_CACHE_TABLE, seed, {State2, Seed, Height, PCHeight}),
%    Seed.


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

%lazy_leader_time_delta() ->
%    case aeu_env:user_config([<<"chain">>, <<"consensus">>, <<"0">>,
%                              <<"config">>, <<"lazy_leader_trigger_time">>]) of
%        {ok, Interval} -> Interval;
%        undefined -> 10000
%    end.

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

next_beneficiary(TxEnv, Trees) ->
    ChildHeight = aetx_env:height(TxEnv),
    case leader_for_height(ChildHeight + 1) of
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
            %% Check whether we can compute the leader schedule
            Epoch = get_child_epoch(TxEnv, Trees),
            EntropyHeight = entropy_height(Epoch),
            lager:debug("Entropy from PC block at height ~p", [EntropyHeight]),
            case aec_parent_chain_cache:get_block_by_height(EntropyHeight, 1000) of
                {ok, Block} ->
                    Validators = get_sorted_validators(TxEnv, Trees),
                    EpochLength = get_child_epoch_length(TxEnv, Trees),
                    Schedule = validator_schedule(Block, ChildHeight + 1, Validators, EpochLength, TxEnv, Trees),
                    cache_schedule(Schedule),
                    lager:debug("Schedule cached", []),
                    next_beneficiary(TxEnv, Trees);
                {error, _Err} ->
                    lager:debug("Unable to pick the next leader for height ~p, parent height ~p; reason is ~p",
                                [ChildHeight + 1, EntropyHeight, _Err]),
                    {error, not_in_cache}
            end
    end.

entropy_height(ChildEpoch) ->
    (ChildEpoch - 1) * parent_generation() + pc_start_height().

cache_schedule(Schedule) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, validator_schedule, Schedule).

leader_for_height(Height) ->
    Schedule =  aeu_ets_cache:get(?ETS_CACHE_TABLE, validator_schedule, fun() -> #{} end),
    maps:find(Height, Schedule).

validator_schedule(Block, ChildHeight, Validators, EpochLength, TxEnv, Trees) ->
    Hash = aec_parent_chain_block:hash(Block),
    Schedule = get_validator_schedule(Hash, Validators, EpochLength, TxEnv, Trees),
    maps:from_list(enumerate(ChildHeight, Schedule)).

-if(?OTP_RELEASE < 25).
enumerate(From, List) ->
  lists:zip(lists:seq(From, From + length(List)-1), List).
-else.
enumerate(From, List) ->
    lists:enumerate(From, List).
-endif.

get_validator_schedule(Seed, Validators, EpochLength, TxEnv, Trees) ->
    Args = [aefa_fate_code:encode_arg({bytes, Seed}),
            aefa_fate_code:encode_arg(Validators),
            aefa_fate_code:encode_arg({integer, EpochLength})
           ],
    {ok, CallResult} =
        call_consensus_contract_result(?ELECTION_CONTRACT, TxEnv, Trees, "validator_schedule", Args),
    lists:map(fun({address, Address}) -> Address end, CallResult).


allow_lazy_leader() ->
    false.

pick_lazy_leader(_TopHash) ->
    error.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

get_block_producer_configs() -> [{instance_not_used,
                                  #{child_block_time => child_block_time()}}].

is_leader_valid(Node, _Trees, TxEnv, _PrevNode) ->
    Height = aetx_env:height(TxEnv),
    case leader_for_height(Height) of
        {ok, ExpectedLeader} ->
            Header = aec_block_insertion:node_header(Node),
            Leader = aec_headers:miner(Header),
            Leader == ExpectedLeader;
            %% Fix this to have stake as target validated here also?
        _ ->
            lager:debug("Waiting for leader schedule", []),
            aec_conductor:throw_error(parent_chain_block_not_synced)
    end.

get_sorted_validators(TxEnv, Trees) ->
    %% TODO: cache this
    %% this could be cached: we only need to track contract call events for
    %% validator creation and going online and offline
    {ok, CallResult} =
        call_consensus_contract_result(?STAKING_CONTRACT, TxEnv, Trees, "sorted_validators", []),
    lists:map(fun({tuple, Staker}) -> Staker end, CallResult).

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
    ParentHeightStart = (EpochNum - 2) * parent_generation() + pc_start_height(),
    ParentHeightEnd = (EpochNum + 2) * parent_generation() + pc_start_height(),
    [ParentHeightStart, ParentHeightEnd].


%elect_lazy_leader(Beneficiary, TxEnv, Trees) ->
%    {ok, CDLazy} = aeb_fate_abi:create_calldata("elect_after_lazy_leader",
%                                                [aefa_fate_code:encode_arg({address, Beneficiary})]),
%    CallDataLazy = aeser_api_encoder:encode(contract_bytearray, CDLazy),
%    case call_consensus_contract_(?ELECTION_CONTRACT,
%                                TxEnv, Trees,
%                                CallDataLazy, "elect_after_lazy_leader", 0) of
%        {ok, Trees2, Call2} ->
%            case aeb_fate_encoding:deserialize(aect_call:return_value(Call2)) of
%                {tuple, {{address, Beneficiary}, AddedStake}} -> %% same beneficiary!
%                    cache(Beneficiary, AddedStake),
%                    Trees2;
%                What ->
%                    %% maybe a softer approach than crash and burn?
%                    aec_conductor:throw_error({failed_to_elect_new_leader, What})
%            end;
%        {error, What} ->
%            aec_conductor:throw_error({failed_to_elect_new_leader, What})
%    end.
