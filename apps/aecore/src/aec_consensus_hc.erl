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
        , start/1
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
        , state_pre_transform_key_node/2
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
        , get_sign_module/0
        , get_type/0
        , get_block_producer_configs/0
        , is_leader_valid/3
        ]).

%% HC specific API
-export([ parent_chain_validators/2
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include("aec_consensus.hrl").

can_be_turned_off() -> false.
assert_config(_Config) -> ok.

start(Config) ->
    #{<<"stakers">> := StakersEncoded,
      <<"parent_chain">> :=
        #{  <<"start_height">> := StartHeight,
            <<"confirmations">> := Confirmations,
            <<"consensus">> :=
                #{  <<"type">> := PCType,
                    <<"network_id">> := NetworkId,
                    <<"spend_address">> := PCSpendAddress
                 }, 
            <<"polling">> :=
                #{  <<"fetch_interval">> := FetchInterval,
                    <<"nodes">> := Nodes0
                 } = Polling
          }} = Config,
    CacheSize = maps:get(<<"cache_size">>, Polling, 200),
    Stakers =
        lists:map(
            fun(#{<<"hyper_chain_account">> := #{<<"pub">> := EncodedPubkey,
                                                 <<"priv">> := EncodedPrivkey},
                  <<"parent_chain_account">> := #{<<"pub">> := _EncodedPubkey,
                                                  <<"priv">> := _EncodedPrivkey}
                 }) ->
                {ok, Pubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                             EncodedPubkey),
                Privkey = aeu_hex:hex_to_bin(EncodedPrivkey),
                case aec_keys:check_sign_keys(Pubkey, Privkey) of
                    true -> pass;
                    false -> throw({error, invalid_staker_pair, {EncodedPubkey, EncodedPrivkey}})
                end,
                {Pubkey, Privkey}
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    %% TODO: ditch this after we move beyond OTP24
    _Mod = aec_preset_keys,
    start_dependency(aec_preset_keys, [StakersMap]),
    lager:debug("Stakers: ~p", [StakersMap]),
    ParentConnMod =
        case PCType of
            <<"AE2AE">> -> aehttpc_aeternity
        end,
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
    SignModule = get_sign_module(),
    {ok, PCSpendPubkey} = aeser_api_encoder:safe_decode(account_pubkey, PCSpendAddress),
    start_dependency(aec_parent_connector, [ParentConnMod, FetchInterval,
                                            ParentHosts, NetworkId,
                                            SignModule, PCSpendPubkey]),
    start_dependency(aec_parent_chain_cache, [StartHeight, CacheSize, Confirmations]),
    ok.

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
keyblock_create_adjust_target(Block, []) -> {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
dirty_validate_header_pre_conductor(_) -> ok.
dirty_validate_key_hash_at_height(_, _) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.
dirty_validate_micro_node_with_ctx(_Node, _Block, _Ctx) -> ok.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, Trees) -> Trees.
state_pre_transform_key_node(Node, Trees) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(
              Header, aec_block_insertion:node_hash(Node),
              aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    Height = aetx_env:height(TxEnv),
    PCHeight = Height + pc_start_height(),
    case aec_parent_chain_cache:get_block_by_height(PCHeight) of
        {error, not_in_cache} ->
            aec_conductor:throw_error(parent_chain_block_not_synced);
        {error, {not_enough_confirmations, Block}} ->
            aec_conductor:throw_error({not_enough_confirmations, aec_parent_chain_block:height(Block)});
        {ok, Block} ->
            Hash = aec_parent_chain_block:hash(Block),
            HashStr = binary_to_list(Hash),
            {ok, CD} = aeb_fate_abi:create_calldata("elect",
                                                    [aefa_fate_code:encode_arg({string, Hash})]),
            CallData = aeser_api_encoder:encode(contract_bytearray, CD),
            case call_consensus_contract(?ELECTION_CONTRACT, Node, Trees, CallData, ["elect(", HashStr,  ")"]) of
                {ok, Trees1, _} ->
                aeu_ets_cache:reinit(
                    ?ETS_CACHE_TABLE,
                    current_leader,
                    fun beneficiary_/0),
                    Trees1;
                {error, What} ->
                    %% maybe a softer approach than crash and burn?
                    aec_conductor:throw_error({failed_to_elect_new_leader, What})
            end
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
    NetworkId = aec_governance:get_network_id(),
    {ok, #{ <<"contracts">> := Contracts
          , <<"calls">> := Calls }} =
        aec_fork_block_settings:hc_seed_contracts(?CERES_PROTOCOL_VSN, NetworkId),
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
        0,
        default,
        ?CERES_PROTOCOL_VSN).
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
    Res = aeu_validation:run(Validators, [Header, Signature, Padding]),
    Res.

seal_correct_padding(_Header, _Signature, Padding) ->
    PaddingSize = seal_padding_size(),
    ExpectedPadding = lists:duplicate(PaddingSize, 0),
    case Padding =/= ExpectedPadding of
        true -> {error, {erroneous_seal, Padding, ExpectedPadding}};
        false -> ok
    end.

seal_correct_signature(Header, Signature, _Padding) ->
    Leader = aec_headers:miner(Header),
    Bin = aec_headers:serialize_to_signature_binary(Header),
    case enacl:sign_verify_detached(Signature, Bin, Leader) of
        true  -> ok;
        false ->
            {error, signature_verification_failed}
    end.

generate_key_header_seal(_, Candidate, PCHeight, #{expected_key_block_rate := _Expected} = _Config, _) ->
    case aec_parent_chain_cache:get_block_by_height(PCHeight) of
        {ok, Block} ->
            Hash = aec_parent_chain_block:hash(Block),
            ParentHash = binary_to_list(Hash),
            {TxEnv0, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
            Height0 = aetx_env:height(TxEnv0),
            Height = Height0 + 1,
            TxEnv = aetx_env:set_height(TxEnv0, Height),
            {ok, CD} = aeb_fate_abi:create_calldata("elect_at_height",
                                                    [aefa_fate_code:encode_arg({integer, Height}),
                                                    aefa_fate_code:encode_arg({string, list_to_binary(ParentHash)})]),
            CallData = aeser_api_encoder:encode(contract_bytearray, CD),
            {ok, _Trees1, Call} = call_consensus_contract_(?ELECTION_CONTRACT,
                                                           TxEnv, Trees,
                                                           CallData,
                                                           ["elect_at_height(", integer_to_list(Height),
                                                            ", " , ParentHash , ")"],
                                                           0),
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            SignModule = get_sign_module(),
            case SignModule:set_candidate(Leader) of
                {error, key_not_found} ->
                    timer:sleep(1000),
                    {continue_mining, {error, no_solution} };
                ok ->
                    Candidate1 = aec_headers:set_beneficiary(Candidate, Leader),
                    Candidate2 = aec_headers:set_miner(Candidate1, Leader),
                    {ok, Signature} = SignModule:produce_key_header_signature(Candidate2, Leader),
                    %% the signature is 64 bytes. The seal is 168 bytes. We add 104 bytes at
                    %% the end of the signature
                    PaddingSize = seal_padding_size(),
                    Padding = << <<E:32>> || E <- lists:duplicate(PaddingSize, 0)>>,
                    Seal = aec_headers:deserialize_pow_evidence_from_binary(<<Signature/binary, Padding/binary>>),
                    {continue_mining, {ok, Seal}}
            end;
        {error, _} ->
            timer:sleep(1000),
            {continue_mining, {error, no_solution} }
    end.

set_key_block_seal(KeyBlock0, Seal) ->
    {TxEnv0, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    Height0 = aetx_env:height(TxEnv0),
    Height = Height0 + 1,
    PCHeight = Height + pc_start_height(),
    {ok, Block} = aec_parent_chain_cache:get_block_by_height(PCHeight),
    Hash = aec_parent_chain_block:hash(Block),
    ParentHash = binary_to_list(Hash),
    TxEnv = aetx_env:set_height(TxEnv0, Height),
    {ok, CD} = aeb_fate_abi:create_calldata("elect_at_height",
                                            [aefa_fate_code:encode_arg({integer, Height}),
                                            aefa_fate_code:encode_arg({string, list_to_binary(ParentHash)})]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    {ok, _Trees1, Call} = call_consensus_contract_(?ELECTION_CONTRACT,
                                                    TxEnv, Trees,
                                                    CallData,
                                                    ["elect_at_height(", integer_to_list(Height),
                                                     ", ", ParentHash,  ")"],
                                                    0),
    {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
    KeyBlock1 = aec_blocks:set_beneficiary(KeyBlock0, Leader),
    KeyBlock2 = aec_blocks:set_miner(KeyBlock1, Leader),
    aec_blocks:set_key_seal(KeyBlock2, Seal).

nonce_for_sealing(Header) ->
    Height = aec_headers:height(Header),
    PCHeight = Height + pc_start_height(),
    PCHeight.

next_nonce_for_sealing(PCHeight, _) ->
    PCHeight.

trim_sealing_nonce(PCHeight, _) ->
    PCHeight.

default_target() ->
    ?TAG.

assert_key_target_range(?TAG) ->
    ok.

key_header_difficulty(_) ->
    ?TAG.

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


%% This is initial height; if neeeded shall be reinit at fork height
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

%% This is initial height; if neeeded shall be reinit at fork height
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


call_consensus_contract(Contract, Node, Trees, EncodedCallData, Keyword) ->
    call_consensus_contract(Contract, Node, Trees, EncodedCallData, Keyword, 0).

call_consensus_contract(Contract, Node, Trees, EncodedCallData, Keyword, Amount) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(
              Header, aec_block_insertion:node_hash(Node),
              aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    call_consensus_contract_(Contract, TxEnv, Trees, EncodedCallData, Keyword, Amount).

call_consensus_contract_(ContractType, TxEnv, Trees, EncodedCallData, Keyword, Amount) ->
    Height = aetx_env:height(TxEnv),
    lager:debug("Height ~p, calling ~s with amount ~p aettos, encoded ~p",
               [Height, Keyword, Amount, EncodedCallData]),
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
            ok = aect_call:return_type(Call),
            %% prune the call being produced. If not done, the fees for it
            %% would be redistributed to the corresponding leaders
            {ok, aect_call_state_tree:prune(Height, Trees2), Call};
        {error, _What} = Err -> Err
    end.

beneficiary() ->
    aeu_ets_cache:get(
        ?ETS_CACHE_TABLE,
        current_leader,
        fun beneficiary_/0).

beneficiary_() ->
    %% TODO: cache this
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "leader()", 0) of
        {ok, _Trees1, Call} ->
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            {ok, Leader};
        {error, What} ->
            %% maybe a softer approach than crash and burn?
            error({failed_to_elect_new_leader, What})
    end.

next_beneficiary() ->
    {TxEnv0, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    Height0 = aetx_env:height(TxEnv0),
    Height = Height0 + 1,
    PCHeight = Height + pc_start_height(),
    case aec_parent_chain_cache:get_block_by_height(PCHeight) of
        {ok, Block} ->
            Hash = aec_parent_chain_block:hash(Block),
            ParentHash = binary_to_list(Hash),
            TxEnv = aetx_env:set_height(TxEnv0, Height),
            {ok, CD} = aeb_fate_abi:create_calldata("elect_at_height",
                                                    [aefa_fate_code:encode_arg({integer, Height}),
                                                    aefa_fate_code:encode_arg({string, list_to_binary(ParentHash)})]),
            CallData = aeser_api_encoder:encode(contract_bytearray, CD),
            {ok, _Trees1, Call} = call_consensus_contract_(?ELECTION_CONTRACT,
                                                            TxEnv, Trees,
                                                            CallData,
                                                            ["elect_at_height(", integer_to_list(Height),
                                                             ", ", ParentHash, ")"],
                                                            0),
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            SignModule = get_sign_module(),
            case SignModule:set_candidate(Leader) of
                {error, key_not_found} ->
                    timer:sleep(1000),
                    {error, not_leader};
                ok ->
                    {ok, Leader}
            end;
        {error, _Err} ->
            epoch_mining:debug("ASDF DID NOT FIND  PARENT BLOCK WITH HEIGHT ~p FOR REASON ~p", [PCHeight, _Err]),
            timer:sleep(1000),
            {error, not_in_cache}
    end.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

get_block_producer_configs() -> [{instance_not_used,
                                  #{expected_key_block_rate => expected_key_block_rate()}}].

is_leader_valid(Node, Trees, TxEnv) ->
    Header = aec_block_insertion:node_header(Node),
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?ELECTION_CONTRACT, TxEnv, Trees, CallData, "leader()", 0) of
        {ok, _Trees1, Call} ->
            {address, ExpectedLeader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            Leader = aec_headers:miner(Header),
            ExpectedLeader =:= Leader;
        {error, What} ->
            lager:info("Block validation failed with a reason ~p", [What]),
            false
    end.

parent_chain_validators(TxEnv, Trees) ->
    %% TODO: cache this
    %% this could be cached: we only need to track contract call events for
    %% validator creation and going online and offline
    {ok, CD} = aeb_fate_abi:create_calldata("sorted_validators", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(?STAKING_CONTRACT, TxEnv, Trees, CallData,
                             "sorted_validators()", 0) of
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

