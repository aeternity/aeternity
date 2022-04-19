%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus module for consensus defined in a smart contract
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_smart_contract).

-export([]).

-behavior(aec_consensus).


-define(TAG, 1337). %% TODO: remove this
-define(SIGNATURE_SIZE, 16).
-define(ETS_CACHE_TABLE, ?MODULE).

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

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include("aec_consensus.hrl").

can_be_turned_off() -> false.
assert_config(Config) ->
    case Config of
        #{<<"contracts">> := Contracts} when is_list(Contracts) ->
            ContractSpecKeys = [<<"amount">>, <<"vm_version">>,
                                <<"abi_version">>, <<"nonce">>, <<"code">>,
                                <<"call_data">>, <<"owner_pubkey">>, <<"pubkey">>],
            lists:all(
                fun(Contract) ->
                    lists:all(
                        fun(K) ->
                            case maps:is_key(K, Contract) of
                                true -> true;
                                false ->  error({missing_contract_key, K})
                            end
                        end,
                        ContractSpecKeys)
                end,
                Contracts);
        _ -> error(contracts_missing_from_config)
    end,
    ok.

start(Config) ->
    #{<<"stakers">> := StakersEncoded} = Config,
    Stakers =
        lists:map(
            fun(#{<<"pub">> := EncodedPubkey, <<"priv">> := EncodedPrivkey}) ->
                {ok, Pubkey} = aeser_api_encoder:safe_decode(account_pubkey,
                                                             EncodedPubkey),
                {ok, Privkey} = aeser_api_encoder:safe_decode(contract_bytearray,
                                                             EncodedPrivkey),
                {Pubkey, Privkey}
            end,
            StakersEncoded),
    StakersMap = maps:from_list(Stakers),
    aec_preset_keys:start_link(StakersMap), %% TODO: attach it to the supervision tree
    lager:debug("Stakers: ~p", [StakersMap]),
    ok.

stop() -> ok.

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
    {ok, CD} = aeb_fate_abi:create_calldata("elect", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract(Node, Trees, CallData, "elect()") of
        {ok, Trees1, _} -> Trees1;
        {error, What} ->
            error({failed_to_elect_new_leader, What}) %% maybe a softer approach than crash and burn?
    end.

state_pre_transform_micro_node(_Node, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Node, Trees, Amount) ->
    {ok, CD} = aeb_fate_abi:create_calldata("reward", [aefa_fate_code:encode_arg({address, Beneficiary})]),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract(Node, Trees, CallData,
                                 "reward(" ++ binary_to_list(aeser_api_encoder:encode(account_pubkey, Beneficiary)) ++ ")", Amount) of
        {ok, Trees1, _} -> Trees1;
        {error, What} ->
            error({failed_to_reward_leader, What}) %% maybe a softer approach than crash and burn?
    end.


%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees0, #{ <<"contracts">> := Contracts
                                 , <<"calls">> := Calls}) ->
    GasLimit = 10000000,
    GasPrice = 10000000000,
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

key_header_for_sealing(Header) ->
    Header1 = aec_headers:set_nonce(Header, 0),
    Header2 = aec_headers:set_key_seal(Header1, no_value),
    aec_headers:serialize_to_binary(Header2).

validate_key_header_seal(Header, _Protocol) ->
    Seal = aec_headers:key_seal(Header),
    {SignaturePart, Padding} = lists:split(?SIGNATURE_SIZE, Seal),
    Signature = << <<E:32>> || E <- SignaturePart >>,
    Validators = [ fun seal_correct_padding/3 
                 , fun seal_correct_signature/3
                 ],
    aeu_validation:run(Validators, [Header, Signature, Padding]).

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

generate_key_header_seal(_, Candidate, ?TAG, #{expected_key_block_rate := Expected} = _Config, _) ->
    {ok, PrevKeyHeader} = aec_chain:get_header(aec_headers:prev_key_hash(Candidate)),
    PrevBlockTime = aec_headers:time_in_msecs(PrevKeyHeader),
    Now = aeu_time:now_in_msecs(),
    SleepTime = max(PrevBlockTime + Expected - Now, 0),
    lager:info("Sleeping for ~p ms before contining", [SleepTime]),
    timer:sleep(SleepTime),
    %% we are to adjust the time set in the block according to the actual time
    %% when we sign it. This would effectively change the candidate. The
    %% signature will be applied only after that. This is done in
    %% set_key_block_seal/2
    { continue_mining, {ok, to_be_signed} }.

set_key_block_seal(KeyBlock0, _Signature) ->
    KeyBlock = aec_blocks:set_time_in_msecs(KeyBlock0, aeu_time:now_in_msecs()),
    KeyHeader = aec_blocks:to_header(KeyBlock),
    Leader = aec_headers:miner(KeyHeader),
    SignModule = get_sign_module(),
    {ok, Signature} = SignModule:produce_key_header_signature(KeyHeader, Leader),
    %% the signature is 64 bytes. The seal is 168 bytes. We add 104 bytes at
    %% the end of the signature
    PaddingSize = seal_padding_size(), 
    Padding = << <<E:32>> || E <- lists:duplicate(PaddingSize, 0)>>,
    Seal = aec_headers:deserialize_pow_evidence_from_binary(<<Signature/binary, Padding/binary>>),
    aec_blocks:set_key_seal(KeyBlock, Seal).

nonce_for_sealing(_Header) ->
    ?TAG.

next_nonce_for_sealing(?TAG, _) ->
    ?TAG.

trim_sealing_nonce(?TAG, _) ->
    ?TAG.

default_target() ->
    ?TAG.

assert_key_target_range(?TAG) ->
    ok.

key_header_difficulty(_) ->
    ?TAG.

%% This is initial height; if neeeded shall be reinit at fork height
contract_pubkey() ->
    aeu_ets_cache:get(
        ?ETS_CACHE_TABLE,
        contract_pubkey,
        fun() ->
            {ok, EncContractId} =
                aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                     <<"0">>,
                                     <<"config">>, <<"consensus_contract">>]),
                {ok, Pubkey}   = aeser_api_encoder:safe_decode(contract_pubkey,
                                                               EncContractId),
                Pubkey
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


call_consensus_contract(Node, Trees, EncodedCallData, Keyword) ->
    call_consensus_contract(Node, Trees, EncodedCallData, Keyword, 0).

call_consensus_contract(Node, Trees, EncodedCallData, Keyword, Amount) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(Node),
                                            aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    call_consensus_contract_(TxEnv, Trees, EncodedCallData, Keyword, Amount).
    
call_consensus_contract_(TxEnv, Trees, EncodedCallData, Keyword, Amount) ->
    Height = aetx_env:height(TxEnv),
    lager:debug("Height ~p, calling ~p with amount ~p aettos, encoded ~p",
               [Height, Keyword, Amount, EncodedCallData]),
    ContractPubkey = contract_pubkey(),
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
    %% TODO: cache this
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    %% call elect_next
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(TxEnv, Trees, CallData, "leader()", 0) of
        {ok, _Trees1, Call} ->
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            {ok, Leader};
        {error, What} ->
            error({failed_to_elect_new_leader, What}) %% maybe a softer approach than crash and burn?
    end.


next_beneficiary() ->
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    %% call elect_next
    {ok, CD} = aeb_fate_abi:create_calldata("elect_next", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(TxEnv, Trees, CallData, "elect_next()", 0) of
        {ok, _Trees1, Call} ->
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            SignModule = get_sign_module(),
            SignModule:set_candidate(Leader),
            {ok, Leader};
        {error, What} ->
            error({failed_to_elect_new_leader, What}) %% maybe a softer approach than crash and burn?
    end.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

get_block_producer_configs() -> [{instance_not_used,
                                  #{expected_key_block_rate => expected_key_block_rate()}}].

is_leader_valid(Node, Trees, TxEnv) ->
    Header = aec_block_insertion:node_header(Node),
    {ok, CD} = aeb_fate_abi:create_calldata("leader", []),
    CallData = aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract_(TxEnv, Trees, CallData, "leader()", 0) of
        {ok, _Trees1, Call} ->
            {address, ExpectedLeader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            Leader = aec_headers:miner(Header),
            ExpectedLeader =:= Leader;
        {error, What} ->
            lager:info("Block validation failed with a reason ~p", [What]),
            false
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

