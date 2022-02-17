%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus module for consensus defined in a smart contract
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_smart_contract).

-export([]).

-behavior(aec_consensus).

-define(TAG, 1337).

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
        , get_sign_module/0
        , get_type/0
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").

can_be_turned_off() -> false.
assert_config(Config) ->
    case Config of
        #{<<"contract">> := Contract} ->
            ContractSpecKeys = [<<"amount">>, <<"vm_version">>,
                                <<"abi_version">>, <<"nonce">>, <<"code">>,
                                <<"call_data">>, <<"owner_pubkey">>, <<"pubkey">>],
            lists:all(
                fun(K) ->
                    case maps:is_key(K, Contract) of
                        true -> true;
                        false ->  error({missing_contract_key, K})
                    end
                end,
                ContractSpecKeys);
        _ -> error(contract_missing_from_config)
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
    lager:info("Stakers: ~p", [StakersMap]),
    ok.

stop() -> ok.

is_providing_extra_http_endpoints() -> false.
%% This is not yet dev mode but it's close :)
%% TODO: Expose via HTTP
client_request(emit_kb) ->
    TopHash = aec_chain:top_block_hash(),
    Beneficiary = next_beneficiary(),
    lager:info("AAAA ~p", [Beneficiary]),
    SignModule = get_sign_module(),
    ok = SignModule:set_candidate(Beneficiary),
    {ok, Block} = aec_block_key_candidate:create(TopHash, Beneficiary),
    ok = aec_conductor:add_synced_block(Block),
    Block;
client_request(emit_mb) ->
    TopHash = aec_chain:top_block_hash(),
    {ok, MicroBlock, _} = aec_block_micro_candidate:create(TopHash),
    SignModule = get_sign_module(),
    {ok, MicroBlockS} = SignModule:sign_micro_block(MicroBlock),
    ok = aec_conductor:post_block(MicroBlockS),
    MicroBlockS;
client_request({mine_blocks, NumBlocksToMine, Type}) ->
    case {aec_conductor:is_leader(), Type} of
        {_, any} ->
            %% Some test might expect to mine a tx - interleave KB with MB
            Pairs = NumBlocksToMine div 2,
            Rem = NumBlocksToMine rem 2,
            P = [[ client_request(emit_kb)
                 , client_request(emit_mb)
                 ] || _ <- lists:seq(1, Pairs)],
            R = [ client_request(emit_kb) || _ <- lists:seq(1, Rem)],
            {ok, lists:flatten([P,R])};
        {_, key} ->
            {ok, [client_request(emit_kb) || _ <- lists:seq(1, NumBlocksToMine)]};
        {true, micro} ->
            {ok, [client_request(emit_mb) || _ <- lists:seq(1, NumBlocksToMine)]};
        {false, micro} ->
            client_request(emit_kb),
            {ok, [client_request(emit_mb) || _ <- lists:seq(1, NumBlocksToMine)]}
    end;
client_request(mine_micro_block_emptying_mempool_or_fail) ->
    KB = client_request(emit_kb),
    MB = client_request(emit_mb),
    %% If instant mining is enabled then we can't have microforks :)
    {ok, []} = aec_tx_pool:peek(infinity),
    {ok, [KB, MB]};
client_request({mine_blocks_until_txs_on_chain, TxHashes, Max}) ->
    mine_blocks_until_txs_on_chain(TxHashes, Max, []).

mine_blocks_until_txs_on_chain(_TxHashes, 0, _Blocks) ->
    {error, max_reached};
mine_blocks_until_txs_on_chain(TxHashes, Max, Blocks) ->
    case aec_conductor:is_leader() of
        false ->
            KB = client_request(emit_kb),
            mine_blocks_until_txs_on_chain(TxHashes, Max-1, [KB|Blocks]);
        true ->
            MB = client_request(emit_mb),
            KB = client_request(emit_kb),
            NewAcc = [KB|Blocks],
            case txs_not_in_microblock(MB, TxHashes) of
                []        -> {ok, lists:reverse(NewAcc)};
                TxHashes1 -> mine_blocks_until_txs_on_chain(TxHashes1, Max - 1, NewAcc)
            end
    end.

txs_not_in_microblock(MB, TxHashes) ->
    [ TxHash || TxHash <- TxHashes, not tx_in_microblock(MB, TxHash) ].

tx_in_microblock(MB, TxHash) ->
    lists:any(fun(STx) ->
                      aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx)) == TxHash
              end, aec_blocks:txs(MB)).

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
    %% TODO: encode the call data OR expose it in the config
    %% the following call data corresponds to elect()
    case call_consensus_contract(Node, Trees, <<"cb_KxHNPVq4P7bBqQA=">>, "elect()") of
        {ok, Trees1, _} -> Trees1;
        {error, What} ->
            error({failed_to_elect_new_leader, What}) %% maybe a softer approach than crash and burn?
    end.

state_pre_transform_micro_node(_Node, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Node, Trees, Amount) ->
    %% TODO: improve this:
    {ok, CD} = aeb_fate_abi:create_calldata("reward_fees", [aefa_fate_code:encode_arg({address, Beneficiary})]),
    CallData =
        aeser_api_encoder:encode(contract_bytearray, CD),
    case call_consensus_contract(Node, Trees, CallData,
                                 "reward_fees(" ++ binary_to_list(aeser_api_encoder:encode(account_pubkey, Beneficiary)) ++ ")", Amount) of
        {ok, Trees1, _} -> Trees1;
        {error, What} ->
            error({failed_to_reward_leader, What}) %% maybe a softer approach than crash and burn?
    end.


%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees0, #{<<"contract">> := Contract}) ->
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
    GenesisHeader = genesis_raw_header(),
    {ok, GenesisHash} = aec_headers:hash_header(GenesisHeader),
    TxEnv = aetx_env:tx_env_from_key_header(GenesisHeader,
                                            GenesisHash,
                                            aec_headers:time_in_msecs(GenesisHeader),
                                            aec_headers:prev_hash(GenesisHeader)),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_create_tx:new(TxSpec#{fee => MinFee}),
    %% Make sure the transaction will give the expected pubkey.
    case aect_contracts:compute_contract_pubkey(Owner, Nonce) of
        Pubkey -> Tx;
        Other          -> error({unexpected_pubkey, Other, Pubkey})
    end,
    Trees1 = aec_block_fork:prepare_contract_owner([Tx], TxEnv, Trees0),
    {_, Trees} = aec_block_fork:apply_contract_create_tx(Tx, Trees1, TxEnv),
    %% TODO: develop a functionality to allow contract calls as well
    Trees.

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
    aec_headers:root_hash(Header).

validate_key_header_seal(_Header, _Protocol) ->
    ok.

generate_key_header_seal(_, _, ?TAG, _, _) ->
    { continue_mining, {ok, ?TAG} }.

set_key_block_seal(Block, ?TAG) ->
    aec_blocks:set_nonce_and_pow(Block, ?TAG, ?TAG).

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

contract_pubkey() ->
  {ok, EncContractId} =
        aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                  <<"0">>, %% TODO: make this configurable
                                  <<"config">>, <<"contract">>, <<"pubkey">>]),
    {ok, Pubkey}   = aeser_api_encoder:safe_decode(contract_pubkey,
                                                   EncContractId),
    Pubkey.

contract_owner() ->
    {ok, EncOwner} =
        aeu_env:user_config([<<"chain">>, <<"consensus">>,
                                  <<"0">>, %% TODO: make this configurable
                                  <<"config">>, <<"contract">>, <<"owner_pubkey">>]),
    {ok, Pubkey}   = aeser_api_encoder:safe_decode(account_pubkey,
                                                   EncOwner),
    Pubkey.

call_consensus_contract(Node, Trees, EncodedCallData, Keyword) ->
    call_consensus_contract(Node, Trees, EncodedCallData, Keyword, 0).

call_consensus_contract(Node, Trees, EncodedCallData, Keyword, Amount) ->
    Header = aec_block_insertion:node_header(Node),
    TxEnv = aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(Node),
                                            aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)),
    call_consensus_contract_(TxEnv, Trees, EncodedCallData, Keyword, Amount).
    
call_consensus_contract_(TxEnv, Trees, EncodedCallData, Keyword, Amount) ->
    lager:info("Height ~p, calling ~p with amount ~p aettos, encoded ~p",
               [aetx_env:height(TxEnv), Keyword, Amount, EncodedCallData]),
    ContractPubkey = contract_pubkey(),
    OwnerPubkey = contract_owner(),
    Contract = aect_state_tree:get_contract(ContractPubkey,
                                            aec_trees:contracts(Trees)),
    OwnerAcc = aec_accounts_trees:get(OwnerPubkey, 
                                            aec_trees:accounts(Trees)),
    Fee = 1000000000000000000, %% TODO: fine tune this
    Gas = 1000000000000000000, %% TODO: fine tune this
    GasPrice = 10000000000, %% TODO: fine tune this
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
            {ok, Trees2, Call};
        {error, _What} = Err -> Err
    end.

beneficiary() -> aec_consensus_bitcoin_ng:beneficiary().

next_beneficiary() ->
    %% TODO: cache this
    {TxEnv, Trees} = aetx_env:tx_env_and_trees_from_top(aetx_transaction),
    %% call elect_next
    case call_consensus_contract_(TxEnv, Trees, <<"cb_KxFodeDRP1Jz2f0=">>, "elect_next()", 0) of
        {ok, _Trees1, Call} ->
            {address, Leader} = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            Leader;
        {error, What} ->
            error({failed_to_elect_new_leader, What}) %% maybe a softer approach than crash and burn?
    end.

get_sign_module() -> aec_preset_keys.

get_type() -> pos.

