%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Hyperchains consensus - https://github.com/aeternity/hyperchains-whitepaper
%%% The deployer of the staking contract is either an ordinary user or 2:32/unit-8
%%% If the address of the contract was specified then the contract MUST exist on-chain when enabling HC consensus
%%% Furthermore the bytecode of the specified contract MUST match the bytecode of our local contract copy
%%% Additionally we sanity check the contract configuration
%%% Keep in mind that the deployer of the contract doesn't have any special privileges
%%% over the contract after it was deployed.
%%% When we don't rely on an already deployed staking contract then we take the first free system account and deploy it as a system contract
%%% in case of ae-mainnet when no ordinary user deployed the staking contract the contract WILL get deployed by 2:32/unit-8
%%% When enabling HC consensus we check whether the staking contract was already predeployed by an ordinary user
%%% All stateful contract calls to the staking contract are made using 2:32/unit-8, stateless calls are made using 1:32/unit-8
%%% Calls originating from 2:32/unit-8 and 1:32/unit-8 are made using funds created from thin air
%%% System calls using 2:32/unit-8 never change the funds of that account nor bump the nonce
%%% Calls made using 1:32/unit-8 never require special preprocessing as we throw away the new state trees
%%% @end
%%% -------------------------------------------------------------------
-module(aehc_consensus_hyperchains).

-behavior(aec_consensus).

-define(STAKING_CONTRACT, {?MODULE, staking_contract}).
-define(STAKING_CONTRACT_ADDR, {?MODULE, staking_contract_addr}).
%% Lima or Iris as we need the FATE VM at genesis
%% In case that's unwanted then start up another consensus before hyperchains
-define(HC_GENESIS_VERSION, ?LIMA_PROTOCOL_VSN).

%% Magic nonces
-define(NONCE_HC_ENABLED, 16#ffffffffffffffff - 1).
-define(NONCE_HC_POGF, 16#ffffffffffffffff).
-define(NONCE_HC_GENESIS, 2). %% Hyperchain at genesis :)

%% Consensus API
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
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        , dirty_validate_header_pre_conductor/1
        , dirty_validate_key_hash_at_height/2
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/4
        , state_pre_transform_key_node/4
        , state_pre_transform_micro_node/4
        %% Block rewards
        , state_grant_reward/3
        %% PoGF
        , pogf_detected/2
        %% Genesis block
        , genesis_transform_trees/2
        , genesis_raw_header/0
        , genesis_difficulty/0
        %% Keyblock creation
        , new_unmined_key_node/8
        , keyblocks_for_unmined_keyblock_adjust/0
        , adjust_unmined_keyblock/2
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
        , key_header_difficulty/1 ]).

%% Staking contract helpers
-export([ get_staking_contract_aci/0
        , get_staking_contract_bytecode/0
        , get_staking_contract_address/0
        , static_staking_contract_call_on_top_block/1
        , static_staking_contract_call_on_block_hash/2
        , load_staking_contract_address/0
        ]).

%% Staking contract predeploy
-export([ get_predeploy_address/0
        , set_predeploy_address/1
        , unset_predeploy_address/0
        ]).

%% Hyperchains activation criteria
-export([ get_hc_activation_criteria/0
        , set_hc_activation_criteria/4
        , unset_hc_activation_criteria/0
        ]).

%% General helpers
-export([ hc_header_type/1
        , is_hc_pos_header/1
        , get_pos_header_parent_hash/1
        , get_pos_header_miner_signature/1
        , set_pos_header_parent_hash/2
        , set_pos_header_miner_signature/2
        , create_pos_pow_field/2
        , deserialize_pos_pow_field/1
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecore/include/blocks.hrl").
-include_lib("aeminer/include/aeminer.hrl").

-record(activation_criteria, {
        minimum_stake :: integer(),
        minimum_delegates :: integer(),
        check_frequency :: integer(),
        confirmations :: integer()
    }).

can_be_turned_off() -> true.
assert_config(_Config) ->
    persistent_term:erase(?STAKING_CONTRACT_ADDR), %% So eunit can simulate node restarts
    %% For now assume that the staking contract can't change during the lifetime of the hyperchain
    case persistent_term:get(?STAKING_CONTRACT, error) of
        error ->
            P = aec_fork_block_settings:hc_staking_contract_file(),
            case filelib:is_file(P) of
                false ->
                    aec_consensus:config_assertion_failed(
                        "Hyperchains staking contract not present.",
                        " Terminating the node - file not found ~p",
                        [P]);
                true ->
                    lager:debug("Loading hyperchains staking contract"),
                    {ok, Data} = file:read_file(P),
                    try
                        JObject = case jsx:decode(Data, [{return_maps,true},{labels,binary}]) of
                                  Map when is_map(Map) ->
                                      Map;
                                  [Map] when is_map(Map) ->
                                      Map
                                  end,
                        #{ <<"bytecode">> := B
                         , <<"aci">> := JText
                         } = JObject,
                        CompiledACI = aeaci_aci:from_string(JText, #{backend => fate}),
                        {contract_bytearray, Bytecode} = aeser_api_encoder:decode(B),
                        persistent_term:put(?STAKING_CONTRACT, #{ bytecode => Bytecode
                                                                , aci => CompiledACI
                                                                })
                    catch E:R:S ->
                        aec_consensus:config_assertion_failed(
                        "Hyperchains staking contract is malformed.",
                        " Terminating the node - ~p ~p ~p",
                        [E, R, S])
                    end
            end;
        X -> X
    end,
    case aeu_env:user_config([<<"hyperchains">>, <<"staking_contract_address">>]) of
        undefined -> ok;
        {ok, EAddr} ->
            lager:debug("Trying to set the staking contract address"),
            {contract_address, Addr} = aeser_api_encoder:decode(EAddr),
            set_staking_contract_address(Addr)
    end,
    case aeu_env:user_config([<<"hyperchains">>, <<"activation_criteria">>]) of
        undefined -> ok;
        {ok, Criteria} ->
%%            lager:debug("Trying to set the activation criteria")
            MinStake = maps:get(<<"minimum_stake">>, Criteria),
            MinDelegates = maps:get(<<"unique_delegates">>, Criteria),
            BlockFreq = maps:get(<<"check_frequency">>, Criteria),
            BlockConfirms = maps:get(<<"confirmation_depth">>, Criteria),
            ok = set_hc_activation_criteria(MinStake, MinDelegates, BlockFreq, BlockConfirms)
    end,
    ok.

start(_Config) ->
    %% We can't load the staking contract address in assert_config as the DB is not ready yet
    load_staking_contract_address(),
    case {get_staking_contract_address(), get_predeploy_address()} of
        {{ok, X}, {ok, X}} -> ok;
        {{ok, X}, {ok, Y}} ->
            aec_consensus:config_assertion_failed(
                "Predeploy address is different from the already deployed contract",
                " Deployed: ~p, Predeploy: ~p",
                [ aeser_api_encoder:encode(contract_pubkey, X)
                , aeser_api_encoder:encode(contract_pubkey, Y)
                ]);
        {_, _} -> ok
    end,
    case get_staking_contract_address() of
        {ok, ContractPubkey} ->
            aec_db:ensure_activity(async_dirty, fun() ->
                TopHash = aec_chain:top_block_hash(),
                {TxEnv0, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', TopHash),
                TxEnv = case aetx_env:height(TxEnv0) of
                            0 ->
                                genesis_tx_env();
                            _ -> TxEnv0
                        end,
                verify_existing_staking_contract(ContractPubkey, Trees, TxEnv)
              end);
        not_deployed -> ok
    end,
    %% Crank down the finalized height delta to 2-4 in case the staking contract is active ;)
    %% Just load whathever the fallback cosensus might need
    M = fallback_consensus(),
    M:start(#{}),
    ok.
stop() -> ok.

is_providing_extra_http_endpoints() -> false.
client_request(_) -> error(todo).

%% -------------------------------------------------------------------
%% Deserialization
extra_from_header(Header) ->
    %% Check if we use a special nonce - if yes then we MIGHT be a POS block
    Type1 = case aec_headers:type(Header) of
               key ->
                    case aec_headers:nonce(Header) of
                       ?NONCE_HC_ENABLED -> key_pos;  %% The first key block after the contract got "activated"
                       ?NONCE_HC_POGF -> key_pos_pogf;
                       _ -> key_pow
                   end;
               micro -> micro
           end,
    IsPoS1 = Type1 =:= key_pos orelse Type1 =:= key_pos_pogf,
    {IsPoS2, Type2, PoSMeta} =
        case IsPoS1 of
            false -> {false, Type1, #{}};
            true -> %% Try interpreting the key_seal as PoS
                case deserialize_pos_pow_field(aec_headers:key_seal(Header)) of
                    {ok, ParentHash, MinerSignature} ->
                        {true, Type1, #{parent_hash => ParentHash, miner_signature => MinerSignature}};
                    error ->
                        {false, key_pow, #{}}
                end
        end,
    %% We can't really switch to another consensus right now as we rely on the global consensus setting most of the time
    maps:merge(PoSMeta,
        #{ consensus => ?MODULE
         , type => Type2
         , pos => IsPoS2
         }).

create_pos_pow_field(ParentHash, Signature) when is_binary(Signature), byte_size(Signature) =:= 64, is_binary(ParentHash), byte_size(ParentHash) =:= 32 ->
    [X || <<X:32>> <= ParentHash]
    ++
    [X || <<X:32>> <= Signature]
    ++
    [0 || _ <- lists:seq(1, 42-24)].

deserialize_pos_pow_field(no_value) -> error;
deserialize_pos_pow_field([H1, H2, H3, H4, H5, H6, H7, H8, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16 | T]) ->
    case [0 || _ <- lists:seq(1, 42-24)] of
        T -> { ok
             , <<H1:32, H2:32, H3:32, H4:32, H5:32, H6:32, H7:32, H8:32>>
             , <<S1:32, S2:32, S3:32, S4:32, S5:32, S6:32, S7:32, S8:32, S9:32, S10:32, S11:32, S12:32, S13:32, S14:32, S15:32, S16:32>>
             };
        _ -> error
    end;
deserialize_pos_pow_field(_) -> error.

-spec hc_header_type(aec_headers:header()) -> key_pos | key_pos_pogf | key_pow | micro | no_return().
hc_header_type(Header) ->
    maps:get(type, aec_headers:extra(Header)).

-spec is_hc_pos_header(aec_headers:header()) -> boolean().
is_hc_pos_header(Header) ->
    maps:get(pos, aec_headers:extra(Header), false).

-spec get_pos_header_parent_hash(aec_headers:header()) -> binary().
get_pos_header_parent_hash(Header) -> maps:get(parent_hash, aec_headers:extra(Header)).

-spec get_pos_header_miner_signature(aec_headers:header()) -> binary().
get_pos_header_miner_signature(Header) -> maps:get(miner_signature, aec_headers:extra(Header)).

-spec set_pos_header_miner_signature(aec_headers:header(), binary()) -> aec_headers:header().
set_pos_header_miner_signature(Header1, Signature) when is_binary(Signature), byte_size(Signature) =:= 64 ->
    ParentHash = get_pos_header_parent_hash(Header1),
    Extra = aec_headers:extra(Header1),
    Seal = create_pos_pow_field(ParentHash, Signature),
    Header2 = aec_headers:set_extra(Header1, maps:put(miner_signature, Signature, Extra)),
    aec_headers:set_key_seal(Header2, Seal).

-spec set_pos_header_parent_hash(aec_headers:header(), binary()) -> aec_headers:header().
set_pos_header_parent_hash(Header1, ParentHash) when is_binary(ParentHash), byte_size(ParentHash) =:= 32 ->
    Signature = get_pos_header_miner_signature(Header1),
    Extra = aec_headers:extra(Header1),
    Seal = create_pos_pow_field(ParentHash, Signature),
    Header2 = aec_headers:set_extra(Header1, maps:put(parent_hash, ParentHash, Extra)),
    aec_headers:set_key_seal(Header2, Seal).

%% -------------------------------------------------------------------
%% Building the Insertion Ctx
recent_cache_n() ->
    M = fallback_consensus(),
    max(1, M:recent_cache_n()).

%% Might get a pow block when switching between consensus algorithms
recent_cache_trim_key_header(H) ->
    case is_hc_pos_header(H) of
        true -> ok;
        false ->
            M = fallback_consensus(),
            M:recent_cache_trim_key_header(H)
    end.

%% -------------------------------------------------------------------
%% Preconductor hook - called in sync process just before invoking the conductor
dirty_validate_block_pre_conductor(B) ->
    case is_hc_pos_header(aec_blocks:to_header(B)) of
        true -> ok;%%error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_block_pre_conductor(B)
    end.

dirty_validate_header_pre_conductor(H) ->
    case is_hc_pos_header(H) of
        true -> ok;%%error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_header_pre_conductor(H)
    end.

dirty_validate_key_hash_at_height(Height, Hash) ->
    M = fallback_consensus(),
    M:dirty_validate_key_hash_at_height(Height, Hash).

%% -------------------------------------------------------------------
%% Dirty validation of keyblocks just before starting the state transition
dirty_validate_key_node_with_ctx(Node, Block, Ctx) ->
    case is_hc_pos_header(aec_blocks:to_header(Block)) of
        true ->
            ok;%%error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_key_node_with_ctx(Node, Block, Ctx)
    end.

%% -------------------------------------------------------------------
%% Dirty validation of microblocks just before starting the state transition
dirty_validate_micro_node_with_ctx(Node, Block, Ctx) ->
    case is_hc_pos_header(aec_blocks:to_header(Block)) of
        true -> ok;%%error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_micro_node_with_ctx(Node, Block, Ctx)
    end.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(KeyNode, _PrevNode, _PrevKeyNode, Trees) ->
    TxEnv = node_tx_env(KeyNode),
    case get_staking_contract_address() of
        not_deployed ->
            ensure_staking_contract_on_consensus_switch(Trees, TxEnv);
        {ok, ContractAddress} ->
            %% After deploying the staking contract we cache the address
            %% Subsequent calls to this functions on the same key node and trees shall
            %% yield the same results
            %% We might be dealing with Consensuses like Pow -> HC -> ??? -> HC
            %% Check whether the contract was already deployed - if not then deploy it
            case aect_state_tree:lookup_contract(ContractAddress, aec_trees:contracts(Trees)) of
                {value, _} ->
                    %% Ok we are just switching between settings
                    verify_existing_staking_contract(ContractAddress, Trees, TxEnv),
                    Trees;
                none ->
                    %% Assume we need to deploy it
                    Trees1 = ensure_staking_contract_on_consensus_switch(Trees, TxEnv),
                    %% Quick sanity check
                    {ok, ContractAddress} = get_staking_contract_address(),
                    Trees1
            end
    end.

ensure_staking_contract_on_consensus_switch(Trees, TxEnv) ->
    case get_predeploy_address() of
        {ok, ContractPubkey} ->
            %% When the staking contract was not found it is better to
            %% bring down the entire node down. By rejecting the state transition
            %% the node will get stuck syncing on the specified height
            %% This actually incurs a risk on whether the staking contract was finalized
            %% As we force finality after 100 blocks we can assume that every fork
            %% at the switch height contains the staking contract
            %% TODO: Don't crash the node fully
            %%       There is an exotic but possible DOS vector here
            verify_existing_staking_contract(ContractPubkey, Trees, TxEnv),
            set_staking_contract_address(ContractPubkey),
            {ok, false} = is_hc_enabled(Trees, TxEnv),
            Trees;
        error ->
            deploy_staking_contract_by_system(Trees, TxEnv)
    end.

state_pre_transform_key_node(KeyNode, _PrevNode, PrevKeyNode, Trees1) ->
    Header = aec_block_insertion:node_header(KeyNode),
    TxEnv = node_tx_env(KeyNode),
    case is_hc_pos_header(Header) of
        true ->
            case get_hc_activation_criteria() of
                error -> aec_block_insertion:abort_state_transition(hc_activation_criteria_not_set);
                _ -> ok
            end,
            %% TODO: refactor after I get it to a working state
            Trees3 = case is_hc_pos_header(aec_block_insertion:node_header(PrevKeyNode)) of
                         true -> Trees1; %% We build on top of a HC block
                         %% Switchover block which enables staking! The prev block is being secured by PoW
                         false ->
                             %% Assert the activation criteria
                             case ensure_hc_activation_criteria(KeyNode, TxEnv, Trees1) of
                                 ok -> ok;
                                 {error, Reason} -> aec_block_insertion:abort_state_transition({hc_activation_criteria_were_not_meet, Reason})
                             end,
                             %% Notify users that staking got enabled :)
                             case protocol_staking_contract_call(Trees1, TxEnv, "protocol_enable()") of
                                 {ok, Trees2, {tuple, {}}} -> Trees2;
                                 Err1 -> aec_block_insertion:abort_state_transition({activation_failed, Err1})
                             end
                     end,
            %% Perform the leader election
            ParentHash = get_pos_header_parent_hash(Header),
            Commitments = aehc_parent_db:get_candidates_in_election_cycle(aec_headers:height(Header), ParentHash),
            %% TODO: actually hardcode the encoding
            Candidates = ["[", lists:join(", ", [aeser_api_encoder:encode(account_pubkey, aehc_commitment_header:hc_delegate(aehc_commitment:header(X))) || X <- Commitments]), "]"],
            Call = lists:flatten(io_lib:format("get_leader(~s, #~s)", [Candidates, lists:flatten([integer_to_list(X,16) || <<X:4>> <= ParentHash])])),
            %%io:format(user, "Election: ~p\n", [Call]),
            case protocol_staking_contract_call(Trees3, TxEnv, Call) of
                {ok, Trees4, {address, Leader}} ->
                    %% Assert that the miner is the person which got elected
                    case aec_block_insertion:node_miner(KeyNode) of
                        Leader -> Trees4;
                        _ -> aec_block_insertion:abort_state_transition(miner_not_leader)
                    end;
                Err2 -> aec_block_insertion:abort_state_transition({election_failed, Err2})
            end;
        false ->
            %% No adjustment required for PoW blocks
            Trees1
    end.

ensure_hc_activation_criteria(KeyNode, TxEnv, Trees) ->
    {ok,
        #activation_criteria{ check_frequency = CheckFrequency
                            , confirmations = Confirmations } = Criteria
    } = get_hc_activation_criteria(),
    Height = aec_block_insertion:node_height(KeyNode),
    case Height rem CheckFrequency =:= 0 of
        false -> {error, invalid_criteria_evaluation_point};
        true ->
            %% First evaluate the criteria at the current trees
            case ensure_hc_activation_criteria_at_trees(TxEnv, Trees, Criteria) of
                {error, _} = Err -> Err;
                ok ->
                    Done = Confirmations =:= 1 andalso aec_block_insertion:node_prev_key_hash(KeyNode) =:= aec_block_insertion:node_prev_hash(KeyNode),
                    %% Now take a look at the criteria in the past
                    case aec_chain_state:find_predecessor_at_height(KeyNode, Height-Confirmations) of
                        KeyNode -> ok; %% Zero confirmations
                        _ when Done -> ok; %% We already evaluated the criteria
                        KeyPredecessor ->
                            {PredecessorTxEnv, PredecessorTrees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', aec_block_insertion:node_hash(KeyPredecessor)),
                            ensure_hc_activation_criteria_at_trees(PredecessorTxEnv, PredecessorTrees, Criteria)
                    end
            end
    end.

-spec ensure_hc_activation_criteria_at_trees(aetx_env:env(), aec_trees:trees(), #activation_criteria{}) -> ok | {error, any()}.
ensure_hc_activation_criteria_at_trees(TxEnv, Trees,
    #activation_criteria{ minimum_delegates = MinimumDelegates
                        , minimum_stake = MinimumStake
                        }) ->
    case { static_contract_call(Trees, TxEnv, "balance()")
         , static_contract_call(Trees, TxEnv, "unique_delegates_count()") } of
        {{ok, Stake}, {ok, Delegates}} when Stake >= MinimumStake, Delegates >= MinimumDelegates ->
            ok;
        {{ok, Stake}, _} when Stake < MinimumStake ->
            {error, not_enough_stake};
        {_, {ok, _Delegates}} ->
            {error, not_enough_delegates};
        {{error, Err}, _} -> {error, {failed_call, Err}};
        {_, {error, Err}} -> {error, {failed_call, Err}}
    end.

state_pre_transform_micro_node(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Trees, Amount) -> %% TODO: we need the header here
    M = fallback_consensus(),
    M:state_grant_reward(Beneficiary, Trees, Amount).

%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok. %% TODO: we can't punish for forks due to forking on the parent chain - inspect the key headers at the given height

%% -------------------------------------------------------------------
%% Genesis block
%%TODO Const hc_genesis_version/0 won't let dialyzer do its job;
%%     remove after rebasing on the refactored branch.
-dialyzer({nowarn_function, genesis_transform_trees/2}).
genesis_transform_trees(Trees0, #{}) ->
    %% At genesis no ordinary user could possibly deploy the contract
    case get_predeploy_address() of
        {ok, Address} ->
            aec_consensus:config_assertion_failed("Unable to find already deployed staking contract", " At ~p", [Address]);
        error ->
            %% WARNING: We fake the genesis hash and the height here -
            %% the init function of the contract MUST NOT rely on the block hash or the height
            %% In case we would need to rely on the hash in the init function then
            %% forbid inserting the contract at genesis
            TxEnv = genesis_tx_env(),
            %% We don't need to check the protocol version against the block
            %% The insertion of the genesis block bypasses the version check
            Trees1 = case hc_genesis_version() of  %% Call a function here to make dialyzer happy
                ?LIMA_PROTOCOL_VSN -> aec_block_fork:apply_lima(Trees0, TxEnv);
                ?IRIS_PROTOCOL_VSN -> Trees0; %% No special changes
                _ -> aec_consensus:config_assertion_failed("Hyperchains from genesis require at least LIMA at genesis", "", [])
            end,
            deploy_staking_contract_by_system(Trees1, TxEnv)
    end.
genesis_raw_header() ->
    aec_headers:new_key_header(
        0,
        aec_governance:contributors_messages_hash(),
        <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
        <<0:32/unit:8>>,
        <<0:?MINER_PUB_BYTES/unit:8>>,
        <<0:?BENEFICIARY_PUB_BYTES/unit:8>>,
        genesis_target(),
        no_value,
        ?NONCE_HC_GENESIS,
        0,
        default,
        ?HC_GENESIS_VERSION).
genesis_difficulty() -> 0.

-ifdef(TEST).
genesis_target() ->
   ?HIGHEST_TARGET_SCI.
-else.
genesis_target() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> 16#1F1F1F1F;
        _                -> ?HIGHEST_TARGET_SCI
    end.
-endif.

%% -------------------------------------------------------------------
%% Keyblock creation
-define(FAKE_SIGNATURE, <<0:?BLOCK_SIGNATURE_BYTES/unit:8>>).
-define(FAKE_BLOCK_HASH, <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>).
-define(FAKE_STATE_HASH, <<1337:?STATE_HASH_BYTES/unit:8>>).
new_unmined_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, TreesIn) ->
    %% Ok this will be really funny
    %% First we need to determine whether we are a PoS or PoW block
    %% If the PrevKeyNode is a PoS block then we are a PoS block
    %% Otherwise check if we are at a possible HC activation point
    %% If yes the evaluate the activation criteria using the provided Trees
    Header = aec_headers:new_key_header(Height,
                               aec_block_insertion:node_hash(PrevNode),
                               aec_block_insertion:node_hash(PrevKeyNode),
                               ?FAKE_STATE_HASH,
                               Miner,
                               Beneficiary,
                               default_target(),
                               no_value,
                               0,
                               aeu_time:now_in_msecs(),
                               InfoField,
                               Protocol),
    PowNode = aec_chain_state:wrap_header(Header, ?FAKE_BLOCK_HASH),
    case is_hc_pos_header(aec_block_insertion:node_header(PrevKeyNode)) of
        true ->
            new_pos_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, TreesIn);
        false -> %% PoW block or a switchover block
            case get_hc_activation_criteria() of
                error -> PowNode;
                {ok, #activation_criteria{check_frequency = Frequency}} when Height rem Frequency =:= 0 ->
                    case ensure_hc_activation_criteria(PowNode, node_tx_env(PowNode), TreesIn) of
                        {error, _} -> PowNode;
                        ok ->
                            new_pos_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, TreesIn)
                    end;
                _ -> %% Not at a possible activation point
                    PowNode
            end
    end.

%%TODO Same as genesis_transform_trees/2. FAKE_BLOCK_HASH breaks hash() type contract.
-dialyzer({nowarn_function, new_pos_key_node/8}).
new_pos_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, _TreesIn) ->
    %% TODO: PoGF - for now just ignore generational fraud - let's first get a basic version working
    %%       When handling PoGF the commitment point is in a different place than usual
    ParentBlock = aehc_utils:submit_commitment(PrevKeyNode, Miner), %% TODO: Miner vs Delegate, Which shall register?
    Seal = create_pos_pow_field(aehc_parent_block:hash_block(ParentBlock), ?FAKE_SIGNATURE),
    Header = aec_headers:new_key_header(Height,
                           aec_block_insertion:node_hash(PrevNode),
                           aec_block_insertion:node_hash(PrevKeyNode),
                           ?FAKE_STATE_HASH,
                           Miner,
                           Beneficiary,
                           default_target(), %% TODO: target calculation - evaluate the stakes
                           Seal,
                           ?NONCE_HC_ENABLED,
                           aeu_time:now_in_msecs(),
                           InfoField,
                           Protocol),
    aec_chain_state:wrap_header(Header, ?FAKE_BLOCK_HASH).

keyblocks_for_unmined_keyblock_adjust() ->
    M = fallback_consensus(),
    max(1, M:keyblocks_for_target_calc()).

adjust_unmined_keyblock(B, R) ->
    case is_hc_pos_header(aec_blocks:to_header(B)) of
        true ->
            {ok, aec_blocks:set_target(B, 0)}; %% No adjustment required - everything is done by new_unmined_key_node
        false ->
            M = fallback_consensus(),
            M:adjust_unmined_keyblock(B, R)
    end.

%% -------------------------------------------------------------------
%% Keyblock sealing
key_header_for_sealing(Header) ->
    case is_hc_pos_header(Header) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:key_header_for_sealing(Header)
    end.

validate_key_header_seal(Header, Protocol) ->
    case is_hc_pos_header(Header) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:validate_key_header_seal(Header, Protocol)
    end.

generate_key_header_seal(HeaderBin, Header, Nonce, MinerConfig, AddressedInstance) ->
    case is_hc_pos_header(Header) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:generate_key_header_seal(HeaderBin, Header, Nonce, MinerConfig, AddressedInstance)
    end.

set_key_block_seal(Block, Seal) ->
    case is_hc_pos_header(aec_blocks:to_header(Block)) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:set_key_block_seal(Block, Seal)
    end.

nonce_for_sealing(Header) ->
    case is_hc_pos_header(Header) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:nonce_for_sealing(Header)
    end.

next_nonce_for_sealing(?NONCE_HC_ENABLED, _) -> ?NONCE_HC_ENABLED;
next_nonce_for_sealing(?NONCE_HC_POGF, _) -> ?NONCE_HC_POGF;
next_nonce_for_sealing(Nonce, MinerConfig) ->
    M = fallback_consensus(),
    M:next_nonce_for_sealing(Nonce, MinerConfig).

trim_sealing_nonce(?NONCE_HC_ENABLED, _) -> ?NONCE_HC_ENABLED;
trim_sealing_nonce(?NONCE_HC_POGF, _) -> ?NONCE_HC_POGF;
trim_sealing_nonce(Nonce, MinerConfig) ->
    M = fallback_consensus(),
    M:trim_sealing_nonce(Nonce, MinerConfig).

%% -------------------------------------------------------------------
%% Block target and difficulty
default_target() ->
    M = fallback_consensus(),
    M:default_target().

assert_key_target_range(Target) ->
    M = fallback_consensus(),
    M:assert_key_target_range(Target).

key_header_difficulty(Header) ->
    case is_hc_pos_header(Header) of
        true -> 0; %% TODO: retrieve the amount of stake which voted in this election
        false ->
            M = fallback_consensus(),
            M:key_header_difficulty(Header)
    end.

%% -------------------------------------------------------------------
%% Hyperchains specific
genesis_tx_env() ->
    Header = aec_headers:set_height(genesis_raw_header(), 1), %% Fake the height
    aetx_env:tx_env_from_key_header(Header, <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
        aec_headers:time_in_msecs(Header), aec_headers:prev_hash(Header)).

node_tx_env(Node) ->
    Header = aec_block_insertion:node_header(Node),
    aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(Node),
        aec_block_insertion:node_time(Node), aec_block_insertion:node_prev_hash(Node)).

get_staking_contract_bytecode() -> maps:get(bytecode, persistent_term:get(?STAKING_CONTRACT)).
get_staking_contract_aci() -> maps:get(aci, persistent_term:get(?STAKING_CONTRACT)).

%% Loads the persisted staking contract address and caches it using persistent term
load_staking_contract_address() ->
    case aec_db:find_hc_staking_contract_address() of
        none -> persistent_term:erase(?STAKING_CONTRACT_ADDR);
        {value, Address} -> persistent_term:put(?STAKING_CONTRACT_ADDR, {ok, Address})
    end.

%% After deploying the staking contract persists the address
set_staking_contract_address(ContractAddress) ->
    aec_db:write_hc_staking_contract_address(ContractAddress),
    persistent_term:put(?STAKING_CONTRACT_ADDR, {ok, ContractAddress}).

-spec get_staking_contract_address() -> not_deployed | {ok, binary()}.
get_staking_contract_address() -> persistent_term:get(?STAKING_CONTRACT_ADDR, not_deployed).

%% Sets the public key of a predeployed staking contract
-spec set_predeploy_address(binary()) -> ok.
set_predeploy_address(Address) ->
    application:set_env(aehyperchains, predeploy_address, Address),
    ok.

%% Unsets the predeployed staking contract
unset_predeploy_address() -> application:unset_env(aehyperchains, predeploy_address).

%% Gets the public key of a predeployed staking contract
-spec get_predeploy_address() -> {ok, binary()} | error.
get_predeploy_address() ->
    %% TODO: Expose in config
    case application:get_env(aehyperchains, predeploy_address, error) of
        error -> error;
        Addr -> {ok, Addr}
    end.

%% 1. The minimum balance of the staking contract required to start the HC
%% 2. The minimum amount of unique delegates required to start the HC
%% 3. The frequency at which we check the criteria
%% 4. The confirmation depth of the criteria
%%    The first HC PoS block MUST fulfill the activation criteria(the state on which we base the block on MUST fulfill the criteria) and the Nth-key predecessor must also fulfill them
%%    For instance when the config is (100AE, 2, 10, 2) and the criteria were first meet at one microblock in the generation with height 19 then:
%%    - HC can't be enabled at the keyblock at height 20 - although the criteria were meet at the predecesor, the keyblock predecesor at height 18 doesn't pass the criteria
%%    - We never run the check at keyblocks with heights not divisible by 10(21-29 in the example)
%%    - HC gets enabled at the keyblock at height 30 - the criteria pass at the direct predecessor and the keyblock at height 28
%%      making the keyblock with height 30 eligible to be the first HC block - validators commit to block 29, block 30 is the FIRST to deviate from PoW
%% More examples(assuming activation at height 30 and where we checked at height 30):
%% * (100AE, 2, 10, 0)
%%                     Chain:            K - m - m - K - m - m - K
%%                     Height:           29  29  29  30  30  30  31
%%                     First HC block:               X
%%                     Criteria pass at:     X   X   X   X   X   X
%%                     Criteria checks:           X
%% * (100AE, 2, 10, 1)
%%                     Chain:            K - m - m - K - m - m - K
%%                     Height:           29  29  29  30  30  30  31
%%                     First HC block:               X
%%                     Criteria pass at: X   X   X   X   X   X   X
%%                     Criteria checks:  X        X
%% * (100AE, 2, 1, 1)
%%                     Chain:                 K - m - K - m - m - K - m - m - K
%%                     Height:                28  28  29  29  29  30  30  30  31
%%                     First HC block:                            X
%%                     Criteria pass at:          X   X   X   X   X   X   X   X
%%                     Criteria checks at 30:         X       X
%%                     Criteria checks at 29: X   X
%% * (100AE, 2, 10, 2)
%%                     Chain:                 K - m - K - m - m - K - m - m - K
%%                     Height:                28  28  29  29  29  30  30  30  31
%%                     First HC block:                            X
%%                     Criteria pass at:      X   X   X   X   X   X   X   X   X
%%                     Criteria checks at 30: X               X

%% Sets the HC activation criteria
-spec set_hc_activation_criteria(integer(), integer(), integer(), integer()) -> ok.
set_hc_activation_criteria(MinimumStake, MinimumDelegates, BlockFrequency, BlockConfirmations) ->
    application:set_env(aehyperchains, activation_criteria,
        #activation_criteria{
            minimum_stake = MinimumStake,
            minimum_delegates = MinimumDelegates,
            check_frequency = BlockFrequency,
            confirmations = BlockConfirmations
        }),
    ok.

%% Unsets the HC activation criteria
unset_hc_activation_criteria() -> application:unset_env(aehyperchains, activation_criteria).

%% Gets the HC activation criteria - if none specified then HC can't be activated
-spec get_hc_activation_criteria() -> {ok, #activation_criteria{}} | error.
get_hc_activation_criteria() ->
    %% TODO: Expose in config
    case application:get_env(aehyperchains, activation_criteria, error) of
        error -> error;
        Criteria -> {ok, Criteria}
    end.

%% Deploys the staking contract using a free system account :)
%% TODO: For now just hardcode the settings
-define(VM_VERSION, 5).
-define(ABI_VERSION, 3).
-define(DEPOSIT_DELAY, 5).
-define(STAKE_RETRACTION_DELAY, 5).
-define(WITHDRAW_DELAY, 10).
-define(DRY_RUN_ACCOUNT, <<1:32/unit:8>>).
-define(RESTRICTED_ACCOUNT, <<2:32/unit:8>>).
deploy_staking_contract_by_system(Trees0, TxEnv) ->
    lager:debug("Deploying the staking contract by a system account"),
    Deployer = ?RESTRICTED_ACCOUNT,
    lager:debug("Staking contract will be deployed by ~p", [aeser_api_encoder:encode(account_pubkey, Deployer)]),
    %% Grant the fresh account enough funds to deploy the contract
    {Trees1, OldA} = prepare_system_owner(Deployer, Trees0),
    Bytecode = get_staking_contract_bytecode(),
    Aci = get_staking_contract_aci(),
    Args = lists:flatten(io_lib:format("init({deposit_delay = ~p, stake_retraction_delay = ~p, withdraw_delay = ~p}, {}, ~s)",
        [?DEPOSIT_DELAY, ?STAKE_RETRACTION_DELAY, ?WITHDRAW_DELAY, aeser_api_encoder:encode(account_pubkey, ?RESTRICTED_ACCOUNT)])),
    {ok, CtorCall} = aeaci_aci:encode_call_data(Aci, Args),
    Nonce = 1,
    TxSpec = #{ owner_id => aeser_id:create(account, Deployer)
              , nonce => aec_accounts:nonce(OldA) + 1
              , code => Bytecode
              , vm_version => ?VM_VERSION
              , abi_version => ?ABI_VERSION
              , deposit => 0
              , amount => 0
              , gas => 1 bsl 30
              , gas_price => 1 bsl 30
              , call_data => CtorCall
              , fee => 1 bsl 60
              },
    {ok, Tx} = aect_create_tx:new(TxSpec),
    case aetx:process(Tx, Trees1, TxEnv) of
        {ok, Trees2, _} ->
            ContractPubkey = aect_contracts:compute_contract_pubkey(Deployer, Nonce),
            CallPubkey     = aect_call:id(Deployer, Nonce, ContractPubkey),
            CallTree       = aec_trees:calls(Trees2),
            {value, Call}  = aect_call_state_tree:lookup_call(ContractPubkey, CallPubkey, CallTree),
            case aect_call:return_type(Call) of
                ok ->
                    lager:debug("System account successfully deployed staking contract at ~p", [aeser_api_encoder:encode(contract_pubkey, ContractPubkey)]),
                    %% Ok contract deployed :)
                    set_staking_contract_address(ContractPubkey),
                    %% Sanity check the deployment - should never fail
                    verify_existing_staking_contract(ContractPubkey, Trees2, TxEnv),
                    {ok, false} = is_hc_enabled(Trees2, TxEnv),
                    Trees2;
                What ->
                    Value = aect_call:return_type(Call),
                    aec_consensus:config_assertion_failed("Failed to deploy staking contract", " Error: ~p Ret: ~p\n", [What, Value])
            end,
            cleanup_system_owner(OldA, Trees2);
        {error, What} ->
            aec_consensus:config_assertion_failed("Unable to deploy staking contract by system", " Error: ~p\n", [What])
    end.

%% Temporarily grants a lot of funds for the system account
prepare_system_owner(Deployer, Trees) ->
    Accounts0 = aec_trees:accounts(Trees),
    {ok, NewA, OldA} = case aec_accounts_trees:lookup(Deployer, Accounts0) of
                  none ->
                      A = aec_accounts:new(Deployer, 1 bsl 61),
                      {ok, A, aec_accounts:new(Deployer, 0)};
                  {value, A0} ->
                      {ok, A1} = aec_accounts:earn(A0, 1 bsl 61),
                      {ok, A1, A0}
              end,
    Accounts1 = aec_accounts_trees:enter(NewA, Accounts0),
    {aec_trees:set_accounts(Trees, Accounts1), OldA}.

%% Ensures the system account has the same balace as before(we don't touch the inflation curve) and bumped nonce
cleanup_system_owner(OldA, Trees) ->
    Accounts0 = aec_trees:accounts(Trees),
    Account = aec_accounts:set_nonce(OldA, aec_accounts:nonce(OldA) + 1),
    Accounts1 = aec_accounts_trees:enter(Account, Accounts0),
    aec_trees:set_accounts(Trees, Accounts1).

verify_existing_staking_contract(Address, Trees, TxEnv) ->
    UserAddr = aeser_api_encoder:encode(contract_pubkey, Address),
    lager:debug("Validating the existing staking contract at ~p", [UserAddr]),
    ErrF = fun(Err) -> aec_consensus:config_assertion_failed("Staking contract validation failed", " Addr: ~p, Reason: ~p\n", [UserAddr, Err]) end,
    case aec_accounts_trees:lookup(Address, aec_trees:accounts(Trees)) of
        none ->
            ErrF("Contract not found");
        {value, Account} ->
            case aec_accounts:is_payable(Account) of
                true -> ErrF("Contract can't be payable");
                false ->
                    case aect_state_tree:lookup_contract_with_code(Address, aec_trees:contracts(Trees), [no_store]) of
                        none ->
                            ErrF("Contract not found");
                        {value, Contract, OnchainCode} ->
                            OnchainAbi = aect_contracts:abi_version(Contract),
                            OnchainVm = aect_contracts:vm_version(Contract),

                            %% TODO: Remove the stripping for IRIS...
                            Code0 = aeser_contract_code:deserialize(get_staking_contract_bytecode()),
                            FateCode0  = aeb_fate_code:deserialize(maps:get(byte_code, Code0)),
                            FateCode1 = aeb_fate_code:strip_init_function(FateCode0),
                            Bytecode = aeb_fate_code:serialize(FateCode1),
                            LocalCode = aeser_contract_code:serialize(Code0#{ byte_code => Bytecode
                                                                            , compiler_version => <<"unknown">>
                                                                            }),

                            if  OnchainAbi /= ?ABI_VERSION -> ErrF("Wrong ABI version");
                                OnchainVm /= ?VM_VERSION -> ErrF("Wrong VM version");
                                LocalCode /= OnchainCode -> ErrF({"Invalid staking contract bytecode"});
                                true ->
                                    set_staking_contract_address(Address),
                                    {ok, {address, Restricted}} = static_contract_call(Trees, TxEnv, "restricted_address()"),
                                    case Restricted of
                                        ?RESTRICTED_ACCOUNT ->
                                            lager:debug("Staking contract at ~p is safe to use", [UserAddr]);
                                        _ ->
                                            ErrF("Invalid restricted account")
                                    end
                            end
                    end
            end
    end.

%% Makes a static query using <<1:32/unit-8>> as the caller - similar to dry run
%% It's safe as we never commit the result to the database - any state mutations are kept in the MPT cache
%% Can be used on the genesis block
static_contract_call(Trees0, TxEnv0, Query) ->
    TxEnv1 = case aetx_env:height(TxEnv0) of
            0 ->
                genesis_tx_env();
            _ -> TxEnv0
        end,
    Aci = get_staking_contract_aci(),
    {ok, ContractPubkey} = get_staking_contract_address(),
    {ok, CallData} = aeaci_aci:encode_call_data(Aci, Query),
    Accounts0 = aec_trees:accounts(Trees0),
    Accounts1 = aec_accounts_trees:enter(aec_accounts:new(?DRY_RUN_ACCOUNT, 1 bsl 61), Accounts0),
    Trees1 = aec_trees:set_accounts(Trees0, Accounts1),
    TxEnv = aetx_env:set_dry_run(TxEnv1, true),
    TxSpec = #{ caller_id   => aeser_id:create(account, ?DRY_RUN_ACCOUNT)
              , nonce       => 1
              , contract_id => aeser_id:create(contract, ContractPubkey)
              , abi_version => ?ABI_VERSION
              , fee         => 1 bsl 60
              , amount      => 0
              , gas         => 1 bsl 30
              , gas_price   => 1 bsl 30
              , call_data   => CallData
              },
    {ok, Tx} = aect_call_tx:new(TxSpec),
    case aetx:process(Tx, Trees1, TxEnv) of
        {ok, Trees2, _} ->
            CallPubkey     = aect_call:id(?DRY_RUN_ACCOUNT, 1, ContractPubkey),
            CallTree       = aec_trees:calls(Trees2),
            {value, Call}  = aect_call_state_tree:lookup_call(ContractPubkey, CallPubkey, CallTree),
            case aect_call:return_type(Call) of
                ok ->
                    {ok, aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                What ->
                    {error, {What, aeb_fate_encoding:deserialize(aect_call:return_value(Call))}}
            end;
        {error, _What} = Err -> Err
    end.

is_hc_enabled(Trees, TxEnv) -> static_contract_call(Trees, TxEnv, "enabled()").

%% Helpers for static queries of the staking contract
%% TODO: Better names - We're not JAVA programmers
static_staking_contract_call_on_top_block(Query) ->
    aec_db:ensure_activity(async_dirty, fun() ->
        case aec_chain:top_block_hash() of
            undefined -> {error, missing_top_block};
            TopHash   ->
                {Env, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', TopHash),
                static_contract_call(Trees, Env, Query)
        end end).

static_staking_contract_call_on_block_hash(BlockHash, Query) ->
    aec_db:ensure_activity(async_dirty, fun() ->
        case aec_chain:get_header(BlockHash) of
            error -> {error, missing_block_hash};
            {ok, _} ->
                {Env, Trees} = aetx_env:tx_env_and_trees_from_hash('aetx_transaction', BlockHash),
                static_contract_call(Trees, Env, Query)
        end end).

%% Stateful staking contract invocation using the restricted account
%% Used to perform the election of the leader of the next generation or to punish someone for misbehaving
%% Protocol calls at genesis are disallowed - it's impossible for the staking contract to be active at genesis
protocol_staking_contract_call(Trees0, TxEnv, Query) ->
    Aci = get_staking_contract_aci(),
    {ok, ContractPubkey} = get_staking_contract_address(),
    {ok, CallData} = aeaci_aci:encode_call_data(Aci, Query),
    Accounts0 = aec_trees:accounts(Trees0),
    SavedAccount = case aec_accounts_trees:lookup(?RESTRICTED_ACCOUNT, Accounts0) of
          none -> error;
          {value, A} -> {ok, A}
      end,
    Accounts1 = aec_accounts_trees:enter(aec_accounts:new(?RESTRICTED_ACCOUNT, 1 bsl 61), Accounts0),
    Trees1 = aec_trees:set_accounts(Trees0, Accounts1),
    TxSpec = #{ caller_id   => aeser_id:create(account, ?RESTRICTED_ACCOUNT)
              , nonce       => 1
              , contract_id => aeser_id:create(contract, ContractPubkey)
              , abi_version => ?ABI_VERSION
              , fee         => 1 bsl 60
              , amount      => 0
              , gas         => 1 bsl 30
              , gas_price   => 1 bsl 30
              , call_data   => CallData
              },
    {ok, Tx} = aect_call_tx:new(TxSpec),
    case aetx:process(Tx, Trees1, TxEnv) of
        {ok, Trees2, _} ->
            CallPubkey     = aect_call:id(?RESTRICTED_ACCOUNT, 1, ContractPubkey),
            CallTree       = aec_trees:calls(Trees2),
            {value, Call}  = aect_call_state_tree:lookup_call(ContractPubkey, CallPubkey, CallTree),
            case aect_call:return_type(Call) of
                ok ->
                    Accounts2 =
                        case SavedAccount of
                           {ok, Pub} -> aec_accounts_trees:enter(Pub, aec_trees:accounts(Trees2));
                           error -> aec_accounts_trees:delete(?RESTRICTED_ACCOUNT, aec_trees:accounts(Trees2))
                        end,
                    Trees3 = aec_trees:set_accounts(Trees2, Accounts2),
                    {ok, aect_call_state_tree:prune(1, Trees3), aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                What ->
                    {error, {What, aeb_fate_encoding:deserialize(aect_call:return_value(Call))}}
            end;
        {error, _What} = Err -> Err
    end.

%% What consensus shall we use in case the activation criteria of the hyperchain consensus weren't meet?
%% TODO: customize
fallback_consensus() ->
    aec_consensus_bitcoin_ng.

hc_genesis_version() -> ?HC_GENESIS_VERSION.
