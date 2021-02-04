%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Hyperchains consensus - https://github.com/aeternity/hyperchains-whitepaper
%%% The deployer of the staking contract is either an ordinary user or <<2:32/unit-8>>
%%% If the address of the contract was specified then the contract MUST exist on-chain when enabling HC consensus
%%% Furthermore the bytecode of the specified contract MUST match the bytecode of our local contract copy
%%% Additionally we sanity check the contract configuration
%%% Keep in mind that the deployer of the contract doesn't have any special privileges
%%% over the contract after it was deployed.
%%% When we don't rely on an already deployed staking contract then we take the first free system account and deploy it as a system contract
%%% in case of ae-mainnet when no ordinary user deployed the staking contract the contract WILL get deployed by <<2:32/unit-8>>
%%% When enabling HC consensus we check whether the staking contract was already predeployed by an ordinary user
%%% All stateful contract calls to the staking contract are made using <<2:32/unit-8>>, stateless calls are made using <<1:32/unit-8>>
%%% Calls originating from <<2:32/unit-8>> and <<1:32/unit-8>> are made using funds created from thin air
%%% System calls using <<2:32/unit-8>> never change the funds of that account nor bump the nonce
%%% Calls made using <<1:32/unit-8>> never require special preprocessing as we throw away the new state trees
%%% @end
%%% -------------------------------------------------------------------
-module(aehc_consensus_hyperchains).

-behavior(aec_consensus).

-define(TAG, 13370).
-define(STAKING_CONTRACT, {?MODULE, staking_contract}).
-define(STAKING_CONTRACT_ADDR, {?MODULE, staking_contract_addr}).
%% Lima or Iris as we need the FATE VM at genesis
%% In case that's unwanted then start up another consensus before hyperchains
-define(HC_GENESIS_VERSION, ?LIMA_PROTOCOL_VSN).

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
        %% Target adjustment when creating keyblocks
        , keyblocks_for_target_calc/0
        , keyblock_create_adjust_target/2
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/2
        , state_pre_transform_key_node/2
        , state_pre_transform_micro_node/2
        %% Block rewards
        , state_grant_reward/3
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
        , key_header_difficulty/1 ]).

%% Staking contract helpers
-export([ get_staking_contract_aci/0
        , get_staking_contract_address/0
        , static_staking_contract_call_on_top_block/1
        , static_staking_contract_call_on_block_hash/2
        ]).

%% Staking contract predeploy
-export([ get_predeploy_address/0
        , set_predeploy_address/1
        , unset_predeploy_address/0
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecore/include/blocks.hrl").

can_be_turned_off() -> true.
assert_config(_Config) ->
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
    end.

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
                , aeser_api_encoder:encode(contract_pubkey, y)
                ]);
        {_, _} -> ok
    end,
    %% Spawn the block generator etc...
    %% Crank down the finalized height delta to 2-4 ;)
    ok.
stop() -> ok.

is_providing_extra_http_endpoints() -> false.
client_request(_) -> error(todo).

extra_from_header(_) ->
    #{consensus => ?MODULE}.

recent_cache_n() -> 1.
recent_cache_trim_key_header(_) -> ok.

keyblocks_for_target_calc() -> 0.
keyblock_create_adjust_target(Block, []) -> {ok, Block}.

dirty_validate_block_pre_conductor(_) -> ok.
%% Don't waste CPU cycles when we are only interested in state transitions...
dirty_validate_key_node_with_ctx(_Node, _Block, _Ctx) -> ok.
dirty_validate_micro_node_with_ctx(_Node, _Block, _Ctx) -> ok.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(KeyNode, Trees) ->
    case get_staking_contract_address() of
        not_deployed ->
            case get_predeploy_address() of
                {ok, Address} ->
                    %% When the staking contract was not found it is better to
                    %% bring down the entire node down. By rejecting the state transition
                    %% the node will get stuck syncing on the specified height
                    Trees;
                    %%find_existing_staking_contract(Deployer, Trees);
                error ->
                    Header = aec_block_insertion:node_header(KeyNode),
                    TxEnv = aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(KeyNode),
                        aec_block_insertion:node_time(KeyNode), aec_block_insertion:node_prev_hash(KeyNode)),
                    deploy_staking_contract_by_system(Trees, TxEnv)
            end;
        {ok, _} -> Trees
    end.

state_pre_transform_key_node(_Node, Trees) -> Trees.
state_pre_transform_micro_node(_Node, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Trees, Amount) -> aec_consensus_bitcoin_ng:state_grant_reward(Beneficiary, Trees, Amount).

%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees0, #{}) ->
    %% At genesis no ordinary user could possibly deploy the contract
    case get_predeploy_address() of
        {ok, Address} ->
            aec_consensus:config_assertion_failed("Unable to find already deployed staking contract", " At ~p", [Address]);
        error ->
            %% WARNING: We fake the genesis hash and the height here -
            %% the init function of the contract MUST NOT rely on the block hash or the height
            %% TODO: In case we would need to rely on the hash in the init function then
            %%       forbid inserting the contract at genesis
            Header = aec_headers:set_height(genesis_raw_header(), 1), %% Fake the height
            TxEnv = aetx_env:tx_env_from_key_header(Header, <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
                aec_headers:time_in_msecs(Header), aec_headers:prev_hash(Header)),
            %% We don't need to check the protocol version against the block
            %% The insertion of the genesis block bypasses the version check
            Trees1 = case ?HC_GENESIS_VERSION of
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
        ?TAG,
        no_value,
        0,
        0,
        default,
        ?HC_GENESIS_VERSION).
genesis_difficulty() -> 0.

key_header_for_sealing(Header) ->
    aec_headers:root_hash(Header).

validate_key_header_seal(_Header, _Protocol) ->
    ok.

generate_key_header_seal(_, _, ?TAG, _, _) ->
    { continue_mining, {ok, ?TAG} }.

set_key_block_seal(Block, ?TAG) ->
    aec_blocks:set_nonce_and_pow(Block, ?TAG, [?TAG]).

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

%% Deploys the staking contract by the first free system account :)
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
                    {ok, {address, ?RESTRICTED_ACCOUNT}} = static_contract_call(Trees2, TxEnv, "restricted_address()"),
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
                      {ok, A, A};
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

%% Makes a static query using <<1:32/unit-8>> as the caller - similar to dry run
%% It's safe as we never commit the result to the database - any state mutations are kept in the MPT cache
static_contract_call(Trees0, TxEnv0, Query) ->
    Aci = get_staking_contract_aci(),
    {ok, ContractPubkey} = get_staking_contract_address(),
    {ok, CallData} = aeaci_aci:encode_call_data(Aci, Query),
    Accounts0 = aec_trees:accounts(Trees0),
    Accounts1 = aec_accounts_trees:enter(aec_accounts:new(?DRY_RUN_ACCOUNT, 1 bsl 61), Accounts0),
    Trees1 = aec_trees:set_accounts(Trees0, Accounts1),
    TxEnv = aetx_env:set_dry_run(TxEnv0, true),
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
                    {error, {What, aect_call:return_type(Call)}}
            end;
        {error, _What} = Err -> Err
    end.

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
