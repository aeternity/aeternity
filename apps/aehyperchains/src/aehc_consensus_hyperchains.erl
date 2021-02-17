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
        , load_staking_contract_address/0
        ]).

%% Staking contract predeploy
-export([ get_predeploy_address/0
        , set_predeploy_address/1
        , unset_predeploy_address/0
        ]).

%% General helpers
-export([ hc_header_type/1
        , is_hc_pos_header/1
        ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecore/include/blocks.hrl").
-include_lib("aeminer/include/aeminer.hrl").

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

% -----------------------------------------

extra_from_header(Header) ->
    Type = case aec_headers:type(Header) of
               key ->
                    case aec_headers:nonce(Header) of
                       ?NONCE_HC_ENABLED -> key_pos;  %% The first key block after the contract got "activated"
                       ?NONCE_HC_POGF -> key_pos_pogf;
                       _ -> key_pow
                   end;
               micro -> micro
           end,
    %% We can't really switch to another consensus right now as we rely on the global consensus setting most of the time
    #{ consensus => ?MODULE
     , type => Type
     , pos => Type =:= key_pos orelse Type =:= key_pos_pogf
     }.

-spec hc_header_type(aec_headers:header()) -> key_pos | key_pos_pogf | key_pow | micro | no_return().
hc_header_type(Header) ->
    maps:get(type, aec_headers:extra(Header)).

-spec is_hc_pos_header(aec_headers:header()) -> boolean().
is_hc_pos_header(Header) ->
    maps:get(pos, aec_headers:extra(Header), false).

% -----------------------------------------

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

keyblocks_for_target_calc() ->
    M = fallback_consensus(),
    max(1, M:keyblocks_for_target_calc()).

keyblock_create_adjust_target(B, R) ->
    case is_hc_pos_header(aec_blocks:to_header(B)) of
        true ->
            {ok, aec_blocks:set_target(B, 0)}; %% TODO: calculate the target based on the voting power
        false ->
            M = fallback_consensus(),
            M:keyblock_create_adjust_target(B, R)
    end.

dirty_validate_block_pre_conductor(B) ->
    case is_hc_pos_header(aec_blocks:to_header(B)) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_block_pre_conductor(B)
    end.

dirty_validate_header_pre_conductor(H) ->
    case is_hc_pos_header(H) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_header_pre_conductor(H)
    end.

dirty_validate_key_hash_at_height(Height, Hash) ->
    M = fallback_consensus(),
    M:dirty_validate_key_hash_at_height(Height, Hash).

dirty_validate_key_node_with_ctx(Node, Block, Ctx) ->
    case is_hc_pos_header(aec_blocks:to_header(Block)) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_key_node_with_ctx(Node, Block, Ctx)
    end.

dirty_validate_micro_node_with_ctx(Node, Block, Ctx) ->
    case is_hc_pos_header(aec_blocks:to_header(Block)) of
        true -> error(todo);
        false ->
            M = fallback_consensus(),
            M:dirty_validate_micro_node_with_ctx(Node, Block, Ctx)
    end.

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(KeyNode, Trees) ->
    Header = aec_block_insertion:node_header(KeyNode),
    TxEnv = aetx_env:tx_env_from_key_header(Header, aec_block_insertion:node_hash(KeyNode),
        aec_block_insertion:node_time(KeyNode), aec_block_insertion:node_prev_hash(KeyNode)),
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
                    verify_existing_staking_contract(ContractAddress, Trees, TxEnv);
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
            {ok, false} = is_hc_enabled(Trees, TxEnv);
        error ->
            deploy_staking_contract_by_system(Trees, TxEnv)
    end.

state_pre_transform_key_node(_Node, Trees) -> Trees.

state_pre_transform_micro_node(_Node, Trees) -> Trees.

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
    M:next_nonce(Nonce, MinerConfig).

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
                false -> ErrF("Contract not payable");
                true ->
                    case aect_state_tree:lookup_contract(Address, aec_trees:contracts(Trees), [no_store]) of
                        none ->
                            ErrF("Contract not found");
                        {value, Contract} ->
                            OnchainAbi = aect_contracts:abi_version(Contract),
                            OnchainVm = aect_contracts:vm_version(Contract),
                            OnchainCode = aect_contracts:code(Contract),

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
                                LocalCode /= OnchainCode -> ErrF("Invalid staking contract bytecode");
                                true ->
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
                    {error, {What, aect_call:return_type(Call)}}
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
            CallPubkey     = aect_call:id(?DRY_RUN_ACCOUNT, 1, ContractPubkey),
            CallTree       = aec_trees:calls(Trees2),
            {value, Call}  = aect_call_state_tree:lookup_call(ContractPubkey, CallPubkey, CallTree),
            case aect_call:return_type(Call) of
                ok ->
                    Accounts2 = aec_accounts_trees:enter(SavedAccount, aec_trees:accounts(Trees2)),
                    Trees3 = aec_trees:set_accounts(Trees2, Accounts2),
                    {ok, Trees3, aeb_fate_encoding:deserialize(aect_call:return_value(Call))};
                What ->
                    {error, {What, aect_call:return_type(Call)}}
            end;
        {error, _What} = Err -> Err
    end.

%% What consensus shall we use in case the activation criteria of the hyperchain consensus weren't meet?
%% TODO: customize
fallback_consensus() ->
    aec_consensus_bitcoin_ng.
