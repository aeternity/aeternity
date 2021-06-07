%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc Consensus behavior for customizing the node
%%%      Consensus is responsible for validating, emitting and processing of keyblocks
%%%      Only one consensus algorithm might be enabled at a given height
%%%      Some consensus algorithms provide a special instrumentation HTTP API
%%%      for controlling consensus specific functionality. Some consensus
%%%      modules might overwrite existing functionality in the node using plugins.
%%%      Some consensus modules cannot be disabled after enabling.
%%%      Consensus is only a small part of the overall on-chain protocol
%%%      Protocol defined features are configured separately - new features
%%%      can get enabled and introduced regardless of the used consensus.
%%%      --------------------------------------------------------------
%%%      "Dev Mode" would work in the following way:
%%%      - Disallow disabling of dev mode after it got enabled
%%%      - Optionally use state from a real block
%%%      - Disable sync, gossip, peer discovery etc...
%%%      - Start an in-ram chain simulator or even multiple ones if requested
%%%      - Mock the tx push HTTP endpoint and instantly mine transactions pushed to it
%%%      - Ignore PoW in the block headers
%%%      - Use plugins almost anywhere
%%%      - Provide an API for instrumenting the chain:
%%%        * Start from real_block_hash - starts a chain simulator based on real-world state
%%%          (might be taken from mainnet/testnet).
%%%        * Start empty - starts a new chain simulator
%%%        * Enable/Disable instant tx processing
%%%        * Commit pending txs to a microblock
%%%        * Clone on microblock/fork etc...
%%%        * Set account state etc...
%%%        * N new keyblocks on top of the current top block(or block with given hash)
%%%        * Set the given block hash as the new top
%%%        * Generate FATE execution traces, change contract code and state at will
%%%      --------------------------------------------------------------
%%%      PoA Consensus with one authority would work in the following way:
%%%      - Can be enabled/disabled and switched to another algorithm
%%%      - No plugins are enabled
%%%      - Keyblocks contain the signature of the chosen authority
%%%      - API for querying and changing the authority in the node
%%%     ---------------------------------------------------------------
%%%     PoA with a contract
%%%     - Can be enabled/disabled and switched to another algorithm
%%%     - No plugins are enabled
%%%     - Keyblocks contain the signature of the chosen authority taken from a contract
%%%     - The leader is chosen by a contract
%%%     - API for querying the authorities, consensus status
%%%     ---------------------------------------------------------------
%%%     HC Consensus
%%%     - Can be enabled/disabled and switched to another algorithm
%%%     - Some plugins might get enabled
%%%     - Keyblocks contain hashes from another blockchain and a signature
%%%     - API for querying election/delegate status etc...
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus).

%% API
-export([ check_env/0
        , set_consensus/0
        , set_genesis_hash/0
        , get_consensus_module_at_height/1
        , get_consensus_config_at_height/1
        , get_genesis_consensus_module/0
        , get_genesis_consensus_config/0
        , get_genesis_hash/0 %% Cached access to the genesis hash using persistent term :)
        ]).

-include("aec_block_insertion.hrl").

%% Global config
%% Height => {ConsensusName, ConsensusConfiguration}
-type consensus_module() :: atom().
-type consensus_config() :: #{binary() => term()}.
-type global_consensus_config() :: [{non_neg_integer(), {consensus_module(), consensus_config()}}].

%% Block sealing
-type key_seal() :: [integer()].
-type key_target() :: integer().
-type key_difficulty() :: integer().
-type key_nonce() :: integer().

-export_type([ key_seal/0
             , key_target/0
             , key_difficulty/0
             , key_nonce/0
             , consensus_module/0
             , consensus_config/0 ]).

%% -------------------------------------------------------------------
%% Global consensus features
%% -------------------------------------------------------------------
%% Some consensus methods alter the node dramatically
%% Dev mode once enabled should never be turned off(unless the node restarts)
%% PoA or HC consensus can be turned off and changed to something else
-callback can_be_turned_off() -> boolean().

%% Assert user configuration
-callback assert_config(consensus_config()) -> ok.

%% Should start the given consensus implementation - called only after prepare_start
%% Some consensus implementations should ensure that it can be enabled at the given point in time
%% for instance by killing the peer pool, bypassing the mining conductor etc...
%% gets the configuration previously validated by assert_config/1
-callback start(consensus_config()) -> ok.
%% Stops the given consensus
%% Only required for consensuses for which can_be_turned_off() =:= true
-callback stop() -> ok.

%% Some consensus implementations might provide external http methods
%% Clique PoA might provide a voting API for delegates
%% HC might provide a debug endpoint for the staking contract and some insights
-callback is_providing_extra_http_endpoints() -> boolean().
-callback extra_http_endpoints() -> term().
%% Special consensus features - some of them are exposed via the above http endpoints
%% This API is meant to be used ONLY by the node operator
%% This will be used by at most one user and all requests go through the mining conductor
-callback client_request(term()) -> term().

%% -------------------------------------------------------------------
%% Deserialization of headers
%% -------------------------------------------------------------------
%% Consensus algorithms might change the block structure and use the extra field
%% Extra data might only be derived from the header itself and the extra field is
%% never saved to the DB as it would be redundant. The main idea behind recycling
%% the existing fields instead of changing the serialization is to ensure no client
%% tooling breaks - middleware can just connect to a HC/PoA/Dev enabled node and
%% just work without refactoring code on the client side
-callback extra_from_header(aec_headers:header()) -> map().

%% -------------------------------------------------------------------
%% Block insertion and state transitions
%% -------------------------------------------------------------------
%% Callbacks for building the db insertion ctx
-callback recent_cache_n() -> non_neg_integer().
-callback recent_cache_trim_key_header(aec_headers:header()) -> term().

-callback keyblocks_for_target_calc() -> non_neg_integer().
-callback keyblock_create_adjust_target(aec_blocks:block(), list(aec_headers:header())) ->
    {ok, aec_blocks:block()} | {error, term()}.

%% Pre conductor validation - filters blocks before passing it to the conductor
%% The idea is that "dev mode" can ensure that after it gets enabled no REAL block
%% can get inserted via the mining conductor. "dev mode" should avoid mutating the real
%% DB with invalid state - after restarting the node the DB should be usable again
%% The chain simulator in dev mode should preferably keep the state in ram or in a temporary
%% mnesia disk table
-callback dirty_validate_block_pre_conductor(aec_blocks:block()) -> ok | {error, any()}.
-callback dirty_validate_header_pre_conductor(aec_headers:header()) -> ok | {error, any()}.
-callback dirty_validate_key_hash_at_height(non_neg_integer(), aec_blocks:block_header_hash()) ->
    ok | {error, any()}.

%% This callback is called in dirty context before starting the DB transaction for block insertion
%% It's called only when the insertion context got properly created:
%%    - It's not a genesis block
%%    - The chain invariants were verified:
%%      * The block is not an orphan - prev block and prev key block are present and were validated
%%      * min(Height-GenesisHeight, recent_cache_n()) previous key headers are present in the DB
%%      * height is consecutive
%%      * prev_block points to the same generation as prev_key_block
%% Do not crash as it's called in dirty context
-callback dirty_validate_key_node_with_ctx(#chain_node{}, aec_blocks:micro_block(), #insertion_ctx{}) -> ok | {error, term()}.
-callback dirty_validate_micro_node_with_ctx(#chain_node{}, aec_blocks:micro_block(), #insertion_ctx{}) -> ok | {error, term()}.

%% Customized state transitions - in case of keyblocks the callbacks are called with pruned state trees
%% Those callbacks run in a DB context - to abort the execution please call aec_block_insertion:abort_state_transition(Reason)
%% Performs initial state transformation when the previous block used a different consensus algorithm
-callback state_pre_transform_key_node_consensus_switch(#chain_node{}, aec_trees:trees()) -> aec_trees:trees() | no_return().
%% State pre transformations on every keyblock
-callback state_pre_transform_key_node(#chain_node{}, aec_trees:trees()) -> aec_trees:trees() | no_return().
%% State pre transformations on every microblock
-callback state_pre_transform_micro_node(#chain_node{}, aec_trees:trees()) -> aec_trees:trees() | no_return().

%% Block rewards :)
-callback state_grant_reward(aec_keys:pubkey(), aec_trees:trees(), non_neg_integer()) -> aec_trees:trees() | no_return().

%% PoGF - called just before exiting the DB transaction and fully validating the node in question
-callback pogf_detected(aec_headers:key_header(), aec_headers:key_header()) -> ok.

%% Consensus modules might define their own genesis block
%% The initial state populated with presets for accounts/contracts can be mutated here
-callback genesis_transform_trees(aec_trees:trees(), consensus_config()) -> aec_trees:trees().
-callback genesis_raw_header() -> aec_headers:key_header().
-callback genesis_difficulty() -> key_difficulty().

%% -------------------------------------------------------------------
%% Block sealing
%% -------------------------------------------------------------------
%% Serializes the key header to the form expected for sealing
-callback key_header_for_sealing(aec_headers:header()) -> binary().
%% Validates the crypto seal on the given key block
-callback validate_key_header_seal(aec_headers:header(), non_neg_integer()) -> ok | {error, term()}.
%% Tries to generate a crypto seal on the given key block
%% In case it's impossible to generate a seal for the given header - this callback can stop the mining
%% conductor by returning stop_mining. After the seal got properly created this callback can decide whether to continue mining
-type conductor_action() :: stop_mining | continue_mining.
-callback generate_key_header_seal(binary(), aec_headers:header(), non_neg_integer(), term(), term()) -> {conductor_action(), {ok, term()}} | {conductor_action(), {error, term()}}.
%% Inserts the generated crypto seal in the block header
-callback set_key_block_seal(aec_blocks:block(), term()) -> term().
%% Gets the nonce used for sealing - please note that this field might be used for instance for a custom voting protocol
%% Some consensus modules might have already calculated the nonce before and stored it in the header
%% PoW usually will increase the nonce after an unsuccessful mining attempt
-callback nonce_for_sealing(aec_headers:header()) -> key_nonce().
-callback next_nonce_for_sealing(key_nonce(), term()) -> key_nonce().
-callback trim_sealing_nonce(integer(), term()) -> key_nonce().

%% Block target and difficulty
-callback default_target() -> key_target().
-callback assert_key_target_range(key_target()) -> ok | no_return().
-callback key_header_difficulty(aec_headers:key_header()) -> key_difficulty().

-optional_callbacks([stop/0, extra_http_endpoints/0]).

%% -------------------------------------------------------------------
%% Consensus utilities
%% -------------------------------------------------------------------

-spec calc_consensus() -> global_consensus_config().
calc_consensus() ->
    NetworkId = aec_governance:get_network_id(),
    lists:keysort(1, consensus_from_network_id(NetworkId)).

%% Consensus configuration
-spec check_env() -> ok.
check_env() ->
    ConsensusSpec = calc_consensus(),
    %% Check that the first consensus specification starts at 0
    case ConsensusSpec of
        [{0,_}|_] -> ok;
        _ -> error(first_consensus_does_not_activate_at_genesis)
    end,
    %% No duplicated specs
    ConsensusSpec = lists:usort(ConsensusSpec),
    %% We know the requested consensus algorithms
    case lists:filter(fun ({_,{undefined,_}}) -> true; (_) -> false end, ConsensusSpec) of
        [] -> ok;
        _ -> error(unknown_consensus)
    end,
    %% Some consensuses like dev mode can't be turned off
    Changeable = [M:can_be_turned_off() || {_,{M,_}} <- ConsensusSpec],
    case lists:filter(fun erlang:'not'/1, Changeable) of
        [] ->
            ok;
        [false] ->
            case lists:last(Changeable) of
                true ->
                    error(cannot_turn_off_consensus);
                false ->
                    ok
            end;
        _ ->
            error(cannot_turn_off_consensus)
    end,
    %% Assert particular consensus configs
    [M:assert_config(Config) || {_, {M, Config}} <- ConsensusSpec],
    ok.

-spec set_consensus() -> ok.
set_consensus() ->
    persistent_term:put({?MODULE, consensus}, calc_consensus()).

-spec get_consensus() -> global_consensus_config().
get_consensus() ->
    case persistent_term:get({?MODULE, consensus}, error) of
        error ->
            set_consensus(),
            get_consensus();
        Consensus ->
            Consensus
    end.

get_consensus_spec_at_height(Height) ->
    [{0,H}|R] = get_consensus(),
    consensus_at_height(H, R, Height).

%% This is a placeholder for later - the idea is that if at some point
%% the network decides to change the configuration variables which are
%% under consensus - for instance the micro block time then it should be easy to do so
%% without introducing a new "protocol" or hiring erlang devs
%% TODO: Gradually move away from aec_governance and rely on the consensus module and this configuration
-spec get_consensus_config_at_height(non_neg_integer()) -> consensus_config().
get_consensus_config_at_height(Height) ->
    {_, Config} = get_consensus_spec_at_height(Height),
    Config.

-spec get_consensus_module_at_height(non_neg_integer()) -> consensus_module().
get_consensus_module_at_height(Height) ->
    {Module, _} = get_consensus_spec_at_height(Height),
    Module.

-spec get_genesis_consensus_module() -> consensus_module().
get_genesis_consensus_module() ->
    get_consensus_module_at_height(0).

-spec get_genesis_consensus_config() -> consensus_config().
get_genesis_consensus_config() ->
    get_consensus_config_at_height(0).

-spec consensus_at_height({consensus_module(), consensus_config()}, global_consensus_config(), non_neg_integer()) -> {consensus_module(), consensus_config()}.
consensus_at_height(R, [], _) -> R;
consensus_at_height(R, [{H1,_}|_], H2) when H1>H2 -> R;
consensus_at_height(_, [{_,R}|T], H) -> consensus_at_height(R, T, H).

-spec consensus_from_network_id(binary()) -> global_consensus_config().
consensus_from_network_id(<<"ae_mainnet">>) ->
    [{0, {aec_consensus_bitcoin_ng, #{}}}];
consensus_from_network_id(<<"ae_uat">>) ->
    [{0, {aec_consensus_bitcoin_ng, #{}}}];
consensus_from_network_id(_) ->
    case aeu_env:env_or_user_map([<<"chain">>, <<"consensus">>], aecore, consensus, undefined) of
        undefined ->
            [{0, {aec_consensus_bitcoin_ng, #{}}}];
        M when is_map(M) ->
            Conf = maps:fold(fun(H, #{<<"name">> := ConsensusName} = V, Acc) ->
                              ConsensusConfig = maps:get(<<"config">>, V, #{}),
                              Acc#{binary_to_integer(H) => {consensus_module_from_name(ConsensusName), ConsensusConfig}}
                      end, #{}, M),
            maps:to_list(Conf)
    end.

%% Don't crash here as config validation is performed during node startup in the consensus module
-spec consensus_module_from_name(binary()) -> consensus_module().

-ifdef(TEST).
consensus_module_from_name(<<"pow_cuckoo">>) -> aec_consensus_bitcoin_ng;
consensus_module_from_name(<<"ct_tests">>) -> aec_consensus_common_tests;
consensus_module_from_name(<<"eunit_one">>) -> module_eunit_one;
consensus_module_from_name(<<"eunit_two">>) -> module_eunit_two;
consensus_module_from_name(<<"eunit_three">>) -> module_eunit_three;
consensus_module_from_name(_) -> undefined.
-else.
consensus_module_from_name(<<"pow_cuckoo">>) -> aec_consensus_bitcoin_ng;
consensus_module_from_name(_) -> undefined.
-endif.

calc_genesis_hash() ->
    Header = aec_block_genesis:genesis_header(),
    {ok, Hash} = aec_headers:hash_header(Header),
    Hash.

set_genesis_hash() ->
    persistent_term:put({?MODULE, genesis_hash}, calc_genesis_hash()).

get_genesis_hash() ->
    case persistent_term:get({?MODULE, genesis_hash}, error) of
        error ->
            set_genesis_hash(),
            get_genesis_hash();
        Consensus ->
            Consensus
    end.
