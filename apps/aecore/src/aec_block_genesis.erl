%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Genesis block definition.
%%%
%%% The genesis block does not follow the validation rules of the
%%% other blocks because:
%%% * Its state trees include preset accounts;
%%% * It does not contain a valid PoW (it is unmined);
%%%   * It implies genesis block cannot be validated PoW wise;
%%%   * Note: the miner account specified in the genesis block is
%%%     still rewarded as for the other blocks.
%%% * Its time is epoch i.e. much in the past;
%%%   * It implies the time difference between genesis block and first
%%%     block is very large - that may be considered abnormal for
%%%     successive blocks (e.g. between blocks 1 and 2 - with block 0
%%%     being genesis).
%%% * The value of the hash of the (nonexistent) previous block is
%%%   special i.e. all zeros.
%%%   * This means that validation function attempting to consider the
%%%     hashes in a block needs to have a special case for genesis.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_block_genesis).

%% Helpers for the consensus active at genesis - all other functions are just helpers for those two
-export([ genesis_populated_trees/0
        , genesis_populated_trees/1
        , genesis_raw_header/0 ]).

%% All functionality is implemented internally using the two functions listed above
-export([ genesis_header/0
        , genesis_block_with_state/0
        , populated_trees/0
        , genesis_difficulty/0
        , beneficiary/0
        , height/0
        , miner/0
        , pow/0
        , prev_hash/0
        , target/0
        , time_in_msecs/0
        , version/0
        ]).

-ifdef(TEST).
-export([ genesis_block_with_state/1 ]).
-endif.

%% Genesis block presets
-type populated_trees_options() :: #{ preset_accounts => [{aec_keys:pubkey(), non_neg_integer()}]
                                    , state_tree => aec_trees:trees() }.

%% Use with caution - the in ram state trees are allocated in the process heap
%% Presets are loaded from files
%% TODO: Optionally load presets from the environment
-spec genesis_populated_trees() -> aec_trees:trees().
genesis_populated_trees() ->
    genesis_populated_trees(#{}).

-spec genesis_populated_trees(populated_trees_options()) -> aec_trees:trees().
genesis_populated_trees(Options) ->
    Module = aec_consensus:get_genesis_consensus_module(),
    Config = aec_consensus:get_genesis_consensus_config(),
    InitialTrees = initial_populated_trees(get_presets(Options)),
    %% Consensus modules might apply additional transformations
    Module:genesis_transform_trees(InitialTrees, Config).

-spec initial_populated_trees(populated_trees_options()) -> aec_trees:trees().
initial_populated_trees(Options) ->
    PresetAccounts = maps:get(preset_accounts, Options),
    StateTrees = maps:get(state_tree, Options),
    PopulatedAccountsTree =
        lists:foldl(fun({PubKey, Amount}, T) ->
                            Account = aec_accounts:new(PubKey, Amount),
                            aec_accounts_trees:enter(Account, T)
                    end, aec_trees:accounts(StateTrees), PresetAccounts),
    aec_trees:set_accounts(StateTrees, PopulatedAccountsTree).

-spec get_presets(populated_trees_options()) -> populated_trees_options().
get_presets(Options) ->
    maps:map(fun (_, F) when is_function(F, 0) -> F();
                 (_, R) -> R
             end, maps:merge(default_presets(), Options)).

default_presets() ->
    %% TODO: Consult the environment and user config add presets for contracts
    #{ preset_accounts => fun() -> aec_fork_block_settings:genesis_accounts() end
     , state_tree => fun() -> aec_trees:new() end
     }.

-spec genesis_raw_header() -> aec_headers:key_header().
genesis_raw_header() ->
    Module = aec_consensus:get_genesis_consensus_module(),
    Module:genesis_raw_header().

%%-------------------------------------------
%% Consensus independent code
%%-------------------------------------------
genesis_header() ->
    Trees = genesis_populated_trees(),
    aec_headers:set_root_hash(genesis_raw_header(), aec_trees:hash(Trees)).

genesis_block_with_state() ->
    genesis_block_with_state(#{}).

genesis_block_with_state(Options) ->
    Trees = genesis_populated_trees(Options),
    Header = aec_headers:set_root_hash(genesis_raw_header(), aec_trees:hash(Trees)),
    {aec_blocks:new_key_from_header(Header), Trees}.

populated_trees()    -> genesis_populated_trees().
beneficiary()        -> aec_headers:beneficiary(genesis_raw_header()).
height()             -> aec_headers:height(genesis_raw_header()).
miner()              -> aec_headers:miner(genesis_raw_header()).
pow()                -> aec_headers:key_seal(genesis_raw_header()).
prev_hash()          -> aec_headers:prev_hash(genesis_raw_header()).
target()             -> aec_headers:target(genesis_raw_header()).
time_in_msecs()      -> aec_headers:time_in_msecs(genesis_raw_header()).
version()            -> aec_headers:version(genesis_raw_header()).

%% Returns the difficulty of the genesis block meant to be used in the
%% computation of the chain difficulty.
genesis_difficulty() ->
    Module = aec_consensus:get_genesis_consensus_module(),
    Module:genesis_difficulty().
