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

%% API
-export([ genesis_header/0,
          genesis_block_with_state/0,
          populated_trees/0 ]).

-export([genesis_difficulty/0]).

-export([beneficiary/0,
         height/0,
         miner/0,
         pow/0,
         prev_hash/0,
         target/0,
         time_in_msecs/0,
         version/0
        ]).

-ifdef(TEST).
-export([genesis_block_with_state/1]).
-endif.

-include("blocks.hrl").
-include_lib("apps/aecore/include/hard_forks.hrl").

-define(GENESIS_VERSION, ?ROMA_PROTOCOL_VSN).
-define(GENESIS_HEIGHT, 0).
-define(GENESIS_TIME, 0).


%% Since preset accounts are being loaded from a file - please use with caution
genesis_header() ->
    {B, _S} = genesis_block_with_state(),
    aec_blocks:to_header(B).

prev_hash() ->
    aec_governance:contributors_messages_hash().

prev_key_hash() ->
    <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>.

pow() ->
    no_value.

height() -> ?GENESIS_HEIGHT.

miner() -> <<0:?MINER_PUB_BYTES/unit:8>>.

beneficiary() -> <<0:?BENEFICIARY_PUB_BYTES/unit:8>>.

version() -> ?GENESIS_VERSION.

time_in_msecs() -> ?GENESIS_TIME.

-ifdef(TEST).
target() ->
   ?HIGHEST_TARGET_SCI.
-else.
target() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> 16#1F1F1F1F;
        _                -> ?HIGHEST_TARGET_SCI
    end.
-endif.

%% Returns the genesis block and the state trees.
%%
%% The current implementation of state trees causes a new Erlang term,
%% representing the initial state trees, to be allocated in the
%% heap memory of the calling process.
%%
%% Since preset accounts are being loaded from a file - please use with caution
genesis_block_with_state() ->
    genesis_block_with_state(#{preset_accounts => aec_genesis_block_settings:preset_accounts()}).

genesis_block_with_state(Map) ->
    Trees = populated_trees(Map),

    Block = aec_blocks:new_key(height(), prev_hash(), prev_key_hash(),
                               aec_trees:hash(Trees),
                               target(), 0, time_in_msecs(),
                               version(), miner(), beneficiary()),
    {Block, Trees}.

%% Returns state trees at genesis block.
%%
%% It includes preset accounts.
%%
%% It does not include reward for miner account.
populated_trees() ->
    populated_trees(#{preset_accounts => aec_genesis_block_settings:preset_accounts()}).

populated_trees(Map) ->
    PresetAccounts = maps:get(preset_accounts, Map),
    StateTrees = maps:get(state_tree, Map, aec_trees:new()),
    PopulatedAccountsTree =
        lists:foldl(fun({PubKey, Amount}, T) ->
                            Account = aec_accounts:new(PubKey, Amount),
                            aec_accounts_trees:enter(Account, T)
                    end, aec_trees:accounts(StateTrees), PresetAccounts),
    aec_trees:set_accounts(StateTrees, PopulatedAccountsTree).


%% Returns the difficulty of the genesis block meant to be used in the
%% computation of the chain difficulty.
genesis_difficulty() ->
    0. %% Genesis block is unmined.
