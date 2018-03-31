%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    CT test suite for AE State Channels on-chain transactions
%%% @end
%%%=============================================================================

-module(aesc_txs_SUITE).

%% common_test exports
-export([all/0,
         groups/0]).

%% test case exports
-export([create/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common test framework
%%%===================================================================

all() ->
    [{group, all_tests}].

groups() ->
    [
     {all_tests, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [create]
     }
    ].

%%%===================================================================
%%% Create
%%%===================================================================

create(Cfg) ->
    S = case proplists:get_value(state, Cfg) of
            undefined -> aesc_test_utils:new_state();
            State0    -> State0
        end,
    {PubKey1, S1} = aesc_test_utils:setup_new_account(S),
    {PubKey2, S2} = aesc_test_utils:setup_new_account(S1),
    PrivKey1 = aesc_test_utils:priv_key(PubKey1, S2),
    PrivKey2 = aesc_test_utils:priv_key(PubKey2, S2),

    %% Create Channel Create tx and apply it on trees
    Trees = aesc_test_utils:trees(S2),
    Height = 1,
    TxSpec = aesc_test_utils:create_tx_spec(PubKey1, PubKey2, S2),
    {ok, Tx} = aesc_create_tx:new(TxSpec),
    SignedTx = aetx_sign:sign(Tx, [PrivKey1, PrivKey2]),
    {ok, [SignedTx], Trees1} = aec_trees:apply_signed_txs([SignedTx], Trees, Height),
    S3 = aesc_test_utils:set_trees(Trees1, S2),

    %% Check channel created
    Trees2 = aesc_test_utils:trees(S3),
    ChannelId = aesc_channels:id(PubKey1, 1, PubKey2),
    {value, Ch} = aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees2)),
    PubKey1 = aesc_channels:initiator(Ch),
    PubKey2 = aesc_channels:participant(Ch),
    0       = aesc_channels:sequence_number(Ch),
    true    = aesc_channels:is_active(Ch),
    ok.
