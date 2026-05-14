-module(aec_accounts_trees_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% M-1: aec_accounts_trees:tree() opaque type audit
%%%===================================================================

%% Verify that aec_accounts_trees:tree() is an opaque record wrapper around
%% aeu_mtrees, not a plain mtree.  Callers must use only the public API.
tree_is_opaque_record_test() ->
    Tree = aec_accounts_trees:empty(),
    %% The value must be an #accounts_tree{} record, not a raw aeu_mtrees tuple.
    %% Records are tuples whose first element is the record tag atom.
    ?assert(is_tuple(Tree)),
    ?assertEqual(accounts_tree, element(1, Tree)),
    %% All public operations must work on the opaque value.
    PK  = <<"_______________k1_______________">>,
    Acc = aec_accounts:new(PK, 100),
    Tree1 = aec_accounts_trees:enter(Acc, Tree),
    ?assertEqual({value, Acc}, aec_accounts_trees:lookup(PK, Tree1)).

smoke_test() ->
    T0 = aec_accounts_trees:empty(),
    {error, empty} = aec_accounts_trees:root_hash(T0),

    A1 = aec_accounts:new(<<"_______________k1_______________">>, 10),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T1)),
    {ok, H1} = aec_accounts_trees:root_hash(T1),

    A2 = aec_accounts:new(<<"_______________k2_______________">>, 20),
    T2 = aec_accounts_trees:enter(A2, T1),
    ?assertEqual({value, A1},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A1), T2)),
    ?assertEqual({value, A2},
                 aec_accounts_trees:lookup(aec_accounts:pubkey(A2), T2)),
    {ok, H2} = aec_accounts_trees:root_hash(T2),

    %% Assert root hash summarizes content.
    ?assertNotEqual(H1, H2),
    ok.

lookup_test() ->
    K1 = <<"_______________k1_______________">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"_______________k2_______________">>,
    T0 = aec_accounts_trees:empty(),
    ?assertEqual(none, aec_accounts_trees:lookup(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(K1, T1)),
    ?assertEqual(none, aec_accounts_trees:lookup(K2, T1)),
    ok.

get_test() ->
    K1 = <<"_______________k1_______________">>,
    A1 = aec_accounts:new(K1, 10),
    K2 = <<"_______________k2_______________">>,
    T0 = aec_accounts_trees:empty(),
    ?assertException(error, {not_present, K1}, aec_accounts_trees:get(K1, T0)),
    T1 = aec_accounts_trees:enter(A1, T0),
    ?assertEqual(A1, aec_accounts_trees:get(K1, T1)),
    ?assertException(error, {not_present, K2}, aec_accounts_trees:get(K2, T1)),
    ok.

get_all_accounts_balances_test() ->
    T0 = aec_accounts_trees:empty(),

    A1 = aec_accounts:new(<<"_______________k1_______________">>, 11),
    A2 = aec_accounts:new(<<"_______________k2_______________">>, 13),

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),

    Expected = [{<<"_______________k2_______________">>, 13},
                {<<"_______________k1_______________">>, 11}],
    Actual   = aec_accounts_trees:get_all_accounts_balances(T2),
    ?assertEqual(lists:sort(Actual), lists:sort(Expected)).

account_for_locking_test() ->
    HolderPubKey = aec_governance:locked_coins_holder_account(),
    T0 = aec_accounts_trees:empty(),

    %% not present in empty tree
    ?assertException(error, {not_present, HolderPubKey}, aec_accounts_trees:get(HolderPubKey, T0)),

    GetHolderBalance =
        fun(Tree) ->
            Acc = aec_accounts_trees:get(HolderPubKey, Tree),
            _Bal = aec_accounts:balance(Acc)
        end,

    Amt1 = 1,
    Amt2 = 2,
    Amt3 = 3,

    %% adding coins for the first time creates the account
    T1 = aec_accounts_trees:lock_coins(Amt1, T0),
    Bal1 = GetHolderBalance(T1),
    ?assertEqual(Bal1, Amt1),

    %% adding more tokens increments locked coins' amount
    T2 = aec_accounts_trees:lock_coins(Amt2, T1),
    Bal2 = GetHolderBalance(T2),
    ?assertEqual(Bal2, Amt1 + Amt2),

    %% adding more tokens increments locked coins' amount even more
    T3 = aec_accounts_trees:lock_coins(Amt3, T2),
    Bal3 = GetHolderBalance(T3),
    ?assertEqual(Bal3, Amt1 + Amt2 + Amt3),
    ok.

%%%===================================================================
%%% Phase 4 — account batch tests
%%%===================================================================

%% enter defers the MPT write; lookup must find the account via the batch.
batch_lookup_after_enter_test() ->
    T0 = aec_accounts_trees:empty(),
    PK = <<"_______________k1_______________">>,
    A  = aec_accounts:new(PK, 42),
    T1 = aec_accounts_trees:enter(A, T0),
    ?assertEqual({value, A}, aec_accounts_trees:lookup(PK, T1)),
    %% Flushing makes the account visible from the MPT as well.
    T2 = aec_accounts_trees:flush_account_batch(T1),
    ?assertEqual({value, A}, aec_accounts_trees:lookup(PK, T2)),
    {ok, _} = aec_accounts_trees:root_hash(T2).

%% Multiple enters for the same pubkey deduplicate: only the latest state is
%% written to the MPT when the batch is flushed.
batch_dedup_test() ->
    T0 = aec_accounts_trees:empty(),
    PK = <<"_______________k1_______________">>,
    A1 = aec_accounts:new(PK, 10),
    A2 = aec_accounts:new(PK, 20),

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),

    %% Batch must hold the latest version.
    ?assertEqual({value, A2}, aec_accounts_trees:lookup(PK, T2)),

    %% After flush, MPT must hold the latest version.
    T3 = aec_accounts_trees:flush_account_batch(T2),
    ?assertEqual({value, A2}, aec_accounts_trees:lookup(PK, T3)).

%% Root hash after batched operations must equal root hash of an equivalent
%% tree built with no intermediate updates.  This is the core correctness invariant.
batch_root_hash_equivalence_test() ->
    PK1 = <<"_______________k1_______________">>,
    PK2 = <<"_______________k2_______________">>,
    A1  = aec_accounts:new(PK1, 10),
    A2  = aec_accounts:new(PK2, 20),
    A1u = aec_accounts:new(PK1, 99),

    %% Tree A: two enters for PK1 in one batch (dedup), plus PK2.
    TA = aec_accounts_trees:flush_account_batch(
           aec_accounts_trees:enter(A2,
             aec_accounts_trees:enter(A1u,
               aec_accounts_trees:enter(A1,
                 aec_accounts_trees:empty())))),

    %% Tree B: single enter for each key — already the final state.
    TB = aec_accounts_trees:flush_account_batch(
           aec_accounts_trees:enter(A2,
             aec_accounts_trees:enter(A1u,
               aec_accounts_trees:empty()))),

    {ok, HA} = aec_accounts_trees:root_hash(TA),
    {ok, HB} = aec_accounts_trees:root_hash(TB),
    ?assertEqual(HA, HB).

%% delete must remove the account from the batch AND prevent it from
%% appearing in the MPT after flush.
batch_delete_clears_batch_test() ->
    T0 = aec_accounts_trees:empty(),
    PK = <<"_______________k1_______________">>,
    A  = aec_accounts:new(PK, 42),

    T1 = aec_accounts_trees:enter(A, T0),
    ?assertEqual({value, A}, aec_accounts_trees:lookup(PK, T1)),

    T2 = aec_accounts_trees:delete(PK, T1),
    %% Must be gone from the batch immediately.
    ?assertEqual(none, aec_accounts_trees:lookup(PK, T2)),

    %% Must also not appear in the MPT after flush.
    T3 = aec_accounts_trees:flush_account_batch(T2),
    ?assertEqual(none, aec_accounts_trees:lookup(PK, T3)).

%% flush_account_batch/1 is idempotent: flushing an already-empty batch is a no-op.
batch_flush_idempotent_test() ->
    T0 = aec_accounts_trees:empty(),
    PK = <<"_______________k1_______________">>,
    A  = aec_accounts:new(PK, 42),

    T1 = aec_accounts_trees:flush_account_batch(
           aec_accounts_trees:enter(A, T0)),
    {ok, H1} = aec_accounts_trees:root_hash(T1),

    %% Second flush of an already-empty batch must not change the root hash.
    T2 = aec_accounts_trees:flush_account_batch(T1),
    {ok, H2} = aec_accounts_trees:root_hash(T2),
    ?assertEqual(H1, H2).

%% Multiple accounts in the batch are all written at flush time.
batch_multi_account_flush_test() ->
    T0  = aec_accounts_trees:empty(),
    PK1 = <<"_______________k1_______________">>,
    PK2 = <<"_______________k2_______________">>,
    PK3 = <<"_______________k3_______________">>,
    A1  = aec_accounts:new(PK1, 10),
    A2  = aec_accounts:new(PK2, 20),
    A3  = aec_accounts:new(PK3, 30),

    T1 = aec_accounts_trees:enter(A1, T0),
    T2 = aec_accounts_trees:enter(A2, T1),
    T3 = aec_accounts_trees:enter(A3, T2),

    %% All three visible from batch (no flush yet).
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(PK1, T3)),
    ?assertEqual({value, A2}, aec_accounts_trees:lookup(PK2, T3)),
    ?assertEqual({value, A3}, aec_accounts_trees:lookup(PK3, T3)),

    %% After flush, all three visible from MPT.
    T4 = aec_accounts_trees:flush_account_batch(T3),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(PK1, T4)),
    ?assertEqual({value, A2}, aec_accounts_trees:lookup(PK2, T4)),
    ?assertEqual({value, A3}, aec_accounts_trees:lookup(PK3, T4)).

%% Deleting a batch-only account (entered but not yet flushed to the MPT)
%% must leave the MPT state identical to the state before the enter.
%% This confirms that aeu_mtrees:delete on a missing key is a safe no-op
%% when the account never reached the MPT.
delete_batch_only_account_test() ->
    %% Establish a baseline tree with one account already flushed to the MPT.
    PK0   = <<"_______________baseline_________">>,
    A0    = aec_accounts:new(PK0, 1),
    TBase = aec_accounts_trees:flush_account_batch(
              aec_accounts_trees:enter(A0, aec_accounts_trees:empty())),
    {ok, BaseHash} = aec_accounts_trees:root_hash(TBase),

    %% Enter a second account into the batch (not yet in the MPT).
    PK1 = <<"_______________batchonly________">>,
    A1  = aec_accounts:new(PK1, 42),
    T1  = aec_accounts_trees:enter(A1, TBase),
    ?assertEqual({value, A1}, aec_accounts_trees:lookup(PK1, T1)),

    %% Delete the batch-only account before flush.
    T2  = aec_accounts_trees:delete(PK1, T1),
    ?assertEqual(none, aec_accounts_trees:lookup(PK1, T2)),

    %% Root hash must equal the baseline — batch-only account left no MPT trace.
    {ok, FinalHash} = aec_accounts_trees:root_hash(T2),
    ?assertEqual(BaseHash, FinalHash).

% channels' rely on accounts with a dict backend being reproducible with
% only the latest state
trunc_test() ->
    T0 = aec_accounts_trees:empty(),

    K1 = <<"_______________k1_______________">>,
    K2 = <<"_______________k2_______________">>,

    A10 = aec_accounts:new(K1, 11),
    A11 = aec_accounts:new(K1, 5),
    A2  = aec_accounts:new(K2, 13),

    T1  = aec_accounts_trees:enter(A10, T0),
    T2  = aec_accounts_trees:enter(A2, T1),
    T30 = aec_accounts_trees:enter(A11, T2),
    {ok, T3Hash} = aec_accounts_trees:root_hash(T30),

    CleanT1 = aec_accounts_trees:enter(A11, T0),
    CleanT2 = aec_accounts_trees:enter(A2, CleanT1),
    {ok, CleanT2Hash} = aec_accounts_trees:root_hash(CleanT2),
    ?assertEqual(T3Hash, CleanT2Hash),

    T11  = aec_accounts_trees:delete(K2, T2),
    {ok, T11Hash} = aec_accounts_trees:root_hash(T11),
    {ok, T1Hash} = aec_accounts_trees:root_hash(T1),
    ?assertEqual(T1Hash, T11Hash).

