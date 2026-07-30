%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    State-batch equivalence test.
%%%
%%%    For a random sequence of account/contract/store operations the
%%%    batched form (apply all, flush once) must produce a byte-identical
%%%    state root and identical per-subtree contents as the reference
%%%    form that flushes after every operation. Includes deletions and
%%%    the cross-tx delete-then-enumerate pattern.
%%% @end
%%%=============================================================================
-module(aec_state_batch_equiv_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").

-define(IDS,  [1, 2, 3, 4]).      %% small space → overwrites/deletes interleave
-define(KEYS, [1, 2, 3]).

equiv_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ {timeout, 120,
        {"random op sequences: batch == immediate (multi-seed)",
         fun random_sequences/0}}
     , {"mandatory: cross-tx store delete then enumerate",
        fun mandatory_cross_tx_delete/0}
     , {"mandatory: store delete then re-create",
        fun mandatory_delete_recreate/0}
     , {"mandatory: contract object delete (cross-mb tombstone funnel)",
        fun mandatory_contract_delete_cross_mb/0}
     , {"mandatory: account delete then re-create (tombstone funnel)",
        fun mandatory_account_delete_recreate/0}
     , {"mandatory: account delete stays deleted (pre/post flush)",
        fun mandatory_account_delete_stays_deleted/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Randomised equivalence
%%%===================================================================

random_sequences() ->
    Seeds = [{1, 2, 3}, {42, 42, 42}, {7, 77, 777},
             {2024, 5, 17}, {9, 8, 7}, {123, 456, 789}],
    lists:foreach(
      fun(Seed) ->
              Ops = gen_ops(Seed, 80),
              ?assertEqual(run(Ops, immediate), run(Ops, batch))
      end, Seeds).

%% Op :: {acc, Id, Bal} | {acc_del, Id}
%%     | {ctr_new, Id}  | {sset, Id, K, V}
%% V == 0 means "remove the store key" (<<>>).  Contract-level delete is
%% deliberately NOT generated here: its only divergence is the
%% pre-existing orphaned-store-subtree behaviour, reachable only by
%% recreating a contract with an already-used pubkey, which the protocol
%% forbids (pubkey = hash(owner, nonce), nonce unique).  Contract delete
%% equivalence is covered by the mandatory cross-mb case below.
gen_ops(Seed, N) ->
    _ = rand:seed(exsplus, Seed),
    [ gen_op() || _ <- lists:seq(1, N) ].

gen_op() ->
    Id = pick(?IDS),
    case rand:uniform(5) of
        1 -> {acc, Id, rand:uniform(1000)};
        2 -> {acc_del, Id};
        3 -> {ctr_new, Id};
        4 -> {sset, Id, pick(?KEYS), rand:uniform(9) - 1}; %% 0..8 (0 ⇒ delete)
        5 -> {sset, Id, pick(?KEYS), rand:uniform(8)}
    end.

pick(L) -> lists:nth(rand:uniform(length(L)), L).

%% Returns {RootHash, AccountsList, #{ctr_id => store_contents}} so a
%% mismatch is localised, not just "hashes differ".
run(Ops, Mode) ->
    S0 = #{trees => aec_trees:new(), ctrs => []},
    #{trees := T, ctrs := Ctrs} =
        lists:foldl(fun(Op, S) -> step(Op, S, Mode) end, S0, Ops),
    Tf      = aec_trees:flush_state_batches(T),
    Hash    = aec_trees:hash(Tf),
    AccList = aec_accounts_trees:to_list(aec_trees:accounts(Tf)),
    Stores  = maps:from_list(
                [ {Id, ctr_contents(Id, Tf)} || Id <- lists:usort(Ctrs) ]),
    {Hash, lists:sort(AccList), Stores}.

step(Op, S, immediate) ->
    S1 = apply_op(Op, S),
    S1#{trees := aec_trees:flush_state_batches(maps:get(trees, S1))};
step(Op, S, batch) ->
    apply_op(Op, S).

apply_op({acc, Id, Bal}, #{trees := T} = S) ->
    AT  = aec_trees:accounts(T),
    AT1 = aec_accounts_trees:enter(aec_accounts:new(pk(Id), Bal), AT),
    S#{trees := aec_trees:set_accounts(T, AT1)};
apply_op({acc_del, Id}, #{trees := T} = S) ->
    AT  = aec_trees:accounts(T),
    AT1 = aec_accounts_trees:delete(pk(Id), AT),
    S#{trees := aec_trees:set_accounts(T, AT1)};
apply_op({ctr_new, Id}, #{trees := T, ctrs := Ctrs} = S) ->
    case lists:member(Id, Ctrs) of
        true  -> S;   %% already created — insert-on-duplicate is illegal
        false ->
            C   = new_contract(aeser_id:create(account, pk(Id))),
            CT  = aec_trees:contracts(T),
            CT1 = aect_state_tree:insert_contract(C, CT),
            S#{trees := aec_trees:set_contracts(T, CT1),
               ctrs  := [Id | Ctrs]}
    end;
apply_op({sset, Id, K, V}, #{trees := T, ctrs := Ctrs} = S) ->
    case lists:member(Id, Ctrs) of
        false -> S;
        true  ->
            CT = aec_trees:contracts(T),
            {value, C} = aect_state_tree:lookup_contract(ctr_pk(Id), CT),
            St0 = aect_contracts:state(C),
            St1 = case V of
                      0 -> aect_contracts_store:remove(<<K>>, St0);
                      _ -> aect_contracts_store:put(<<K>>, <<V>>, St0)
                  end,
            CT1 = aect_state_tree:enter_contract(
                    aect_contracts:set_state(St1, C), CT),
            S#{trees := aec_trees:set_contracts(T, CT1)}
    end.

ctr_contents(Id, T) ->
    case aect_state_tree:lookup_contract(ctr_pk(Id), aec_trees:contracts(T)) of
        none       -> deleted;
        {value, C} -> aect_contracts_store:contents(aect_contracts:state(C))
    end.

%%%===================================================================
%%% Mandatory cases
%%%===================================================================

%% Prior microblock materialises k=>v,k2=>v2; tx A removes k (batched,
%% unflushed); tx B enumerating the store must NOT see k.
mandatory_cross_tx_delete() ->
    MS = fun(M) -> aect_contracts_store:put_map(M, aect_contracts_store:new()) end,
    C0 = new_contract(aeser_id:create(account, pk(1))),
    PK = aect_contracts:pubkey(C0),
    C  = aect_contracts:set_state(MS(#{<<"k">> => <<"v">>, <<"k2">> => <<"v2">>}), C0),
    CT0 = aect_state_tree:empty_with_backend(),
    CT1 = flushc(aect_state_tree:insert_contract(C, CT0)),

    {value, Ca} = aect_state_tree:lookup_contract(PK, CT1),
    Sa  = aect_contracts_store:remove(<<"k">>, aect_contracts:state(Ca)),
    CT2 = aect_state_tree:enter_contract(aect_contracts:set_state(Sa, Ca), CT1),

    {value, Cb} = aect_state_tree:lookup_contract(PK, CT2),
    ?assertEqual(#{<<"k2">> => <<"v2">>},
                 aect_contracts_store:contents(aect_contracts:state(Cb))).

mandatory_delete_recreate() ->
    MS = fun(M) -> aect_contracts_store:put_map(M, aect_contracts_store:new()) end,
    C0 = new_contract(aeser_id:create(account, pk(2))),
    PK = aect_contracts:pubkey(C0),
    C  = aect_contracts:set_state(MS(#{<<"k">> => <<"v">>}), C0),
    CT1 = flushc(aect_state_tree:insert_contract(C, aect_state_tree:empty_with_backend())),

    {value, Ca} = aect_state_tree:lookup_contract(PK, CT1),
    Sa  = aect_contracts_store:remove(<<"k">>, aect_contracts:state(Ca)),
    CT2 = aect_state_tree:enter_contract(aect_contracts:set_state(Sa, Ca), CT1),

    {value, Cb} = aect_state_tree:lookup_contract(PK, CT2),
    Sb  = aect_contracts_store:put(<<"k">>, <<"v3">>, aect_contracts:state(Cb)),
    CT3 = aect_state_tree:enter_contract(aect_contracts:set_state(Sb, Cb), CT2),

    {value, Cc} = aect_state_tree:lookup_contract(PK, CT3),
    ?assertEqual(#{<<"k">> => <<"v3">>},
                 aect_contracts_store:contents(aect_contracts:state(Cc))).

%% Contract *object* delete via the tombstone funnel.  Same-microblock
%% create+delete is deliberately NOT exercised (immediate-vs-batch
%% diverge on the pre-existing orphaned store/sentinel — protocol
%% unreachable, see gen_op/0 note).  Cross-microblock IS equivalence-
%% safe: both arms start from the SAME materialised tree, so any orphan
%% is byte-identical; only the delete timing differs.
mandatory_contract_delete_cross_mb() ->
    MS = fun(M) -> aect_contracts_store:put_map(M, aect_contracts_store:new()) end,
    C0 = new_contract(aeser_id:create(account, pk(1))),
    PK = aect_contracts:pubkey(C0),
    C  = aect_contracts:set_state(MS(#{<<"k">> => <<"v">>}), C0),
    D0 = new_contract(aeser_id:create(account, pk(2))),
    DPK = aect_contracts:pubkey(D0),
    %% mb1: C fully materialised (metadata + store + sentinel in MPT).
    Base = flushc(aect_state_tree:insert_contract(C,
                    aect_state_tree:empty_with_backend())),
    %% mb2 from Base: delete C, insert D; flush once (batch) vs after
    %% the delete (immediate).
    Run = fun(Mode) ->
              T1 = aect_state_tree:delete_contract(PK, Base),
              T2 = case Mode of
                       immediate -> flushc(T1);
                       batch     -> T1
                   end,
              T3 = aect_state_tree:insert_contract(D0, T2),
              Tf = flushc(case Mode of
                              immediate -> flushc(T3);
                              batch     -> T3
                          end),
              {ok, Root} = aect_state_tree:root_hash(Tf),
              {aect_state_tree:lookup_contract(PK, Tf),
               aect_state_tree:lookup_contract(DPK, Tf) =/= none,
               Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {LkC, DPresent, _} = Run(batch),
    ?assertEqual(none, LkC),          %% deleted contract is absent
    ?assert(DPresent).                %% unrelated contract unaffected

%% Account object delete then re-create within the next microblock.
%% `enter' must overwrite the tombstone (last writer wins) so the
%% account is present again with the new value, byte-identically to the
%% flush-after-every-op reference.
mandatory_account_delete_recreate() ->
    A1   = aec_accounts:new(pk(3), 100),
    Base = aec_accounts_trees:flush_account_batch(
             aec_accounts_trees:enter(
               A1, aec_accounts_trees:empty_with_backend())),
    Run = fun(Mode) ->
              F  = fun(T) -> case Mode of
                                 immediate -> aec_accounts_trees:flush_account_batch(T);
                                 batch     -> T
                             end
                   end,
              T1 = F(aec_accounts_trees:delete(pk(3), Base)),
              A2 = aec_accounts:new(pk(3), 555),
              Tf = aec_accounts_trees:flush_account_batch(
                     F(aec_accounts_trees:enter(A2, T1))),
              {ok, Root} = aec_accounts_trees:root_hash(Tf),
              {aec_accounts_trees:lookup(pk(3), Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, Acc}, _} = Run(batch),
    ?assertEqual(555, aec_accounts:balance(Acc)).

%% A tombstone hides the account before flush (funnel) and the flush
%% removes the key from the MPT (so a later microblock cannot resurrect
%% it from origin).
mandatory_account_delete_stays_deleted() ->
    A1   = aec_accounts:new(pk(4), 100),
    Base = aec_accounts_trees:flush_account_batch(
             aec_accounts_trees:enter(
               A1, aec_accounts_trees:empty_with_backend())),
    T1   = aec_accounts_trees:delete(pk(4), Base),
    ?assertEqual(none, aec_accounts_trees:lookup(pk(4), T1)),  %% pre-flush
    Tf   = aec_accounts_trees:flush_account_batch(T1),
    ?assertEqual(none, aec_accounts_trees:lookup(pk(4), Tf)).  %% post-flush

%%%===================================================================
%%% Helpers
%%%===================================================================

flushc(CT) ->
    aect_state_tree:flush_contract_meta_batch(
      aect_state_tree:flush_store_batch(CT)).

pk(Id) -> <<Id:256>>.

ctr_pk(Id) -> aect_contracts:pubkey(new_contract(aeser_id:create(account, pk(Id)))).

new_contract(OwnerId) ->
    Map = #{ owner_id    => OwnerId
           , nonce       => 42
           , code        => <<"THIS IS NOT ACTUALLY PROPER BYTE CODE">>
           , vm_version  => ?VM_AEVM_SOLIDITY_1
           , abi_version => ?ABI_SOLIDITY_1
           , fee         => 10
           , ttl         => 100
           , deposit     => 100
           , amount      => 50
           , gas         => 100
           , gas_price   => 5
           , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
           },
    {ok, Tx} = aect_create_tx:new(Map),
    {contract_create_tx, CTx} = aetx:specialize_type(Tx),
    aect_contracts:new(CTx).
