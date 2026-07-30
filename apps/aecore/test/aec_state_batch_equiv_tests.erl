%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc Batch-vs-immediate state equivalence: for any op sequence, flushing
%%%    once must yield the same state root and per-subtree contents as flushing
%%%    after every op, across all 9 batched kinds.
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
        {"random op sequences: batch == immediate (multi-seed, all 9 kinds)",
         fun random_sequences/0}}
     , {timeout, 300,
        {"random op sequences: many seeds with shrink-on-failure",
         fun random_sequences_many_seeds/0}}
     , {timeout, 60,
        {"order-permutation: disjoint-key writes, any order, identical root",
         fun order_permutation/0}}
     , {"cross-tx store delete then enumerate",
        fun mandatory_cross_tx_delete/0}
     , {"store delete then re-create",
        fun mandatory_delete_recreate/0}
     , {"contract object delete (cross-mb tombstone funnel)",
        fun mandatory_contract_delete_cross_mb/0}
     , {"account delete then re-create (tombstone funnel)",
        fun mandatory_account_delete_recreate/0}
     , {"account delete stays deleted (pre/post flush)",
        fun mandatory_account_delete_stays_deleted/0}
     , {"call — two distinct calls coexist (insert-only kind)",
        fun mandatory_call_two_calls_coexist/0}
     , {"auth_call — create then update in place (same key)",
        fun mandatory_auth_call_update/0}
     , {"oracle — create then update (query fee)",
        fun mandatory_oracle_update/0}
     , {"oracle_query — create then respond (update)",
        fun mandatory_oracle_query_response/0}
     , {"name — delete then re-create",
        fun mandatory_name_delete_recreate/0}
     , {"name — tombstone hides pre-flush, gone post-flush",
        fun mandatory_name_tombstone_then_read/0}
     , {"get_name/2 honours a same-batch tombstone",
        fun mandatory_get_name_tombstone_blindness/0}
     , {"commitment — delete then re-create",
        fun mandatory_commitment_delete_recreate/0}
     , {"commitment delete-then-recreate — ns_cache_hash equivalence",
        fun mandatory_commitment_delete_recreate_cache_hash_equiv/0}
     , {"commitment — tombstone hides pre-flush, gone post-flush",
        fun mandatory_commitment_tombstone_then_read/0}
     , {"name_auction — update (bid raise / extend)",
        fun mandatory_name_auction_update/0}
     , {"name_auction — delete then re-create",
        fun mandatory_name_auction_delete_recreate/0}
     , {"name_auction — tombstone hides pre-flush, gone post-flush",
        fun mandatory_name_auction_tombstone_then_read/0}
     ]}.

setup() ->
    ok = aec_test_utils:start_chain_db(),
    ok.

teardown(_) ->
    ok = aec_test_utils:stop_chain_db(),
    ok.

%%%===================================================================
%%% Randomised equivalence — all 9 batched kinds
%%%===================================================================

random_sequences() ->
    Seeds = [{1, 2, 3}, {42, 42, 42}, {7, 77, 777},
             {2024, 5, 17}, {9, 8, 7}, {123, 456, 789}],
    lists:foreach(
      fun(Seed) ->
              Ops = gen_ops(Seed, 120),
              ?assertEqual(run(Ops, immediate), run(Ops, batch))
      end, Seeds).

%% On any divergence, shrink the failing op list to a minimal reproducer
%% and fail with the exact sequence.
-define(MANY_SEEDS, 80).
-define(MANY_SEEDS_OPS, 70).

random_sequences_many_seeds() ->
    Failures =
        lists:filtermap(
          fun(N) ->
                  Seed = {N, N * 7919 + 1, N * 104729 + 5},
                  Ops = gen_ops(Seed, ?MANY_SEEDS_OPS),
                  case run(Ops, immediate) =:= run(Ops, batch) of
                      true  -> false;
                      false -> {true, {N, Seed, Ops}}
                  end
          end, lists:seq(1, ?MANY_SEEDS)),
    case Failures of
        [] ->
            ok;
        [{N, Seed, Ops} | Rest] ->
            Min = shrink(Ops),
            ImmR = run(Min, immediate),
            BatchR = run(Min, batch),
            ?debugFmt(
               "~n~n=== CRITICAL: batch/immediate DIVERGENCE ===~n"
               "seed index=~p seed=~p (~p other seed(s) also diverged)~n"
               "minimal reproducing op sequence (~p ops):~n~p~n"
               "immediate result: ~p~n"
               "batch    result: ~p~n",
               [N, Seed, length(Rest), length(Min), Min, ImmR, BatchR]),
            ?assertEqual(ImmR, BatchR)
    end.

%% Greedy shrink: drop one op at a time while the divergence still reproduces.
shrink(Ops) ->
    case try_remove_one(Ops) of
        {ok, Smaller} -> shrink(Smaller);
        none -> Ops
    end.

try_remove_one(Ops) ->
    try_remove_one(Ops, lists:seq(1, length(Ops))).

try_remove_one(_Ops, []) ->
    none;
try_remove_one(Ops, [I | Rest]) ->
    Candidate = remove_nth(I, Ops),
    case Candidate =/= [] andalso still_diverges(Candidate) of
        true  -> {ok, Candidate};
        false -> try_remove_one(Ops, Rest)
    end.

remove_nth(N, L) ->
    {Pre, [_ | Post]} = lists:split(N - 1, L),
    Pre ++ Post.

still_diverges(Ops) ->
    run(Ops, immediate) =/= run(Ops, batch).

%% Contract-level delete is not generated: its only divergence needs
%% recreating a contract with an already-used pubkey, which the protocol
%% forbids; contract delete is covered by the cross-microblock case below.
%% `commitment_new' is likewise never generated for an already-present Id:
%% a genuine, non-deleted same-key double-create is the duplicate-insert
%% case, covered separately.
gen_ops(Seed, N) ->
    _ = rand:seed(exsplus, Seed),
    [ gen_op() || _ <- lists:seq(1, N) ].

gen_op() ->
    Id = pick(?IDS),
    case rand:uniform(20) of
        1  -> {acc, Id, rand:uniform(1000)};
        2  -> {acc_del, Id};
        3  -> {ctr_new, Id};
        4  -> {sset, Id, pick(?KEYS), rand:uniform(9) - 1}; %% 0..8 (0 ⇒ delete)
        5  -> {sset, Id, pick(?KEYS), rand:uniform(8)};
        6  -> {call_new, Id};
        7  -> {auth_new, Id};
        8  -> {auth_upd, Id, rand:uniform(100000)};
        9  -> {oracle_new, Id, rand:uniform(1000)};
        10 -> {oracle_upd, Id, rand:uniform(1000)};
        11 -> {query_new, Id, rand:uniform(100)};
        12 -> {query_upd, Id, rand:uniform(2) - 1};
        13 -> {name_new, Id, 400000 + rand:uniform(100000)};
        14 -> {name_upd, Id, 400000 + rand:uniform(100000)};
        15 -> {name_del, Id};
        16 -> {commitment_new, Id, rand:uniform(500)};
        17 -> {commitment_del, Id};
        18 -> {auction_new, Id, rand:uniform(10000)};
        19 -> {auction_upd, Id, rand:uniform(10000)};
        20 -> {auction_del, Id}
    end.

pick(L) -> lists:nth(rand:uniform(length(L)), L).

%% Returns a big comparable tuple so a mismatch is localised to a
%% specific kind, not just "hashes differ".
run(Ops, Mode) ->
    S0 = #{ trees    => aec_trees:new()
          , ctrs     => []
          , call_n   => #{}
          , auth     => #{}
          , oracles  => []
          , queries  => #{}
          , names    => []
          , commits  => []
          , auctions => []
          },
    FinalS = #{trees := T} = lists:foldl(fun(Op, S) -> step(Op, S, Mode) end, S0, Ops),
    Tf      = aec_trees:flush_state_batches(T),
    Hash    = aec_trees:hash(Tf),
    AccList = aec_accounts_trees:to_list(aec_trees:accounts(Tf)),
    Stores  = maps:from_list(
                [ {Id, ctr_contents(Id, Tf)} || Id <- lists:usort(maps:get(ctrs, FinalS)) ]),
    CallsList = lists:sort(aect_call_state_tree:to_list(aec_trees:calls(Tf))),
    OraclesMap = maps:from_list(
                   [ {Id, oracle_contents(Id, Tf)} || Id <- lists:usort(maps:get(oracles, FinalS)) ]),
    QueriesMap = maps:from_list(
                   [ {Id, query_contents(Id, FinalS, Tf)} || Id <- maps:keys(maps:get(queries, FinalS)) ]),
    NamesMap = maps:from_list(
                 [ {Id, name_contents(Id, Tf)} || Id <- lists:usort(maps:get(names, FinalS)) ]),
    CommitsMap = maps:from_list(
                   [ {Id, commitment_contents(Id, Tf)} || Id <- lists:usort(maps:get(commits, FinalS)) ]),
    AuctionsMap = maps:from_list(
                    [ {Id, auction_contents(Id, Tf)} || Id <- lists:usort(maps:get(auctions, FinalS)) ]),
    { Hash, lists:sort(AccList), Stores, CallsList
    , OraclesMap, QueriesMap, NamesMap, CommitsMap, AuctionsMap
    }.

step(Op, S, immediate) ->
    S1 = apply_op(Op, S),
    S1#{trees := aec_trees:flush_state_batches(maps:get(trees, S1))};
step(Op, S, batch) ->
    apply_op(Op, S).

%%%===================================================================
%%% Op application
%%%===================================================================

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
    end;
%% call: insert-only. A fresh per-Id nonce makes every key genuinely new,
%% so this is never a duplicate insert.
apply_op({call_new, Id}, #{trees := T, call_n := CN} = S) ->
    N = maps:get(Id, CN, 0) + 1,
    CallerId = aeser_id:create(account, call_caller_pk(Id)),
    CtId     = aeser_id:create(contract, call_ct_pk(Id)),
    Call = aect_call:new(CallerId, N, CtId, 1, 1000000),
    CT1 = aect_call_state_tree:insert_call(Call, aec_trees:calls(T)),
    S#{trees := aec_trees:set_calls(T, CT1), call_n := CN#{Id => N}};
%% auth_call: create + update-in-place on the same key, exercising `enter'.
apply_op({auth_new, Id}, #{trees := T, auth := A} = S) ->
    case maps:is_key(Id, A) of
        true -> S;
        false ->
            CallerId = aeser_id:create(account, auth_caller_pk(Id)),
            CtId     = aeser_id:create(contract, auth_ct_pk(Id)),
            N = 1,
            Call = aect_call:new(CallerId, N, CtId, 1, 1000000),
            CT1 = aect_call_state_tree:enter_auth_call(Call, aec_trees:calls(T)),
            S#{trees := aec_trees:set_calls(T, CT1),
               auth  := A#{Id => {CallerId, CtId, N}}}
    end;
apply_op({auth_upd, Id, GasUsed}, #{trees := T, auth := A} = S) ->
    case maps:find(Id, A) of
        error -> S;
        {ok, {CallerId, CtId, N}} ->
            Call0 = aect_call:new(CallerId, N, CtId, 1, 1000000),
            Call1 = aect_call:set_gas_used(GasUsed, Call0),
            CT1 = aect_call_state_tree:enter_auth_call(Call1, aec_trees:calls(T)),
            S#{trees := aec_trees:set_calls(T, CT1)}
    end;
%% oracle: create + update, always `enter'.
apply_op({oracle_new, Id, Fee}, #{trees := T, oracles := Os} = S) ->
    case lists:member(Id, Os) of
        true -> S;
        false ->
            O = aeo_oracles:new(oracle_pk(Id), <<"q">>, <<"r">>, Fee, 1000000000, 1),
            OT1 = aeo_state_tree:enter_oracle(O, aec_trees:oracles(T)),
            S#{trees := aec_trees:set_oracles(T, OT1), oracles := [Id | Os]}
    end;
apply_op({oracle_upd, Id, Fee}, #{trees := T, oracles := Os} = S) ->
    case lists:member(Id, Os) of
        false -> S;
        true ->
            OT = aec_trees:oracles(T),
            {value, O0} = aeo_state_tree:lookup_oracle(oracle_pk(Id), OT),
            O1 = aeo_oracles:set_query_fee(Fee, O0),
            OT1 = aeo_state_tree:enter_oracle(O1, OT),
            S#{trees := aec_trees:set_oracles(T, OT1)}
    end;
%% oracle_query: create then respond, same key. `query_new' only fires once
%% its oracle exists — a causal ordering dependency, so it is excluded from
%% the disjoint order-permutation set (`disjoint_create_ops/0').
apply_op({query_new, Id, Fee}, #{trees := T, oracles := Os, queries := Qs} = S) ->
    case lists:member(Id, Os) andalso not maps:is_key(Id, Qs) of
        true ->
            OPK = oracle_pk(Id),
            SPK = query_sender_pk(Id),
            Q = aeo_query:new(OPK, SPK, 1, <<"q">>, Fee, 1000000000, {delta, 100}),
            OT1 = aeo_state_tree:insert_query(Q, aec_trees:oracles(T)),
            S#{trees := aec_trees:set_oracles(T, OT1),
               queries := Qs#{Id => {OPK, SPK, 1}}};
        false -> S
    end;
apply_op({query_upd, Id, RespFlag}, #{trees := T, queries := Qs} = S) ->
    case maps:find(Id, Qs) of
        error -> S;
        {ok, {OPK, SPK, N}} ->
            OT = aec_trees:oracles(T),
            QId = aeo_query:id(SPK, N, OPK),
            {value, Q0} = aeo_state_tree:lookup_query(OPK, QId, OT),
            Q1 = aeo_query:add_response(1, <<RespFlag>>, Q0),
            OT1 = aeo_state_tree:enter_query(Q1, OT),
            S#{trees := aec_trees:set_oracles(T, OT1)}
    end;
%% name: create / update / delete, always `enter'/`delete'.
apply_op({name_new, Id, TTL}, #{trees := T, names := Ns} = S) ->
    case lists:member(Id, Ns) of
        true -> S;
        false ->
            Name = aens_names:new(nh(name, Id), pk(Id), TTL),
            NT1 = aens_state_tree:enter_name(Name, aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), names := [Id | Ns]}
    end;
apply_op({name_upd, Id, TTL}, #{trees := T, names := Ns} = S) ->
    case lists:member(Id, Ns) of
        false -> S;
        true ->
            NT = aec_trees:ns(T),
            {value, N0} = aens_state_tree:lookup_name(nh(name, Id), NT),
            N1 = aens_names:update(N0, TTL, 0, []),
            NT1 = aens_state_tree:enter_name(N1, NT),
            S#{trees := aec_trees:set_ns(T, NT1)}
    end;
apply_op({name_del, Id}, #{trees := T, names := Ns} = S) ->
    case lists:member(Id, Ns) of
        false -> S;
        true ->
            NT1 = aens_state_tree:delete_name(nh(name, Id), aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), names := lists:delete(Id, Ns)}
    end;
%% commitment: create / delete only; re-entering a live commitment key is
%% the duplicate-insert case, covered separately (see gen_ops/2).
apply_op({commitment_new, Id, DeltaTTL}, #{trees := T, commits := Cs} = S) ->
    case lists:member(Id, Cs) of
        true -> S;
        false ->
            CId = aeser_id:create(commitment, nh(commitment, Id)),
            OwnerId = aeser_id:create(account, pk(Id)),
            C = aens_commitments:new(CId, OwnerId, DeltaTTL, 1),
            NT1 = aens_state_tree:enter_commitment(C, aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), commits := [Id | Cs]}
    end;
apply_op({commitment_del, Id}, #{trees := T, commits := Cs} = S) ->
    case lists:member(Id, Cs) of
        false -> S;
        true ->
            NT1 = aens_state_tree:delete_commitment(nh(commitment, Id), aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), commits := lists:delete(Id, Cs)}
    end;
%% name_auction: create / update (bid raise via `extend') / delete.
apply_op({auction_new, Id, Bid}, #{trees := T, auctions := As} = S) ->
    case lists:member(Id, As) of
        true -> S;
        false ->
            A = aens_auctions:new(auction_hash(Id), pk(Id), Bid, 500000, 1),
            NT1 = aens_state_tree:enter_name_auction(A, aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), auctions := [Id | As]}
    end;
apply_op({auction_upd, Id, Bid}, #{trees := T, auctions := As} = S) ->
    case lists:member(Id, As) of
        false -> S;
        true ->
            NT = aec_trees:ns(T),
            {value, A0} = aens_state_tree:lookup_name_auction(auction_hash(Id), NT),
            OldTTL = aens_auctions:ttl(A0),
            A1 = aens_auctions:extend(auction_hash(Id), pk(Id), Bid, OldTTL, 500000, 1),
            NT1 = aens_state_tree:enter_name_auction(A1, NT),
            S#{trees := aec_trees:set_ns(T, NT1)}
    end;
apply_op({auction_del, Id}, #{trees := T, auctions := As} = S) ->
    case lists:member(Id, As) of
        false -> S;
        true ->
            NT1 = aens_state_tree:delete_name_auction(auction_hash(Id), aec_trees:ns(T)),
            S#{trees := aec_trees:set_ns(T, NT1), auctions := lists:delete(Id, As)}
    end.

ctr_contents(Id, T) ->
    case aect_state_tree:lookup_contract(ctr_pk(Id), aec_trees:contracts(T)) of
        none       -> deleted;
        {value, C} -> aect_contracts_store:contents(aect_contracts:state(C))
    end.

%% Serialized comparison so mismatches are byte-exact.
oracle_contents(Id, Tf) ->
    case aeo_state_tree:lookup_oracle(oracle_pk(Id), aec_trees:oracles(Tf)) of
        {value, O} -> aeo_oracles:serialize(O);
        none       -> none
    end.

query_contents(Id, FinalS, Tf) ->
    case maps:find(Id, maps:get(queries, FinalS)) of
        error -> none;
        {ok, {OPK, SPK, N}} ->
            QId = aeo_query:id(SPK, N, OPK),
            case aeo_state_tree:lookup_query(OPK, QId, aec_trees:oracles(Tf)) of
                {value, Q} -> aeo_query:serialize(Q);
                none       -> none
            end
    end.

name_contents(Id, Tf) ->
    case aens_state_tree:lookup_name(nh(name, Id), aec_trees:ns(Tf)) of
        {value, N} -> aens_names:serialize(N);
        none       -> none
    end.

commitment_contents(Id, Tf) ->
    case aens_state_tree:lookup_commitment(nh(commitment, Id), aec_trees:ns(Tf)) of
        {value, C} -> aens_commitments:serialize(C);
        none       -> none
    end.

auction_contents(Id, Tf) ->
    case aens_state_tree:lookup_name_auction(auction_hash(Id), aec_trees:ns(Tf)) of
        {value, A} -> aens_auctions:serialize(A);
        none       -> none
    end.

%%%===================================================================
%%% Order-permutation testing
%%%===================================================================

%% One "create" op per (kind, Id), each on a structurally-distinct key, so no
%% op can affect another's outcome. `query_new' is excluded: it depends on its
%% oracle already existing, which is a data dependency, not a batching one.
disjoint_create_ops() ->
    lists:flatmap(
      fun(Id) ->
              [ {acc, Id, 1000 + Id}
              , {ctr_new, Id}
              , {oracle_new, Id, 100 + Id}
              , {name_new, Id, 500000 + Id}
              , {commitment_new, Id, 100 + Id}
              , {auction_new, Id, 1000 + Id}
              , {call_new, Id}
              , {auth_new, Id}
              ]
      end, ?IDS).

%% The same disjoint-key op multiset in any order, batched or immediate, must
%% yield a byte-identical root: no fold or flush order may leak into the root.
order_permutation() ->
    BaseOps = disjoint_create_ops(),
    Seeds = [{11, 22, 33}, {44, 55, 66}, {77, 88, 99}, {5, 15, 25}],
    lists:foreach(
      fun(Seed) ->
              _ = rand:seed(exsplus, Seed),
              Perms = [shuffle(BaseOps) || _ <- lists:seq(1, 6)],
              Results = [ {run(P, immediate), run(P, batch)} || P <- Perms ],
              %% within each permutation: batch == immediate
              lists:foreach(fun({I, B}) -> ?assertEqual(I, B) end, Results),
              %% and identical across every permutation and mode
              [First | Rest] = lists:flatten([[I, B] || {I, B} <- Results]),
              lists:foreach(fun(R) -> ?assertEqual(First, R) end, Rest)
      end, Seeds).

shuffle(L) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- L])].

%%%===================================================================
%%% Targeted cases — account / contract
%%%===================================================================

%% Prior microblock materialises k=>v,k2=>v2; a later batched-but-unflushed
%% tx removes k; enumerating the store must not see k.
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

%% Cross-microblock contract-object delete. Same-microblock create+delete is
%% not exercised (protocol-unreachable divergence on the orphaned store). Here
%% both modes start from the same materialised tree, so only delete timing
%% differs.
mandatory_contract_delete_cross_mb() ->
    MS = fun(M) -> aect_contracts_store:put_map(M, aect_contracts_store:new()) end,
    C0 = new_contract(aeser_id:create(account, pk(1))),
    PK = aect_contracts:pubkey(C0),
    C  = aect_contracts:set_state(MS(#{<<"k">> => <<"v">>}), C0),
    D0 = new_contract(aeser_id:create(account, pk(2))),
    DPK = aect_contracts:pubkey(D0),
    %% C fully materialised.
    Base = flushc(aect_state_tree:insert_contract(C,
                    aect_state_tree:empty_with_backend())),
    %% From Base: delete C, insert D; flush once (batch) vs after delete (immediate).
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
    ?assertEqual(none, LkC),
    ?assert(DPresent).

%% Account delete then re-create: `enter' must overwrite the tombstone so the
%% account is present again with the new value.
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

%% A tombstone hides the account before flush, and the flush removes the key
%% so a later microblock cannot resurrect it.
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
%%% Targeted cases — call / auth_call
%%%===================================================================

mandatory_call_two_calls_coexist() ->
    CallerId = aeser_id:create(account, call_caller_pk(1)),
    CtId     = aeser_id:create(contract, call_ct_pk(1)),
    Run = fun(Mode) ->
              F = fun(CT) -> case Mode of immediate -> flusha(CT); batch -> CT end end,
              Call1 = aect_call:new(CallerId, 1, CtId, 1, 1000000),
              Call2 = aect_call:new(CallerId, 2, CtId, 1, 1000000),
              CT1 = F(aect_call_state_tree:insert_call(
                        Call1, aect_call_state_tree:empty_with_backend())),
              CT2 = F(aect_call_state_tree:insert_call(Call2, CT1)),
              Tf = flusha(CT2),
              {ok, Root} = aect_call_state_tree:root_hash(Tf),
              {lists:sort(aect_call_state_tree:to_list(Tf)), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {List, _} = Run(batch),
    ?assertEqual(2, length(List)).

%% Create then update-in-place on the same key, exercising the `enter'
%% overwrite semantics that keep auth_call safe from a duplicate insert.
mandatory_auth_call_update() ->
    CallerId = aeser_id:create(account, auth_caller_pk(1)),
    CtId     = aeser_id:create(contract, auth_ct_pk(1)),
    Run = fun(Mode) ->
              F = fun(CT) -> case Mode of immediate -> flusha(CT); batch -> CT end end,
              Call0 = aect_call:new(CallerId, 1, CtId, 1, 1000000),
              CT1 = F(aect_call_state_tree:enter_auth_call(
                        Call0, aect_call_state_tree:empty_with_backend())),
              Call1 = aect_call:set_gas_used(4242, Call0),
              CT2 = F(aect_call_state_tree:enter_auth_call(Call1, CT1)),
              Tf = flusha(CT2),
              {ok, Root} = aect_call_state_tree:root_hash(Tf),
              {lists:sort(aect_call_state_tree:to_list(Tf)), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {[{_Key, C}], _} = Run(batch),
    ?assertEqual(4242, aect_call:gas_used(C)).

%%%===================================================================
%%% Targeted cases — oracle / oracle_query
%%%===================================================================

mandatory_oracle_update() ->
    PK = oracle_pk(1),
    Run = fun(Mode) ->
              F = fun(OT) -> case Mode of immediate -> flusho(OT); batch -> OT end end,
              O0 = aeo_oracles:new(PK, <<"q">>, <<"r">>, 10, 1000000000, 1),
              OT1 = F(aeo_state_tree:enter_oracle(
                        O0, aeo_state_tree:empty_with_backend())),
              O1 = aeo_oracles:set_query_fee(999, O0),
              OT2 = F(aeo_state_tree:enter_oracle(O1, OT1)),
              Tf = flusho(OT2),
              {ok, Root} = aeo_state_tree:root_hash(Tf),
              {aeo_state_tree:lookup_oracle(PK, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, O}, _} = Run(batch),
    ?assertEqual(999, aeo_oracles:query_fee(O)).

mandatory_oracle_query_response() ->
    OPK = oracle_pk(2),
    SPK = query_sender_pk(2),
    Run = fun(Mode) ->
              F = fun(OT) -> case Mode of immediate -> flusho(OT); batch -> OT end end,
              Q0 = aeo_query:new(OPK, SPK, 1, <<"q">>, 10, 1000000000, {delta, 100}),
              OT1 = F(aeo_state_tree:insert_query(
                        Q0, aeo_state_tree:empty_with_backend())),
              Q1 = aeo_query:add_response(1, <<"answer">>, Q0),
              OT2 = F(aeo_state_tree:enter_query(Q1, OT1)),
              Tf = flusho(OT2),
              {ok, Root} = aeo_state_tree:root_hash(Tf),
              QId = aeo_query:id(SPK, 1, OPK),
              {aeo_state_tree:lookup_query(OPK, QId, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, Q}, _} = Run(batch),
    ?assert(aeo_query:is_closed(Q)).

%%%===================================================================
%%% Targeted cases — name / commitment / name_auction
%%%===================================================================

mandatory_name_delete_recreate() ->
    NH = nh(name, 1),
    Base = flushn(aens_state_tree:enter_name(
                    aens_names:new(NH, pk(1), 1000),
                    aens_state_tree:empty_with_backend())),
    Run = fun(Mode) ->
              F = fun(NT) -> case Mode of immediate -> flushn(NT); batch -> NT end end,
              NT1 = F(aens_state_tree:delete_name(NH, Base)),
              N2 = aens_names:new(NH, pk(2), 5000),
              Tf = flushn(F(aens_state_tree:enter_name(N2, NT1))),
              {ok, Root} = aens_state_tree:root_hash(Tf),
              {aens_state_tree:lookup_name(NH, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, N}, _} = Run(batch),
    ?assertEqual(pk(2), aens_names:owner_pubkey(N)).

mandatory_name_tombstone_then_read() ->
    NH = nh(name, 3),
    Base = flushn(aens_state_tree:enter_name(
                    aens_names:new(NH, pk(3), 1000),
                    aens_state_tree:empty_with_backend())),
    NT1 = aens_state_tree:delete_name(NH, Base),
    ?assertEqual(none, aens_state_tree:lookup_name(NH, NT1)),  %% pre-flush
    Tf = flushn(NT1),
    ?assertEqual(none, aens_state_tree:lookup_name(NH, Tf)).   %% post-flush

%% get_name/2 must honour a same-batch tombstone: without the fix it fell
%% through to the still-materialised mtree and returned the stale deleted
%% Name instead of raising {not_present, NH}. lookup_name/2 was already correct.
mandatory_get_name_tombstone_blindness() ->
    NH = nh(name, 6),
    Base = flushn(aens_state_tree:enter_name(
                    aens_names:new(NH, pk(6), 1000),
                    aens_state_tree:empty_with_backend())),
    NT1 = aens_state_tree:delete_name(NH, Base),  %% tombstoned, not flushed
    ?assertEqual(none, aens_state_tree:lookup_name(NH, NT1)),
    ?assertError({not_present, NH}, aens_state_tree:get_name(NH, NT1)).

%% Delete-then-recreate is safe: each `enter_commitment' is preceded by a
%% delete of that key, so at most one net insert per key reaches the MPT.
mandatory_commitment_delete_recreate() ->
    CH = nh(commitment, 1),
    C0 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, pk(1)), 100, 1),
    Base = flushn(aens_state_tree:enter_commitment(
                    C0, aens_state_tree:empty_with_backend())),
    Run = fun(Mode) ->
              F = fun(NT) -> case Mode of immediate -> flushn(NT); batch -> NT end end,
              NT1 = F(aens_state_tree:delete_commitment(CH, Base)),
              C1 = aens_commitments:new(aeser_id:create(commitment, CH),
                                        aeser_id:create(account, pk(2)), 500, 1),
              Tf = flushn(F(aens_state_tree:enter_commitment(C1, NT1))),
              {ok, Root} = aens_state_tree:root_hash(Tf),
              {aens_state_tree:lookup_commitment(CH, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, C}, _} = Run(batch),
    ?assertEqual(pk(2), aens_commitments:owner_pubkey(C)).

%% Same delete-then-recreate as above, but also compares ns_cache_hash (the
%% TTL-expiry cache tree, outside the consensus root) across modes. The TTL
%% entry is re-pushed on every flush, so this checks batching leaves no
%% duplicate or stale cache entry.
mandatory_commitment_delete_recreate_cache_hash_equiv() ->
    CH = nh(commitment, 5),
    C0 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, pk(1)), 100, 1),
    Base = flushn(aens_state_tree:enter_commitment(
                    C0, aens_state_tree:empty_with_backend())),
    Run = fun(Mode) ->
              F = fun(NT) -> case Mode of immediate -> flushn(NT); batch -> NT end end,
              NT1 = F(aens_state_tree:delete_commitment(CH, Base)),
              C1 = aens_commitments:new(aeser_id:create(commitment, CH),
                                        aeser_id:create(account, pk(2)), 500, 1),
              Tf = flushn(F(aens_state_tree:enter_commitment(C1, NT1))),
              {ok, Root}      = aens_state_tree:root_hash(Tf),
              {ok, CacheRoot} = aens_state_tree:cache_root_hash(Tf),
              {aens_state_tree:lookup_commitment(CH, Tf), Root, CacheRoot}
          end,
    ImmR = Run(immediate),
    BatchR = Run(batch),
    ?assertEqual(ImmR, BatchR),
    {_, _, ImmCacheRoot} = ImmR,
    {_, _, BatchCacheRoot} = BatchR,
    ?assertEqual(ImmCacheRoot, BatchCacheRoot).

mandatory_commitment_tombstone_then_read() ->
    CH = nh(commitment, 4),
    C0 = aens_commitments:new(aeser_id:create(commitment, CH),
                              aeser_id:create(account, pk(4)), 100, 1),
    Base = flushn(aens_state_tree:enter_commitment(
                    C0, aens_state_tree:empty_with_backend())),
    NT1 = aens_state_tree:delete_commitment(CH, Base),
    ?assertEqual(none, aens_state_tree:lookup_commitment(CH, NT1)), %% pre-flush
    Tf = flushn(NT1),
    ?assertEqual(none, aens_state_tree:lookup_commitment(CH, Tf)).  %% post-flush

mandatory_name_auction_update() ->
    AH = auction_hash(3),
    Run = fun(Mode) ->
              F = fun(NT) -> case Mode of immediate -> flushn(NT); batch -> NT end end,
              A0 = aens_auctions:new(AH, pk(1), 100, 500000, 1),
              NT1 = F(aens_state_tree:enter_name_auction(
                        A0, aens_state_tree:empty_with_backend())),
              A1 = aens_auctions:extend(AH, pk(2), 500, 500001, 500000, 1),
              Tf = flushn(F(aens_state_tree:enter_name_auction(A1, NT1))),
              {ok, Root} = aens_state_tree:root_hash(Tf),
              {aens_state_tree:lookup_name_auction(AH, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, A}, _} = Run(batch),
    ?assertEqual(500, aens_auctions:name_fee(A)).

mandatory_name_auction_delete_recreate() ->
    AH = auction_hash(1),
    A0 = aens_auctions:new(AH, pk(1), 100, 500000, 1),
    Base = flushn(aens_state_tree:enter_name_auction(
                    A0, aens_state_tree:empty_with_backend())),
    Run = fun(Mode) ->
              F = fun(NT) -> case Mode of immediate -> flushn(NT); batch -> NT end end,
              NT1 = F(aens_state_tree:delete_name_auction(AH, Base)),
              A1 = aens_auctions:new(AH, pk(2), 9999, 500000, 1),
              Tf = flushn(F(aens_state_tree:enter_name_auction(A1, NT1))),
              {ok, Root} = aens_state_tree:root_hash(Tf),
              {aens_state_tree:lookup_name_auction(AH, Tf), Root}
          end,
    ?assertEqual(Run(immediate), Run(batch)),
    {{value, A}, _} = Run(batch),
    ?assertEqual(9999, aens_auctions:name_fee(A)).

mandatory_name_auction_tombstone_then_read() ->
    AH = auction_hash(2),
    A0 = aens_auctions:new(AH, pk(1), 100, 500000, 1),
    Base = flushn(aens_state_tree:enter_name_auction(
                    A0, aens_state_tree:empty_with_backend())),
    NT1 = aens_state_tree:delete_name_auction(AH, Base),
    ?assertEqual(none, aens_state_tree:lookup_name_auction(AH, NT1)), %% pre-flush
    Tf = flushn(NT1),
    ?assertEqual(none, aens_state_tree:lookup_name_auction(AH, Tf)).  %% post-flush

%%%===================================================================
%%% Helpers
%%%===================================================================

flushc(CT) ->
    aect_state_tree:flush_contract_meta_batch(
      aect_state_tree:flush_store_batch(CT)).

flusho(OT) -> aeo_state_tree:flush_oracle_batch(OT).
flushn(NT) -> aens_state_tree:flush_name_batch(NT).
flusha(CT) -> aect_call_state_tree:flush_call_batch(CT).

pk(Id) -> <<Id:256>>.

ctr_pk(Id) -> aect_contracts:pubkey(new_contract(aeser_id:create(account, pk(Id)))).

%% Disjoint pubkey namespaces so kinds sharing a tree (plain vs auth calls;
%% name/commitment/auction in the ns tree) never collide with each other or
%% with the account/contract-owner space (`pk/1').
call_caller_pk(Id) -> <<(1_000_000 + Id):256>>.
call_ct_pk(Id)     -> <<(2_000_000 + Id):256>>.
auth_caller_pk(Id) -> <<(3_000_000 + Id):256>>.
auth_ct_pk(Id)     -> <<(4_000_000 + Id):256>>.
oracle_pk(Id)       -> <<(5_000_000 + Id):256>>.
query_sender_pk(Id) -> <<(6_000_000 + Id):256>>.

%% Domain-separated 32-byte hash per (Kind, Id) for the shared ns_tree
%% (names / commitments / auctions all live in one mtree).
nh(Kind, Id) ->
    aec_hash:hash(pubkey, <<(atom_to_binary(Kind, utf8))/binary, Id:256>>).

%% 33-byte auction hash, structurally distinct from any 32-byte
%% name/commitment key in the same tree.
auction_hash(Id) -> aens_hash:to_auction_hash(nh(auction, Id)).

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
