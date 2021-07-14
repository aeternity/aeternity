%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_peers module
%%% @end
%%%=============================================================================
-module(aec_peers_pool_tests).

-ifdef(TEST).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

-import(aec_peers_pool, [
    new/1,
    count/3,
    find/2,
    peer_state/2,
    is_verified/2,
    is_unverified/2,
    is_available/2,
    update/3,
    verify/3,
    random_subset/3,
    random_select/4,
    select/3,
    reject/3,
    release/3,
    delete/2,
    available/2
]).

-export([seed_process_random/0,
         random_peer/0,
         random_peer/1,
         random_address/0
]).

%=== MACROS ====================================================================

-define(BASE_POOL_OPTS, []).

%% Some tests are statistical tests, for example `bucket_index_test_` that
%% validate the bucket indexes cover enough of the bucket space.
%% These tests may fail randomly for some seeds so fixing the seed make the
%% test more reliable on CI.
-define(USE_FIXED_SEED, true).

-ifdef(USE_FIXED_SEED).

-define(RANDOM_PROCESS_SEED, {1533,565972,58584}).
-define(RANDOM_POOL_SEED, {1533,565990,602018}).
-define(POOL_OPTS, [{secret, <<"some fixed secret">>},
                    {disable_strong_random, true},
                    {seed, ?RANDOM_POOL_SEED}
                    | ?BASE_POOL_OPTS]).

-else.

-define(POOL_OPTS, ?BASE_POOL_OPTS).

-endif.


%=== TEST CASES ================================================================

regressions_test() ->
    {foreach,
     fun db_setup/0,
     fun db_teardown/1,
     [
        {"Check for regression of aec_peers_pool:unverified_add_reference/4", fun reference_bug/0}
     ]}.
    
%% Test that put in evidence a bug in aec_peers_pool:unverified_add_reference/4.
%% The test depends heavily on the fixed random seeds.
reference_bug() ->
    seed_process_random(),
    P = new([
        {verif_bcount, 1},
        {verif_bsize, 2},
        {unver_bcount, 4},
        {unver_bsize, 4}
        | ?POOL_OPTS
    ]),

    % Create 4 source addresses
    Sources = lists:foldl(fun(_, Acc) ->
        [random_address() | Acc]
    end, [], lists:seq(1, 4)),

    % Create 50 peers
    Peers = lists:foldl(fun(_, Acc) ->
        [random_peer(#{source => rand_peek(Sources)}) | Acc]
    end, [], lists:seq(1, 50)),

    lists:foldl(fun(_, {S, N}) ->
        % Update 10 random peers
        {S2, N2} = lists:foldl(fun(_, {Sf, Nf}) ->
            RandPeer  = rand_peek(Peers),
            {_, Sf2} = aec_peers_pool:update(Sf, Nf, RandPeer),
            {Sf2, Nf + 1}
        end, {S, N}, lists:seq(1, 10)),

        % Mark a random unverified peer as verified
        case aec_peers_pool:random_select(S2, N2, both, undefined) of
            {unavailable, S3} -> {S3, N2 + 1};
            {selected, {I, _}, S3} ->
                {_, S4} = aec_peers_pool:verify(S3, N2, I),
                {S4, N2 + 1}
        end
    end, {P, 1}, lists:seq(1, 5)),
    ok.

pools_test() ->
    {foreach,
     fun db_setup/0,
     fun db_teardown/1,
     [
        {"Check when empty", fun empty_pool/0},
        {"Check when with a single untrusted peer", fun add_single_normal/0},
        {"Check when with a single trusted peer", fun add_single_trusted/0},
        {"Check when with multiple peers subset", fun multiple_peers_subset/0},
        {"Select multiple peers", fun multiple_peers_select/0},
        {"Check peer selection unavailablility", fun peer_selection_unavailability/0},
        {"Check peer release", fun peer_release/0},
        {"Check peer rejection", fun peer_rejection/0},
        {"Check peer rejection downgrade", fun rejection_downgrade/0},
        {"Check peer verification", fun basic_verification/0},
        {"Check peer verification: selected peer", fun verification_of_selected_peer/0},
        {"Check peer verification: standby peer", fun verification_of_standby_peer/0},
        {"Check canceled verification", fun verification_canceled/0},
        {"Check ignored update", fun update_ignored/0},
        {"Check downgrade when no space in unverified", fun downgrade_to_bucket_with_no_eviction_possible/0},
        {"Check manual select of a standby peer", fun manual_selection_of_standby_peer/0},
        {"Check unverified selected peeres are not evicted", fun unverified_selected_are_not_evicted/0},
        {"Check unverified old peers are removed", fun unverified_old_peers_are_removed/0},
        {"Check unverified multiple references", fun unverified_multiple_references/0},
        {"Check unverified references are evicted", fun unverified_reference_eviction/0},
        {"Check verified selected and trusted peers are not evicted", fun verified_selected_and_trusted_peers_are_not_evicted/0},
        {"Check verified old peers are removed", fun verified_old_peers_are_removed/0},
        {"Test pool counter synchronization", {timeout, 100, fun validate_counters/0}}
     ]}.

%% Tests pool behavior when empty.
empty_pool() ->
    P = new(?BASE_POOL_OPTS),
    A = random_peer_id(),
    Now = erlang:system_time(millisecond),
    FilterFun = make_ext_exclude_filter([]),
    ?assertEqual(0, count(P, all, both)),
    ?assertEqual(0, count(P, all, verified)),
    ?assertEqual(0, count(P, all, unverified)),
    ?assertEqual(0, count(P, available, both)),
    ?assertEqual(0, count(P, available, verified)),
    ?assertEqual(0, count(P, available, unverified)),
    ?assertEqual(0, count(P, standby, both)),
    ?assertEqual(0, count(P, standby, verified)),
    ?assertEqual(0, count(P, standby, unverified)),
    ?assertEqual([], available(P, both)),
    ?assertEqual([], available(P, verified)),
    ?assertEqual([], available(P, unverified)),
    ?assertEqual(error, find(P, A)),
    ?assertEqual(undefined, is_verified(P, A)),
    ?assertEqual(undefined, is_unverified(P, A)),
    ?assertEqual(undefined, is_available(P, A)),
    ?assertMatch({[], _}, random_subset(P, all, undefined)),
    ?assertMatch({[], _}, random_subset(P, all, FilterFun)),
    ?assertMatch({[], _}, random_subset(P, 1, undefined)),
    ?assertMatch({[], _}, random_subset(P, 1, FilterFun)),
    ?assertMatch({[], _}, random_subset(P, 10, undefined)),
    ?assertMatch({[], _}, random_subset(P, 10, FilterFun)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, both, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, both, FilterFun)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, verified, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, verified, FilterFun)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, unverified, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(P, Now, unverified, FilterFun)),
    ok.

%% Tests pool behaviour with a single normal peer.
add_single_normal() ->
    P = new(?BASE_POOL_OPTS),
    RandomPeer = random_peer(),
    Id1 = aec_peer:id(RandomPeer),
    Now = erlang:system_time(millisecond),
    NoFilterFun = make_ext_exclude_filter([]),
    FilterFun = make_ext_exclude_filter([Id1]),
    {unverified, P2} = update(P, Now, RandomPeer),
    ?assertEqual(1, count(P2, all, both)),
    ?assertEqual(0, count(P2, all, verified)),
    ?assertEqual(1, count(P2, all, unverified)),
    ?assertEqual(1, count(P2, available, both)),
    ?assertEqual(0, count(P2, available, verified)),
    ?assertEqual(1, count(P2, available, unverified)),
    ?assertEqual(0, count(P2, standby, both)),
    ?assertEqual(0, count(P2, standby, verified)),
    ?assertEqual(0, count(P2, standby, unverified)),
    ?assertEqual([{Id1, RandomPeer}], available(P2, both)),
    ?assertEqual([], available(P2, verified)),
    ?assertEqual([{Id1, RandomPeer}], available(P2, unverified)),
    ?assertEqual({ok, RandomPeer}, find(P2, Id1)),
    ?assertEqual(false, is_verified(P2, Id1)),
    ?assertEqual(true, is_unverified(P2, Id1)),
    ?assertMatch({[], _}, random_subset(P2, all, undefined)),
    ?assertMatch({[], _}, random_subset(P2, all, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, all, FilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 1, undefined)),
    ?assertMatch({[], _}, random_subset(P2, 1, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 1, FilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 10, undefined)),
    ?assertMatch({[], _}, random_subset(P2, 10, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 10, FilterFun)),
    {verified, P3} = verify(P2, Now, Id1),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, all, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, all, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P3, all, FilterFun)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, 1, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, 1, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P3, 1, FilterFun)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, 10, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P3, 10, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P3, 10, FilterFun)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P3, Now, both, undefined)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P3, Now, both, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P3, Now, both, FilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P3, Now, unverified, undefined)),
    ?assertMatch({unavailable, _},
               random_select(P3, Now, unverified, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P3, Now, unverified, FilterFun)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P3, Now, verified, undefined)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P3, Now, verified, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P3, Now, verified, FilterFun)),
    ok.

%% Tests pool behavior with a single trusted peer.
add_single_trusted() ->
    P = new(?BASE_POOL_OPTS),
    RandomPeer = random_peer(#{trusted => true}),
    Id1 = aec_peer:id(RandomPeer),
    Now = erlang:system_time(millisecond),
    NoFilterFun = make_ext_exclude_filter([]),
    FilterFun = make_ext_exclude_filter([Id1]),
    {verified, P2} = update(P, Now, RandomPeer),
    ?assertEqual(1, count(P2, all, both)),
    ?assertEqual(1, count(P2, all, verified)),
    ?assertEqual(0, count(P2, all, unverified)),
    ?assertEqual(1, count(P2, available, both)),
    ?assertEqual(1, count(P2, available, verified)),
    ?assertEqual(0, count(P2, available, unverified)),
    ?assertEqual(0, count(P2, standby, both)),
    ?assertEqual(0, count(P2, standby, verified)),
    ?assertEqual(0, count(P2, standby, unverified)),
    ?assertEqual([{Id1, RandomPeer}], available(P2, both)),
    ?assertEqual([{Id1, RandomPeer}], available(P2, verified)),
    ?assertEqual([], available(P2, unverified)),
    ?assertEqual(true, is_verified(P2, Id1)),
    ?assertEqual(false, is_unverified(P2, Id1)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, all, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, all, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, all, FilterFun)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, 1, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, 1, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 1, FilterFun)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, 10, undefined)),
    ?assertMatch({[{Id1, RandomPeer}], _}, random_subset(P2, 10, NoFilterFun)),
    ?assertMatch({[], _}, random_subset(P2, 10, FilterFun)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P2, Now, both, undefined)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P2, Now, both, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P2, Now, both, FilterFun)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P2, Now, verified, undefined)),
    ?assertMatch({selected, {Id1, RandomPeer}, _},
               random_select(P2, Now, verified, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P2, Now, verified, FilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P2, Now, unverified, undefined)),
    ?assertMatch({unavailable, _},
               random_select(P2, Now, unverified, NoFilterFun)),
    ?assertMatch({unavailable, _},
               random_select(P2, Now, unverified, FilterFun)),
    ok.

%% Tests adding multiple peers both normal and trusted.
multiple_peers_subset() ->
    seed_process_random(),
    TotalCount = 1000,
    VerifCount = 50,
    ExcludedCount = 10,

    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),
    Peers = make_peers(TotalCount, VerifCount),
    ExcludedIdxs = rand_int_list(1, VerifCount + 1, ExcludedCount),
    VerifiedIds = [I || {I, P} <- maps:to_list(Peers), aec_peer:is_trusted(P)],
    ExcludedIds = [lists:nth(Int, VerifiedIds) || Int <- ExcludedIdxs],
    FilterFun = make_ext_exclude_filter(ExcludedIds),

    UnverCount = TotalCount - VerifCount,
    UnexCount = VerifCount - ExcludedCount,

    Pool2 = maps:fold(fun(_, Peer, P) ->
        {_, P2} = update(P, Now, Peer),
        P2
    end, Pool1, Peers),

    ?assertEqual(TotalCount, count(Pool2, all, both)),
    ?assertEqual(VerifCount, count(Pool2, all, verified)),
    ?assertEqual(UnverCount, count(Pool2, all, unverified)),
    ?assertEqual(TotalCount, count(Pool2, available, both)),
    ?assertEqual(VerifCount, count(Pool2, available, verified)),
    ?assertEqual(UnverCount, count(Pool2, available, unverified)),
    ?assertEqual(lists:sort([{aec_peer:id(P), P} || P <- maps:values(Peers)]),
                 lists:sort(available(Pool2, both))),
    ?assertEqual(lists:sort([{aec_peer:id(P), P}
                             ||  P <- maps:values(Peers), aec_peer:is_trusted(P)]),
                 lists:sort(available(Pool2, verified))),
    ?assertEqual(lists:sort([{aec_peer:id(P), P}
                             ||  P <- maps:values(Peers),
                                 aec_peer:is_trusted(P) =:= false]),
                 lists:sort(available(Pool2, unverified))),

    {S2a, _} = random_subset(Pool2, all, undefined),
    ?assertEqual(VerifCount, length(S2a)),
    ?assertEqual(VerifCount, length(lists:usort([I || {I, _} <- S2a]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P) %% same peer
    end, S2a),

    {S2b, _} = random_subset(Pool2, all, FilterFun),
    ?assertEqual(UnexCount, length(S2b)),
    ?assertEqual(UnexCount, length(lists:usort([I || {I, _} <- S2b]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P),
        ?assertNot(lists:member(I, ExcludedIds))
    end, S2b),

    {S2c, _} = random_subset(Pool2, VerifCount * 2, undefined),
    ?assertEqual(VerifCount, length(S2c)),
    ?assertEqual(VerifCount, length(lists:usort([I || {I, _} <- S2c]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P)
    end, S2c),

    {S2d, _} = random_subset(Pool2, VerifCount * 2, FilterFun),
    ?assertEqual(UnexCount, length(S2d)),
    ?assertEqual(UnexCount, length(lists:usort([I || {I, _} <- S2d]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P),
        ?assertNot(lists:member(I, ExcludedIds))
    end, S2d),

    {S2e, _} = random_subset(Pool2, 30, undefined),
    ?assertEqual(30, length(S2e)),
    ?assertEqual(30, length(lists:usort([I || {I, _} <- S2e]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P)
    end, S2e),

    {S2f, _} = random_subset(Pool2, 30, FilterFun),
    ?assertEqual(30, length(S2f)),
    ?assertEqual(30, length(lists:usort([I || {I, _} <- S2f]))),
    lists:foreach(fun({I, P}) ->
        ?assertEqual(maps:get(I, Peers), P),
        ?assertNot(lists:member(I, ExcludedIds))
    end, S2f),
    ok.

%% Test peer selection behavior.
multiple_peers_select() ->
    TotalCount = 1000,
    VerifCount = 50,
    ExcludedCount = 10,

    Now = erlang:system_time(millisecond),
    Pool1 = new(?BASE_POOL_OPTS),
    Peers = make_peers(TotalCount, VerifCount),
    ExcludedIdxs = rand_int_list(1, VerifCount + 1, ExcludedCount),
    VerifiedIds = [I || {I, P} <- maps:to_list(Peers), aec_peer:is_trusted(P)],
    ExcludedIds = [lists:nth(Int, VerifiedIds) || Int <- ExcludedIdxs],
    FilterFun = make_ext_exclude_filter(ExcludedIds),

    AllIds = [I || {I, _} <- maps:to_list(Peers)],
    UnexIds = [I || I <- AllIds, not lists:member(I, ExcludedIds)],
    VerifIds = [I || {I, P} <- maps:to_list(Peers), aec_peer:is_trusted(P)],
    UnexVerifIds = [I || I <- VerifIds, not lists:member(I, ExcludedIds)],
    UnverIds = [I || {I, P} <- maps:to_list(Peers), aec_peer:is_trusted(P) =:= false],
    UnexUnverIds = [I || I <- UnverIds, not lists:member(I, ExcludedIds)],

    Pool2 = maps:fold(fun(_, Peer, P) ->
        {_, P2} = update(P, Now, Peer),
        P2
    end, Pool1, Peers),

    {selected, {I2a, P2a}, _} =
        random_select(Pool2, Now, both, undefined),
    ?assertMatch(I2a, aec_peer:id(P2a)),
    ?assert(lists:member(I2a, AllIds)),
    {selected, {I2b, P2b}, _} =
        random_select(Pool2, Now, both, FilterFun),
    ?assertMatch(I2b, aec_peer:id(P2b)),
    ?assert(lists:member(I2b, UnexIds)),
    {selected, {I2c, P2c}, _} =
        random_select(Pool2, Now, verified, undefined),
    ?assertMatch(I2c, aec_peer:id(P2c)),
    ?assertMatch(true, aec_peer:is_trusted(P2c)),
    ?assert(lists:member(I2c, VerifIds)),
    {selected, {I2d, P2d}, _} =
        random_select(Pool2, Now, verified, FilterFun),
    ?assertMatch(I2d, aec_peer:id(P2d)),
    ?assertMatch(true, aec_peer:is_trusted(P2d)),
    ?assert(lists:member(I2d, UnexVerifIds)),
    {selected, {I2e, P2e}, _} =
        random_select(Pool2, Now, unverified, undefined),
    ?assertMatch(I2e, aec_peer:id(P2e)),
    ?assertMatch(false, aec_peer:is_trusted(P2e)),
    ?assert(lists:member(I2e, UnverIds)),
    {selected, {I2f, P2f}, _} =
        random_select(Pool2, Now, unverified, FilterFun),
    ?assertMatch(I2f, aec_peer:id(P2f)),
    ?assertMatch(false, aec_peer:is_trusted(P2f)),
    ?assert(lists:member(I2f, UnexUnverIds)),
    ok.

%% Tests that selected peers cannot be selected.
peer_selection_unavailability() ->
    seed_process_random(),
    UnverCount = 500,
    VerifCount = 500,

    TotalCount = UnverCount + VerifCount,
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),
    Peers = make_peers(TotalCount, VerifCount),

    Pool2 = maps:fold(fun(_, Peer, P) ->
        {_, P2} = update(P, Now, Peer),
        P2
    end, Pool1, Peers),

    {Pool3, _, _, _} = lists:foldl(fun(_, {P, A, V, U}) ->
        {selected, {Id, Peer}, P2} = random_select(P, Now, both, undefined),
        ?assertNot(lists:member(Id, A)),
        ?assertEqual(true, is_available(P, Id)),
        ?assertEqual(false, is_available(P2, Id)),
        IsTrusted = aec_peer:is_trusted(Peer),
        ?assertEqual(Id, aec_peer:id(Peer)),
        {A2, V2, U2} = case IsTrusted of
            true -> {[Id | A], [Id | V], U};
            false -> {[Id | A], V, [Id | U]}
        end,
        ?assertEqual(TotalCount, count(P2, all, both)),
        ?assertEqual(VerifCount, count(P2, all, verified)),
        ?assertEqual(UnverCount, count(P2, all, unverified)),
        ?assertEqual(TotalCount - length(A2), count(P2, available, both)),
        ?assertEqual(VerifCount - length(V2), count(P2, available, verified)),
        ?assertEqual(UnverCount - length(U2), count(P2, available, unverified)),
        {P2, A2, V2, U2}
    end, {Pool2, [], [], []}, lists:seq(1, TotalCount)),

    ?assertMatch({unavailable, _},
                 random_select(Pool3, Now, both, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool3, Now, verified, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool3, Now, unverified, undefined)),

    {Pool4, _} = lists:foldl(fun(_, {P, V}) ->
        {selected, {Id, Peer}, P2} = random_select(P, Now, verified, undefined),
        ?assertNot(lists:member(Id, V)),
        ?assertEqual(Id, aec_peer:id(Peer)),
        ?assertEqual(true, aec_peer:is_trusted(Peer)),
        ?assertEqual(true, is_verified(P2, Id)),
        ?assertEqual(false, is_unverified(P2, Id)),
        V2 = [Id | V],
        ?assertEqual(TotalCount, count(P2, all, both)),
        ?assertEqual(VerifCount, count(P2, all, verified)),
        ?assertEqual(UnverCount, count(P2, all, unverified)),
        ?assertEqual(TotalCount - length(V2), count(P2, available, both)),
        ?assertEqual(VerifCount - length(V2), count(P2, available, verified)),
        ?assertEqual(UnverCount, count(P2, available, unverified)),
        {P2, V2}
    end, {Pool2, []}, lists:seq(1, VerifCount)),

    ?assertMatch({selected, _, _},
                 random_select(Pool4, Now, both, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool4, Now, verified, undefined)),
    ?assertMatch({selected, _, _},
                 random_select(Pool4, Now, unverified, undefined)),

    {Pool5, _} = lists:foldl(fun(_, {P, U}) ->
        {selected, {Id, Peer}, P2} = random_select(P, Now, unverified, undefined),
        ?assertNot(lists:member(Id, U)),
        ?assertEqual(Id, aec_peer:id(Peer)),
        ?assertEqual(false, aec_peer:is_trusted(Peer)),
        ?assertEqual(true, is_unverified(P2, Id)),
        U2 = [Id | U],
        ?assertEqual(TotalCount, count(P2, all, both)),
        ?assertEqual(VerifCount, count(P2, all, verified)),
        ?assertEqual(UnverCount, count(P2, all, unverified)),
        ?assertEqual(TotalCount - length(U2), count(P2, available, both)),
        ?assertEqual(VerifCount, count(P2, available, verified)),
        ?assertEqual(UnverCount - length(U2), count(P2, available, unverified)),
        {P2, U2}
    end, {Pool2, []}, lists:seq(1, UnverCount)),

    ?assertMatch({selected, _, _},
                 random_select(Pool5, Now, both, undefined)),
    ?assertMatch({selected, _, _},
                 random_select(Pool5, Now, verified, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool5, Now, unverified, undefined)),

    % Check explicit selection.

    {Pool6, _, _, _} = lists:foldl(fun(Id, {P, A, V, U}) ->
        #{Id := Peer} = Peers,
        ?assertNot(lists:member(Id, A)),
        P2 = select(P, Now, Id),
        ?assertEqual(true, is_available(P, Id)),
        ?assertEqual(false, is_available(P2, Id)),
        IsTrusted = aec_peer:is_trusted(Peer),
        {A2, V2, U2} = case IsTrusted of
            true -> {[Id | A], [Id | V], U};
            false -> {[Id | A], V, [Id | U]}
        end,
        ?assertEqual(TotalCount, count(P2, all, both)),
        ?assertEqual(VerifCount, count(P2, all, verified)),
        ?assertEqual(UnverCount, count(P2, all, unverified)),
        ?assertEqual(TotalCount - length(A2), count(P2, available, both)),
        ?assertEqual(VerifCount - length(V2), count(P2, available, verified)),
        ?assertEqual(UnverCount - length(U2), count(P2, available, unverified)),
        {P2, A2, V2, U2}
    end, {Pool2, [], [], []}, maps:keys(Peers)),

    ?assertMatch({unavailable, _},
                 random_select(Pool6, Now, both, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool6, Now, verified, undefined)),
    ?assertMatch({unavailable, _},
                 random_select(Pool6, Now, unverified, undefined)),
    ok.

%% Tests peer release behavior.
peer_release() ->
    seed_process_random(),
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),
    Peers = make_peers(6, 3),

    Pool2 = maps:fold(fun(_, Peer, P) ->
        {_, P2} = update(P, Now, Peer),
        P2
    end, Pool1, Peers),

    {selected, {S1, _}, Pool3} =
        random_select(Pool2, Now, verified, undefined),
    {selected, {S2, _}, Pool4} =
        random_select(Pool3, Now, unverified, undefined),
    {selected, {S3, _}, Pool5} =
        random_select(Pool4, Now, unverified, undefined),

    ?assertEqual(false, is_available(Pool5, S1)),
    ?assertEqual(true, is_verified(Pool5, S1)),
    ?assertEqual(false, is_unverified(Pool5, S1)),
    ?assertEqual(false, is_available(Pool5, S2)),
    ?assertEqual(false, is_verified(Pool5, S2)),
    ?assertEqual(true, is_unverified(Pool5, S2)),
    ?assertEqual(false, is_available(Pool5, S3)),
    ?assertEqual(false, is_verified(Pool5, S3)),
    ?assertEqual(true, is_unverified(Pool5, S3)),

    ?assertEqual(6, count(Pool5, all, both)),
    ?assertEqual(3, count(Pool5, all, verified)),
    ?assertEqual(3, count(Pool5, all, unverified)),
    ?assertEqual(3, count(Pool5, available, both)),
    ?assertEqual(2, count(Pool5, available, verified)),
    ?assertEqual(1, count(Pool5, available, unverified)),

    Pool6 = aec_peers_pool:release(Pool5, Now, S1),

    ?assertEqual(true, is_available(Pool6, S1)),
    ?assertEqual(true, is_verified(Pool6, S1)),
    ?assertEqual(false, is_unverified(Pool6, S1)),

    ?assertEqual(6, count(Pool6, all, both)),
    ?assertEqual(3, count(Pool6, all, verified)),
    ?assertEqual(3, count(Pool6, all, unverified)),
    ?assertEqual(4, count(Pool6, available, both)),
    ?assertEqual(3, count(Pool6, available, verified)),
    ?assertEqual(1, count(Pool6, available, unverified)),

    Pool7 = aec_peers_pool:release(Pool6, Now, S2),

    ?assertEqual(true, is_available(Pool7, S2)),
    ?assertEqual(false, is_verified(Pool7, S2)),
    ?assertEqual(true, is_unverified(Pool7, S2)),

    ?assertEqual(6, count(Pool7, all, both)),
    ?assertEqual(3, count(Pool7, all, verified)),
    ?assertEqual(3, count(Pool7, all, unverified)),
    ?assertEqual(5, count(Pool7, available, both)),
    ?assertEqual(3, count(Pool7, available, verified)),
    ?assertEqual(2, count(Pool7, available, unverified)),

    Pool8 = aec_peers_pool:release(Pool7, Now, S3),

    ?assertEqual(true, is_available(Pool8, S3)),
    ?assertEqual(false, is_verified(Pool8, S3)),
    ?assertEqual(true, is_unverified(Pool8, S3)),

    ?assertEqual(6, count(Pool8, all, both)),
    ?assertEqual(3, count(Pool8, all, verified)),
    ?assertEqual(3, count(Pool8, all, unverified)),
    ?assertEqual(6, count(Pool8, available, both)),
    ?assertEqual(3, count(Pool8, available, verified)),
    ?assertEqual(3, count(Pool8, available, unverified)),

    % Check released peers can be selected again.

    {AllSelIds, _} = select_all(Pool8, Now, both, undefined),
    ?assertEqual(lists:sort([I || {I, _} <- maps:to_list(Peers)]),
                 lists:sort(AllSelIds)),

    {VerifSelIds, _} = select_all(Pool8, Now, verified, undefined),
    ?assertEqual(lists:sort([I || {I, P} <- maps:to_list(Peers),
                             aec_peer:is_trusted(P)]),
                 lists:sort(VerifSelIds)),

    {UnverSelIds, _} = select_all(Pool8, Now, unverified, undefined),
    ?assertEqual(lists:sort([I || {I, P} <- maps:to_list(Peers),
                             aec_peer:is_trusted(P) =:= false]),
                 lists:sort(UnverSelIds)),
    ok.

%% Tests peer rejection behavior.
peer_rejection() ->
    seed_process_random(),
    % Assumes backoff delays are [5, 15, 30, 60, 120, 300, 600]
    Now1 = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),
    Peers = make_peers(3, 1),

    Pool2 = maps:fold(fun(_, Peer, P) ->
        {_, P2} = update(P, Now1, Peer),
        P2
    end, Pool1, Peers),

    {selected, {S1, _}, Pool3} =
        random_select(Pool2, Now1, verified, undefined),
    ?assertEqual(false, is_available(Pool3, S1)),

    Now2 = Now1 + 1000,

    {selected, {S2, _}, Pool4} =
        random_select(Pool3, Now2, unverified, undefined),
    ?assertEqual(false, is_available(Pool4, S2)),

    Now3 = Now2 + 1000,

    {selected, {S3, _}, Pool5} =
        random_select(Pool4, Now3, unverified, undefined),
    ?assertEqual(false, is_available(Pool5, S3)),

    Now4 = Now3 + 1000,

    {unavailable, Pool6} = random_select(Pool5, Now4, both, undefined),
    {unavailable, Pool7} = random_select(Pool6, Now4, verified, undefined),
    {unavailable, Pool8} = random_select(Pool7, Now4, unverified, undefined),

    Now5 = Now4 + 1000,

    ?assertEqual(0, count(Pool8, standby, both)),
    ?assertEqual(0, count(Pool8, standby, verified)),
    ?assertEqual(0, count(Pool8, standby, unverified)),

    Pool9 = reject(Pool8, Now5, S1),
    ?assertEqual(true, is_available(Pool9, S1)),
    ?assertEqual(0, count(Pool9, standby, both)),
    ?assertEqual(0, count(Pool9, standby, verified)),
    ?assertEqual(0, count(Pool9, standby, unverified)),

    {selected, {S1, _}, Pool10} = random_select(Pool9, Now5, both, undefined),
    {unavailable, Pool11} = random_select(Pool10, Now5, verified, undefined),
    {unavailable, Pool12} = random_select(Pool11, Now5, unverified, undefined),

    Now6 = Now5 + 1000,

    Pool13 = reject(Pool12, Now6, S2),
    ?assertEqual(false, is_available(Pool13, S2)),
    ?assertEqual(1, count(Pool13, standby, both)),
    ?assertEqual(0, count(Pool13, standby, verified)),
    ?assertEqual(1, count(Pool13, standby, unverified)),

    {wait, 5000, Pool14} = random_select(Pool13, Now6, both, undefined),
    {unavailable, Pool15} = random_select(Pool14, Now6, verified, undefined),
    {wait, 5000, Pool16} = random_select(Pool15, Now6, unverified, undefined),

    Now7 = Now6 + 2000,

    Pool17 = reject(Pool16, Now7, S3),
    ?assertEqual(false, is_available(Pool17, S3)),
    ?assertEqual(2, count(Pool17, standby, both)),
    ?assertEqual(0, count(Pool17, standby, verified)),
    ?assertEqual(2, count(Pool17, standby, unverified)),

    {wait, 3000, Pool18} = random_select(Pool17, Now7, both, undefined),
    {unavailable, Pool19} = random_select(Pool18, Now7, verified, undefined),
    {wait, 3000, Pool20} = random_select(Pool19, Now7, unverified, undefined),

    Now8 = Now7 + 1000,

    {wait, 2000, Pool21} = random_select(Pool20, Now8, both, undefined),
    {unavailable, Pool22} = random_select(Pool21, Now8, verified, undefined),
    {wait, 2000, Pool23} = random_select(Pool22, Now8, unverified, undefined),

    Now9 = Now8 + 1000,

    {wait, 1000, Pool24} = random_select(Pool23, Now9, both, undefined),
    {unavailable, _} = random_select(Pool23, Now9, verified, undefined),
    {wait, 1000, Pool25} = random_select(Pool24, Now9, unverified, undefined),

    ?assertEqual(2, count(Pool25, standby, both)),
    ?assertEqual(0, count(Pool25, standby, verified)),
    ?assertEqual(2, count(Pool25, standby, unverified)),

    Now10 = Now9 + 1000,

    {selected, {S2, _}, Pool26} = random_select(Pool25, Now10, both, undefined),
    {unavailable, _} = random_select(Pool25, Now10, verified, undefined),
    {selected, {S2, _}, _} = random_select(Pool25, Now10, unverified, undefined),

    {wait, 2000, Pool27} = random_select(Pool26, Now10, both, undefined),
    {unavailable, Pool28} = random_select(Pool27, Now10, verified, undefined),
    {wait, 2000, Pool29} = random_select(Pool28, Now10, unverified, undefined),

    ?assertEqual(1, count(Pool29, standby, both)),
    ?assertEqual(0, count(Pool29, standby, verified)),
    ?assertEqual(1, count(Pool29, standby, unverified)),

    Now11 = Now10 + 2000,

    {selected, {S3, _}, _} = random_select(Pool29, Now11, both, undefined),
    {unavailable, _} = random_select(Pool29, Now11, verified, undefined),
    {selected, {S3, _}, Pool30} =
        random_select(Pool29, Now11, unverified, undefined),

    {unavailable, _} = random_select(Pool30, Now11, both, undefined),
    {unavailable, _} = random_select(Pool30, Now11, verified, undefined),
    {unavailable, _} = random_select(Pool30, Now11, unverified, undefined),

    ?assertEqual(0, count(Pool30, standby, both)),
    ?assertEqual(0, count(Pool30, standby, verified)),
    ?assertEqual(0, count(Pool30, standby, unverified)),
    ok.

%% Tests peer downgrading behavior.
rejection_downgrade() ->
    seed_process_random(),
    % Assumes backoff delays are [5, 15, 30, 60, 120, 300, 600]
    Now1 = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),

    Peer = random_peer(),
    Id = aec_peer:id(Peer),


    {unverified, Pool2} = update(Pool1, Now1, Peer),
    {verified, Pool3} = verify(Pool2, Now1, Id),
    ?assertEqual({verified, true}, peer_state(Pool3, Id)),

    ?assertEqual(1, count(Pool3, all, both)),
    ?assertEqual(1, count(Pool3, all, verified)),
    ?assertEqual(0, count(Pool3, all, unverified)),
    ?assertEqual(1, count(Pool3, available, both)),
    ?assertEqual(1, count(Pool3, available, verified)),
    ?assertEqual(0, count(Pool3, available, unverified)),

    {selected, {Id, _}, Pool4} = random_select(Pool3, Now1, both, undefined),
    ?assertEqual({verified, false}, peer_state(Pool4, Id)),
    % Should downgrade the peer.
    Pool5 = reject(Pool4, Now1, Id),
    ?assertEqual({unverified, true}, peer_state(Pool5, Id)),

    ?assertEqual(1, count(Pool5, all, both)),
    ?assertEqual(0, count(Pool5, all, verified)),
    ?assertEqual(1, count(Pool5, all, unverified)),
    ?assertEqual(1, count(Pool5, available, both)),
    ?assertEqual(0, count(Pool5, available, verified)),
    ?assertEqual(1, count(Pool5, available, unverified)),

    % Rejection counter should be reset.

    {Pool6, Now2} = lists:foldl(fun(T, {P, N}) ->
        {selected, {Id, _}, P2} = random_select(P, N, both, undefined),
        ?assertEqual({unverified, false}, peer_state(P2, Id)),
        P3 = reject(P2, N, Id),
        ?assertEqual({unverified, false}, peer_state(P3, Id)),
        {P3, N + T * 1000}
    end, {Pool5, Now1}, [5, 15, 30, 60, 120, 300, 600]),

    {selected, {Id, _}, Pool7} = random_select(Pool6, Now2, both, undefined),
    ?assertEqual({unverified, false}, peer_state(Pool7, Id)),
    % Should delete the peer.
    Pool8 = reject(Pool7, Now2, Id),
    ?assertEqual({undefined, undefined}, peer_state(Pool8, Id)),

    ?assertEqual(0, count(Pool8, all, both)),
    ?assertEqual(0, count(Pool8, all, verified)),
    ?assertEqual(0, count(Pool8, all, unverified)),
    ?assertEqual(0, count(Pool8, available, both)),
    ?assertEqual(0, count(Pool8, available, verified)),
    ?assertEqual(0, count(Pool8, available, unverified)),
    ok.

%% Tests peer verification behavior.
basic_verification() ->
    seed_process_random(),
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),

    Peer = random_peer(),
    Id = aec_peer:id(Peer),

    {unverified, Pool2} = update(Pool1, Now, Peer),
    ?assertEqual(true, is_available(Pool2, Id)),
    ?assertEqual(false, is_verified(Pool2, Id)),
    ?assertEqual(true, is_unverified(Pool2, Id)),
    {selected, {Id, _}, _} = random_select(Pool2, Now, both, undefined),
    {unavailable, _} = random_select(Pool2, Now, verified, undefined),
    {selected, {Id, _}, _} = random_select(Pool2, Now, unverified, undefined),

    {verified, Pool3} = verify(Pool2, Now, Id),
    ?assertEqual(true, is_available(Pool3, Id)),
    ?assertEqual(true, is_verified(Pool3, Id)),
    ?assertEqual(false, is_unverified(Pool3, Id)),
    {selected, {Id, _}, _} = random_select(Pool3, Now, both, undefined),
    {selected, {Id, _}, _} = random_select(Pool3, Now, verified, undefined),
    {unavailable, _} = random_select(Pool3, Now, unverified, undefined),

    % Can be called with an already verified peer.
    {verified, Pool4} = verify(Pool3, Now, Id),
    ?assertEqual(true, is_available(Pool4, Id)),
    ?assertEqual(true, is_verified(Pool4, Id)),
    ?assertEqual(false, is_unverified(Pool4, Id)),
    {selected, {Id, _}, _} = random_select(Pool4, Now, both, undefined),
    {selected, {Id, _}, _} = random_select(Pool4, Now, verified, undefined),
    {unavailable, _} = random_select(Pool4, Now, unverified, undefined),
    ok.

%% Tests the verification of a selected peer.
verification_of_selected_peer() ->
    seed_process_random(),
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),

    Peer = random_peer(),
    Id = aec_peer:id(Peer),

    {unverified, Pool2} = update(Pool1, Now, Peer),
    {selected, {Id, _}, Pool3} =
        random_select(Pool2, Now, unverified, undefined),
    ?assertEqual(false, is_available(Pool3, Id)),
    ?assertEqual(false, is_verified(Pool3, Id)),
    ?assertEqual(true, is_unverified(Pool3, Id)),

    {verified, Pool4} = verify(Pool3, Now, Id),
    ?assertEqual(false, is_available(Pool4, Id)),
    ?assertEqual(true, is_verified(Pool4, Id)),
    ?assertEqual(false, is_unverified(Pool4, Id)),
    {unavailable, _} = random_select(Pool4, Now, both, undefined),
    {unavailable, _} = random_select(Pool4, Now, verified, undefined),
    {unavailable, _} = random_select(Pool4, Now, unverified, undefined),

    % Can be called with an already verified peer.
    {verified, Pool5} = verify(Pool4, Now, Id),
    ?assertEqual(false, is_available(Pool5, Id)),
    ?assertEqual(true, is_verified(Pool5, Id)),
    ?assertEqual(false, is_unverified(Pool5, Id)),
    {unavailable, _} = random_select(Pool5, Now, both, undefined),
    {unavailable, _} = random_select(Pool5, Now, verified, undefined),
    {unavailable, _} = random_select(Pool5, Now, unverified, undefined),
    ok.

%% Tests the verification of a peer on standby.
verification_of_standby_peer() ->
    seed_process_random(),
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),

    Peer = random_peer(),
    Id = aec_peer:id(Peer),


    {unverified, Pool2} = update(Pool1, Now, Peer),
    {selected, {Id, _}, Pool3} =
        random_select(Pool2, Now, unverified, undefined),
    Pool4 = reject(Pool3, Now, Id),
    ?assertEqual(false, is_available(Pool4, Id)),
    ?assertEqual(false, is_verified(Pool4, Id)),
    ?assertEqual(true, is_unverified(Pool4, Id)),
    {wait, 5000, _} = random_select(Pool4, Now, both, undefined),
    {unavailable, _} = random_select(Pool4, Now, verified, undefined),
    {wait, 5000, _} = random_select(Pool4, Now, unverified, undefined),

    {verified, Pool5} = verify(Pool4, Now, Id),
    ?assertEqual(false, is_available(Pool5, Id)),
    ?assertEqual(true, is_verified(Pool5, Id)),
    ?assertEqual(false, is_unverified(Pool5, Id)),
    {wait, 5000, _} = random_select(Pool5, Now, both, undefined),
    {wait, 5000, _} = random_select(Pool5, Now, verified, undefined),
    {unavailable, _} = random_select(Pool5, Now, unverified, undefined),

    ok.

%% Tests when no free space can be allocated in the target bucket
%% of a new verified peer; the peer should stay in the unverified pool.
verification_canceled() ->
    seed_process_random(),
    PoolOpts = [{verif_bcount, 1}, {verif_bsize, 1} | ?POOL_OPTS],
    Pool1 = new(PoolOpts),
    Now = erlang:system_time(millisecond),

    Peer1 = random_peer(),
    Id1 = aec_peer:id(Peer1),

    Peer2 = random_peer(),
    Id2 = aec_peer:id(Peer2),


    {unverified, Pool2} =
        update(Pool1, Now, Peer1),
    {unverified, Pool3} =
        update(Pool2, Now, Peer2),

    {verified, Pool4} = verify(Pool3, Now, Id1),
    ?assertEqual({verified, true}, peer_state(Pool4, Id1)),
    ?assertEqual({unverified, true}, peer_state(Pool4, Id2)),

    {verified, Pool5} = verify(Pool4, Now, Id2),
    ?assertEqual({unverified, true}, peer_state(Pool5, Id1)),
    ?assertEqual({verified, true}, peer_state(Pool5, Id2)),

    % Now we block the peer by selecting it; nothing can be added to the bucket.
    Pool6 = select(Pool5, Now, Id2),
    ?assertEqual({unverified, true}, peer_state(Pool6, Id1)),
    ?assertEqual({verified, false}, peer_state(Pool6, Id2)),
    {unverified, Pool7} = verify(Pool6, Now, Id1),
    ?assertEqual({unverified, true}, peer_state(Pool7, Id1)),
    ?assertEqual({verified, false}, peer_state(Pool7, Id2)),
    ok.

%% Tests when a peer update is ignoring the update due to being unable to
%% allocate space in the unverified pool, or the peer address/trusted status
%% changed.
update_ignored() ->
    seed_process_random(),
    PoolOpts = [{unver_bcount, 1}, {unver_bsize, 1} | ?POOL_OPTS],
    Pool1 = new(PoolOpts),
    Now = erlang:system_time(millisecond),

    Peer1 = random_peer(),
    Id1 = aec_peer:id(Peer1),
    Peer1DifferentAddress = random_peer(#{pubkey => Id1}),
    Peer1Trusted = random_peer(#{pubkey => Id1,
                                 address => aec_peer:ip(Peer1),
                                 trusted => true}), %% it is false by default

    Peer2 = random_peer(),
    Id2 = aec_peer:id(Peer2),

    {unverified, Pool2} =
        update(Pool1, Now, Peer1),
    ?assertEqual({unverified, true}, peer_state(Pool2, Id1)),
    ?assertEqual({undefined, undefined}, peer_state(Pool2, Id2)),

    % Updating with a different address is ignored.
    {ignored, Pool3} =
        update(Pool2, Now, Peer1DifferentAddress),
    ?assertEqual({unverified, true}, peer_state(Pool3, Id1)),
    ?assertEqual({undefined, undefined}, peer_state(Pool3, Id2)),

    % Updating with a different trust status is ignored.
    {ignored, Pool4} = update(Pool3, Now, Peer1Trusted),
    ?assertEqual({unverified, true}, peer_state(Pool4, Id1)),
    ?assertEqual({undefined, undefined}, peer_state(Pool4, Id2)),

    % Normal update evict the old peer.
    {unverified, Pool5} =
        update(Pool4, Now, Peer2),
    ?assertEqual({undefined, undefined}, peer_state(Pool5, Id1)),
    ?assertEqual({unverified, true}, peer_state(Pool5, Id2)),

    % Selecting the peer so it cannot be evicted
    Pool6 = select(Pool5, Now, Id2),
    ?assertEqual({undefined, undefined}, peer_state(Pool6, Id1)),
    ?assertEqual({unverified, false}, peer_state(Pool6, Id2)),

    % Update should be ignored due to not being able to evict any peer.
    {ignored, Pool7} = update(Pool6, Now, Peer1),
    ?assertEqual({undefined, undefined}, peer_state(Pool7, Id1)),
    ?assertEqual({unverified, false}, peer_state(Pool7, Id2)),
    ok.

%% Tests when a peer evicted from the verified pool cannot be added to its
%% target bucket in the unverified pool due to not being able to free space.
downgrade_to_bucket_with_no_eviction_possible() ->
    seed_process_random(),
    PoolOpts = [
        {verif_bcount, 1}, {verif_bsize, 1},
        {unver_bcount, 1}, {unver_bsize, 1}
        | ?POOL_OPTS
    ],
    Pool1 = new(PoolOpts),
    Now = erlang:system_time(millisecond),

    RandomPeer1 = random_peer(),
    Id1 = aec_peer:id(RandomPeer1),

    RandomPeer2 = random_peer(),
    Id2 = aec_peer:id(RandomPeer2),

    {unverified, Pool2} =
        update(Pool1, Now, RandomPeer1),
    {verified, Pool3} = verify(Pool2, Now, Id1),
    {unverified, Pool4} =
        update(Pool3, Now, RandomPeer2),
    ?assertEqual({verified, true}, peer_state(Pool4, Id1)),
    ?assertEqual({unverified, true}, peer_state(Pool4, Id2)),

    % Select the unverified peer to lock it in the bucket.
    Pool5 = select(Pool4, Now, Id2),
    ?assertEqual({verified, true}, peer_state(Pool5, Id1)),
    ?assertEqual({unverified, false}, peer_state(Pool5, Id2)),

    % When verifying peer 2, peer 1 will be downgraded; but because there is
    % no space in the unverified bucket (the peer being upgraded is selected)
    % it is deleted. Not very realistic in itself, but theorically possible.
    {verified, Pool6} = verify(Pool5, Now, Id2),
    ?assertEqual({undefined, undefined}, peer_state(Pool6, Id1)),
    ?assertEqual({verified, false}, peer_state(Pool6, Id2)),

    ?assertEqual(1, count(Pool6, all, both)),
    ?assertEqual(1, count(Pool6, all, verified)),
    ?assertEqual(0, count(Pool6, all, unverified)),
    ?assertEqual(0, count(Pool6, available, both)),
    ?assertEqual(0, count(Pool6, available, verified)),
    ?assertEqual(0, count(Pool6, available, unverified)),
    ok.

%% Tests explicit peer selection behavior.
manual_selection_of_standby_peer() ->
    seed_process_random(),
    Now = erlang:system_time(millisecond),
    Pool1 = new(?POOL_OPTS),

    RandomPeer = random_peer(),
    Id = aec_peer:id(RandomPeer),
    false = aec_peer:is_trusted(RandomPeer),

    {unverified, Pool2} = update(Pool1, Now, RandomPeer),
    {selected, {Id, _}, Pool3} =
        random_select(Pool2, Now, unverified, undefined),
    Pool4 = reject(Pool3, Now, Id),
    ?assertEqual(false, is_available(Pool4, Id)),
    {wait, 5000, _} = random_select(Pool4, Now, both, undefined),
    {unavailable, _} = random_select(Pool4, Now, verified, undefined),
    {wait, 5000, _} = random_select(Pool4, Now, unverified, undefined),

    Pool5 = select(Pool4, Now, Id),
    ?assertEqual(false, is_available(Pool5, Id)),
    {unavailable, _} = random_select(Pool5, Now, both, undefined),
    {unavailable, _} = random_select(Pool5, Now, verified, undefined),
    {unavailable, _} = random_select(Pool5, Now, unverified, undefined),
    ok.

%% Tests that selected peers are not evicted from the unverified pool.
unverified_selected_are_not_evicted() ->
    seed_process_random(),
    % Use a single bucket of 10 peers to simplify testing.
    PoolOpts = [{unver_bcount, 1}, {unver_bsize, 10} | ?POOL_OPTS],
    Pool1 = new(PoolOpts),

    Now1 = erlang:system_time(millisecond),
    Peers = make_peers(200, 0),
    PeersList = [P || {_, P} <- maps:to_list(Peers)],

    {Pool2, Now2} =
        lists:foldl(
            fun(Peer, {P, N}) ->
                {unverified, P2} = update(P, N, Peer),
                {P2, N + 1000}
            end,
            {Pool1, Now1},
            lists:sublist(PeersList, 10)),

    % Select some of the peer to ensure they never get evicted.
    {selected, {SelId1, _}, Pool3} =
        random_select(Pool2, Now2, unverified, undefined),
    {selected, {SelId2, _}, Pool4} =
        random_select(Pool3, Now2, unverified, undefined),
    {selected, {SelId3, _}, Pool5} =
        random_select(Pool4, Now2, unverified, undefined),

    ?assertEqual(10, count(Pool5, all, both)),
    ?assertEqual( 0, count(Pool5, all, verified)),
    ?assertEqual(10, count(Pool5, all, unverified)),
    ?assertEqual( 7, count(Pool5, available, both)),
    ?assertEqual( 0, count(Pool5, available, verified)),
    ?assertEqual( 7, count(Pool5, available, unverified)),

    Pool6 =
        lists:foldl(
            fun(Peer, P) ->
                {unverified, P2} = update(P, Now2, Peer),
                P2
            end,
            Pool5,
            lists:sublist(PeersList, 11, 100)),

    ?assertEqual(true, is_unverified(Pool6, SelId1)),
    ?assertEqual(true, is_unverified(Pool6, SelId2)),
    ?assertEqual(true, is_unverified(Pool6, SelId3)),

    ?assertEqual(10, count(Pool6, all, both)),
    ?assertEqual( 0, count(Pool6, all, verified)),
    ?assertEqual(10, count(Pool6, all, unverified)),
    ?assertEqual( 7, count(Pool6, available, both)),
    ?assertEqual( 0, count(Pool6, available, verified)),
    ?assertEqual( 7, count(Pool6, available, unverified)),
    ok.

%% Tests that peers not updates since a configured time are removed from
%% the pool when trying to free space in a bucket.
unverified_old_peers_are_removed() ->
    seed_process_random(),
    % Use a single bucket of 10 peers to simplify testing.
    PoolOpts = [
        {unver_bcount, 1}, {unver_bsize, 10},
        {max_update_lapse, 30000}
        | ?POOL_OPTS
    ],
    Pool1 = new(PoolOpts),

    Now1 = erlang:system_time(millisecond),
    Peers = make_peers(20, 0),
    PeersList = [P || {_, P} <- maps:to_list(Peers)],

    {Pool2, Now2} =
        lists:foldl(
            fun(Peer, {P, N}) ->
                {unverified, P2} = update(P, N, Peer),
                {P2, N + 1000}
            end,
            {Pool1, Now1},
            lists:sublist(PeersList, 10)),

    % Select some of the peer to ensure they never get evicted.
    {selected, {SelId1, _}, Pool3} =
        random_select(Pool2, Now2, unverified, undefined),
    {selected, {SelId2, _}, Pool4} =
        random_select(Pool3, Now2, unverified, undefined),
    {selected, {SelId3, _}, Pool5} =
        random_select(Pool4, Now2, unverified, undefined),

    ?assertEqual(10, count(Pool5, all, both)),
    ?assertEqual( 0, count(Pool5, all, verified)),
    ?assertEqual(10, count(Pool5, all, unverified)),
    ?assertEqual( 7, count(Pool5, available, both)),
    ?assertEqual( 0, count(Pool5, available, verified)),
    ?assertEqual( 7, count(Pool5, available, unverified)),

    Now3 = Now2 + 30000,

    Peer11 = lists:nth(11, PeersList),
    {unverified, Pool6} = update(Pool5, Now3, Peer11),

    % Selected peers should NOT be removed
    ?assertEqual(true, is_unverified(Pool6, SelId1)),
    ?assertEqual(true, is_unverified(Pool6, SelId2)),
    ?assertEqual(true, is_unverified(Pool6, SelId3)),

    ?assertEqual(4, count(Pool6, all, both)),
    ?assertEqual(0, count(Pool6, all, verified)),
    ?assertEqual(4, count(Pool6, all, unverified)),
    ?assertEqual(1, count(Pool6, available, both)),
    ?assertEqual(0, count(Pool6, available, verified)),
    ?assertEqual(1, count(Pool6, available, unverified)),
    ok.

%% Tests the multi-reference handling in the unverified pool.
unverified_multiple_references() ->
    seed_process_random(),
    % Use only 10 buckets to make it faster.
    PoolOpts = [{unver_bcount, 10}, {unver_bsize, 10} | ?POOL_OPTS],
    MaxRefs = 8,

    Pool1 = new(PoolOpts),

    Now = erlang:system_time(millisecond),
    Peer = random_peer(),
    PeerId = aec_peer:id(Peer),
    Pool2 = unver_add_refs(Pool1, Now, Peer, random_source, MaxRefs, 10000),

    ?assertEqual(MaxRefs, aec_peers_pool:reference_count(Pool2, PeerId)),
    ?assertEqual(1, count(Pool2, all, both)),
    ?assertEqual(0, count(Pool2, all, verified)),
    ?assertEqual(1, count(Pool2, all, unverified)),
    ?assertEqual(1, count(Pool2, available, both)),
    ?assertEqual(0, count(Pool2, available, verified)),
    ?assertEqual(1, count(Pool2, available, unverified)),

    % Checks than 1000 more updates do not add anymore references.
    Pool3 = unver_add_refs(Pool2, Now, Peer, random_source, MaxRefs + 1, 1000),

    ?assertEqual(MaxRefs, aec_peers_pool:reference_count(Pool3, PeerId)),
    ?assertEqual(1, count(Pool3, all, both)),
    ?assertEqual(0, count(Pool3, all, verified)),
    ?assertEqual(1, count(Pool3, all, unverified)),
    ?assertEqual(1, count(Pool3, available, both)),
    ?assertEqual(0, count(Pool3, available, verified)),
    ?assertEqual(1, count(Pool3, available, unverified)),
    ok.

%% Tests evicting references do not delete the peer in the unverified pool.
unverified_reference_eviction() ->
    seed_process_random(),
    % Use only 2 buckets to make it faster.
    PoolOpts = [{unver_bcount, 2}, {unver_bsize, 1} | ?POOL_OPTS],
    Pool1 = new(PoolOpts),
    Now = erlang:system_time(millisecond),

    Peer1 = random_peer(),
    Id1 = aec_peer:id(Peer1),
    Addr1 = aec_peer:ip(Peer1),
    Src1a = aec_peer:source(Peer1),
    BIdx1a = aec_peers_pool:unverified_bucket_index(Pool1, Src1a, Addr1),
    % Find a source address that match to the other bucket...
    BIdx1b = abs(BIdx1a - 1),
    Src1b = find_unverified_source(Pool1, Addr1, BIdx1b),

    {unverified, Pool2} = update(Pool1, Now, Peer1),
    ?assertEqual(true, is_unverified(Pool2, Id1)),
    ?assertEqual(1, aec_peers_pool:reference_count(Pool2, Id1)),
    ?assertEqual(1, count(Pool2, all, both)),
    ?assertEqual(1, count(Pool2, available, both)),

    Pool3 = unver_add_refs(Pool2, Now, aec_peer:set_source(Peer1, Src1b), 2, 1000),
    ?assertEqual(true, is_unverified(Pool2, Id1)),
    ?assertEqual(2, aec_peers_pool:reference_count(Pool3, Id1)),
    ?assertEqual(1, count(Pool3, all, both)),
    ?assertEqual(1, count(Pool3, available, both)),

    Peer2 = random_peer(),
    Id2 = aec_peer:id(Peer2),

    {unverified, Pool4} = update(Pool3, Now, Peer2),
    ?assertEqual(true, is_unverified(Pool4, Id1)),
    ?assertEqual(true, is_unverified(Pool4, Id2)),
    ?assertEqual(1, aec_peers_pool:reference_count(Pool4, Id1)),
    ?assertEqual(2, count(Pool4, all, both)),
    ?assertEqual(2, count(Pool4, available, both)),
    ok.

%% Tests that selected and trusted peers are not evicted from the verified pool.
verified_selected_and_trusted_peers_are_not_evicted() ->
    seed_process_random(),
    PoolOpts = [
        {verif_bcount, 1}, {verif_bsize, 10},
        {unver_bcount, 5}, {unver_bsize, 10}
        | ?POOL_OPTS
    ],
    Pool1 = new(PoolOpts),

    Now = erlang:system_time(millisecond),
    Peers = make_peers(500, 2), % 2 peers are trusted
    TrustedPeers = [TrustedPeer1, TrustedPeer2] =
        [P || {_, P} <- maps:to_list(Peers), aec_peer:is_trusted(P)],
    TrustedId1 = aec_peer:id(TrustedPeer1),
    TrustedId2 = aec_peer:id(TrustedPeer2),
    UntrustedPeers = 
        [P || {_, P} <- maps:to_list(Peers), aec_peer:is_trusted(P) =:= false],
    PeersToPost = TrustedPeers ++ lists:sublist(UntrustedPeers, 8),

    UpdatePeers =
        fun(Pool, PList, Verify) ->
            lists:foldl(
                fun(Peer, P) ->
                    {_, P1} = update(P, Now, Peer),
                    case Verify of
                        true ->
                            Id = aec_peer:id(Peer),
                            {verified, P2} = verify(P1, Now, Id),
                            P2;
                        false -> P1
                    end
                end,
                Pool,
                PList)
        end,
    VerifyPeers =
        fun(Pool, PList) ->
            lists:foldl(
                fun(Peer, P) ->
                    Id = aec_peer:id(Peer),
                    {verified, P1} = verify(P, Now, Id),
                    P1
                end,
                Pool,
                PList)
        end,

    Pool2 = UpdatePeers(Pool1, PeersToPost, false),

    % Select some of the peer to ensure they never get evicted.
    {selected, {SelId1, _}, Pool4} =
        random_select(Pool2, Now, unverified, undefined),
    {selected, {SelId2, _}, Pool5} =
        random_select(Pool4, Now, unverified, undefined),
    {selected, {SelId3, _}, Pool6} =
        random_select(Pool5, Now, unverified, undefined),

    % Verify all the peers to fill the single verified bucket
    Pool7 = VerifyPeers(Pool6, PeersToPost),

    ?assertEqual(10, count(Pool7, all, both)),
    ?assertEqual(10, count(Pool7, all, verified)),
    ?assertEqual( 0, count(Pool7, all, unverified)),
    ?assertEqual( 7, count(Pool7, available, both)),
    ?assertEqual( 7, count(Pool7, available, verified)),
    ?assertEqual( 0, count(Pool7, available, unverified)),

    % Update and verify all the other peers.
    SecondBatchOfPeers = lists:sublist(UntrustedPeers, 9, 100),
    Pool8 = UpdatePeers(Pool7, SecondBatchOfPeers, true),

    % Selected peers didn't get evicted.
    ?assertEqual(true, is_verified(Pool8, SelId1)),
    ?assertEqual(true, is_verified(Pool8, SelId2)),
    ?assertEqual(true, is_verified(Pool8, SelId3)),
    % Trusted peers didn't get evicted.
    ?assertEqual(true, is_verified(Pool8, TrustedId1)),
    ?assertEqual(true, is_verified(Pool8, TrustedId2)),

    ?assert(60 >= count(Pool8, all, both)),
    ?assertEqual(10, count(Pool8, all, verified)),
    ?assert(50 >= count(Pool8, all, unverified)),
    ?assert(57 >= count(Pool8, available, both)),
    ?assertEqual(7, count(Pool8, available, verified)),
    ?assert(50 >= count(Pool8, available, unverified)),
    ok.

%% Tests that peers not updated for a configurable time are removed from the
%% verified pool buckets when trying to free space.
verified_old_peers_are_removed() ->
    seed_process_random(),
    % Use a single bucket of 10 peers to simplify testing.
    PoolOpts = [
        {verif_bcount, 1}, {verif_bsize, 10},
        {max_update_lapse, 30000}
        | ?POOL_OPTS
    ],
    Pool1 = new(PoolOpts),

    Now1 = erlang:system_time(millisecond),
    TrustedCnt = 2,
    Peers = make_peers(20, TrustedCnt),
    TrustedPeers = [TrustedPeer1, TrustedPeer2] =
        [P || {_, P} <- maps:to_list(Peers), aec_peer:is_trusted(P)],
    TrustedId1 = aec_peer:id(TrustedPeer1),
    TrustedId2 = aec_peer:id(TrustedPeer2),
    UntrustedPeers = 
        [P || {_, P} <- maps:to_list(Peers), aec_peer:is_trusted(P) =:= false],

    
    PeersToPost = TrustedPeers ++ lists:sublist(UntrustedPeers, 8),
    {Pool2, Now2} = lists:foldl(fun(Peer, {P, N}) ->
        {_, P2} = update(P, N, Peer),
        {P2, N + 1000}
    end, {Pool1, Now1}, PeersToPost),

    % Select some of the peer to ensure they never get evicted.
    {selected, {SelId1, _}, Pool3} =
        random_select(Pool2, Now2, unverified, undefined),
    {selected, {SelId2, _}, Pool4} =
        random_select(Pool3, Now2, unverified, undefined),
    {selected, {SelId3, _}, Pool5} =
        random_select(Pool4, Now2, unverified, undefined),

    % Verify all the peers to fill the single verified bucket
    Pool6 = lists:foldl(fun(Peer, P) ->
        Id = aec_peer:id(Peer),
        {verified, P2} = verify(P, Now2, Id),
        P2
    end, Pool5, PeersToPost),

    ?assertEqual(10, count(Pool6, all, both)),
    ?assertEqual(10, count(Pool6, all, verified)),
    ?assertEqual( 0, count(Pool6, all, unverified)),
    ?assertEqual( 7, count(Pool6, available, both)),
    ?assertEqual( 7, count(Pool6, available, verified)),
    ?assertEqual( 0, count(Pool6, available, unverified)),

    Now3 = Now2 + 30000,

    [Peer11 | _] = maps:values(Peers) -- PeersToPost,
    Peer11Trusted = aec_peer:set_trusted(Peer11, true),
    {verified, Pool7} = update(Pool6, Now3, Peer11Trusted),

    % Selected peers should NOT be removed
    ?assertEqual(true, is_verified(Pool7, SelId1)),
    ?assertEqual(true, is_verified(Pool7, SelId2)),
    ?assertEqual(true, is_verified(Pool7, SelId3)),
    % Neither trusted peers
    ?assertEqual(true, is_verified(Pool7, TrustedId1)),
    ?assertEqual(true, is_verified(Pool7, TrustedId2)),

    % But all the other should have been removed.
    ?assertEqual( 6, count(Pool7, all, both)),
    ?assertEqual( 6, count(Pool7, all, verified)),
    ?assertEqual( 0, count(Pool7, all, unverified)),
    ?assertEqual( 3, count(Pool7, available, both)),
    ?assertEqual( 3, count(Pool7, available, verified)),
    ?assertEqual( 0, count(Pool7, available, unverified)),
    ok.

%% Tests that the counters and internal structures stay synchronized when
%% performing a lot of different operations.
validate_counters() ->
    seed_process_random(),
    PoolOpts = [
        {verif_bcount, 256},
        {verif_bsize, 32},
        {verif_group_shard, 8},
        {unver_bcount, 1024},
        {unver_source_shard, 32},
        {unver_group_shard, 4},
        {unver_max_refs, 8},
        {max_update_lapse, 80000 * 500} % for the last 20000 rounds
        | ?POOL_OPTS
    ],
    Pool1 = new(PoolOpts),

    Now1 = erlang:system_time(millisecond),

    {Pool2, Now2, All2, Selected2} =
        lists:foldl(fun(_K, {P, N, A, S}) ->
            case {rand_int(1, 8), A, S} of
                {1, _, _} ->
                    case random_select(P, N, both, undefined) of
                        {wait, _, P2} -> {P2, N + 500, A, S};
                        {unavailable, P2} -> {P2, N + 500, A, S};
                        {selected, {Id, _}, P2} -> {P2, N + 500, A, [Id | S]}
                    end;
                {2, _, [_|_]} ->
                    {Id, S2} = rand_take(S),
                    P2 = aec_peers_pool:release(P, N, Id),
                    {P2, N + 500, A, S2};
                {3, _, [_|_]} ->
                    {Id, S2} = rand_take(S),
                    P2 = reject(P, N, Id),
                    {P2, N + 500, A, S2};
                {4, [_|_], _} ->
                    {{Id, _}, A2} = rand_take(A),
                    case peer_state(P, Id) of
                        {undefined, _} -> {P, N + 500, A2, S};
                        _ ->
                            {_, P2} = verify(P, N, Id),
                            {P2, N + 500, A, S}
                    end;
                {5, [_|_], _} ->
                    {{Id, Addr}, A2} = rand_take(A),
                    case peer_state(P, Id) of
                        {undefined, _} -> {P, N + 500, A2, S};
                        _ ->
                            RandomPeer = random_peer(#{pubkey => Id,
                                                       address => Addr}),
                            {_, P2} = update(P, N, RandomPeer),
                            {P2, N + 500, A, S}
                    end;
                _ ->
                    RandomPeer = random_peer(),
                    Id = aec_peer:id(RandomPeer),
                    Addr = aec_peer:ip(RandomPeer),
                    {_, P2} = update(P, N, RandomPeer),
                    {P2, N + 500, [{Id, Addr} | A], S}
            end
    end, {Pool1, Now1, [], []}, lists:seq(1, 100000)),

    % Release all selected peers.
    Pool3 = lists:foldl(fun(I, P) ->
        aec_peers_pool:release(P, Now2, I)
    end, Pool2, Selected2),

    % Delete all the peers.
    Pool4 = lists:foldl(fun({I, _}, P) ->
        case peer_state(P, I) of
            {undefined, _} -> P;
            _ -> aec_peers_pool:delete(P, I)
        end
    end, Pool3, All2),

    % Check counters are all zero as expected.
    ?assertEqual(0, count(Pool4, all, both)),
    ?assertEqual(0, count(Pool4, all, verified)),
    ?assertEqual(0, count(Pool4, all, unverified)),
    ?assertEqual(0, count(Pool4, available, both)),
    ?assertEqual(0, count(Pool4, available, verified)),
    ?assertEqual(0, count(Pool4, available, unverified)),
    ok.


%--- INTERNAL POOL DATA STRUCTURE TEST CASES -----------------------------------

%% Tests the internal pool data structure.
pool_buckets_manipulation_test() ->
    seed_process_random(),
    P1 = aec_peers_pool:pool_new(3, 4, 1, 1.2),
    ?assertEqual(0, aec_peers_pool:pool_bucket_size(P1, 0)),
    ?assertEqual(0, aec_peers_pool:pool_bucket_size(P1, 1)),
    ?assertEqual(0, aec_peers_pool:pool_bucket_size(P1, 2)),
    P2 = aec_peers_pool:pool_add(P1, 0, foo),
    P3 = aec_peers_pool:pool_add(P2, 2, bar),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P3, 0)),
    ?assertEqual(0, aec_peers_pool:pool_bucket_size(P3, 1)),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P3, 2)),
    P4 = aec_peers_pool:pool_add(P3, 1, buz),
    P5 = aec_peers_pool:pool_add(P4, 2, boz),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P5, 0)),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P5, 1)),
    ?assertEqual(2, aec_peers_pool:pool_bucket_size(P5, 2)),
    P6 = aec_peers_pool:pool_add(P5, 0, biz),
    P7 = aec_peers_pool:pool_add(P6, 1, bez),
    ?assertEqual(2, aec_peers_pool:pool_bucket_size(P7, 0)),
    ?assertEqual(2, aec_peers_pool:pool_bucket_size(P7, 1)),
    ?assertEqual(2, aec_peers_pool:pool_bucket_size(P7, 2)),
    P8 = aec_peers_pool:pool_del(P7, 1, buz),
    P9 = aec_peers_pool:pool_del(P8, 2, bar),
    ?assertEqual(2, aec_peers_pool:pool_bucket_size(P9, 0)),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P9, 1)),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P9, 2)),
    P10 = aec_peers_pool:pool_del(P9, 1, bez),
    P11 = aec_peers_pool:pool_del(P10, 0, foo),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P11, 0)),
    ?assertEqual(0, aec_peers_pool:pool_bucket_size(P11, 1)),
    ?assertEqual(1, aec_peers_pool:pool_bucket_size(P11, 2)),
    ok.

pool_make_space_test() ->
    seed_process_random(),
    RandState = rand_state(),
    KFilterFun = fun
        (V) when (V rem 2) =:= 0 -> keep;
        (_) -> evict
    end,
    RFilterFun = fun
        (V) when (V > 8) -> remove;
        (_) -> evict
    end,
    SortKeyFun = fun(V) -> -V end,

    % Set a heavy skew to "guarantee" the skew check.
    Pool1 = aec_peers_pool:pool_new(1, 16, 4, 2.0),

    ?assertMatch({free_space, [], undefined, _, _},
                 aec_peers_pool:pool_make_space(Pool1, RandState, 0,
                                                KFilterFun, SortKeyFun)),
    ?assertMatch({free_space, [], undefined, _, _},
                 aec_peers_pool:pool_make_space(Pool1, RandState, 0,
                                                RFilterFun, SortKeyFun)),

    Pool2 = lists:foldl(fun(V, P) ->
        aec_peers_pool:pool_add(P, 0, V)
    end, Pool1, lists:seq(0, 10)),

    ?assertMatch({free_space, [], undefined, _, _},
                 aec_peers_pool:pool_make_space(Pool2, RandState, 0,
                                                KFilterFun, SortKeyFun)),
    ?assertMatch({free_space, [], undefined, _, _},
                 aec_peers_pool:pool_make_space(Pool2, RandState, 0,
                                                RFilterFun, SortKeyFun)),

    Pool3 = lists:foldl(fun(V, P) ->
        aec_peers_pool:pool_add(P, 0, V)
    end, Pool2, lists:seq(11, 15)),

    % Check that there is space and the entries are removed.
    {free_space, Removed, undefined, _, Pool3b} =
        aec_peers_pool:pool_make_space(Pool3, RandState, 0,
                                       RFilterFun, SortKeyFun),
    ?assertEqual([9, 10, 11, 12, 13, 14, 15], lists:sort(Removed)),
    ?assertEqual(9, aec_peers_pool:pool_bucket_size(Pool3b, 0)),

    % Check that only the entries marked for eviction are elected.
    {_, EvictCounters} = lists:foldl(fun(_, {R, M}) ->
        {free_space, [], Evicted, R2, _} =
            aec_peers_pool:pool_make_space(Pool3, R, 0, KFilterFun, SortKeyFun),
        ?assert(Evicted =/= undefined),
        ?assert((Evicted rem 2) =/= 0),
        M2 = maps:put(Evicted, maps:get(Evicted, M, 0) + 1, M),
        {R2, M2}
    end, {RandState, #{}}, lists:seq(1, 5000)),

    % Check that the eviction is skewed toward the larger values
    % (due to the sort key).
    SortedEvictions = lists:keysort(2, maps:to_list(EvictCounters)),
    ?assertEqual(8, length(SortedEvictions)),
    [{LessEvicted, _} | _] = SortedEvictions,
    {MostEvicted, _} = lists:last(SortedEvictions),
    ?assert(LessEvicted =< 3),
    ?assert(MostEvicted >= 13),
    ok.

%--- INTERNAL LOOKUP TABLE DATA STRUCTURE TEST CASES ---------------------------

%% Tests the growth/shrink of the internal data structure for lookup.
lookup_growth_test() ->
    seed_process_random(),
    % Assumes minimum internal size is 8 and maximum size increment is 128.
    StartSize = 8,
    MaxInc = 128,

    Lookup = aec_peers_pool:lookup_new(),
    ?assertEqual(StartSize, aec_peers_pool:lookup_internal_size(Lookup)),
    ?assertEqual(0, aec_peers_pool:lookup_size(Lookup)),
    % Grow the lookup table to 300.
    Lookup2 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V * 3),
        Size = aec_peers_pool:lookup_size(L2),
        ?assertEqual(V + 1, Size),
        FreeSpace = aec_peers_pool:lookup_internal_free(L2),
        ?assert((FreeSpace >= 0)
                and (FreeSpace < min(max(Size, StartSize), MaxInc))),
        L2
    end, Lookup, lists:seq(0, 299)),
    % Check values.
    lists:foreach(fun(V) ->
    ?assertEqual(V * 3, aec_peers_pool:lookup_get(Lookup2, V))
    end, lists:seq(0, 299)),
    % Shrink the lookup table to 0.
    Lookup3 = lists:foldl(fun(V, L) ->
        L2 = aec_peers_pool:lookup_shrink(L),
        Size = aec_peers_pool:lookup_size(L2),
        ?assertEqual(V, Size),
        ?assert(aec_peers_pool:lookup_internal_size(L2) >= StartSize),
        FreeSpace = aec_peers_pool:lookup_internal_free(L2),
        ?assert((Size < StartSize)
                or ((FreeSpace >= 0)
                    and (FreeSpace < min(max(Size, StartSize), MaxInc)))),
        L2
    end, Lookup2, lists:seq(299, 0, -1)),
    ?assertEqual(StartSize, aec_peers_pool:lookup_internal_size(Lookup3)),
    ok.

%% Tests the lookup internal data structure swapping of elements.
lookup_swap_test() ->
    seed_process_random(),
    Lookup1 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V * 3),
        L2
    end, aec_peers_pool:lookup_new(), lists:seq(0, 29)),
    { 5 * 3, 12 * 3, Lookup2} = aec_peers_pool:lookup_swap(Lookup1,  5, 12),
    {10 * 3, 17 * 3, Lookup3} = aec_peers_pool:lookup_swap(Lookup2, 10, 17),
    {12 * 3, 10 * 3, Lookup4} = aec_peers_pool:lookup_swap(Lookup3,  5, 17),
    ?assertEqual(10 * 3, aec_peers_pool:lookup_get(Lookup4,  5)),
    ?assertEqual(17 * 3, aec_peers_pool:lookup_get(Lookup4, 10)),
    ?assertEqual( 5 * 3, aec_peers_pool:lookup_get(Lookup4, 12)),
    ?assertEqual(12 * 3, aec_peers_pool:lookup_get(Lookup4, 17)),
    ok.

%% Tests adding elements to the internal lookup data structure.
lookup_randomized_add_test() ->
    seed_process_random(),
    lists:foldl(fun(V, {L, R}) ->
        case aec_peers_pool:lookup_add(L, R, V * 3) of
            {0, undefined, R2, L2} ->
                {L2, R2};
            {1, undefined, R2, L2} ->
                {L2, R2};
            {I, {I2, O}, R2, L2} ->
                ?assert(V =/= I),
                ?assert(I2 =/= I),
                ?assertEqual(V * 3, aec_peers_pool:lookup_get(L2, I)),
                ?assertEqual(O, aec_peers_pool:lookup_get(L2, I2)),
                {L2, R2}
        end
    end, {aec_peers_pool:lookup_new(), rand_state()}, lists:seq(0, 29)),
    ok.

%% Tests deleting elements from the internal lookup data structure.
lookup_randomized_del_test() ->
    seed_process_random(),
    L1 = aec_peers_pool:lookup_new(),
    {0, L2} = aec_peers_pool:lookup_append(L1, foo),
    {1, L3} = aec_peers_pool:lookup_append(L2, bar),
    {2, L4} = aec_peers_pool:lookup_append(L3, buz),
    {3, L5} = aec_peers_pool:lookup_append(L4, boz),
    ?assertEqual(4, aec_peers_pool:lookup_size(L5)),

    {{0, boz}, _} = aec_peers_pool:lookup_del(L5, 0),
    {{1, boz}, L6} = aec_peers_pool:lookup_del(L5, 1),
    {{2, boz}, _} = aec_peers_pool:lookup_del(L5, 2),
    {undefined, _} = aec_peers_pool:lookup_del(L5, 3),

    ?assertEqual(3, aec_peers_pool:lookup_size(L6)),
    ?assertEqual(boz, aec_peers_pool:lookup_get(L6, 1)),

    {{0, buz}, L7} = aec_peers_pool:lookup_del(L6, 0),
    {{1, buz}, _} = aec_peers_pool:lookup_del(L6, 1),
    {undefined, _} = aec_peers_pool:lookup_del(L6, 2),

    ?assertEqual(2, aec_peers_pool:lookup_size(L7)),
    ?assertEqual(buz, aec_peers_pool:lookup_get(L7, 0)),

    {{0, boz}, _} = aec_peers_pool:lookup_del(L7, 0),
    {undefined, L8} = aec_peers_pool:lookup_del(L7, 1),

    ?assertEqual(1, aec_peers_pool:lookup_size(L8)),

    {undefined, L9} = aec_peers_pool:lookup_del(L8, 0),

    ?assertEqual(0, aec_peers_pool:lookup_size(L9)),
    ok.

%% Test the internal lookup data structure selection.
lookup_select_test() ->
    seed_process_random(),
    R = rand_state(),
    Excluded = [0, 1, 2, 6, 18, 22, 27],
    ExcludeFun = make_int_exclude_filter(Excluded),
    Lookup1 = aec_peers_pool:lookup_new(),

    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup1, R, true, undefined),
    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup1, R, true, undefined),
    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup1, R, false, ExcludeFun),
    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup1, R, false, ExcludeFun),

    {0, Lookup2} = aec_peers_pool:lookup_append(Lookup1, 0),

    {0, _} = aec_peers_pool:lookup_select(Lookup2, R, true, undefined),
    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup2, R, true, ExcludeFun),

    Lookup3 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V),
        L2
    end, Lookup2, lists:seq(1, 2)),

    {V3a, _} = aec_peers_pool:lookup_select(Lookup3, R, true, undefined),
    ?assert(lists:member(V3a, lists:seq(0, 2))),
    {unavailable, _} =
        aec_peers_pool:lookup_select(Lookup3, R, true, ExcludeFun),

    Lookup4 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V),
        L2
    end, Lookup3, lists:seq(3, 29)),

    {V4a, _} = aec_peers_pool:lookup_select(Lookup4, R, true, undefined),
    ?assert(lists:member(V4a, lists:seq(0, 29))),
    {V4b, _} = aec_peers_pool:lookup_select(Lookup4, R, true, ExcludeFun),
    ?assert(lists:member(V4b, lists_difference(lists:seq(0, 29), Excluded))),
    ok.

%% Tests the internal lookup data structure sampling.
lookup_sample_test() ->
    seed_process_random(),
    R = rand_state(),
    Excluded = [0, 1, 2, 6, 18, 22, 27],
    ExcludeFun = make_int_exclude_filter(Excluded),
    Lookup1 = aec_peers_pool:lookup_new(),

    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, all, undefined),
    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, all, ExcludeFun),
    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, 1, undefined),
    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, 1, ExcludeFun),
    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, 10, undefined),
    {[], _} = aec_peers_pool:lookup_sample(Lookup1, R, true, 10, ExcludeFun),

    Lookup2 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V),
        L2
    end, Lookup1, lists:seq(0, 3)),

    {S2a, _} = aec_peers_pool:lookup_sample(Lookup2, R, true, all, undefined),
    ?assertEqual(lists:seq(0, 3), lists:sort(S2a)),
    {[3], _} = aec_peers_pool:lookup_sample(Lookup2, R, false, all, ExcludeFun),
    {[V2a], _} = aec_peers_pool:lookup_sample(Lookup2, R, true, 1, undefined),
    ?assert(lists:member(V2a, lists:seq(0, 3))),
    {[3], _} = aec_peers_pool:lookup_sample(Lookup2, R, false, 1, ExcludeFun),
    {S2b, _} = aec_peers_pool:lookup_sample(Lookup2, R, true, 2, undefined),
    ?assertEqual(2, length(lists_intersection(S2b, lists:seq(0, 3)))),
    {[3], _} = aec_peers_pool:lookup_sample(Lookup2, R, false, 2, ExcludeFun),
    {S2c, _} = aec_peers_pool:lookup_sample(Lookup2, R, true, 10, undefined),
    ?assertEqual(lists:seq(0, 3), lists:sort(S2c)),
    {[3], _} = aec_peers_pool:lookup_sample(Lookup2, R, false, 10, ExcludeFun),

    Lookup3 = lists:foldl(fun(V, L) ->
        {V, L2} = aec_peers_pool:lookup_append(L, V),
        L2
    end, Lookup2, lists:seq(4, 29)),

    {S3a, _} = aec_peers_pool:lookup_sample(Lookup3, R, true, all, undefined),
    ?assertEqual(lists:seq(0, 29), lists:sort(S3a)),
    {S3b, _} = aec_peers_pool:lookup_sample(Lookup3, R, false, all, ExcludeFun),
    ?assertEqual(lists_difference(lists:seq(0, 29), Excluded), lists:sort(S3b)),
    {[V3a], _} = aec_peers_pool:lookup_sample(Lookup3, R, true, 1, undefined),
    ?assert(lists:member(V3a, lists:seq(0, 29))),
    {[V3b], _} = aec_peers_pool:lookup_sample(Lookup3, R, false, 1, ExcludeFun),
    ?assert(lists:member(V3b, lists_difference(lists:seq(0, 29), Excluded))),
    {S3c, _} = aec_peers_pool:lookup_sample(Lookup3, R, true, 2, undefined),
    ?assertEqual(2, length(lists_intersection(S3c, lists:seq(0, 29)))),
    {S3d, _} = aec_peers_pool:lookup_sample(Lookup3, R, false, 2, ExcludeFun),
    ?assertEqual(2, length(lists_intersection(S3d,
        lists_difference(lists:seq(0, 29), Excluded)))),
    {S3e, _} = aec_peers_pool:lookup_sample(Lookup3, R, true, 10, undefined),
    ?assertEqual(10, length(lists_intersection(S3e, lists:seq(0, 29)))),
    {S3f, _} = aec_peers_pool:lookup_sample(Lookup3, R, false, 10, ExcludeFun),
    ?assertEqual(10, length(lists_intersection(S3f,
        lists_difference(lists:seq(0, 29), Excluded)))),
    {S3g, _} = aec_peers_pool:lookup_sample(Lookup3, R, true, 100, undefined),
    ?assertEqual(lists:seq(0, 29), lists:sort(S3g)),
    {S3h, _} = aec_peers_pool:lookup_sample(Lookup3, R, false, 100, ExcludeFun),
    ?assertEqual(lists_difference(lists:seq(0, 29), Excluded), lists:sort(S3h)),
    ok.

%--- BUCKET SELECTION TEST CASES -----------------------------------------------

%% Tests the bucket selection algorithm.
bucket_index_test_() -> [
    {"Unverified bucket index selection for same source group",
     {timeout, 100, fun test_unverified_bucket_source_group_selection/0}},
    {"Unverified bucket index selection for same source and peer group",
     {timeout, 100, fun test_unverified_bucket_groups_selection/0}},
    {"Verified bucket index selection for same peer group",
     {timeout, 100, fun test_verified_bucket_peer_group_selection/0}}
].

%% Tests that for the same source address group, there is at most 64 possible
%% unverified buckets selected.
%% Checks that at least 90% of the buckets get selected during the test.
test_unverified_bucket_source_group_selection() ->
    seed_process_random(),
    PoolOpts = [
        {unver_bcount, 1024},
        {unver_source_shard, 64},
        {unver_group_shard, 4}
        | ?POOL_OPTS
    ],
    Pool = new(PoolOpts),
    SourceGroups = [{rand_byte(), rand_byte()} || _  <- lists:seq(1, 100)],
    SelectedBuckets = lists:map(fun({A, B}) ->
        Addresses = [{random_address(A, B), random_address()}
                     || _ <- lists:seq(1, 10000)],
        Buckets = [aec_peers_pool:unverified_bucket_index(Pool, S, P)
                   || {S, P} <- Addresses],
        BucketCount = length(lists:usort(Buckets)),
        % At least 55 different bucket indexes
        ?assertMatch(_ when (BucketCount =< 64) and (BucketCount >= 55), BucketCount),
        Buckets
    end, SourceGroups),
    SelectedBucketsCount = length(lists:usort(lists:flatten(SelectedBuckets))),
    ?assert((SelectedBucketsCount =< 1024) and (SelectedBucketsCount >= 922)),
    ok.

%% Tests that for the same source group and peer group, there is at most 4
%% possible unverified buckets selected.
%% Checks that at least 90% of the buckets get selected during the test.
test_unverified_bucket_groups_selection() ->
    seed_process_random(),
    PoolOpts = [
        {unver_bcount, 1024},
        {unver_source_shard, 64},
        {unver_group_shard, 4}
        | ?POOL_OPTS
    ],
    Pool = new(PoolOpts),
    Groups = [{rand_byte(), rand_byte(), rand_byte(), rand_byte()}
              || _  <- lists:seq(1, 1000)],
    SelectedBuckets = lists:map(fun({A, B, C, D}) ->
        Addresses = [{random_address(A, B), random_address(C, D)}
                     || _ <- lists:seq(1, 1000)],
        Buckets = [aec_peers_pool:unverified_bucket_index(Pool, S, P)
                   || {S, P} <- Addresses],
        BucketCount = length(lists:usort(Buckets)),
        % At least 3 different bucket indexes
        ?assertMatch(_ when (BucketCount =< 4) and (BucketCount >= 3), BucketCount),
        Buckets
    end, Groups),
    SelectedBucketsCount = length(lists:usort(lists:flatten(SelectedBuckets))),
    ?assert((SelectedBucketsCount =< 1024) and (SelectedBucketsCount >= 922)),
    ok.

%% Tests that for the same peer group, there is at most 8 possible verified
%% buckets selected.
%% Verifies that at least 90% of the buckets get selected during the test.
test_verified_bucket_peer_group_selection() ->
    seed_process_random(),
    PoolOpts = [
        {verif_bcount, 256},
        {verif_group_shard, 8}
        | ?POOL_OPTS
    ],
    Pool = new(PoolOpts),
    SourceGroups = [{rand_byte(), rand_byte()} || _  <- lists:seq(1, 1000)],
    SelectedBuckets = lists:map(fun({A, B}) ->
        Addresses = [random_address(A, B) || _ <- lists:seq(1, 1000)],
        Buckets = [aec_peers_pool:verified_bucket_index(Pool, P)
                   || P <- Addresses],
        BucketCount = length(lists:usort(Buckets)),
        % At least 5 different bucket indexes
        ?assertMatch(_ when (BucketCount =< 8) and (BucketCount >= 5), BucketCount),
        Buckets
    end, SourceGroups),
    SelectedBucketsCount = length(lists:usort(lists:flatten(SelectedBuckets))),
    ?assert((SelectedBucketsCount =< 256) and (SelectedBucketsCount >= 230)),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

-ifdef(USE_FIXED_SEED).

seed_process_random() ->
    rand:seed(exrop, ?RANDOM_PROCESS_SEED).

rand_state() ->
    rand:seed_s(exrop, ?RANDOM_POOL_SEED).

-else.

seed_process_random() ->
    rand:seed(exrop).

rand_state() ->
    rand:seed_s(exrop).

-endif.

rand_byte() ->
    rand:uniform(256) - 1.

rand_int(Max) ->
    rand:uniform(Max) - 1.

rand_int(Min, Max) ->
    Min + rand_int(Max - Min).

rand_int_list(Min, Max, Size) ->
    rand_int_list(Min, Max, Size, 0, []).

rand_int_list(_Min, _Max, Size, Size, Acc) -> Acc;
rand_int_list(Min, Max, Size, Curr, Acc) ->
    RandInt = rand_int(Min, Max),
    case lists:member(RandInt, Acc) of
        true -> rand_int_list(Min, Max, Size, Curr, Acc);
        false -> rand_int_list(Min, Max, Size, Curr + 1, [RandInt | Acc])
    end.

rand_peek(Col) when is_list(Col)->
    lists:nth(rand_int(1, length(Col)), Col).

rand_take(Col) when is_list(Col)->
    {L1, [R | L2]} = lists:split(rand_int(length(Col)), Col),
    {R, L1 ++ L2}.

random_peer_id() ->
    A = rand_int(1, 1234567),
    <<A:32/unit:8>>.

random_address() ->
    {rand_byte(), rand_byte(), rand_byte(), rand_byte()}.

random_address(A, B) ->
    {A, B, rand_byte(), rand_byte()}.

make_peers(TotalCount, TrustedCount) ->
    ?assert(TotalCount >= TrustedCount),
    make_peers(TotalCount, TrustedCount, #{}).

make_peers(0, _, Acc) -> Acc;
make_peers(TotalCount, TrustedCount,  Acc) ->
    Peer = random_peer(#{trusted => TrustedCount > 0}), 
    make_peers(TotalCount - 1, TrustedCount - 1,
               Acc#{aec_peer:id(Peer) => Peer}).

make_ext_exclude_filter(ExcludePeerIds) ->
    fun(PeerId, _) -> not lists:member(PeerId, ExcludePeerIds) end.

make_int_exclude_filter(ExcludePeerIds) ->
    fun(PeerId) -> not lists:member(PeerId, ExcludePeerIds) end.

lists_intersection(L1, L2) ->
    lists_intersection(L1, L2, []).

lists_intersection([], _L2, Acc) ->
    lists:reverse(Acc);
lists_intersection([V | L1], L2, Acc) ->
    case (not lists:member(V, L1)) and lists:member(V, L2) of
        true -> lists_intersection(L1, L2, [V | Acc]);
        false -> lists_intersection(L1, L2, Acc)
    end.

lists_difference(L1, L2) ->
    lists_difference(L1, L2, []).

lists_difference([], _L2, Acc) ->
    lists:reverse(Acc);
lists_difference([V | L1], L2, Acc) ->
    case lists:member(V, L1) or lists:member(V, L2) of
        false -> lists_difference(L1, L2, [V | Acc]);
        true -> lists_difference(L1, L2, Acc)
    end.

find_unverified_source(Pool, Addr, BucketIdx) ->
    Src = random_address(),
    case aec_peers_pool:unverified_bucket_index(Pool, Src, Addr) of
        BucketIdx -> Src;
        _ -> find_unverified_source(Pool, Addr, BucketIdx)
    end.

select_all(Pool, Now, Target, FilterFun) ->
    select_all(Pool, Now, Target, FilterFun, []).

select_all(Pool, Now, Target, FilterFun, Acc) ->
    case random_select(Pool, Now, Target, FilterFun) of
        {unavailable, Pool2} -> {Acc, Pool2};
        {selected, {I, _}, Pool2} ->
            select_all(Pool2, Now, Target, FilterFun, [I | Acc])
    end.

unver_add_refs(Pool, Now, Peer, Count, Max) ->
    unver_add_refs(Pool, Now, Peer, keep_source, Count, Max).

unver_add_refs(Pool, _Now, _Peer, _Source, _Count, 0) -> Pool;
unver_add_refs(Pool, Now, Peer0, SourceAtom, Count, Max) ->
    Peer =
        case SourceAtom of
            random_source -> aec_peer:set_source(Peer0, random_address());
            keep_source -> Peer0
        end,
    {unverified, Pool2} = update(Pool, Now, Peer),
    PeerId = aec_peer:id(Peer),
    case aec_peers_pool:reference_count(Pool2, PeerId) =:= Count of
        true -> Pool2;
        false ->
            unver_add_refs(Pool2, Now, Peer, SourceAtom, Count, Max - 1)
    end.

random_peer() ->
    random_peer(#{}).

random_peer(Opts) ->
    PeerInfo =
        maps:merge(#{ pubkey => random_peer_id(), host => <<>>,
                      port => rand_int(4000, 4999) },
                   Opts),
    aec_peer:new(maps:get(address, Opts, random_address()),
                 maps:get(source, Opts, random_address()), 
                 PeerInfo,
                 maps:get(trusted, Opts, false)).



db_setup() ->
    Persist = application:get_env(aecore, persist),
    application:set_env(aecore, persist, false),
    aec_db:check_db(),
    aec_db:prepare_mnesia_bypass(),
    aec_db:clear_db(),
    Persist.

db_teardown(Persist) ->
    application:stop(mnesia),
    application:set_env(aecore, persist, Persist),
    ok = mnesia:delete_schema([node()]).

-endif.
