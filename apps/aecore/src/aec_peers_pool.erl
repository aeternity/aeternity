%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc Stochastic pool.
%%%
%%% Opaque data structure that handle a collection of peers.
%%% Implements the design described in:
%%%   [https://github.com/aeternity/protocol/blob/master/GOSSIP.md]
%%%
%%% Usage:
%%% <ul>
%%%   <li>When setting up, add trusted peers:
%%%     <ul>
%%%       <li>Call {@link update/7} with the trust flag set; this will add
%%%           the peer to the verified pool and prevent it from ever being
%%%           evicted.
%%%       </li>
%%%     </ul>
%%%   </li>
%%%   <li>When receiving a peer through gossip:
%%%     <ul>
%%%       <li>Call {@link update/7}; if this is a new peer this will add the
%%%           peer to the unverifiedpool; if the peer is already pooled it will
%%%           just refresh it.
%%%       </li>
%%%     </ul>
%%%   </li>
%%%   <li>To select a peer to connect to:
%%%     <ul>
%%%       <li>Call {@link random_select/4}; this will return a peer and mark it
%%%           as selected so it is not returned from any further selection.
%%%       </li>
%%%     </ul>
%%%   </li>
%%%   <li>When an outbound connection failed:
%%%     <ul>
%%%       <li>Call {@link reject/3}; this put the connection on standby for a
%%%           time related to the number of time it got rejected (exponentional
%%%           backoff). After the standby time has passed it will be selectable
%%%           again.
%%%       </li>
%%%     </ul>
%%%   </li>
%%%   <li>When an outbound connection succeed:
%%%     <ul>
%%%       <li>Call {@link verify/3} to move the peer to the verified pool.</li>
%%%     </ul>
%%%   </li>
%%%   <li>When a connection is closed:
%%%     <ul>
%%%       <li>Call {@link release/3}; this will reset the rejection counter and
%%%           make the peer selectable again.
%%%       </li>
%%%     </ul>
%%%   </li>
%%%   <li>When an inbound connection is established and the first gossip
%%%    message is received:
%%%     <ul>
%%%       <li>Call {@link update/7} to add the connecting peer to the pool.</li>
%%%       <li>Call {@link verify/3} to move it to the verified pool.</li>
%%%       <li>Call {@link select/4} to mark it as selected.</li>
%%%     </ul>
%%%   </li>
%%%   <li>When building a gossip message
%%%     <ul>
%%%       <li>Call {@link random_subset/3} to get a random subset of the pooled
%%%           peer</li>
%%%     </ul>
%%%   </li>
%%% </ul>
%%%
%%% This data structur do not make any assumtion about the time; it requires
%%% it to be passed as parameter to function requiring it.
%%% Some functions takes a time in millisecond as it would be returned from
%%% `erlang:system_time(millisecond)' and uses it as if it was the current time.
%%% Every calls should be given a time greater or equal to the last call.
%%%
%%% To support selecting peers from a new address group, a filter function
%%% must be given to {@link random_select/4} that will reject peers from the
%%% current connections address groups.
%%%
%%% @end
%%%=============================================================================
-module(aec_peers_pool).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== EXPORTS ===================================================================

-export([address_group/1]).
-export([new/1]).
-export([count/3]).
-export([peer_state/2]).
-export([is_verified/2]).
-export([is_unverified/2]).
-export([is_available/2]).
-export([update/7]).
-export([verify/3]).
-export([random_subset/3]).
-export([random_select/4]).
-export([select/3]).
-export([reject/3]).
-export([release/3]).
-export([delete/2]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

%=== MACROS ====================================================================

-define(ST, ?MODULE).

%% The size in bytes of the generated secret.
-define(SECRET_SIZE, 128).
%% The algorithm to use for weak random number generator.
-define(RAND_ALGO, exsplus).
%% The initial size of the lookup tables.
-define(LOOKUP_START_SIZE, 8).
%% The maximum increment of size of the lookup tables underlying arrays;
%% must be a multiple of ?LOOKUP_START_SIZE.
-define(MAX_LOOKUP_SIZE_INC, ?LOOKUP_START_SIZE * 16).

%% Backoff lookup table for standby duration in seconds.
-define(BACKOFF_TIMES, [5, 15, 30, 60, 120, 300, 600]).
%% Maximum number of times a peer can get rejected;
%% when reached, the peer is downgraded/removed (if not trusted).
-define(MAX_REJECTIONS, 7).

%% The default number of buckets in the verified pool.
-define(DEFAULT_VERIF_BUCKET_COUNT,      256).
%% The default number of peers in each verified pool buckets.
-define(DEFAULT_VERIF_BUCKET_SIZE,        32).
%% The default number of possible buckets for the same peer address group.
-define(DEFAULT_VERIF_GROUP_SHARD,         8).
%% The default number of buckets in the unverified pool.
-define(DEFAULT_UNVER_BUCKET_COUNT,     1024).
%% The default number of peers in each unverified buckets.
-define(DEFAULT_UNVER_BUCKET_SIZE,        64).
%% The default number of possible buckets for the same source address group.
%% MUST BE A MULTIPLE OF ?DEFAULT_UNVER_GROUP_SHARD.
-define(DEFAULT_UNVER_SOURCE_SHARD,       64).
%% The default number of possible buckets for the same peer
%% and source address groups.
-define(DEFAULT_UNVER_GROUP_SHARD,         4).
%% The default maximum number of unverified peer references
%% in the unverified pool.
-define(DEFAULT_UNVER_MAX_REFS,            8).
%% The default eviction skew toward older peers;
%% it is used to skew the randomly evicted peer.
-define(DEFAULT_EVICTION_SKEW,           1.2).
%% The default probability of selecting a peer from the verified pool.
-define(DEFAULT_SELECT_VERIFIED_PROB,    0.5).
%% The default time without a peer being updated after which it get removed.
-define(DEFAULT_MAX_UPDATE_LAPSE,         30 * 24 * 50 * 60 * 1000). % 30 days


%=== TYPES =====================================================================

-record(peer, {
    % The peer unique identifier.
    id                     :: binary(),
    % If the peer is trusted and should never be downgraded.
    trusted = false        :: boolean(),
    % The peer's IP address.
    addr                   :: peer_addr(),
    % The IP address of the source of the peer.
    source                 :: peer_addr(),
    % Some opaque extra information.
    extra                  :: extra(),
    % The index of the verified pool's bucket the peer is located in.
    vidx                   :: non_neg_integer() | undefined,
    % A list of unverified pool bucket index the peer is located in.
    uidxs = []             :: [non_neg_integer()],
    % If the peer has been selected.
    selected = false       :: boolean(),
    % The number of time peers got rejected.
    rejected = 0           :: non_neg_integer(),
    % The time the peer was last updated.
    update_time            :: pos_integer() | undefined,
    % The time the peer was last selected.
    select_time            :: pos_integer() | undefined,
    % The time the peer was last rejected.
    reject_time            :: pos_integer() | undefined,
    % The index in the randomized lookup table for all peers.
    lookup_all_idx         :: non_neg_integer() | undefined,
    % The index in the randomized lookup table for available verified peers.
    lookup_verif_idx       :: non_neg_integer() | undefined,
    % The index in the randomized lookup table for available unverified peers.
    lookup_unver_idx       :: non_neg_integer() | undefined
}).

-record(pool, {
    % The number of peers in the pool.
    size                   :: non_neg_integer(),
    % The maximum number of references in the pool.
    max_refs               :: pos_integer(),
    % The eviction skew.
    skew                   :: float(),
    % The number of buckets in the pool.
    bucket_count           :: pos_integer(),
    % The pool's bucket size.
    bucket_size            :: pos_integer(),
    % The pool's buckets.
    buckets                :: buckets(peer_id())
}).

-record(lookup, {
    % The number of element in the lookup table.
    size                   :: non_neg_integer(),
    % The array of element; its size may be larger.
    array                  :: array:array(peer_id())
}).

-record(?MODULE, {
    % The secret to randomize the pool.
    secret                 :: binary(),
    % The state of the weak random generator.
    rand                   :: rand_state(),
    % The map of all the pooled peers.
    peers                  :: peer_map(),
    % The set of peers that have been rejected and are now on standby.
    standby                :: #{peer_id() => true},
    % The verified pool.
    verif_pool             :: pool(),
    % The unverified pool.
    unver_pool             :: pool(),
    % The randomized list of all peers.
    lookup_all             :: lookup(),
    % The randomized list of verified peers that are neither selected
    % nor rejected.
    lookup_verif           :: lookup(),
    % The randomized list of unverified peers that are neither selected
    % nor rejected.
    lookup_unver           :: lookup(),
    % The probability to select a verified peer when selecting from both pools.
    select_verif_prob      :: float(),
    % The time after which a peer got removed if never updated.
    max_update_lapse       :: pos_integer(),
    % The sharding configuration.
    verif_group_shard      :: pos_integer(),
    unver_source_shard     :: pos_integer(),
    unver_group_shard      :: pos_integer(),
    % If a strong random number should be used as an offset of weak ransdom
    % number for random_select/4 and random_subset/3.
    use_rand_offset        :: boolean()
}).

-type state() :: #aec_peers_pool{}.
-type pool() :: #pool{}.
-type lookup() :: #lookup{}.
-type peer() :: #peer{}.
-type peer_map() :: #{peer_id() => peer()}.
-type extra() :: term().
-type rand_state() :: term().
-type peer_id() :: binary().
-type peer_addr() :: inet:ip_address().
-type ext_peer() :: {peer_id(), term()}.
-type millitimestamp() :: pos_integer().
-type milliseconds() :: pos_integer().
-type bucket(Type) :: [Type].
-type buckets(Type) :: array:array(bucket(Type)).
-type select_target() :: verified | unverified | both.
-type ext_filter_fun() :: fun((peer_id(), extra()) -> boolean()).
-type int_filter_fun() :: fun((peer_id()) -> boolean()).
-type bucket_filter_fun() :: fun((peer_id()) -> keep | remove | evict).
-type bucket_sort_key_fun() :: fun((peer_id()) -> term()).
-type select_fun() :: fun((state(), millitimestamp(),
                           int_filter_fun() | undefined)
    -> {unavailable, state()} | {peer_id(), state()}).
-type options() :: [option()].
-type option() :: {verif_bcount, pos_integer()}
                | {verif_bsize, pos_integer()}
                | {verif_group_shard, pos_integer()}
                | {unver_bcount, pos_integer()}
                | {unver_bsize, pos_integer()}
                | {unver_source_shard, pos_integer()}
                | {unver_group_shard, pos_integer()}
                | {unver_max_refs, pos_integer()}
                | {eviction_skew, float()}
                | {select_verif_prob, float()}
                | {max_update_lapse, pos_integer()}
                | {secret, binary()}
                | {seed, {integer(), integer(), integer()}}
                | {disable_strong_random, boolean()}.

%=== API FUNCTIONS =============================================================

%% @doc Returns a binary representing the address group of given IP address.
%% Only supports IPv4 for now.
-spec address_group(peer_addr()) -> binary().
address_group({A, B, _, _}) -> <<A:8, B:8>>.

%% @doc Creates a new stochastic pool.
%%
%% Options:
%% <ul>
%%  <li>`verif_bcount': Number of buckets in the verified pool.
%%    Default: `256'.</li>
%%  <li>`verif_bsize': Verified pool's buckets size. Default: `32'.</li>
%%  <li>`verif_group_shard': The number of possible buckets for the same peer
%%    address group. Default: `8'.</li>
%%  <li>`unver_bcount': Number of buckets in the unverified pool.
%%    Default: `1024'.</li>
%%  <li>`unver_bsize': Unverified pool's bucket size. Default: `64'.</li>
%%  <li>`unver_source_shard': The default number of possible buckets for the
%%    same source address group. Default: `64'.
%%    This <b>must</b> be a multiple of `unver_group_shard'.</li>
%%  <li>`unver_group_shard': The default number of possible buckets for the same
%%    peer and source address groups. Default: `4'.</li>
%%  <li>`unver_max_refs': Maximum number of peer references in the unverified
%%    pool. Default: `8'.</li>
%%  <li>`eviction_skew': The skew of the eviction algorithm. `1.0' means no
%%    skew, and the larger the value the more the eviction is skewed toward the
%%    oldest peers. Default: `1.2'.</li>
%%  <li>`select_verif_prob': The probability of selecting a verified peer when
%%    selecting from both pools. Regardless of the probability, if there are no
%%    peers in one of the pool, it will be taken from the other one.
%%    Default: `0.5'.</li>
%%  <li>`max_update_lapse': The maximum time after which a peer is removed if
%%    never updated in milliseconds. Default: `30 days'.</li>
%%  <li>`secret': A secret binary used to further randomize the pool. If not
%%    specified a random one will be generated.</li>
%%  <li>`seed': A seed for the weak random generator. If not specified, a seed
%%    will be generated from a strong source of randomness.</li>
%%  <li>`disable_strong_random': If `true' no strong random number will be used
%%    as offset for {@link random_subset/3} and {@link random_select/4};
%%    this ensure reproducibility if a specific seed was provided.</li>
%% </ul>
-spec new(options()) -> state().
new(Opts) ->
    Secret = get_opt(secret, Opts, gen_secret()),
    Seed = get_opt(seed, Opts, gen_seed()),
    VBCount = get_opt(verif_bcount, Opts, ?DEFAULT_VERIF_BUCKET_COUNT),
    VBSize = get_opt(verif_bsize, Opts, ?DEFAULT_VERIF_BUCKET_SIZE),
    VGroupShard = get_opt(verif_group_shard, Opts, ?DEFAULT_VERIF_GROUP_SHARD),
    UBCount = get_opt(unver_bcount, Opts, ?DEFAULT_UNVER_BUCKET_COUNT),
    UBSize = get_opt(unver_bsize, Opts, ?DEFAULT_UNVER_BUCKET_SIZE),
    USourceShard = get_opt(unver_source_shard, Opts, ?DEFAULT_UNVER_SOURCE_SHARD),
    UGroupShard = get_opt(unver_group_shard, Opts, ?DEFAULT_UNVER_GROUP_SHARD),
    UMaxRef = get_opt(unver_max_refs, Opts, ?DEFAULT_UNVER_MAX_REFS),
    EvictSkew = get_opt(eviction_skew, Opts, ?DEFAULT_EVICTION_SKEW),
    SelectProb = get_opt(select_verif_prob, Opts, ?DEFAULT_SELECT_VERIFIED_PROB),
    MaxLapse = get_opt(max_update_lapse, Opts, ?DEFAULT_MAX_UPDATE_LAPSE),
    DisableStrongRandom = get_opt(disable_strong_random, Opts, false),
    if (VBCount =< 0) -> erlang:error(badarg);
       (VBSize =< 0) -> erlang:error(badarg);
       (VGroupShard =< 0) -> erlang:error(badarg);
       (UBCount =< 0) -> erlang:error(badarg);
       (UBSize =< 0) -> erlang:error(badarg);
       (USourceShard =< 0) -> erlang:error(badarg);
       (UGroupShard =< 0) -> erlang:error(badarg);
       (USourceShard rem UGroupShard) > 0 -> erlang:error(badarg);
       (UMaxRef < 1) -> erlang:error(badarg);
       (SelectProb < 0) or (SelectProb > 1) -> erlang:error(badarg);
       (MaxLapse =< 0) -> erlang:error(badarg);
       true -> ok
    end,
    RSt = rand:seed_s(?RAND_ALGO, Seed),
    #?ST{
        secret = Secret,
        rand = RSt,
        peers = #{},
        standby = #{},
        verif_pool = pool_new(VBCount, VBSize, 1, EvictSkew),
        unver_pool = pool_new(UBCount, UBSize, UMaxRef, EvictSkew),
        lookup_all = lookup_new(),
        lookup_verif = lookup_new(),
        lookup_unver = lookup_new(),
        select_verif_prob = SelectProb,
        max_update_lapse = MaxLapse,
        verif_group_shard = VGroupShard,
        unver_group_shard = UGroupShard,
        unver_source_shard = USourceShard div UGroupShard,
        use_rand_offset = not DisableStrongRandom
    }.

%% @doc Counts the pooled peers given discriminators.
%%
%% Descriminators:
%% <ul>
%%  <li>`all' + `both': Returns the total number of pooled peers.</li>
%%  <li>`all' + `verified': Returns the total number of verified peers.</li>
%%  <li>`all' + `unverified': Returns the total number of unverified peers.</li>
%%  <li>`available' + `both': Returns the number of peers selectable peers.</li>
%%  <li>`available' + `verified': Returns the number of selectable verified
%%    peers.</li>
%%  <li>`available' + `unverified': Returns the number of selectable unverified
%%    peers.</li>
%% </ul>
%%
%% Note that the result of `available' peers do not take into account any
%% additional restrictions that could be given to {@link random_select/4}.
%%
%% In addition, peers currently in standby that may have exhausted there standby
%% time will <b>not</b> be counted.
-spec count(state(), all | available, select_target()) -> non_neg_integer().
count(St, all, both) ->
    #?ST{verif_pool = VerifPool, unver_pool = UnverPool} = St,
    pool_size(VerifPool) + pool_size(UnverPool);
count(St, all, verified) ->
    #?ST{verif_pool = VerifPool} = St,
    pool_size(VerifPool);
count(St, all, unverified) ->
    #?ST{unver_pool = UnverPool} = St,
    pool_size(UnverPool);
count(St, available, both) ->
    #?ST{lookup_verif = VerifLookup, lookup_unver = UnverLookup} = St,
    lookup_size(VerifLookup) + lookup_size(UnverLookup);
count(St, available, verified) ->
    #?ST{lookup_verif = VerifLookup} = St,
    lookup_size(VerifLookup);
count(St, available, unverified) ->
    #?ST{lookup_unver = UnverLookup} = St,
    lookup_size(UnverLookup).

%% @doc Returns where a peer identifier is pooled and if it is available.
-spec peer_state(state(), peer_id())
    -> {verified | unverified | undefined, boolean() | undefined}.
peer_state(St, PeerId) ->
    #?ST{standby = Standby} = St,
    case find_peer(St, PeerId) of
        undefined ->
            {undefined, undefined};
        Peer ->
            IsAvailable = not (peer_is_selected(Peer)
                               or maps:is_key(PeerId, Standby)),
            {peer_state(Peer), IsAvailable}
    end.

%% @doc Returns if the given peer identifier is pooled as verified.
%% If given an unknown identifier it returns `undefined'.
-spec is_verified(state(), peer_id()) -> undefined | boolean().
is_verified(St, PeerId) ->
    case find_peer(St, PeerId) of
        undefined -> undefined;
        Peer -> peer_state(Peer) =:= verified
    end.

%% @doc Returns if the given peer identifier is pooled as unverified.
%% If given an unknown identifier it returns `undefined'.
-spec is_unverified(state(), peer_id()) -> undefined | boolean().
is_unverified(St, PeerId) ->
    case find_peer(St, PeerId) of
        undefined -> undefined;
        Peer -> peer_state(Peer) =:= unverified
    end.

%% @doc Returns if the given peer identifier is available for selection.
%% If given an unknown identifier it returns `undefined'.
-spec is_available(state(), peer_id()) -> undefined | boolean().
is_available(St, PeerId) ->
    #?ST{standby = Standby} = St,
    case find_peer(St, PeerId) of
        undefined -> undefined;
        Peer -> not (peer_is_selected(Peer) or maps:is_key(PeerId, Standby))
    end.

%% @doc Updates/inserts a peer.
%%
%% It adds or refresh a peer.
%%
%% If it is the first time this peer is seen, it is added to the verified
%% pool if trusted, or to the unverified pool otherwise.
%%
%% If the peer is already pooled but its address changed, the update is ignored;
%% this is so the pool cannot get poisoned by nodes gossiping wrong addresses
%% for valid peers. If a peer rightfully changed its address, the old one will
%% get removed after the standard retry procedure; at that point, the new one
%% will be added. Note that this could take quite some time.
%%
%% If the peer is already in the verified pool, nothing is done beside updating
%% the peer last update time and source address.
%%
%% If the peer is already in the unverified pool but has not reached the maximum
%% number of references, another reference may be added to the unverified pool;
%% the peer last update time and source address fields are updated too.
%%
%% Some opaque term can be specified; this term will be returned by
%% {@link random_select/4} alongside the peer identifier. This could be used by
%% the caller to store extra connection information like protocol and port
%% number.
%% This extra information will <b>not</b> be updated if the peer already exists.
-spec update(state(), millitimestamp(), peer_id(),
             peer_addr(), peer_addr(), boolean(), extra())
    -> {verified | unverified | ignored, state()}.
update(St, Now, PeerId, PeerAddr, SourceAddr, IsTrusted, Extra) ->
    ?assertNotEqual(undefined, PeerId),
    ?assertNotEqual(undefined, PeerAddr),
    ?assertNotEqual(undefined, SourceAddr),
    case update_peer(St, Now, PeerId, PeerAddr, SourceAddr, IsTrusted, Extra) of
        ignored ->
            {ignored, St};
        {updated, St2} ->
            Result = case IsTrusted of
                true -> verified_maybe_add(St2, Now, PeerId);
                false -> unverified_maybe_add(St2, Now, PeerId, undefined)
            end,
            case Result of
                {ignored, St3} ->
                    % Couldn't be added to any pool; must be removed.
                    {ignored, del_peer(St3, PeerId)};
                _ -> Result
            end
    end.

%% @doc Marks a peer as verified.
%%
%% If the peer is in the unverified pool, it is added to the verified pool
%% and all its references are removed from the unverified pool. If for some
%% reason the insertion in the verified pool is not possible, the peer stays in
%% the unverified pool.
-spec verify(state(), millitimestamp(), peer_id())
    -> {verified | ignored, state()}.
verify(St, Now, PeerId) ->
    verified_maybe_add(St, Now, PeerId).

%% @doc Returns a random subset of all the pooled peers.
%%
%% Return a list of peer identifiers and there corresponding extra data given
%% the <b>first</b> time it was added by {@link update/7}.
%%
%% A filtering function can be specified to limit the possible results.
-spec random_subset(state(), all | pos_integer(), ext_filter_fun() | undefined)
    -> {[ext_peer()], state()}.
random_subset(St, Size, ExtFilterFun) ->
    #?ST{rand = RState, use_rand_offset = ROffset, lookup_all = Lookup} = St,
    IntFilterFun = wrap_filter_fun(St, ExtFilterFun),
    {PeerIds, RState2} = lookup_sample(Lookup, RState, ROffset,
                                       Size, IntFilterFun),
    Result = export_results(St, PeerIds),
    {Result, St#?ST{rand = RState2}}.

%% @doc Select a random peer from the available ones.
%%
%% The target parameter is used to select from which pool the peer should be
%% selected:
%% <ul>
%%  <li>`verified': Only verified peers will get selected.</li>
%%  <li>`unverified': Only unverified peers will get selected.</li>
%%  <li>`both': A peer will be selected from any of the pools; the pool the peer
%%    will come from will depend on the configured probability option
%%    `select_verif_prob'.</li>
%% </ul>
%%
%% A peer is considered available if it is not selected and if it is not in
%% standby after being rejected; for a peer to become available again,
%% {@link release/3} has to be called or the standby time for rejection has to
%% be exhausted after calling {@link reject/3}.
%%
%% A filter function can be specified to restrict further the set of peer
%% that can be selected.
%%
%% If a peer is available in targeted pools, the call returns the peer
%% identifier and its extra data given the <b>first</b> time it was added by
%% {@link update/7}.
%%
%% If there is no peers readily available because they are on standby, the call
%% returns the minimum delay afterwich one will get out of standby.
%%
%% When a peer is returned it is considered selected; it means that it will
%% <b>never</b> be returned by @see select/3 again until it is released with
%% {@link release/3} or marked as rejected with {@link reject/3} and the standby
%% time is exausted.
-spec random_select(state(), millitimestamp(), select_target(),
             ext_filter_fun() | undefined)
    -> {selected, ext_peer(), state()}
     | {wait, milliseconds(), state()}
     | {unavailable, state()}.
random_select(St, Now, Target, FilterFun) ->
    select_peer(St, Now, Target, FilterFun).

%% @doc Marks a peer as selected.
%%
%% This call is intended to mark a peer retrieved through other means than
%% {@link random_select/4} as selected. For example, a peer that was just added
%% to the pool with {@link update/7} and {@link verify/3}.
%%
%% If the peer wasn't selected, this call mark it as selected; it means that it
%% will <b>never</b> be returned by {@link select/3} again until it is released
%% with {@link release/3} or marked as rejected with {@link reject/3}.
%%
%% If the peer was in standby due to rejection, this call removes it from
%% the standby list, but it doesn't reset the rejection counter.
-spec select(state(), millitimestamp(), peer_id()) -> state().
select(St, Now, PeerId) ->
    make_selected(St, Now, PeerId).

%% @doc Marks a peer as rejected.
%%
%% This means that the peer couldn't be contacted and shouldn't be selected
%% again before some time has passed. The time the peer will stay on standby
%% depends on the number of time it got rejected.
%%
%% If the peer reached the maximum number of rejections, it is downgraded to
%% the unverified pool if verified, or removed completely if unverified; the
%% rejection counter is reset when downgrading.
-spec reject(state(), millitimestamp(), peer_id()) -> state().
reject(St, Now, PeerId) ->
    reject_peer(St, Now, PeerId).

%% @doc Releases a peer that was previously selected/verified.
%%
%% The rejection counter for the peer is reset.
%%
%% Remove the select mark from the peer; after this call, it can be returned
%% again by {@link random_select/4}.
-spec release(state(), millitimestamp(), peer_id()) -> state().
release(St, Now, PeerId) ->
    release_peer(St, Now, PeerId).

%% @doc Deletes given peer from the pool.
-spec delete(state(), peer_id()) -> state().
delete(St, PeerId) ->
    del_peer(St, PeerId).

%=== INTERNAL FUNCTIONS ========================================================

-spec get_opt(atom(), options(), term()) -> term().
get_opt(Key, Opts, Default) when is_list(Opts) ->
    proplists:get_value(Key, Opts, Default).

%% Generate a new secret from a strong random source.
-spec gen_secret() -> binary().
gen_secret() ->
    crypto:strong_rand_bytes(?SECRET_SIZE).

%% Generate a seed for the weak random generator from a strong random source.
-spec gen_seed() -> {integer(), integer(), integer()}.
gen_seed() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(4 * 3),
    {A, B, C}.

%% Generates a strongly random 16 bits unsinged integer.
-spec strong_randword(boolean()) -> non_neg_integer().
strong_randword(false) -> 0;
strong_randword(true) ->
    <<Result:16/unsigned-integer>> = crypto:strong_rand_bytes(2),
    Result.

%% Returns a weak random integer `X` where `0 <= X < MAX`
-spec randint(rand_state(), non_neg_integer())
    -> {non_neg_integer(), rand_state()}.
randint(RSt, Max) ->
    {RandInt, RSt2} = rand:uniform_s(Max, RSt),
    {RandInt - 1, RSt2}.

%% Generates a weak random integer `X' where `0 <= X < MAX' with a skewed
%% distribution. If the given skew is `1.0' the distribution is uniform.
%% The larger than `1.0'  the skew is, the more the distribution is skewed
%% toward the small values.
-spec skewed_randint(rand_state(), non_neg_integer(), number())
    -> {non_neg_integer(), rand_state()}.
skewed_randint(RSt, Max, Skew) ->
    {RandFloat, RSt2} = rand:uniform_s(RSt),
    {floor(Max * math:pow(RandFloat, Skew)), RSt2}.

-spec safe_min(undefined | number(), number()) -> number().
safe_min(undefined, Value) -> Value;
safe_min(Value1, Value2) -> min(Value1, Value2).

%% Returns the time in miliseconds a peer should stay in standby when rejected.
-spec rejection_delay(pos_integer()) -> pos_integer().
rejection_delay(RejectionCount) ->
    BackoffIndex = min(RejectionCount, length(?BACKOFF_TIMES)),
    lists:nth(BackoffIndex, ?BACKOFF_TIMES) * 1000.

%% Tells if we can add another reference to a pool.
-spec should_add_ref(rand_state(), pos_integer()) -> {boolean(), rand_state()}.
should_add_ref(RSt, RefCount) ->
    {RandVal, RSt2} = randint(RSt, floor(math:pow(2, RefCount))),
    {RandVal =:= 0, RSt2}.

%% Returns the modulo of the integer extracted from the SHA1 hash of the binary.
-spec hash_modulo(binary(), pos_integer()) -> non_neg_integer().
hash_modulo(Bin, Modulo) ->
    <<I:160/little-unsigned-integer>> = crypto:hash(sha, Bin),
    I rem Modulo.

%% Returns a binary describing the given IP address.
%% Only supports IPv4 for now.
-spec address_descriptor(peer_addr()) -> binary().
address_descriptor({A, B, C, D}) -> <<A:8, B:8, C:8, D:8>>.

%--- STATE HANDLING FUNCTIONS --------------------------------------------------

-ifdef(TEST).

reference_count(St, PeerId) ->
    #peer{uidxs = Idxs} = get_peer(St, PeerId),
    length(Idxs).

-endif.

%% Returns a peer record if it exists, `undefined' otherwise.
-spec find_peer(state(), peer_id()) -> undefined | peer().
find_peer(St, PeerId) ->
    #?ST{peers = Peers} = St,
    case maps:find(PeerId, Peers) of
        error -> undefined;
        {ok, Peer} -> Peer
    end.

%% Gets a peer record by peer identifier; fails if the id doesn't exists.
-spec get_peer(state(), peer_id()) -> peer().
get_peer(St, PeerId) ->
    #?ST{peers = Peers} = St,
    #{PeerId := Peer} = Peers,
    Peer.

%% Sets a peer record by peer identifier; fails if the id doesn't exists.
-spec set_peer(state(), peer_id(), peer()) -> state().
set_peer(St, PeerId, Peer) ->
    #?ST{peers = Peers} = St,
    Peers2 = Peers#{PeerId := Peer},
    St#?ST{peers = Peers2}.

%% Updates or adds a peer; only the source address can be updated.
-spec update_peer(state(), millitimestamp(), peer_id(), peer_addr(),
                  peer_addr(), boolean(), extra())
    -> ignored | {updated, state()}.
update_peer(St, Now, PeerId, PeerAddr, SourceAddr, IsTrusted, Extra) ->
    case find_peer(St, PeerId) of
        undefined ->
            #?ST{peers = Peers} = St,
            Peer = peer_new(PeerId, PeerAddr, SourceAddr, IsTrusted, Extra),
            Peer2 = Peer#peer{update_time = Now},
            Peers2 = Peers#{PeerId => Peer2},
            St2 = St#?ST{peers = Peers2},
            {updated, add_lookup_all(St2, PeerId)};
        #peer{addr = PeerAddr, trusted = IsTrusted} = CurrPeer ->
            Peer2 = CurrPeer#peer{update_time = Now, source = SourceAddr},
            {updated, set_peer(St, PeerId, Peer2)};
        _ ->
            ignored
    end.

%% Deletes a peer and all its references.
-spec del_peer(state(), peer_id()) -> state().
del_peer(St, PeerId) ->
    St2 = standby_del(St, PeerId),
    St3 = verified_del(St2, PeerId),
    St4 = unverified_del(St3, PeerId),
    St5 = lists:foldl(fun({LookupRecField, PeerRecField}, S) ->
        #?ST{peers = Peers} = S,
        Lookup = element(LookupRecField, S),
        {Lookup2, Peers2} =
            peers_lookup_del(Peers, PeerId, Lookup, PeerRecField),
        S2 = setelement(LookupRecField, S, Lookup2),
        S2#?ST{peers = Peers2}
    end, St4, [
        {#?ST.lookup_all, #peer.lookup_all_idx},
        {#?ST.lookup_verif, #peer.lookup_verif_idx},
        {#?ST.lookup_unver, #peer.lookup_unver_idx}
    ]),
    #?ST{peers = Peers} = St5,
    ?assert(maps:is_key(PeerId, Peers)),
    ?assertEqual(false, (maps:get(PeerId, Peers))#peer.selected),
    St5#?ST{peers = maps:remove(PeerId, Peers)}.

%% Returns the order in which pools should be used to select a peer.
-spec select_order(state(), select_target()) -> {[select_fun()], state()}.
select_order(St, verified) ->
    {[fun verified_select/3], St};
select_order(St, unverified) ->
    {[fun unverified_select/3], St};
select_order(St, both) ->
    #?ST{rand = RSt, select_verif_prob = Prob} = St,
    IntProb = floor(Prob * 1000),
    {RandInt, RSt2} = randint(RSt, 1001),
    St2 = St#?ST{rand = RSt2},
    case RandInt < IntProb of
        true -> {[fun verified_select/3, fun unverified_select/3], St2};
        false -> {[fun unverified_select/3, fun verified_select/3], St2}
    end.

%% Selects a peer from the pools in given order using given restrictions.
-spec select_peer(state(), millitimestamp(), select_target(),
                  ext_filter_fun() | undefined)
    -> {unavailable, state()}
     | {selected, peer_id(), state()}
     | {wait, milliseconds(), state()}.
select_peer(St, Now, Target, ExtFilterFun) ->
    IntFilterFun = wrap_filter_fun(St, ExtFilterFun),
    {Order, St2} = select_order(St, Target),
    St3 = standby_refresh(St2, Now),
    case select_available_peer(St3, Now, Order, IntFilterFun) of
        {selected, _, _} = Result -> Result;
        {unavailable, St4} ->
            select_standby_peer(St4, Now, Target, IntFilterFun)
    end.

%% Selects a peer using given selection functions.
-spec select_available_peer(state(), millitimestamp(), [select_fun()],
                            int_filter_fun() | undefined)
    -> {unavailable, state()} | {selected, peer_id(), state()}.
select_available_peer(St, _Now, [], _FilterFun) ->
    {unavailable, St};
select_available_peer(St, Now, [Selector | Rest], FilterFun) ->
    case Selector(St, Now, FilterFun) of
        {unavailable, St2} ->
            select_available_peer(St2, Now, Rest, FilterFun);
        {PeerId, St2} ->
            St3 = make_selected(St2, Now, PeerId),
            {selected, export_result(St3, PeerId), St3}
    end.

%% Return the minimum time to wait for a standby peer matching given
%% target and restrictions.
-spec select_standby_peer(state(), millitimestamp(), select_target(),
                          int_filter_fun() | undefined)
    -> {unavailable, state()} | {wait, milliseconds(), state()}.
select_standby_peer(St, Now, Target, FilterFun) ->
    #?ST{peers = Peers, standby = Standby} = St,
    MinExpiration = maps:fold(fun(PeerId, _, Min) ->
        #{PeerId := Peer} = Peers,
        PeerState = peer_state(Peer),
        ?assertNotEqual(undefined, PeerState),
        IsAccepted = (FilterFun =:= undefined) orelse FilterFun(PeerId),
        case IsAccepted and ((Target =:= both) or (PeerState =:= Target)) of
            true -> safe_min(Min, peer_standby_recovery_time(Peer));
            false -> Min
        end
    end, undefined, Standby),
    case MinExpiration of
        undefined -> {unavailable, St};
        Exp -> {wait, Exp - Now, St}
    end.

% Makes a peer unavailable.
-spec make_selected(state(), millitimestamp(), peer_id()) -> state().
make_selected(St, Now, PeerId) ->
    Peer = get_peer(St, PeerId),
    ?assertEqual(false, Peer#peer.selected),
    Peer2 = peer_select(Peer, Now),
    St2 = set_peer(St, PeerId, Peer2),
    St3 = standby_del(St2, PeerId),
    make_unavailable(St3, PeerId).

%% Rejects a selected peer.
-spec reject_peer(state(), millitimestamp(), peer_id()) -> state().
reject_peer(St, Now, PeerId) ->
    St2 = make_unavailable(St, PeerId),
    Peer = get_peer(St2, PeerId),
    ?assertEqual(true, Peer#peer.selected),
    Peer2 = peer_reject(Peer, Now),
    Peer3 = peer_deselect(Peer2, Now),
    St3 = set_peer(St2, PeerId, Peer3),
    case {peer_has_expired(Peer3), peer_state(Peer3)} of
        {true, unverified} ->
            del_peer(St3, PeerId);
        {true, verified} ->
            verified_downgrade(St3, Now, PeerId);
        {false, _} ->
            standby_add(St3, PeerId)
    end.

%% Releases a selected peer.
-spec release_peer(state(), millitimestamp(), peer_id()) -> state().
release_peer(St, Now, PeerId) ->
    Peer = get_peer(St, PeerId),
    ?assertEqual(true, Peer#peer.selected),
    Peer2 = peer_reset(Peer, Now),
    Peer3 = peer_deselect(Peer2, Now),
    St2 = set_peer(St, PeerId, Peer3),
    make_available(St2, PeerId).

%% Puts a peer on standby.
-spec standby_add(state(), peer_id()) -> state().
standby_add(St, PeerId) ->
    #?ST{standby = Standby} = St,
    Standby2 = Standby#{PeerId => true},
    St#?ST{standby = Standby2}.

%% removed a peer from standby.
-spec standby_del(state(), peer_id()) -> state().
standby_del(St, PeerId) ->
    #?ST{standby = Standby} = St,
    Standby2 = maps:remove(PeerId, Standby),
    St#?ST{standby = Standby2}.

%% Checks peers on standby and make them available again if they exausted
%% there standby time.
-spec standby_refresh(state(), millitimestamp()) -> state().
standby_refresh(St0, Now) ->
    #?ST{standby = Standby} = St0,
    lists:foldl(fun(PeerId, St) ->
        Peer = get_peer(St, PeerId),
        case peer_is_in_standby(Peer, Now) of
            true -> St;
            false ->
                St2 = set_peer(St, PeerId, Peer),
                St3 = make_available(St2, PeerId),
                standby_del(St3, PeerId)
        end
    end, St0, maps:keys(Standby)).

%% Make a peer unavailable for selection.
-spec make_available(state(), peer_id()) -> state().
make_available(St, PeerId) ->
    Peer = get_peer(St, PeerId),
    case {peer_state(Peer), Peer} of
        {verified, #peer{lookup_verif_idx = undefined}} ->
            add_lookup_verif(St, PeerId);
        {unverified, #peer{lookup_unver_idx = undefined}} ->
            add_lookup_unver(St, PeerId);
        _ -> St
    end.

%% Make a peer unavailable for selection.
-spec make_unavailable(state(), peer_id()) -> state().
make_unavailable(St, PeerId) ->
    Peer = get_peer(St, PeerId),
    case {peer_state(Peer), Peer} of
        {verified, #peer{lookup_verif_idx = Idx}} when Idx =/= undefined ->
            del_lookup_verif(St, PeerId);
        {unverified, #peer{lookup_unver_idx = Idx}} when Idx =/= undefined ->
            del_lookup_unver(St, PeerId);
        _ -> St
    end.

-spec add_lookup_all(state(), peer_id()) -> state().
add_lookup_all(St, PeerId) ->
    #?ST{peers = Peers, rand = Rand, lookup_all = Lookup} = St,
    {Lookup2, Rand2, Peers2} =
        peers_lookup_add(Peers, Rand, PeerId, Lookup, #peer.lookup_all_idx),
    St#?ST{peers = Peers2, rand = Rand2, lookup_all = Lookup2}.

-spec add_lookup_verif(state(), peer_id()) -> state().
add_lookup_verif(St, PeerId) ->
    #?ST{peers = Peers, rand = Rand, lookup_verif = Lookup} = St,
    {Lookup2, Rand2, Peers2} =
        peers_lookup_add(Peers, Rand, PeerId, Lookup, #peer.lookup_verif_idx),
    St#?ST{peers = Peers2, rand = Rand2, lookup_verif = Lookup2}.

-spec add_lookup_unver(state(), peer_id()) -> state().
add_lookup_unver(St, PeerId) ->
    #?ST{peers = Peers, rand = Rand, lookup_unver = Lookup} = St,
    {Lookup2, Rand2, Peers2} =
        peers_lookup_add(Peers, Rand, PeerId, Lookup, #peer.lookup_unver_idx),
    St#?ST{peers = Peers2, rand = Rand2, lookup_unver = Lookup2}.

-spec del_lookup_verif(state(), peer_id()) -> state().
del_lookup_verif(St, PeerId) ->
    #?ST{peers = Peers, lookup_verif = Lookup} = St,
    {Lookup2, Peers2} =
        peers_lookup_del(Peers, PeerId, Lookup, #peer.lookup_verif_idx),
    St#?ST{peers = Peers2, lookup_verif = Lookup2}.

-spec del_lookup_unver(state(), peer_id()) -> state().
del_lookup_unver(St, PeerId) ->
    #?ST{peers = Peers, lookup_unver = Lookup} = St,
    {Lookup2, Peers2} =
        peers_lookup_del(Peers, PeerId, Lookup, #peer.lookup_unver_idx),
    St#?ST{peers = Peers2, lookup_unver = Lookup2}.

%% Exports a list of peer identifier to the external format.
-spec export_results(state(), [peer_id()]) -> [ext_peer()].
export_results(St, PeerIds) ->
    lists:foldl(fun(PeerId, Acc) ->
        [export_result(St, PeerId) | Acc]
    end, [], PeerIds).

%% Exports a peer identifier to the external format.
-spec export_result(state(), peer_id()) -> ext_peer().
export_result(St, PeerId) ->
    #peer{extra = Extra} = get_peer(St, PeerId),
    {PeerId, Extra}.

%% Wraps a filtering function to only require the peer identifier.
%% The result should not be used if the list of peers is mutated.
-spec wrap_filter_fun(state(), ext_filter_fun() | undefined)
    -> int_filter_fun() | undefined.
wrap_filter_fun(_St, undefined) -> undefined;
wrap_filter_fun(St, FilterFun) ->
    #?ST{peers = Peers} = St,
    fun(PeerId) ->
        #{PeerId := Peer} = Peers,
        FilterFun(PeerId, Peer#peer.extra)
    end.

%--- FUNCTIONS FOR BOTH VERIFIED AND UNVERIFIED POOLS --------------------------

%% Creates a generic bucket filtering function that always keeps selected and
%% trusted peers, removes out dated peers and elect the rest for eviction.
-spec make_bucket_filtering_fun(state(), millitimestamp(),
                                peer_id() | undefined)
    -> bucket_filter_fun().
make_bucket_filtering_fun(St, Now, KeepPeerId) ->
    #?ST{peers = Peers, max_update_lapse = MaxLapse} = St,
    fun
        (PeerId) when PeerId =:= KeepPeerId -> keep;
        (PeerId) ->
            #{PeerId := Peer} = Peers,
            case Peer of
                #peer{trusted = true} -> keep;
                #peer{selected = true} -> keep;
                #peer{update_time = T} when (Now - T) > MaxLapse -> remove;
                _ -> evict
            end
    end.

%--- VERIFIED POOL HANDLING FUNCTIONS ------------------------------------------

%% Returns the verified pool bucket index for given address.
%% For the same peer address group, it will return at most `verif_group_shard'
%% different indexes.
-spec verified_bucket_index(state(), peer_addr()) -> non_neg_integer().
verified_bucket_index(St, PeerAddr) ->
    #?ST{
        secret = Secret,
        verif_pool = Pool,
        verif_group_shard = GroupShard
    } = St,
    #pool{bucket_count = BucketCount} = Pool,
    PeerGroup = address_group(PeerAddr),
    PeerDesc = address_descriptor(PeerAddr),
    GroupSlot = hash_modulo(<<Secret/binary, PeerDesc/binary>>, GroupShard),
    hash_modulo(<<Secret/binary, PeerGroup/binary, GroupSlot:8>>, BucketCount).

%% Adds a peer to the verified pool if required.
-spec verified_maybe_add(state(), millitimestamp(), peer_id())
    -> {verified, state()} | {unverified, state()} | {ignored, state()}.
verified_maybe_add(St, Now, PeerId) ->
    Peer = get_peer(St, PeerId),
    case peer_state(Peer) of
        verified ->
            % Already verified.
            {verified, St};
        undefined ->
            % Not yet pooled.
            verified_add(St, Now, Peer);
        unverified ->
            % Currently unverified
            case verified_add(St, Now, Peer) of
                {verified, St2} ->
                    % Succeed to add to the verified pool;
                    % remove it from unverified.
                    St3 = unverified_del(St2, PeerId),
                    {verified, St3};
                {ignored, St2} ->
                    % Failed to add to verified pool;
                    % keep the peer as unverified.
                    {unverified, St2}
            end
    end.

%% Adds given peer to the verified pool; doesn't check if it is already there.
-spec verified_add(state(), millitimestamp(), peer())
    -> {verified, state()} | {ignored, state()}.
verified_add(St, Now, Peer) ->
    ?assertEqual(undefined, Peer#peer.vidx),
    #peer{id = PeerId, addr = PeerAddr} = Peer,
    BucketIdx = verified_bucket_index(St, PeerAddr),
    case verified_make_space_for(St, Now, BucketIdx, PeerId) of
        {no_space, St2} ->
            % Failed to allocate space in the pool bucket.
            {ignored, St2};
        {free_space, St2} ->
            % There is space to add the peer to the bucket.
            #?ST{verif_pool = Pool} = St2,
            Pool2 = pool_add(Pool, BucketIdx, PeerId),
            Pool3 = pool_update_size(Pool2, 1),
            St3 = St2#?ST{verif_pool = Pool3},
            % Peer may have been changed by verified_make_space_for/4.
            Peer2 = get_peer(St3, PeerId),
            Peer3 = Peer2#peer{vidx = BucketIdx},
            St4 = set_peer(St3, PeerId, Peer3),
            case is_available(St4, PeerId) of
                false ->
                    {verified, St4};
                true ->
                    {verified, add_lookup_verif(St4, PeerId)}
            end
    end.

%% Deletes a peer from the verified pool.
%% It ONLY deletes the peer from the verified pool and verified lookup
%% table.
-spec verified_del(state(), peer_id()) -> state().
verified_del(St, PeerId) ->
    case get_peer(St, PeerId) of
        #peer{vidx = undefined} -> St;
        #peer{vidx = VIdx} = Peer ->
            #?ST{verif_pool = Pool} = St,
            Pool2 = pool_del(Pool, VIdx, PeerId),
            Pool3 = pool_update_size(Pool2, -1),
            St2 = St#?ST{verif_pool = Pool3},
            Peer2 = Peer#peer{vidx = undefined},
            St3 = set_peer(St2, PeerId, Peer2),
            del_lookup_verif(St3, PeerId)
    end.

%% Downgrade a verified peer to unverified.
%% Returns a boolean stating if the peer was added to the unverified pool of
%% deleted COMPLETLY.
-spec verified_downgrade(state(), millitimestamp(), peer_id()) -> state().
verified_downgrade(St, Now, PeerId) ->
    Peer = get_peer(St, PeerId),
    Peer2 = peer_reset(Peer, Now),
    St2 = set_peer(St, PeerId, Peer2),
    St3 = verified_del(St2, PeerId),
    case unverified_maybe_add(St3, Now, PeerId, undefined) of
        {unverified, St4} -> St4;
        {ignored, St4} ->
            % Failed to add it to unverified pool; removing peer completly.
            del_peer(St4, PeerId)
    end.

%% Tries to free space in a verified pool bucket for given peer.
%% The peer is required because we don't want it evicted from the unverified
%% pool by a downgraded peer.
-spec verified_make_space_for(state(), millitimestamp(),
                              non_neg_integer(), peer_id())
    -> {no_space, state()} | {free_space, state()}.
verified_make_space_for(St, Now, BucketIdx, PeerId) ->
    % When evicting we want to skew the random selection in favor of the peers
    % selected the longest time ago, but we never evict selected peers.
    #?ST{rand = RSt, peers = Peers, verif_pool = Pool} = St,
    BucketFilterFun = make_bucket_filtering_fun(St, Now, undefined),
    SortKeyFun = fun(I) ->
        #{I := Peer} = Peers,
        case Peer of
            #peer{select_time = undefined} -> 0;
            #peer{select_time = Time} -> Time
        end
    end,
    case pool_make_space(Pool, RSt, BucketIdx,
                         BucketFilterFun, SortKeyFun) of
        no_space ->
            {no_space, St};
        {free_space, [], undefined, RSt2, Pool2} ->
            St2 = St#?ST{rand = RSt2, verif_pool = Pool2},
            {free_space, St2};
        {free_space, RemovedIds, undefined, RSt2, Pool2} ->
            Pool3 = pool_update_size(Pool2, -length(RemovedIds)),
            St2 = St#?ST{rand = RSt2, verif_pool = Pool3},
            St3 = lists:foldl(fun(I, S) ->
                Peer = get_peer(S, I),
                Peer2 = Peer#peer{vidx = undefined},
                S2 = set_peer(S, I, Peer2),
                del_peer(S2, I)
            end, St2, RemovedIds),
            {free_space, St3};
        {free_space, [], EvictedId, RSt2, Pool2} ->
            % Try downgrading the evicted peer, if any;
            % delete it if downgrade fail.
            Pool3 = pool_update_size(Pool2, -1),
            St2 = St#?ST{rand = RSt2, verif_pool = Pool3},
            Peer = get_peer(St2, EvictedId),
            Peer2 = Peer#peer{vidx = undefined},
            St3 = set_peer(St2, EvictedId, Peer2),
            St4 = del_lookup_verif(St3, EvictedId),
            case unverified_maybe_add(St4, Now, EvictedId, PeerId) of
                {unverified, St5} ->
                    {free_space, St5};
                {ignored, St5} ->
                    {free_space, del_peer(St5, EvictedId)}
            end
    end.

%% Selects a random available peer from the verified pool.
-spec verified_select(state(), millitimestamp(), int_filter_fun() | undefined)
    -> {unavailable, state()} | {peer_id(), state()}.
verified_select(St, _Now, FilterFun) ->
    #?ST{rand = RState, use_rand_offset = ROffset, lookup_verif = Lookup} = St,
    {Result, RSt2} = lookup_select(Lookup, RState, ROffset, FilterFun),
    {Result, St#?ST{rand = RSt2}}.

%--- UNVERIFIED POOL HANDLING FUNCTIONS ----------------------------------------

%% Returns the unverified pool bucket index for given source and peer address.
%% For the same source address group, it will return at most
%% `verif_source_shard' different indexes.
%% For the same source and peer address group, it will return at most
%% `verif_group_shard' different indexes.
-spec unverified_bucket_index(state(), peer_addr(), peer_addr())
    -> non_neg_integer().
unverified_bucket_index(St, SourceAddr, PeerAddr) ->
    #?ST{
        secret = Secret,
        unver_pool = Pool,
        unver_source_shard = SourceShard,
        unver_group_shard = GroupShard
    } = St,
    #pool{bucket_count = BucketCount} = Pool,
    SourceGroup = address_group(SourceAddr),
    PeerGroup = address_group(PeerAddr),
    PeerDesc = address_descriptor(PeerAddr),
    SourceSlot = hash_modulo(<<Secret/binary, PeerGroup/binary>>, SourceShard),
    GroupSlot = hash_modulo(<<Secret/binary, PeerDesc/binary>>, GroupShard),
    hash_modulo(<<Secret/binary, SourceGroup/binary,
                  SourceSlot:8, GroupSlot:8>>, BucketCount).

%% Adds a peer to the unverified pool if required.
%% A peer identifier that MUST NOT be removed can be specified;
%% this is required when a peer is upgraded while still being kept in
%% the unverified pool.
-spec unverified_maybe_add(state(), millitimestamp(), peer_id(),
                           peer_id() | undefined)
    -> {verified, state()} | {unverified, state()} | {ignored, state()}.
unverified_maybe_add(St, Now, PeerId, KeepPeerId) ->
    Peer = get_peer(St, PeerId),
    case peer_state(Peer) of
        verified ->
            {verified, St};
        undefined ->
            unverified_add(St, Now, Peer, KeepPeerId);
        unverified  ->
            #?ST{rand = RSt, unver_pool = Pool} = St,
            #peer{uidxs = Idxs} = Peer,
            case pool_should_add_ref(Pool, RSt, Idxs) of
                {false, RSt2} ->
                    {unverified, St#?ST{rand = RSt2}};
                {true, RSt2} ->
                    St2 = St#?ST{rand = RSt2},
                    St3 = unverified_add_reference(St2, Now, Peer, KeepPeerId),
                    {unverified, St3}
            end
    end.

%% Adds a peer to the unverified pool without any check.
-spec unverified_add(state(), millitimestamp(), peer(), peer_id() | undefined)
    -> {unverified, state()} | {ignored, state()}.
unverified_add(St, Now, Peer, KeepPeerId) ->
    #peer{id = PeerId, addr = PeerAddr, source = SourceAddr} = Peer,
    ?assertEqual([], Peer#peer.uidxs),
    BucketIdx = unverified_bucket_index(St, SourceAddr, PeerAddr),
    case unverified_make_space(St, Now, BucketIdx, KeepPeerId) of
        {no_space, St2} ->
            {ignored, St2};
        {free_space, St2} ->
            #?ST{unver_pool = Pool} = St2,
            Pool2 = pool_add(Pool, BucketIdx, PeerId),
            Pool3 = pool_update_size(Pool2, 1),
            St3 = St2#?ST{unver_pool = Pool3},
            % Peer may have been changed by unverified_make_space/3.
            Peer2 = get_peer(St3, PeerId),
            Peer3 = Peer2#peer{uidxs = [BucketIdx]},
            St4 = set_peer(St3, PeerId, Peer3),
            case is_available(St4, PeerId) of
                true ->
                    {unverified, add_lookup_unver(St4, PeerId)};
                false ->
                    {unverified, St4}
            end
    end.

%% Adds another reference to a peer already in the unverified pool.
-spec unverified_add_reference(state(), millitimestamp(), peer(),
                               peer_id() | undefined)
    -> state().
unverified_add_reference(St, Now, Peer, KeepPeerId) ->
    #peer{
        id = PeerId,
        addr = PeerAddr,
        source = SourceAddr,
        uidxs = Idxs
    } = Peer,
    ?assertNotEqual([], Idxs),
    BucketIdx = unverified_bucket_index(St, SourceAddr, PeerAddr),
    case lists:member(BucketIdx, Idxs) of
        true -> St;
        false ->
            case unverified_make_space(St, Now, BucketIdx, KeepPeerId) of
                {no_space, St2} -> St2;
                {free_space, St2} ->
                    #?ST{unver_pool = Pool} = St2,
                    Pool2 = pool_add(Pool, BucketIdx, PeerId),
                    St3 = St2#?ST{unver_pool = Pool2},
                    Peer2 = Peer#peer{uidxs = [BucketIdx | Idxs]},
                    set_peer(St3, PeerId, Peer2)
            end
    end.

%% Deletes all the references to a peer from the unverified pool.
%% MUST work even when peers are in both pools because peers are first added
%% to the verified pool and THEN deleted from the unverified pool.
%% It ONLY deletes the peer from the unverified pool and unverified lookup
%% table.
-spec unverified_del(state(), peer_id()) -> state().
unverified_del(St, PeerId) ->
    case get_peer(St, PeerId) of
        #peer{uidxs = []} -> St;
        #peer{uidxs = BucketIdxs} = Peer->
            #?ST{unver_pool = Pool} = St,
            Pool2 = lists:foldl(fun(I, P) ->
                pool_del(P, I, PeerId)
            end, Pool, BucketIdxs),
            Pool3 = pool_update_size(Pool2, -1),
            St2 = St#?ST{unver_pool = Pool3},
            Peer2 = Peer#peer{uidxs = []},
            St3 = set_peer(St2, PeerId, Peer2),
            del_lookup_unver(St3, PeerId)
    end.

%% Tries to free space in an unverified pool bucket.
%% If not `undefined', the specified peer identifier will never be removed
%% or evicted.
-spec unverified_make_space(state(), millitimestamp(), non_neg_integer(),
                            peer_id() | undefined)
    -> {no_space, state()} | {free_space, state()}.
unverified_make_space(St, Now, BucketIdx, KeepPeerId) ->
    % When evicting we want to skew the random selection in favor of the peers
    % updated the longest time ago, but we never evict selected peers.
    #?ST{rand = RSt, peers = Peers, unver_pool = Pool} = St,
    BucketFilterFun = make_bucket_filtering_fun(St, Now, KeepPeerId),
    SortKeyFun = fun(PeerId) ->
        #{PeerId := Peer} = Peers,
        Peer#peer.update_time
    end,
    case pool_make_space(Pool, RSt, BucketIdx,
                         BucketFilterFun, SortKeyFun) of
        no_space -> {no_space, St};
        {free_space, RemovedIds, EvictedId, RSt2, Pool2} ->
            St2 = St#?ST{rand = RSt2, unver_pool = Pool2},
            St3 = lists:foldl(fun
                (undefined, S) -> S;
                (PeerId, S) ->
                    unverified_ref_deleted(S, PeerId, BucketIdx)
            end, St2, [EvictedId | RemovedIds]),
            {free_space, St3}
    end.

%% Acts on a peer reference being removed from the given unverified pool bucket.
%% If it is the last reference, the pool size is decremented and the peer is
%% COMPLETLY removed.
-spec unverified_ref_deleted(state(), peer_id(), non_neg_integer()) -> state().
unverified_ref_deleted(St, PeerId, BucketIdx) ->
    case get_peer(St, PeerId) of
        #peer{uidxs = [BucketIdx]} = Peer ->
            % Last reference is removed
            #?ST{unver_pool = Pool} = St,
            Pool2 = pool_update_size(Pool, -1),
            Peer2 = Peer#peer{uidxs = []},
            St2 = St#?ST{unver_pool = Pool2},
            St3 = set_peer(St2, PeerId, Peer2),
            del_peer(St3, PeerId);
        #peer{uidxs = RefIdxs} = Peer ->
            % Peer is still referenced in other buckets
            ?assert(lists:member(BucketIdx, RefIdxs)),
            RefIdxs2 = lists:delete(BucketIdx, RefIdxs),
            ?assertNotMatch([], RefIdxs2),
            Peer2 = Peer#peer{uidxs = RefIdxs2},
            set_peer(St, PeerId, Peer2)
    end.

%% Selects a random available peer from the unverified pool.
-spec unverified_select(state(), millitimestamp(), int_filter_fun())
    -> {unavailable, state()} | {peer_id(), state()}.
unverified_select(St, _Now, FilterFun) ->
    #?ST{rand = RState, use_rand_offset = ROffset, lookup_unver = Lookup} = St,
    {Result, RSt2} = lookup_select(Lookup, RState, ROffset, FilterFun),
    {Result, St#?ST{rand = RSt2}}.

%--- PEER HANDLING FUNCTIONS ---------------------------------------------------

%% Creates a new peer record.
-spec peer_new(peer_id(), peer_addr(), peer_addr(), boolean(), extra())
    -> peer().
peer_new(PeerId, PeerAddr, SourceAddr, IsTrusted, Extra) ->
    #peer{
        id = PeerId,
        trusted = IsTrusted,
        addr = PeerAddr,
        source = SourceAddr,
        extra = Extra
    }.

%% Returns if the given peer is verified or unverified;
%% does extra exhaustive check for sanity, and thus should not be called
%% when the peer is in both verified and unverified pool (when upgrading).
-spec peer_state(peer()) -> verified | unverified | undefined.
peer_state(#peer{vidx = undefined, uidxs = []}) -> undefined;
peer_state(#peer{vidx = undefined, uidxs = [_|_]}) -> unverified;
peer_state(#peer{vidx = _, uidxs = []}) -> verified.

peer_is_selected(#peer{selected = Selected}) -> Selected.

%% Updates peer rejection status.
-spec peer_reject(peer(), millitimestamp()) -> peer().
peer_reject(Peer, Now) ->
    #peer{rejected = Rejected} = Peer,
    Peer#peer{rejected = Rejected + 1, reject_time = Now}.

%% Updates peer rejection status.
-spec peer_reset(peer(), millitimestamp()) -> peer().
peer_reset(Peer, _Now) ->
    Peer#peer{rejected = 0, reject_time = undefined}.

%% Makes the peer selected.
-spec peer_select(peer(), millitimestamp()) -> peer().
peer_select(Peer, Now) ->
    Peer#peer{selected = true, select_time = Now}.

%% Makes the peer deselected.
-spec peer_deselect(peer(), millitimestamp()) -> peer().
peer_deselect(Peer, _Now) ->
    Peer#peer{selected = false}.

%% Gives the time at which the peer should get out of standby
-spec peer_standby_recovery_time(peer()) -> millitimestamp().
peer_standby_recovery_time(Peer) ->
    #peer{rejected = RejectCount, reject_time = RejectTime} = Peer,
    RejectTime + rejection_delay(RejectCount).

%% Returns if a peer is currently in standby.
-spec peer_is_in_standby(peer(), millitimestamp()) -> boolean().
peer_is_in_standby(Peer, Now) ->
    Now < peer_standby_recovery_time(Peer).

%% Returns if the given peer reached its rejection limit.
-spec peer_has_expired(peer()) -> boolean().
peer_has_expired(#peer{trusted = true}) -> false;
peer_has_expired(#peer{rejected = Rejections}) ->
    Rejections > ?MAX_REJECTIONS.

%% Adds a peer to a lookup table, handling other peers being moved.
-spec peers_lookup_add(peer_map(), rand_state(), peer_id(),
                       lookup(), pos_integer())
    -> {lookup(), rand_state(), peer_map()}.
peers_lookup_add(Peers, RSt, PeerId, Lookup, RecField) ->
    #{PeerId := Peer} = Peers,
    ?assertEqual(undefined, element(RecField, Peer)),
    case lookup_add(Lookup, RSt, PeerId) of
        {Idx, undefined, RSt2, Lookup2} ->
            Peer2 = setelement(RecField, Peer, Idx),
            Peers2 = Peers#{PeerId := Peer2},
            {Lookup2, RSt2, Peers2};
        {Idx, {MovedPeerIdx, MovedPeerId}, RSt2, Lookup2} ->
            #{MovedPeerId := MovedPeer} = Peers,
            Peer2 = setelement(RecField, Peer, Idx),
            MovedPeer2 = setelement(RecField, MovedPeer, MovedPeerIdx),
            Peers2 = Peers#{PeerId := Peer2, MovedPeerId := MovedPeer2},
            {Lookup2, RSt2, Peers2}
    end.

%% remove a peer from a lookup table, handling other peers being moved.
-spec peers_lookup_del(peer_map(), peer_id(), lookup(), pos_integer())
    -> {lookup(), peer_map()}.
peers_lookup_del(Peers, PeerId, Lookup, RecField) ->
    #{PeerId := Peer} = Peers,
    case element(RecField, Peer) of
        undefined ->
            {Lookup, Peers};
        Idx ->
            ?assertEqual(PeerId, lookup_get(Lookup, Idx)),
            case lookup_del(Lookup, Idx) of
                {undefined, Lookup2} ->
                    Peer2 = setelement(RecField, Peer, undefined),
                    Peers2 = Peers#{PeerId := Peer2},
                    {Lookup2, Peers2};
                {{MovedPeerIdx, MovedPeerId}, Lookup2} ->
                    #{MovedPeerId := MovedPeer} = Peers,
                    Peer2 = setelement(RecField, Peer, undefined),
                    MovedPeer2 = setelement(RecField, MovedPeer, MovedPeerIdx),
                    Peers2 = Peers#{PeerId := Peer2, MovedPeerId := MovedPeer2},
                    {Lookup2, Peers2}
            end
    end.

%--- GENERIC POOL HANDLING FUNCTIONS -------------------------------------------

-ifdef(TEST).

pool_bucket_size(Pool, BucketIdx) ->
    ?assert(BucketIdx < Pool#pool.bucket_count),
    #pool{buckets = Buckets} = Pool,
    Bucket = array:get(BucketIdx, Buckets),
    length(Bucket).

-endif.

%% Creates a new pool record.
-spec pool_new(pos_integer(), pos_integer(), pos_integer(), number()) -> pool().
pool_new(Count, Size, MaxRefs, EvictSkew) ->
    #pool{
        size = 0,
        max_refs = MaxRefs,
        skew  = EvictSkew,
        bucket_count = Count,
        bucket_size = Size,
        buckets = array:new(Count, [{default, []}])
    }.

%% Returns the number of peers in a pool.
-spec pool_size(pool()) -> non_neg_integer().
pool_size(#pool{size = Size}) -> Size.

%% Adds given value to the pool size.
-spec pool_update_size(pool(), integer()) -> pool().
pool_update_size(Pool, Diff) ->
    ?assert((Pool#pool.size + Diff) >= 0),
    Pool#pool{size = Pool#pool.size + Diff}.

%% Adds a value to given pool's bucket.
%% It doesn't increment the pool size, pool_update_size/2 should be called.
-spec pool_add(pool(), non_neg_integer(), term()) -> pool().
pool_add(Pool, BucketIdx, Value) ->
    ?assert(BucketIdx < Pool#pool.bucket_count),
    #pool{buckets = Buckets} = Pool,
    Bucket = array:get(BucketIdx, Buckets),
    ?assert(length(Bucket) < Pool#pool.bucket_size),
    ?assertNot(lists:member(Value, Bucket)),
    Buckets2 = array:set(BucketIdx, [Value | Bucket], Buckets),
    Pool#pool{buckets = Buckets2}.

%% Removes a value from given pool's bucket.
%% It doesn't decrement the pool size, pool_update_size/2 should be called.
-spec pool_del(pool(), non_neg_integer(), term()) -> pool().
pool_del(Pool, BucketIdx, Value) ->
    ?assert(BucketIdx < Pool#pool.bucket_count),
    #pool{buckets = Buckets} = Pool,
    Bucket = array:get(BucketIdx, Buckets),
    ?assert(lists:member(Value, Bucket)),
    Buckets2 = array:set(BucketIdx, lists:delete(Value, Bucket), Buckets),
    Pool#pool{buckets = Buckets2}.

%% Tells if given the pool configuration and the current references, the caller
%% should try to add another reference to the pool.
-spec pool_should_add_ref(pool(), rand_state(), [non_neg_integer()])
    -> {boolean(), rand_state()}.
pool_should_add_ref(Pool, RSt, RefIdxs) ->
    #pool{max_refs = MaxRefCount} = Pool,
    RefCount = length(RefIdxs),
    case RefCount < MaxRefCount of
        true -> should_add_ref(RSt, RefCount);
        false -> {false, RSt}
    end.

%% Makes space in the pool to add a new value.
%% The filter function can either mark entries to keep them, remove them
%% or elect them for eviction. The sort key function is used to order the
%% buckets before selecting a random entry for eviction with the
%% pool configured skew toward the entries with the smallest key.
-spec pool_make_space(pool(), rand_state(), non_neg_integer(),
                      bucket_filter_fun(), bucket_sort_key_fun())
    -> {free_space, [peer_id()], peer_id() | undefined, rand_state(), pool()}
     | no_space.
pool_make_space(Pool, RSt, BucketIdx, FilterFun, SortKeyFun) ->
    ?assert(BucketIdx < Pool#pool.bucket_count),
    #pool{buckets = Buckets, bucket_size = MaxBucketSize, skew = Skew} = Pool,
    Bucket = array:get(BucketIdx, Buckets),
    BucketSize = length(Bucket),
    case BucketSize < MaxBucketSize of
        true ->
            {free_space, [], undefined, RSt, Pool};
        false ->
            {Bucket2, Removed, KeyedEvictable, EvictableSize}
                = bucket_prepare(Bucket, FilterFun, SortKeyFun),
            case {Removed, EvictableSize} of
                {[_|_], _} ->
                    % If some entry are removed, there is no need for eviction.
                    Buckets2 = array:set(BucketIdx, Bucket2, Buckets),
                    Pool2 = Pool#pool{buckets = Buckets2},
                    {free_space, Removed, undefined, RSt, Pool2};
                {[], 0} ->
                    % Nothing removed and nothing to evict.
                    no_space;
                _ ->
                    % We need to evict an entry.
                    SortedEvictable = lists:keysort(1, KeyedEvictable),
                    {EvictedIdx, RSt2} =
                        skewed_randint(RSt, EvictableSize, Skew),
                    {_, EvictedValue} =
                        lists:nth(EvictedIdx + 1, SortedEvictable),
                    Bucket3 = lists:delete(EvictedValue, Bucket2),
                    Buckets2 = array:set(BucketIdx, Bucket3, Buckets),
                    Pool2 = Pool#pool{buckets = Buckets2},
                    {free_space, [], EvictedValue, RSt2, Pool2}
            end
    end.

%% Prepares a bucket for freeing space.
%% Returns the new bucket, the removed entries and the entries elected
%% for eviction keyed for sorting.
-spec bucket_prepare([peer_id()], bucket_filter_fun(), bucket_sort_key_fun())
    -> {[peer_id()], [peer_id()], [{term(), peer_id()}], non_neg_integer()}.
bucket_prepare(Bucket, FilterFun, SortKeyFun) ->
    bucket_prepare(Bucket, FilterFun, SortKeyFun, [], [], [], 0).

bucket_prepare([], _FFun, _SortKeyFun, BAcc, RAcc, EAcc, ECount) ->
    {BAcc, RAcc, EAcc, ECount};
bucket_prepare([Val | Rest], FFun, KFun, BAcc, RAcc, EAcc, ECount) ->
    case FFun(Val) of
        keep ->
            bucket_prepare(Rest, FFun, KFun, [Val | BAcc], RAcc, EAcc, ECount);
        remove ->
            bucket_prepare(Rest, FFun, KFun, BAcc, [Val | RAcc], EAcc, ECount);
        evict ->
            BAcc2 = [Val | BAcc],
            EAcc2 = [{KFun(Val), Val} | EAcc],
            bucket_prepare(Rest, FFun, KFun, BAcc2, RAcc, EAcc2, ECount + 1)
    end.

%--- LOOKUP HANDLING FUNCTIONS -------------------------------------------------

-ifdef(TEST).

lookup_internal_size(#lookup{array = Array}) -> array:size(Array).

lookup_internal_free(#lookup{size = Size, array = Array}) ->
    array:size(Array) - Size.

-endif.

%% Creates a new lookup data structure.
-spec lookup_new() -> lookup().
lookup_new() ->
    #lookup{
        size = 0,
        array = array:new(?LOOKUP_START_SIZE)
    }.

%% Returns the number of element in a lookup table.
-spec lookup_size(lookup()) -> non_neg_integer().
lookup_size(#lookup{size = Size}) -> Size.

%% Get a value from the lookup table
-spec lookup_get(lookup(), non_neg_integer()) -> peer_id().
lookup_get(#lookup{size = Size, array = Array}, Idx)
  when Idx < Size ->
    array:get(Idx, Array).

%% Gets two values, swap them in the array and return them.
-spec lookup_swap(lookup(), non_neg_integer(), non_neg_integer())
    -> {peer_id(), peer_id(), lookup()}.
lookup_swap(#lookup{size = Size, array = Array} = Lookup, Idx1, Idx2)
  when Idx1 < Size, Idx2 < Size ->
    A = array:get(Idx1, Array),
    B = array:get(Idx2, Array),
    Array2 = array:set(Idx2, A, array:set(Idx1, B, Array)),
    {A, B, Lookup#lookup{array = Array2}}.

%% Appends a value at the end of the lookup table;
%% eventually resize the underlying array.
-spec lookup_append(lookup(), peer_id()) -> {non_neg_integer(), lookup()}.
lookup_append(Lookup, Value) ->
    #lookup{size = Size, array = Array} = Lookup,
    ArraySize = array:size(Array),
    case ArraySize > Size of
        true ->
            Array2 = array:set(Size, Value, Array),
            {Size, Lookup#lookup{size = Size + 1, array = Array2}};
        false ->
            NewArraySize = ArraySize + min(ArraySize, ?MAX_LOOKUP_SIZE_INC),
            Array2 = array:resize(NewArraySize, Array),
            Array3 = array:set(Size, Value, Array2),
            {Size, Lookup#lookup{size = Size + 1, array = Array3}}
    end.

%% Shrinks the lookup table, removing the last value;
%% eventually resize the underlying array.
-spec lookup_shrink(lookup()) -> lookup().
lookup_shrink(Lookup) ->
    #lookup{size = OldSize, array = Array} = Lookup,
    NewSize = OldSize - 1,
    ArraySize = array:size(Array),
    FreeSpace = (ArraySize - NewSize),
    MaxFreeSpace = min(?MAX_LOOKUP_SIZE_INC, NewSize),
    case (NewSize < ?LOOKUP_START_SIZE) or (FreeSpace < MaxFreeSpace) of
        true ->
            Array2 = array:reset(NewSize, Array),
            Lookup#lookup{size = NewSize, array = Array2};
        false ->
            Array2 = array:resize(NewSize, Array),
            Lookup#lookup{size = NewSize, array = Array2}
    end.

%% Adds the value in the given lookup table at a random position;
%% if the lookup table had to move and existing value, it returns a tuple
%% with the value and its new index, otherwise it returns `undefined'.
%% Needs the state of the random number generator.
-spec lookup_add(lookup(), term(), term())
    -> {non_neg_integer(), {non_neg_integer(), term()}, term(), lookup()}
                           | undefined.
lookup_add(#lookup{size = 0} = Lookup, RSt, Value) ->
    {0, Lookup2} = lookup_append(Lookup, Value),
    {0, undefined, RSt, Lookup2};
lookup_add(#lookup{size = 1} = Lookup, RSt, Value) ->
    % even if there is a only two values we want the table randomized.
    {1, Lookup2} = lookup_append(Lookup, Value),
    {SwapFlag, RSt2} = randint(RSt, 2),
    case SwapFlag =:= 0 of
        false ->
            {1, undefined, RSt2, Lookup2};
        true ->
            {OldValue, _, Lookup3} = lookup_swap(Lookup2, 0, 1),
            {0, {1, OldValue}, RSt2, Lookup3}
    end;
lookup_add(Lookup, RSt, Value) ->
    {Idx, Lookup2} = lookup_append(Lookup, Value),
    {RandIdx, RSt2} = randint(RSt, Idx),
    {OldValue, _, Lookup3} = lookup_swap(Lookup2, RandIdx, Idx),
    {RandIdx, {Idx, OldValue}, RSt2, Lookup3}.

%% Removes the value at the given index;
%% if the lookup table had to move and existing value, it returns a tuple
%% with the value and its new index, otherwise it returns `undefined'.
-spec lookup_del(lookup(), non_neg_integer())
    -> {undefined | {non_neg_integer(), term()}, lookup()}.
lookup_del(#lookup{size = 1} = Lookup, 0) ->
    {undefined, lookup_shrink(Lookup)};
lookup_del(#lookup{size = Size} = Lookup, Idx) when Idx =:= (Size - 1)->
    {undefined, lookup_shrink(Lookup)};
lookup_del(#lookup{size = Size} = Lookup, Idx) when Size > 1, Idx < Size ->
    OldIdx = Size - 1,
    {OldValue, _, Lookup2} = lookup_swap(Lookup, OldIdx, Idx),
    {{Idx, OldValue}, lookup_shrink(Lookup2)}.

%% Selects a random value from the lookup table.
%% If a restriction function is specified, it will keep selecting random values
%% until either there is no more values or the function returns `true'.
%% Optionally uses a strong random number as offset to weak random numbers
%% to ensure relatively strong randomness.
-spec lookup_select(lookup(), rand_state(), boolean(), int_filter_fun())
    -> {unavailable, rand_state()} | {peer_id(), rand_state()}.
lookup_select(#lookup{size = 0}, RSt, _UseRandOff, _FilterFun) ->
    {unavailable, RSt};
lookup_select(#lookup{size = 1} = Lookup, RSt, _UseRandOff, undefined) ->
    {lookup_get(Lookup, 0), RSt};
lookup_select(Lookup, RSt, UseRandOff, undefined) ->
    #lookup{size = Size} = Lookup,
    RandOffset = strong_randword(UseRandOff),
    {RandInt, RSt2} = randint(RSt, Size),
    RandIdx = (RandInt + RandOffset) rem Size,
    {lookup_get(Lookup, RandIdx), RSt2};
lookup_select(Lookup, RSt, UseRandOff, FilterFun) ->
    #lookup{size = Size} = Lookup,
    RandOffset = strong_randword(UseRandOff),
    lookup_select(Lookup, RSt, FilterFun, RandOffset, Size).

lookup_select(Lookup, RSt, FilterFun, _Offset, 1) ->
    Value = lookup_get(Lookup, 0),
    case FilterFun(Value) of
        true -> {Value, RSt};
        false -> {unavailable, RSt}
    end;
lookup_select(Lookup, RSt, FilterFun, Offset, SamplingSize) ->
    LastIdx = SamplingSize - 1,
    {RandInt, RSt2} = randint(RSt, SamplingSize),
    RandIdx = (RandInt + Offset) rem SamplingSize,
    Value = lookup_get(Lookup, RandIdx),
    case FilterFun(Value) of
        true -> {Value, RSt2};
        false ->
            {_, _, Lookup2} = lookup_swap(Lookup, RandIdx, LastIdx),
            lookup_select(Lookup2, RSt2, FilterFun, Offset, SamplingSize - 1)
    end.

%% Samples a random number of peer identifers from given lookup table.
%% If the requested sample size is `all' or larger than the size of the table
%% the result will not be shuffled (maybe reversed).
%% Optionaly uses a strong random number as offset to weak random numbers to
%% ensure relatively strong randomness.
-spec lookup_sample(lookup(), rand_state(), boolean(), non_neg_integer() | all,
                    int_filter_fun() | undefined)
    -> {[peer_id()], rand_state()}.
lookup_sample(#lookup{size = 0}, RSt, _UseRandOff, _SampleSize, _FilterFun) ->
    {[], RSt};
lookup_sample(#lookup{size = Size} = Lookup, RSt, _, SampleSize, undefined)
  when SampleSize =:= all; SampleSize >= Size ->
    #lookup{array = Array} = Lookup,
    {array:sparse_to_list(Array), RSt};
lookup_sample(Lookup, RSt, _UseRandOff, all, FilterFun) ->
    #lookup{array = Array} = Lookup,
    Result = array:sparse_foldl(fun(_, V, Acc) ->
        case FilterFun(V) of
            true -> [V | Acc];
            false -> Acc
        end
    end, [], Array),
    {Result, RSt};
lookup_sample(Lookup, RSt, UseRandOff, SampleSize, FilterFun) ->
    #lookup{size = Size} = Lookup,
    RandOffset = strong_randword(UseRandOff),
    lookup_sample(Lookup, RSt, FilterFun, RandOffset, SampleSize, Size, []).

lookup_sample(_Lookup, RSt, _FilterFun, _Offset, 0, _SamplingSize, Acc) ->
    {Acc, RSt};
lookup_sample(Lookup, RSt, FilterFun, _Offset, Remaining, 1, Acc) ->
    Value = lookup_get(Lookup, 0),
    {_, Acc2} = lookup_filter(Value, FilterFun, Remaining, Acc),
    {Acc2, RSt};
lookup_sample(Lookup, RSt, FilterFun, Offset, Remaining, SamplingSize, Acc) ->
    {RandInt, RSt2} = randint(RSt, SamplingSize),
    RandIdx = (Offset + RandInt) rem SamplingSize,
    LastIdx = SamplingSize - 1,
    {RandValue, _, Lookup2} = lookup_swap(Lookup, RandIdx, LastIdx),
    {Remaining2, Acc2} = lookup_filter(RandValue, FilterFun, Remaining, Acc),
    lookup_sample(Lookup2, RSt2, FilterFun, Offset, Remaining2, LastIdx, Acc2).

lookup_filter(Value, undefined, Rem, Acc) ->
    {Rem - 1, [Value | Acc]};
lookup_filter(Value, FilterFun, Rem, Acc) ->
    case FilterFun(Value) of
        true -> {Rem - 1, [Value | Acc]};
        false -> {Rem, Acc}
    end.
