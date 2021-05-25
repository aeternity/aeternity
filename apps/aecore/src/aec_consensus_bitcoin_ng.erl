%%% -*- erlang-indent-level: 4 -*-
%%% -------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc BitcoinNG consensus module
%%% @end
%%% -------------------------------------------------------------------
-module(aec_consensus_bitcoin_ng).
-behavior(aec_consensus).

%% API
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
        %% Preconductor hook
        , dirty_validate_block_pre_conductor/1
        , dirty_validate_header_pre_conductor/1
        , dirty_validate_key_hash_at_height/2
        %% Dirty validation before starting the state transition
        , dirty_validate_key_node_with_ctx/3
        , dirty_validate_micro_node_with_ctx/3
        %% State transition
        , state_pre_transform_key_node_consensus_switch/4
        , state_pre_transform_key_node/4
        , state_pre_transform_micro_node/4
        %% Block rewards
        , state_grant_reward/3
        %% PoGF
        , pogf_detected/2
        %% Genesis block
        , genesis_transform_trees/2
        , genesis_raw_header/0
        , genesis_difficulty/0
        %% Keyblock creation
        , new_unmined_key_node/8
        , keyblocks_for_unmined_keyblock_adjust/0
        , adjust_unmined_keyblock/2
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
        , key_header_difficulty/1
        , keyblocks_for_target_calc/0 ]).

-ifdef(TEST).
-export([load_whitelist/0]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").
-include_lib("aeminer/include/aeminer.hrl").

-define(WHITELIST, {?MODULE, whitelist}).

%% -------------------------------------------------------------------
%% Configuration and extra features/http endpoints
can_be_turned_off() -> true.
assert_config(_Config) -> ok.
start(_Config) ->
    load_whitelist(),
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> force_community_fork();
        _                -> ok
    end,
    ok.
stop() -> ok.

is_providing_extra_http_endpoints() -> false.
client_request(_) -> error(unsupported).

%% If a node is not on the community fork then force it...
force_community_fork() ->
    %% The start of the 51% attack!
    %% The common ancestor between the attacker and the community fork is
    %% 366181 - kh_jzsdZmVDgoG9QQ8zQBLE6sURzukcmpSbume1jyfcHEpT1DEQ2
    %% Check for the NEXT height
    CommunityForkHeight = 366182,
    {key_block_hash, CommunityForkHash} = aeser_api_encoder:decode(<<"kh_YGDYbJQL4TV84CSaEA4VgcppUkpVyxSWFyHbS3frKkPjHPw2m">>),
    TopHash = aec_chain:top_block_hash(),
    {ok, TopHeader} = aec_chain:get_header(TopHash),
    TopHeight = aec_headers:height(TopHeader),
    case TopHeight > CommunityForkHeight of
        false -> ok;
        true ->
            case aec_chain_state:get_key_block_hash_at_height(CommunityForkHeight) of
                {ok, CommunityForkHash} -> ok; %% Everything is OK and we are on the proper fork
                {ok, ForkPoint} ->
                    %% We're not on the community fork - rollback to fork point
                    do_rollback(ForkPoint, CommunityForkHeight, TopHeight),
                    %% Ensure the node comes back up gracefully
                    %% Make sure we don't block here
                    spawn(fun() ->
                        %% Teardown some apps
                        _ = (catch application:stop(aehttp)),
                        _ = (catch application:stop(aesc)),
                        _ = (catch application:stop(aecore)),
                        %% Start them again
                        _ = (catch application:start(aecore)),
                        _ = (catch application:start(aesc)),
                        _ = (catch application:start(aehttp))
                          end)
            end
    end.

do_rollback(ForkPoint, Height, TopHeight) ->
    lager:info("Jumping to the community fork", []),
    ensure_gc_disabled(),
    {value, FPHeader} = aec_db:find_header(ForkPoint),
    SafetyMargin = 1000, %% Why not?
    aec_db:ensure_activity(sync_dirty, fun() ->
        [begin
             [begin
                  Del = element(2, T),
                  ok = mnesia:delete(aec_headers, Del, write),
                  ok = mnesia:delete(aec_blocks, Del, write),
                  ok = mnesia:delete(aec_block_state, Del, write)
              end || T <- mnesia:index_read(aec_headers, H, height)]
         end || H <- lists:seq(Height+1, TopHeight+SafetyMargin)],
        aec_db:write_top_block_node(ForkPoint, FPHeader)
      end),
    ok.

ensure_gc_disabled() ->
    case aec_db_gc:config() of
        #{enabled := false} -> ok;
        #{enabled := true} ->
            Msg = "It looks like you want to join the community fork but you are already on the wrong chain. Unfortunatelly as the garbage collector is enabled there is nothing we can do switch automatically - please either sync from genesis or start the node from a db backup from before the forking point and reenable GC after syncing with the correct chain",
            lager:error(Msg, []),
            init:stop(Msg)
    end.

%% -------------------------------------------------------------------
%% Deserialization
extra_from_header(_) ->
    #{consensus => ?MODULE}.

%% -------------------------------------------------------------------
%% Building the Insertion Ctx
recent_cache_n() -> max(aec_governance:key_blocks_to_check_difficulty_count(), aec_governance:median_timestamp_key_blocks()) + 1.
recent_cache_trim_key_header(Header) -> {aec_headers:target(Header), aec_headers:time_in_msecs(Header)}.

%% -------------------------------------------------------------------
%% Preconductor hook - called in sync process just before invoking the conductor
dirty_validate_block_pre_conductor(B) ->
    Header = aec_blocks:to_header(B),
    dirty_validate_header_pre_conductor(Header).

dirty_validate_header_pre_conductor(H) ->
    W = persistent_term:get(?WHITELIST),
    Height = aec_headers:height(H),
    case aec_headers:type(H) of
        key ->
            case maps:find(Height, W) of
                {ok, Hash} ->
                    case aec_headers:hash_header(H) of
                        {ok, Hash} -> ok;
                        _ -> {error, blocked_by_whitelist}
                    end;
                error ->
                    ok
            end;
        micro -> ok
    end.

dirty_validate_key_hash_at_height(Height, Hash) ->
    W = persistent_term:get(?WHITELIST),
    case maps:find(Height, W) of
        {ok, Hash} ->
            ok;
        {ok, _} ->
            {error, blocked_by_whitelist};
        error ->
            ok
    end.

%% -------------------------------------------------------------------
%% Dirty validation of keyblocks just before starting the state transition
dirty_validate_key_node_with_ctx(Node, Block, Ctx) ->
    Validators = [ fun ctx_enforce_whitelist/3
                 , fun ctx_validate_key_time/3
                 , fun ctx_validate_key_target/3
                 ],
    aeu_validation:run(Validators, [Node, Block, Ctx]).

ctx_enforce_whitelist(_Node, Block, _Ctx) ->
    dirty_validate_block_pre_conductor(Block).

ctx_validate_key_time(Node, _Block, Ctx) ->
    Time = aec_block_insertion:node_time(Node),
    case median_timestamp(Node, Ctx) of
        {ok, Median} when Time > Median -> ok;
        {ok,_Median} -> {error, key_block_from_the_past}
    end.
%% To assert key block target calculation we need DeltaHeight headers counted
%% backwards from the node we want to assert.
ctx_validate_key_target(Node, _Block, Ctx) ->
    Delta         = keyblocks_for_unmined_keyblock_adjust() + 1,
    Height        = aec_block_insertion:node_height(Node),
    GenesisHeight = aec_block_genesis:height(),
    case Delta >= Height - GenesisHeight of
        true ->
            %% We only need to verify that the target is equal to its predecessor.
            assert_target_equal_to_prev(Node, Ctx);
        false ->
            assert_calculated_target(Node, Delta, Ctx)
    end.

%% Compute the median timestamp for last aec_governance:median_timestamp_key_blocks()
median_timestamp(Node, Ctx) ->
    TimeStampKeyBlocks = aec_governance:median_timestamp_key_blocks(),
    case aec_block_insertion:node_height(Node) =< TimeStampKeyBlocks of
        true ->
            {ok, aec_block_genesis:time_in_msecs()};
        false ->
            Stats = aec_block_insertion:ctx_get_recent_n(Ctx, TimeStampKeyBlocks),
            Times = [T || {_, T} <- Stats],
            {ok, median(Times)}
    end.

median(Xs) ->
    Sorted = lists:sort(Xs),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (lists:nth(Mid+Rem, Sorted) + lists:nth(Mid+1, Sorted)) div 2.

assert_target_equal_to_prev(Node, Ctx) ->
    PrevKeyNode = aec_block_insertion:ctx_prev_key(Ctx),
    case {aec_block_insertion:node_target(Node), aec_block_insertion:node_target(PrevKeyNode)} of
        {X, X} -> ok;
        {X, Y} -> {error, {target_not_equal_to_parent, Node, X, Y}}
    end.

assert_calculated_target(Node, Delta, Ctx) ->
    Stats = aec_block_insertion:ctx_get_recent_n(Ctx, Delta),
    case aec_target:verify(aec_block_insertion:node_header(Node), Stats) of
        {error, {wrong_target, Actual, Expected}} ->
            {error, {wrong_target, Node, Actual, Expected}};
        R ->
            R
    end.

%% -------------------------------------------------------------------
%% Dirty validation of microblocks just before starting the state transition
dirty_validate_micro_node_with_ctx(Node, Block, Ctx) ->
    Validators = [ fun ctx_validate_micro_block_time/3
                 , fun ctx_validate_micro_signature/3
                 , fun ctx_validate_micro_pof/3
                 ],
    aeu_validation:run(Validators, [Node, Block, Ctx]).

ctx_validate_micro_block_time(Node, _Block, Ctx) ->
    PrevNode = aec_block_insertion:ctx_prev(Ctx),
    case aec_block_insertion:node_is_micro_block(PrevNode) of
        true ->
            case time_diff_greater_than_minimal(Node, PrevNode) of
                true  -> ok;
                false -> {error, micro_block_time_too_low}
            end;
        false ->
            case aec_block_insertion:node_time(Node) > aec_block_insertion:node_time(PrevNode) of
                true  -> ok;
                false -> {error, micro_block_time_too_low}
            end
    end.
ctx_validate_micro_signature(Node, _Block, Ctx) ->
    case aeu_sig:verify(aec_block_insertion:node_header(Node), aec_block_insertion:node_miner(aec_block_insertion:ctx_prev_key(Ctx))) of
        ok         -> ok;
        {error, _} -> {error, signature_verification_failed}
    end.

ctx_validate_micro_pof(_Node, Block, Ctx) ->
    case aec_blocks:pof(Block) of
        no_fraud ->
            ok;
        Pof ->
            case {get_fraud_miner(Ctx), aec_pof:pubkey(Pof)} of
                {{ok, M}, M} ->
                    ok;
                _ ->
                    {error, wrong_pubkey_in_pof}
            end
    end.

get_fraud_miner(Ctx) ->
    %% TODO: If I don't get a good argument on why lifting this requirement for the Iris hard fork is a bad idea then
    %%       PoF will be revamped for the Iris hard fork
    PrevNode = aec_block_insertion:ctx_prev(Ctx),
    case aec_block_insertion:node_type(PrevNode) of
        micro ->
            {ok, aec_block_insertion:node_miner(aec_block_insertion:ctx_prev_key(Ctx))};
        key ->
            case aec_chain:dirty_get_header(aec_block_insertion:node_prev_key_hash(PrevNode)) of
                {ok, H} -> {ok, aec_headers:miner(H)};
                _ -> error
            end
    end.

time_diff_greater_than_minimal(Node, PrevNode) ->
    aec_block_insertion:node_time(Node) >= aec_block_insertion:node_time(PrevNode) + aec_governance:micro_block_cycle().

%% -------------------------------------------------------------------
%% Custom state transitions
state_pre_transform_key_node_consensus_switch(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.
state_pre_transform_key_node(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.
state_pre_transform_micro_node(_Node, _PrevNode, _PrevKeyNode, Trees) -> Trees.

%% -------------------------------------------------------------------
%% Block rewards
state_grant_reward(Beneficiary, Trees, Amount) -> aec_trees:grant_fee(Beneficiary, Trees, Amount).

%% -------------------------------------------------------------------
%% PoGF
pogf_detected(_H1, _H2) -> ok.

%% -------------------------------------------------------------------
%% Genesis block
genesis_transform_trees(Trees, #{}) -> Trees.
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
        0,
        0,
        default,
        ?ROMA_PROTOCOL_VSN).
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

%% -------------------------------------------------------------------
%% Keyblock creation
new_unmined_key_node(PrevNode, PrevKeyNode, Height, Miner, Beneficiary, Protocol, InfoField, _TreesIn) ->
    FakeBlockHash = <<0:?BLOCK_HEADER_HASH_BYTES/unit:8>>,
    FakeStateHash = <<1337:?STATE_HASH_BYTES/unit:8>>,
    Header = aec_headers:new_key_header(Height,
                               aec_block_insertion:node_hash(PrevNode),
                               aec_block_insertion:node_hash(PrevKeyNode),
                               FakeStateHash,
                               Miner,
                               Beneficiary,
                               default_target(),
                               no_value,
                               0,
                               aeu_time:now_in_msecs(),
                               InfoField,
                               Protocol),
    aec_chain_state:wrap_header(Header, FakeBlockHash).

keyblocks_for_unmined_keyblock_adjust() ->
    aec_governance:key_blocks_to_check_difficulty_count().

adjust_unmined_keyblock(Block, AdjHeaders) ->
    Header = aec_blocks:to_header(Block),
    DeltaHeight = aec_governance:key_blocks_to_check_difficulty_count() + 1,
    case aec_headers:height(Header) =< DeltaHeight of
        true ->
            %% For the first DeltaHeight blocks, use pre-defined target
            Target = aec_block_genesis:target(),
            {ok, aec_blocks:set_target(Block, Target)};
        false when DeltaHeight == length(AdjHeaders) ->
            CalculatedTarget = aec_target:recalculate(AdjHeaders),
            Block1 = aec_blocks:set_target(Block, CalculatedTarget),
            {ok, Block1};
        false -> %% Wrong number of headers in AdjHeaders...
            {error, {wrong_headers_for_target_adjustment, DeltaHeight, length(AdjHeaders)}}
    end.

%% -------------------------------------------------------------------
%% Keyblock sealing
key_header_for_sealing(Header) ->
    Header1 = aec_headers:set_nonce(Header, 0),
    Header2 = aec_headers:set_key_seal(Header1, no_value),
    aec_headers:serialize_to_binary(Header2).

validate_key_header_seal(Header, _Protocol) ->
    %% Zero nonce and pow_evidence before hashing, as this is how the mined block
    %% got hashed.
    Nonce = aec_headers:nonce(Header),
    Evd = aec_headers:pow(Header),
    Target = aec_headers:target(Header),
    HeaderBinary = key_header_for_sealing(Header),
    case aec_mining:verify(HeaderBinary, Nonce, Evd, Target) of
        true ->
            ok;
        false ->
            {error, incorrect_pow}
    end.

generate_key_header_seal(HeaderBin, Header, Nonce, MinerConfig, AddressedInstance) ->
    Target = aec_headers:target(Header),
    { continue_mining,
      aec_mining:generate(HeaderBin, Target, Nonce, MinerConfig, AddressedInstance)
    }.

set_key_block_seal(Block, {Nonce, Evd}) ->
    aec_blocks:set_nonce_and_pow(Block, Nonce, Evd).

nonce_for_sealing(_Header) ->
    aeminer_pow:pick_nonce().

next_nonce_for_sealing(Nonce, MinerConfig) ->
    aeminer_pow:next_nonce(Nonce, MinerConfig).

trim_sealing_nonce(Nonce, MinerConfig) ->
    aeminer_pow:trim_nonce(Nonce, MinerConfig).

%% -------------------------------------------------------------------
%% Block target and difficulty
default_target() ->
    ?HIGHEST_TARGET_SCI.

assert_key_target_range(_Target) ->
    ok.

key_header_difficulty(Header) ->
    aeminer_pow:target_to_difficulty(aec_headers:target(Header)).

load_whitelist() ->
    W = aec_fork_block_settings:block_whitelist(),
    persistent_term:put(?WHITELIST, W).

keyblocks_for_target_calc() ->
    1.
