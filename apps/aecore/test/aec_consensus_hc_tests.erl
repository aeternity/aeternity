%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Foundation
%%% @doc Producer selection in aec_consensus_hc:next_producer/0 after a
%%%      production stall that crosses the end of an epoch.
%%%
%%%      The chain is stalled at height 89, one below the last slot (90) of
%%%      epoch 9. This node holds only Bob's key. Lisa, whose node is down,
%%%      leads slot 90, so the wall-clock slot runs past the epoch and every
%%%      call goes through the stall-recovery path.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_consensus_hc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(HC, aec_consensus_hc).
-define(BLOCK_TIME, 800).
-define(PRODUCTION_TIME, 120).
-define(PARENT_EPOCH_LENGTH, 3).
-define(PC_START_HEIGHT, 107).
%% Time of block 80, which epoch 9's slots are counted from.
-define(EPOCH_9_T0, 1789347260000).

-define(ALICE, <<1:256>>).
-define(BOB,   <<2:256>>).
-define(LISA,  <<3:256>>).
-define(VALIDATORS, [{?ALICE, 3000}, {?BOB, 2000}, {?LISA, 1000}]).

%% Parent block whose hash seeds epoch 11: (11 - 3) * 3 + 107.
-define(EPOCH_11_ENTROPY_HEIGHT, 131).
-define(EPOCH_11_SEED, <<131:256>>).

stall_recovery_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"A short next epoch led by the down validator does not hide the "
       "live producer's slot in the epoch after it",
       fun() ->
               short_next_epoch(),
               set_wall_clock_slot(95),
               ?assertMatch({ok, {leader_hole, ?BOB, 3}, _}, ?HC:next_producer())
       end},
      {"The epoch after next is looked up for the stall, never cached",
       fun() ->
               short_next_epoch(),
               set_wall_clock_slot(95),
               _ = ?HC:next_producer(),
               ?assertEqual({error, not_in_cache}, ?HC:leader_for_height(93))
       end},
      {"No slot is claimed ahead of the wall clock",
       fun() ->
               short_next_epoch(),
               set_wall_clock_slot(92),
               ?assertMatch({wait, not_producer, _}, ?HC:next_producer())
       end},
      {"Without the parent block that seeds the epoch after next, the "
       "producer waits",
       fun() ->
               short_next_epoch(),
               meck:expect(aec_parent_chain_cache, get_block_by_height,
                           fun(_) -> {error, not_in_cache} end),
               set_wall_clock_slot(95),
               ?assertMatch({wait, not_producer, _}, ?HC:next_producer())
       end},
      {"A next epoch longer than this one is scanned to its own end",
       fun() ->
               long_next_epoch(),
               set_wall_clock_slot(120),
               ?assertMatch({ok, {leader_hole, ?BOB, 12}, _}, ?HC:next_producer())
       end}
     ]}.

setup() ->
    catch ets:delete(?HC),
    aeu_ets_cache:put(?HC, child_block_time, ?BLOCK_TIME),
    aeu_ets_cache:put(?HC, child_block_production_time, ?PRODUCTION_TIME),
    aeu_ets_cache:put(?HC, parent_epoch_length, ?PARENT_EPOCH_LENGTH),
    aeu_ets_cache:put(?HC, pc_start_height, ?PC_START_HEIGHT),
    Mods = [aec_chain, aec_chain_hc, aetx_env, aeu_time,
            aec_parent_chain_cache, aec_preset_keys],
    [ meck:new(M, [passthrough]) || M <- Mods ],
    meck:expect(aec_chain, top_key_block, fun() -> {ok, key_block(89, ?EPOCH_9_T0 + 9 * ?BLOCK_TIME)} end),
    meck:expect(aec_chain, get_key_block_by_height, fun(80) -> {ok, key_block(80, ?EPOCH_9_T0)} end),
    meck:expect(aetx_env, tx_env_and_trees_from_top, fun(aetx_transaction) -> {tx_env, trees} end),
    meck:expect(aec_chain_hc, epoch_info, fun({tx_env, trees}) -> {ok, epoch_info(9, 81, 10, <<9:256>>)} end),
    meck:expect(aec_parent_chain_cache, get_block_by_height,
                fun(?EPOCH_11_ENTROPY_HEIGHT) ->
                        {ok, aec_parent_chain_block:new(?EPOCH_11_SEED, ?EPOCH_11_ENTROPY_HEIGHT, <<130:256>>, 0)};
                   (_) ->
                        {error, not_in_cache}
                end),
    meck:expect(aec_preset_keys, is_key_present, fun(Pubkey) -> Pubkey =:= ?BOB end),
    meck:expect(aec_preset_keys, set_candidate,
                fun(?BOB) -> ok;
                   (_)    -> {error, key_not_found}
                end),
    Mods.

teardown(Mods) ->
    meck:unload(Mods),
    catch ets:delete(?HC),
    ok.

%% Epoch 10 is a single slot, as after an epoch length vote of 1 - length.
%% Lisa leads slot 90 and the whole of epoch 10; Bob's first slot is 93, in
%% epoch 11, whose seed is only written by the end-of-epoch step at height 90.
short_next_epoch() ->
    cache_schedule([{9, 81, [?ALICE, ?BOB, ?ALICE, ?LISA, ?ALICE, ?BOB, ?ALICE, ?LISA, ?ALICE, ?LISA]},
                    {10, 91, [?LISA]}]),
    Epoch10 = epoch_info(10, 91, 1, <<10:256>>),
    Epoch11 = epoch_info(11, 92, 23, undefined),
    Schedule11 = [?LISA, ?BOB | lists:duplicate(21, ?ALICE)],
    meck:expect(aec_chain_hc, epoch_info_for_epoch,
                fun({tx_env, trees}, 10) -> {ok, Epoch10};
                   ({tx_env, trees}, 11) -> {ok, Epoch11}
                end),
    meck:expect(aec_chain_hc, validator_schedule,
                fun({tx_env, trees}, ?EPOCH_11_SEED, ?VALIDATORS, 23) -> {ok, Schedule11} end).

%% Epoch 10 runs 91..113. Bob's first slot is 102, past 90 + epoch 9's length.
long_next_epoch() ->
    cache_schedule([{9, 81, [?ALICE, ?BOB, ?ALICE, ?LISA, ?ALICE, ?BOB, ?ALICE, ?LISA, ?ALICE, ?LISA]},
                    {10, 91, lists:duplicate(11, ?LISA) ++ [?BOB | lists:duplicate(11, ?ALICE)]}]),
    Epoch10 = epoch_info(10, 91, 23, <<10:256>>),
    meck:expect(aec_chain_hc, epoch_info_for_epoch,
                fun({tx_env, trees}, 10) -> {ok, Epoch10} end).

cache_schedule(Epochs) ->
    Cache = lists:foldl(
              fun({Epoch, First, Leaders}, #{epochs := Es, epoch_infos := EIs, schedule := S}) ->
                      Last = First + length(Leaders) - 1,
                      #{epochs      => Es ++ [Epoch],
                        epoch_infos => EIs#{Epoch => {First, Last}},
                        schedule    => maps:merge(S, maps:from_list(lists:zip(lists:seq(First, Last), Leaders)))}
              end, #{epochs => [], epoch_infos => #{}, schedule => #{}}, Epochs),
    aeu_ets_cache:put(?HC, validator_schedule, Cache).

set_wall_clock_slot(Slot) ->
    Now = ?EPOCH_9_T0 + (Slot - 81) * ?BLOCK_TIME + 10,
    meck:expect(aeu_time, now_in_msecs, fun() -> Now end).

epoch_info(Epoch, First, Length, Seed) ->
    #{epoch => Epoch, first => First, last => First + Length - 1, length => Length,
      seed => Seed, validators => ?VALIDATORS}.

key_block(Height, Time) ->
    aec_blocks:new_key(Height, <<0:256>>, <<0:256>>, <<0:256>>, undefined,
                       0, Time, default, ?CERES_PROTOCOL_VSN, ?BOB, ?BOB).
