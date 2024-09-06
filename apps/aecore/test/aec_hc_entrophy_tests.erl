%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, Aeternity Anstalt
%%% @doc
%%% EUnit tests for hyper chains entrophy
%%% @end
%%%-------------------------------------------------------------------
-module(aec_hc_entrophy_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_CACHE_TABLE, aec_consensus_hc).
-define(CHILD_START_HEIGHT, 101).
-define(CHILD_EPOCH_LENGTH, 10).
-define(CHILD_BLOCK_TIME, 1000).
-define(PARENT_GENERATION, 10).
-define(PARENT_FINALITY, 3).
-define(ACCEPTABLE_SYNC_OFFSET, 60).
-define(KEY_HASH_1, <<"kh_oRJt1P7b9vnB6ZyeqEFYkYR6u13vmxYfi3iw2v8AqdG62DtnK">>).
-define(KEY_HASH_2, <<"kh_sJwBjpA9rZYYR15aE1U9ohZ7em2iygxoshRKhpNjekgK8rzAk">>).
-define(KEY_HASH_3, <<"kh_245yJhNabPFMcrByHVho2e48rdKQgswy7tGv3kaLNaV49zDArB">>).
-define(KEY_HASH_4, <<"kh_oRJt1P7b9vnB6ZyeqEFYkYR6u13vmxYfi3iw2v8AqdG62DtnK">>).
-define(EPOCH_KEY_PC_BLOCK_TIME_1, 0).
-define(EPOCH_KEY_PC_BLOCK_TIME_2, ?CHILD_BLOCK_TIME * ?CHILD_EPOCH_LENGTH).



epoch_test_() ->
    {foreach,
     fun() ->
            set_defaults(),
            meck:new(aec_parent_chain_cache, []),
            meck:expect(aec_parent_chain_cache, get_block_by_height,
                            fun(Height) -> {ok, new_block(?KEY_HASH_3, Height, ?KEY_HASH_4, 0)} end),
            meck:new(aec_consensus_hc, [passthrough]),
            meck:expect(aec_consensus_hc, child_epoch_length,
                            fun(_EpochLength, _TxEnv, Trees) -> Trees end)

     end,
     fun(_) ->
            meck:unload(aec_parent_chain_cache),
            meck:unload(aec_consensus_hc),
            cleanup_cache()
     end,
     [  {"Test a new epoch is created", fun new_epoch/0},
        {"Test a cached epoch is used", fun cached_new_epoch/0},
        {"Test next entropy", fun next_entropy/0},
        {"Test next entropy comes from the cache", fun next_entropy_from_cache/0},
        {"Test next entropy with a seed", fun next_entropy_with_seed/0},
        {"Test next entropy with a seed comes from the cache", fun next_entropy_with_seed_from_cache/0},
        {"Test a full epoch", fun full_epoch/0}
     ]}.

sync_test_() ->
    {foreach,
     fun() ->
            set_defaults(),
            meck:new(aec_parent_chain_cache, []),
            meck:expect(aec_parent_chain_cache, get_block_by_height,
                            fun(Height) -> {ok, new_block(?KEY_HASH_3, Height, ?KEY_HASH_4, 0)} end),
            meck:new(aec_consensus_hc, [passthrough]),
            meck:expect(aec_consensus_hc, child_epoch_length,
                            fun(_EpochLength, _TxEnv, Trees) -> Trees end)

     end,
     fun(_) ->
            meck:unload(aec_parent_chain_cache),
            meck:unload(aec_consensus_hc),
            cleanup_cache()
     end,
     [ {"Test a fast parent chain", fun fast_parent/0},
       {"Test a slow parent chain", fun slow_parent/0}
     ]}.

set_defaults() ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, hash_to_int, fun hash_to_int/1),
    set_pc_start_height(?CHILD_START_HEIGHT),
    set_pc_finality(?PARENT_FINALITY),
    set_parent_generation(?PARENT_GENERATION),
    set_child_epoch_length(?CHILD_EPOCH_LENGTH),
   set_child_block_time(?CHILD_BLOCK_TIME),
    set_acceptable_sync_offset(?ACCEPTABLE_SYNC_OFFSET).

new_epoch() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    ok.

cached_new_epoch() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    ok.

next_entropy() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    ok.

next_entropy_from_cache() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    ok.

next_entropy_with_seed() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    State = check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    _State2 = check_next_entrophy_from_seed(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 2, ?EPOCH_KEY_PC_BLOCK_TIME_1, State),
    ok.

next_entropy_with_seed_from_cache() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 0, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    State = check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    State = check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    State2 = check_next_entrophy_from_seed(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 2, ?EPOCH_KEY_PC_BLOCK_TIME_1, State),
    State2 = check_next_entrophy_from_seed(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, 2, ?EPOCH_KEY_PC_BLOCK_TIME_1, State),
    ok.

full_epoch() ->
    full_epoch(?EPOCH_KEY_PC_BLOCK_TIME_1, ?EPOCH_KEY_PC_BLOCK_TIME_2).

fast_parent() ->
    PCTime2 = ?EPOCH_KEY_PC_BLOCK_TIME_2 * 0.8,
    full_epoch(?EPOCH_KEY_PC_BLOCK_TIME_1, PCTime2),
    ExpectedChildEpochLength = round(?CHILD_EPOCH_LENGTH * 0.8),
    ?assertEqual(get_child_epoch_length(), ExpectedChildEpochLength).

slow_parent() ->
    PCTime2 = ?EPOCH_KEY_PC_BLOCK_TIME_2 * 1.2,
    full_epoch(?EPOCH_KEY_PC_BLOCK_TIME_1, PCTime2),
    ExpectedChildEpochLength = round(?CHILD_EPOCH_LENGTH * 1.2),
    ?assertEqual(get_child_epoch_length(), ExpectedChildEpochLength),
    ok.

full_epoch(PCTime1, PCTime2) ->
    Height = 0,
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, Height, PCTime1),
    State  = check_next_entrophy_from_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, Height + 1, ?EPOCH_KEY_PC_BLOCK_TIME_1),
    _State2 = lists:foldl(fun(ChildHeight, NewState) -> check_next_entrophy_from_seed(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ChildHeight, ?EPOCH_KEY_PC_BLOCK_TIME_1, NewState) end,
                            State, lists:seq(Height +2, ?CHILD_EPOCH_LENGTH -1)),
    check_new_epoch(?KEY_HASH_3, ?CHILD_START_HEIGHT + ?PARENT_GENERATION, ?KEY_HASH_4, Height + ?CHILD_EPOCH_LENGTH, PCTime2),
    ok.

check_new_epoch(PCHash, PCHeight, PCPrevHash, Height, PCTime) ->
    Block = new_block(PCHash, PCHeight, PCPrevHash, PCTime),
    {Entrophy, _} = aec_consensus_hc:entropy(Block, Height, txenv, trees),
    ?assertEqual(PCHash, Entrophy),
    Entrophy.

check_next_entrophy_from_epoch(PCHash, PCHeight, PCPrevHash, Height, Time) ->
    Block = new_block(PCHash, PCHeight, PCPrevHash, Time),
    {Entrophy,_} = aec_consensus_hc:entropy(Block, Height, txenv, trees),
    {ExpectedEntropy, State} = next_entropy_from_hash(PCHash),
    ?assertEqual(ExpectedEntropy, Entrophy),
    State.

check_next_entrophy_from_seed(PCHash, PCHeight, PCPrevHash, Height, Time, State) ->
    Block = new_block(PCHash, PCHeight, PCPrevHash, Time),
    {Entrophy, _} = aec_consensus_hc:entropy(Block, Height, txenv, trees),
    {ExpectedEntropy, State2} = next_entropy_from_seed(State),
    ?assertEqual(ExpectedEntropy, Entrophy),
    State2.

next_entropy_from_seed(State) ->
    {IntEntropy, State2} = rand:bytes_s(256, State),
    Entropy = int_to_entropy(IntEntropy),
    {Entropy, State2}.

next_entropy_from_hash(PCHash) ->
    Seed = hash_to_int(PCHash),
    State = rand:seed_s(exsss, Seed),
    {IntEntropy, State2} = rand:bytes_s(256, State),
    Entropy = int_to_entropy(IntEntropy),
    {Entropy, State2}.

int_to_entropy(IntEntropy) ->
    list_to_bitstring(base58:binary_to_base58(IntEntropy)).

new_block(Hash, Height, PrevHash, Time) ->
    aec_parent_chain_block:new(Hash, Height, PrevHash, Time).

hash_to_int(Hash) ->
    aehttpc_aeternity:hash_to_integer(Hash).

set_pc_start_height(Height) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, pc_start_height, Height).

set_pc_finality(Finality) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, finality, Finality).

set_parent_generation(Gen) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, parent_generation, Gen).

set_child_epoch_length(Length) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, child_epoch_length, Length).

get_child_epoch_length() ->
    {ok, Length} = aeu_ets_cache:lookup(?ETS_CACHE_TABLE, child_epoch_length),
    Length.

set_child_block_time(Time) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, child_block_time, Time).

set_acceptable_sync_offset(Offset) ->
    aeu_ets_cache:put(?ETS_CACHE_TABLE, acceptable_sync_offset, Offset).

cleanup_cache() ->
    ets:delete(?ETS_CACHE_TABLE, seed),
    ets:delete(?ETS_CACHE_TABLE, parent_generation_block_time),
    ets:delete(?ETS_CACHE_TABLE, child_epoch_height).
