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
-define(PARENT_GENERATION, 10).
-define(PARENT_FINALITY, 3).
-define(KEY_HASH_1, <<"kh_oRJt1P7b9vnB6ZyeqEFYkYR6u13vmxYfi3iw2v8AqdG62DtnK">>).
-define(KEY_HASH_2, <<"kh_sJwBjpA9rZYYR15aE1U9ohZ7em2iygxoshRKhpNjekgK8rzAk">>).
-define(KEY_HASH_3, <<"kh_245yJhNabPFMcrByHVho2e48rdKQgswy7tGv3kaLNaV49zDArB">>).
-define(KEY_HASH_4, <<"kh_oRJt1P7b9vnB6ZyeqEFYkYR6u13vmxYfi3iw2v8AqdG62DtnK">>).



epoch_test_() ->
    {foreach,
     fun() ->
            aeu_ets_cache:put(?ETS_CACHE_TABLE, hash_to_int, fun hash_to_int/1),
            set_pc_start_height(?CHILD_START_HEIGHT),
            set_pc_finality(?PARENT_FINALITY),
            set_parent_generation(?PARENT_GENERATION),
            set_child_epoch_length(?CHILD_EPOCH_LENGTH),
            meck:new(aec_parent_chain_cache, []),
            meck:expect(aec_parent_chain_cache, get_block_by_height,
                            fun(Height) -> {ok, new_block(?KEY_HASH_3, Height, ?KEY_HASH_4)} end),
            meck:new(aeu_env, [passthrough]),
            meck:expect(aeu_env, user_config, fun(_Keys) -> {ok, ok} end)
     end,
     fun(_) ->
            meck:unload(aec_parent_chain_cache),
            meck:unload(aeu_env),
            ets:delete(?ETS_CACHE_TABLE)
     end,
     [  {"Test a new epoch is created", fun new_epoch/0},
        {"Test a cached epoch is used", fun cached_new_epoch/0},
        {"Test next entropy", fun next_entropy/0},
        {"Test next entropy comes from the cache", fun next_entropy_from_cache/0}
     ]}.


new_epoch() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ?CHILD_EPOCH_LENGTH),
    ok.

cached_new_epoch() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ?CHILD_EPOCH_LENGTH),
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ?CHILD_EPOCH_LENGTH),
    ok.

next_entropy() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ?CHILD_EPOCH_LENGTH),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?KEY_HASH_3, ?CHILD_START_HEIGHT, ?KEY_HASH_4, ?CHILD_EPOCH_LENGTH + 1),
    ok.

next_entropy_from_cache() ->
    check_new_epoch(?KEY_HASH_1, ?CHILD_START_HEIGHT, ?KEY_HASH_2, ?CHILD_EPOCH_LENGTH),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?KEY_HASH_3, ?CHILD_START_HEIGHT, ?KEY_HASH_4, ?CHILD_EPOCH_LENGTH + 1),
    check_next_entrophy_from_epoch(?KEY_HASH_1, ?KEY_HASH_3, ?CHILD_START_HEIGHT, ?KEY_HASH_4, ?CHILD_EPOCH_LENGTH + 1),
    ok.

check_new_epoch(PCHash, PCHeight, PCPrevHash, Height) ->
    Block = new_block(PCHash, PCHeight, PCPrevHash),
    Entrophy = aec_consensus_hc:entropy(Block, Height),
    ?assertEqual(PCHash, Entrophy),
    Entrophy.

check_next_entrophy_from_epoch(EpochPCHash, PCHash, PCHeight, PCPrevHash, Height) ->
    Block = new_block(PCHash, PCHeight, PCPrevHash),
    Entrophy = aec_consensus_hc:entropy(Block, Height),
    {IntEntropy, State} = next_entropy_from_hash(EpochPCHash),
    ExpectedEntropy = list_to_bitstring(base58:binary_to_base58(IntEntropy)),
    ?assertEqual(ExpectedEntropy, Entrophy),
    State.

next_entropy_from_hash(PCHash) ->
    Seed = hash_to_int(PCHash),
    State = rand:seed_s(exsss, Seed),
    rand:bytes_s(256, State).


new_block(Hash, Height, PrevHash) ->
    aec_parent_chain_block:new(Hash, Height, PrevHash).

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
