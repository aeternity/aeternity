-module(aestratum_reward_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([reward_1/1,
         reward_2/1,
         reward_3/1,
         purge_records_1/1,
         reward_7/1,
         purge_records_7/1,
         purge_records_2/1]).

-define(LAST_N, 2).
-define(BLOCK_REWARD, 100000000).
-define(MAX_TARGET, 16#ffff000000000000000000000000000000000000000000000000000000000000).
-define(KEYBLOCK_ROUNDS_DELAY, 180).
-define(MAP(X), aestratum_reward:to_map(X)).
-define(LOG_FILE, "/tmp/aestratum_reward_SUITE_log.txt").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [reward_1,
     reward_2,
     reward_3,
     purge_records_1,
     reward_7,
     purge_records_7,
     purge_records_2].

init_per_suite(_) ->
    ok = application:start(gproc),
    ok = mnesia:start(),
    [_ | _] = aestratum_db:create_tables(ram),

    meck:new(aestratum_chain, [passthrough]),
    meck:expect(aestratum_chain, get_reward_key_header, 1, {error, cant_get_top_key_block}),

    %% dbg:tracer(),
    %% dbg:p(all, call),
    %% dbg:tpl(aestratum_reward, start_link, [{'_', [], [{return_trace}]}]),
    %% dbg:tpl(aestratum_reward, submit_share, [{'_', [], [{return_trace}]}]),

    spawn(fun () ->
                  Benefs = {lists:sum(maps:values(beneficiaries())), beneficiaries()},
                  {ok, _} = aestratum_reward:start_link(?LAST_N, Benefs)
          end),
    Wait = fun F() ->
                   is_pid(whereis(aestratum_reward)) orelse
                       begin timer:sleep(10), F() end
           end,
    Wait(),

    [_] = mnesia:dirty_all_keys(aestratum_round),
    [] = mnesia:dirty_all_keys(aestratum_reward),
    [] = mnesia:dirty_all_keys(aestratum_share),
    [] = mnesia:dirty_all_keys(aestratum_hash),

    KeyShareHashes = submit_rand_rounds_with_key_shares(10, 100),
    gen_server:call(aestratum_reward, sync),
    {PoolSharePct, _MinersSharePct} = shares_split(),
    PoolTotalTokens = round(?BLOCK_REWARD / PoolSharePct),
    MinersTotalTokens = ?BLOCK_REWARD - PoolTotalTokens,
    [{key_hashes, KeyShareHashes},
     {pool_total_tokens, PoolTotalTokens},
     {miners_total_tokens, MinersTotalTokens}].

end_per_suite(_) ->
    ok.

%%%%%%%%%%

reward_1(Config) ->
    Height = 1,
    Hash = get_key_share_hash(Height, Config),

    mock_chain(Height, Hash),
    aestratum_reward:keyblock(),
    gen_server:call(aestratum_reward, sync),

    #{key := SortKey} = get_map(aestratum_hash, Hash),
    [OldestRound, NextOldestRound | _] = lists:reverse(mnesia:dirty_all_keys(aestratum_round)),
    true = OldestRound > SortKey,
    true = NextOldestRound < SortKey,

    [1] = mnesia:dirty_all_keys(aestratum_reward),

    check_amounts(Config, Height).


reward_2(Config) ->
    Height = 2,
    Hash = get_key_share_hash(Height, Config),
    mock_chain(Height, Hash),
    aestratum_reward:keyblock(),
    gen_server:call(aestratum_reward, sync),

    #{key := SortKey} = get_map(aestratum_hash, Hash),
    [OldestRound, _, BoundaryRound | _] =
        lists:reverse(mnesia:dirty_all_keys(aestratum_round)),
    true = OldestRound > SortKey,
    true = BoundaryRound < SortKey,

    [1, 2] = mnesia:dirty_all_keys(aestratum_reward),

    check_amounts(Config, Height).

    %% #{pool := Pool, miners := Miners, unpaid := Unpaid} = get_map(aestratum_reward, Height).
    %% %% true = (sum_values(Pool) + sum_values(Miners) + Unpaid) == ?BLOCK_REWARD.


reward_3(Config) ->
    Height = 3,
    Hash = get_key_share_hash(Height, Config),
    mock_chain(Height, Hash),
    aestratum_reward:keyblock(),
    gen_server:call(aestratum_reward, sync),

    #{key := SortKey} = get_map(aestratum_hash, Hash),
    [_, OlderBoundaryRound, _, NewerBoundaryRound | _] =
        lists:reverse(mnesia:dirty_all_keys(aestratum_round)),
    true = OlderBoundaryRound > SortKey,
    true = NewerBoundaryRound < SortKey,

    [1, 2, 3] = mnesia:dirty_all_keys(aestratum_reward),

    check_amounts(Config, Height).


purge_records_1(_Config) ->
    Height = 1,
    [_] = mnesia:dirty_read(aestratum_reward, Height),
    Sizes1 = [{X, mnesia:table_info(X, size)} || X <- tabs() -- [aestratum_reward]],
    aestratum_reward:confirm_payout(Height),
    gen_server:call(aestratum_reward, sync),
    [] = mnesia:dirty_read(aestratum_reward, Height),
    Sizes2 = [{X, mnesia:table_info(X, size)} || X <- tabs() -- [aestratum_reward]],
    true = Sizes1 == Sizes2.


reward_7(Config) ->
    Height = 7,
    Hash = get_key_share_hash(Height, Config),
    mock_chain(Height, Hash),
    aestratum_reward:keyblock(),
    gen_server:call(aestratum_reward, sync),

    #{key := SortKey} = get_map(aestratum_hash, Hash),
    [_, _, _, _, _, OlderBoundaryRound, _, NewerBoundaryRound | _] =
        lists:reverse(mnesia:dirty_all_keys(aestratum_round)),
    true = OlderBoundaryRound > SortKey,
    true = NewerBoundaryRound < SortKey,

    [2, 3, 7] = mnesia:dirty_all_keys(aestratum_reward),

    check_amounts(Config, Height).


purge_records_7(_Config) ->
    Height = 7,
    #{round_key := RoundKey} = get_map(aestratum_reward, Height),
    aestratum_reward:confirm_payout(Height),
    gen_server:call(aestratum_reward, sync),

    [2, 3] = mnesia:dirty_all_keys(aestratum_reward),

    AllVals = fun (Tab) -> mnesia:dirty_select(Tab, [{'$1', [], ['$1']}]) end,
    CheckKey = fun (X) -> maps:get(key, aestratum_reward:to_map(X)) < RoundKey end,

    true = lists:all(CheckKey, AllVals(aestratum_hash)),
    true = lists:all(CheckKey, AllVals(aestratum_share)),
    true = lists:all(CheckKey, AllVals(aestratum_round) --
                         [{aestratum_round, RoundKey, undefined}]).


purge_records_2(_Config) ->
    Height = 2,
    [_] = mnesia:dirty_read(aestratum_reward, Height),
    Sizes1 = [{X, mnesia:table_info(X, size)} || X <- tabs() -- [aestratum_reward]],
    aestratum_reward:confirm_payout(Height),
    gen_server:call(aestratum_reward, sync),
    [3] = mnesia:dirty_all_keys(aestratum_reward),
    Sizes2 = [{X, mnesia:table_info(X, size)} || X <- tabs() -- [aestratum_reward]],
    true = Sizes1 == Sizes2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_amounts(Config, Height) ->
    #{pool := Pool, miners := Miners} = get_map(aestratum_reward, Height),
    {_, PoolTokens} = lists:keyfind(pool_total_tokens, 1, Config),
    {_, MinerTokens} = lists:keyfind(miners_total_tokens, 1, Config),
    PoolSum = sum_values(aestratum_chain:calculate_rewards(Pool, PoolTokens)),
    MinerSum = sum_values(aestratum_chain:calculate_rewards(Miners, MinerTokens)),
    %% during computations with floats, imprecisions can add up so we distribute
    %% smaller amount than we could.
    true = PoolSum =< PoolTokens andalso (PoolTokens - PoolSum) =< 10,
    true = MinerSum =< MinerTokens andalso (MinerTokens - MinerSum) =< 10.


tabs() -> [aestratum_hash, aestratum_share, aestratum_round, aestratum_reward].

sum_values(#{} = Xs) -> lists:sum(maps:values(Xs)).

get_key_share_hash(Height, Config) ->
    {key_hashes, KeyShareHashes} = lists:keyfind(key_hashes, 1, Config),
    <<"ak_MINER_", _/binary>> = lists:nth(Height, KeyShareHashes).

get_map(Tab, Key) ->
    [Record] = mnesia:dirty_read(Tab, Key),
    aestratum_reward:to_map(Record).

mock_chain(Height, <<"ak_MINER_", _/binary>> = Hash) ->
    [<<"ak_MINER_", _/binary>> = Miner, _] = binary:split(Hash, <<" ">>),
    meck:expect(aestratum_chain, get_reward_key_header,
                fun (_) ->
                        KH = {key_header,
                              Height,      % height
                              <<>>,        % prev_hash
                              <<>>,        % prev_key
                              <<>>,        % root_hash
                              ?MAX_TARGET, % target
                              0,           % nonce
                              0,           % time
                              1,           % version
                              [],          % pow_evidence
                              <<>>,        % miner
                              Miner,       % beneficiary
                              <<>>},       % info
                        {ok, KH}
                end),
    meck:expect(aestratum_chain, hash_header, 1, Hash),
    meck:expect(aestratum_chain, header_info, 1, {ok, Height, ?MAX_TARGET, ?BLOCK_REWARD}),
    meck:expect(aestratum_chain, payout_rewards,
                fun (_, _PoolRewards, _MinersRewards) ->
                        %% log("!!!!!!!!!! REWARDS (~p) = ~p | ~p",
                        %%     [Height, PoolRewards, MinersRewards]),
                        ok
                end),
    ok.


-define(TEST_MINERS, [<<"ak_MINER_A_012345678901234567890123">>,
                      <<"ak_MINER_B_012345678901234567890123">>,
                      <<"ak_MINER_C_012345678901234567890123">>,
                      <<"ak_MINER_D_012345678901234567890123">>,
                      <<"ak_MINER_E_012345678901234567890123">>,
                      <<"ak_MINER_F_012345678901234567890123">>,
                      <<"ak_MINER_G_012345678901234567890123">>,
                      <<"ak_MINER_H_012345678901234567890123">>,
                      <<"ak_MINER_I_012345678901234567890123">>,
                      <<"ak_MINER_J_012345678901234567890123">>,
                      <<"ak_MINER_K_012345678901234567890123">>,
                      <<"ak_MINER_L_012345678901234567890123">>,
                      <<"ak_MINER_M_012345678901234567890123">>,
                      <<"ak_MINER_N_012345678901234567890123">>,
                      <<"ak_MINER_O_012345678901234567890123">>,
                      <<"ak_MINER_P_012345678901234567890123">>,
                      <<"ak_MINER_Q_012345678901234567890123">>,
                      <<"ak_MINER_R_012345678901234567890123">>,
                      <<"ak_MINER_S_012345678901234567890123">>,
                      <<"ak_MINER_T_012345678901234567890123">>,
                      <<"ak_MINER_U_012345678901234567890123">>,
                      <<"ak_MINER_V_012345678901234567890123">>,
                      <<"ak_MINER_W_012345678901234567890123">>,
                      <<"ak_MINER_X_012345678901234567890123">>,
                      <<"ak_MINER_Y_012345678901234567890123">>,
                      <<"ak_MINER_Z_012345678901234567890123">>]).

-define(TEST_MINERS_LENGTH, 26).

miner_target(<<"ak_MINER_", X:8, "_", _/binary>>) -> round(?MAX_TARGET / (2 * 0.4 * (X - 63))).

submit_share({Miner, Target, Hash}) ->
    aestratum_reward:submit_share(Miner, Target, Hash).

rand_miner() ->
    lists:nth(rand:uniform(?TEST_MINERS_LENGTH), ?TEST_MINERS).

rand_hash() ->
    Uniq = io_lib:format("~p ~p", [erlang:timestamp(), erlang:unique_integer()]),
    crypto:hash(sha, iolist_to_binary(Uniq)).


rand_hashes_with_key_hash(Num) when Num > 0 ->
    KeyHash = fmt("~s KEY-HASH~p", [rand_miner(), erlang:unique_integer()]),
    KeyPos = rand:uniform(Num),
    {KeyHash, [if I == KeyPos -> KeyHash; true -> rand_hash() end ||
                  I <- lists:seq(1, Num)]}.

submit_rand_shares_with_key_share(Num) when Num > 0 ->
    {KeyHash, AllHashes} = rand_hashes_with_key_hash(Num),
    Submit = fun (Miner, Hash) -> submit_share({Miner, miner_target(Miner), Hash}) end,
    {KeyHash, [if Hash == KeyHash ->
                       [Miner, _] = binary:split(Hash, <<" ">>),
                       Submit(Miner, Hash);
                  true ->
                       Submit(rand_miner(), Hash)
               end || Hash <- AllHashes]}.

submit_rand_rounds_with_key_shares(Rounds, SharesPerRound)
  when Rounds > 0, SharesPerRound > 0 ->
    [begin
         {KeyShareHash, _} = submit_rand_shares_with_key_share(SharesPerRound),
         aestratum_reward:keyblock(),
         KeyShareHash
     end || _ <- lists:seq(1, Rounds)].


beneficiaries() ->
    #{<<"ak_benef_1">> => 1,
      <<"ak_benef_2">> => 2,
      <<"ak_benef_4">> => 4,
      <<"ak_benef_8">> => 8}.

shares_split() ->
    PoolShare = sum_values(beneficiaries()),
    {PoolShare, 100 - PoolShare}.

fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).

log(Format, Args) ->
    file:write_file(?LOG_FILE, fmt(Format ++ "\n", Args), [write, append]).
