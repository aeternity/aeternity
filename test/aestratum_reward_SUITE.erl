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
    aestratum_reward:create_tables(ram),

    meck:new(aestratum_chain),
    meck:expect(aestratum_chain, get_reward_key_header, 1, {error, cant_get_top_key_block}),

    %% dbg:tracer(),
    %% dbg:p(all, call),
    %% dbg:tpl(aestratum_reward, sort_key, [{'_', [], [{return_trace}]}]),
    %% dbg:tpl(aestratum_reward, submit_share, [{'_', [], [{return_trace}]}]),

    spawn(fun () -> {ok, _} = aestratum_reward:start_link(?LAST_N, beneficiaries()) end),
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

    [{key_hashes, KeyShareHashes}].

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

    #{pool := Pool, miners := Miners, unpaid := Unpaid} = get_map(aestratum_reward, Height),
    true = (sum_values(Pool) + sum_values(Miners) + Unpaid) == ?BLOCK_REWARD.


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

    #{pool := Pool, miners := Miners, unpaid := Unpaid} = get_map(aestratum_reward, Height),
    true = (sum_values(Pool) + sum_values(Miners) + Unpaid) == ?BLOCK_REWARD.


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

    #{pool := Pool, miners := Miners, unpaid := Unpaid} = get_map(aestratum_reward, Height),
    true = (sum_values(Pool) + sum_values(Miners) + Unpaid) == ?BLOCK_REWARD.


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

    #{pool := Pool, miners := Miners, unpaid := Unpaid} = get_map(aestratum_reward, Height),
    true = (sum_values(Pool) + sum_values(Miners) + Unpaid) == ?BLOCK_REWARD.


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

tabs() -> [aestratum_hash, aestratum_share, aestratum_round, aestratum_reward].

sum_values(#{} = Xs) -> lists:sum(maps:values(Xs)).

get_key_share_hash(Height, Config) ->
    {key_hashes, KeyShareHashes} = lists:keyfind(key_hashes, 1, Config),
    <<"MINER_", _/binary>> = lists:nth(Height, KeyShareHashes).

get_map(Tab, Key) ->
    [Record] = mnesia:dirty_read(Tab, Key),
    aestratum_reward:to_map(Record).

mock_chain(Height, <<"MINER_", _/binary>> = Hash) ->
    [<<"MINER_", _:8>> = Miner, _] = binary:split(Hash, <<" ">>),
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


-define(TEST_MINERS, [<<"MINER_A">>,<<"MINER_B">>,<<"MINER_C">>,<<"MINER_D">>,
                      <<"MINER_E">>,<<"MINER_F">>,<<"MINER_G">>,<<"MINER_H">>,
                      <<"MINER_I">>,<<"MINER_J">>,<<"MINER_K">>,<<"MINER_L">>,
                      <<"MINER_M">>,<<"MINER_N">>,<<"MINER_O">>,<<"MINER_P">>,
                      <<"MINER_Q">>,<<"MINER_R">>,<<"MINER_S">>,<<"MINER_T">>,
                      <<"MINER_U">>,<<"MINER_V">>,<<"MINER_W">>,<<"MINER_X">>,
                      <<"MINER_Y">>,<<"MINER_Z">>]).

-define(TEST_MINERS_LENGTH, 26).

miner_target(<<"MINER_", X:8>>) -> round(?MAX_TARGET / (2 * 0.4 * (X - 63))).

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
                       <<"MINER_", X:8, _/binary>> = Hash,
                       Submit(<<"MINER_", X:8>>, Hash);
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
    #{<<"benef_1">> => 1,
      <<"benef_2">> => 2,
      <<"benef_4">> => 4,
      <<"benef_8">> => 8}.

fmt(Format, Args) ->
    iolist_to_binary(io_lib:format(Format, Args)).

%% log(Format, Args) ->
%%     file:write_file(?LOG_FILE, fmt(Format ++ "\n", Args), [write, append]).
