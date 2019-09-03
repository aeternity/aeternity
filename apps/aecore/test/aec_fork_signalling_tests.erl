%%%============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_fork_signalling.
%%% @end
%%%============================================================================
-module(aec_fork_signalling_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("aeminer/include/aeminer.hrl").

-define(TEST_MODULE, aec_fork_signalling).

-define(SIGNALLING_START_HEIGHT, 3).
-define(SIGNALLING_END_HEIGHT, 6).
-define(SIGNALLING_BLOCK_COUNT, 2).

-define(FORK_HEIGHT, 8).

-define(INFO_FIELD_AGAINST, make_info(0)).
-define(INFO_FIELD_SUPPORT, make_info(1)).
-define(INFO_FIELD_OTHER, make_info(999)).

-define(VERSION_OLD, 4).
-define(VERSION_NEW, 5).

-define(TARGET_HIGH, ?HIGHEST_TARGET_SCI).
-define(TARGET_LOW, ?HIGHEST_TARGET_SCI - 1).

-define(FORK_CFG,
        #{signalling_start_height => ?SIGNALLING_START_HEIGHT,
          signalling_end_height   => ?SIGNALLING_END_HEIGHT,
          signalling_block_count  => ?SIGNALLING_BLOCK_COUNT,
          fork_height             => ?FORK_HEIGHT,
          info_field              => ?INFO_FIELD_SUPPORT,
          version                 => ?VERSION_NEW}).

-define(BASIC_CHAIN_NEGATIVE_RESULT,
        #{1                        => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER},
          ?SIGNALLING_START_HEIGHT => #{version => ?VERSION_OLD, info => ?INFO_FIELD_AGAINST},
          ?SIGNALLING_END_HEIGHT   => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER}}).

-define(BASIC_CHAIN_POSITIVE_RESULT,
        #{1                        => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER},
          ?SIGNALLING_START_HEIGHT => #{version => ?VERSION_OLD, info => ?INFO_FIELD_SUPPORT},
          ?SIGNALLING_END_HEIGHT   => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER}}).

-define(FORK_CHAIN_COMMON,
        #{1                        => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER, target => ?TARGET_HIGH}}).

-define(FORK_CHAIN_LOW_DIFFICULTY_NEGATIVE_RESULT,
        #{?SIGNALLING_START_HEIGHT => #{version => ?VERSION_OLD, info => ?INFO_FIELD_AGAINST, target => ?TARGET_HIGH},
          ?SIGNALLING_END_HEIGHT   => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER, target => ?TARGET_HIGH}}).

-define(FORK_CHAIN_HIGH_DIFFICULTY_POSITIVE_RESULT,
        #{?SIGNALLING_START_HEIGHT => #{version => ?VERSION_OLD, info => ?INFO_FIELD_SUPPORT, target => ?TARGET_LOW},
          ?SIGNALLING_END_HEIGHT   => #{version => ?VERSION_OLD, info => ?INFO_FIELD_OTHER, target => ?TARGET_LOW}}).

worker_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:stop_chain_db(),
             aec_test_utils:unmock_genesis_and_forks()
     end,
     [{"Spawn worker and abort fork signalling result computation",
       fun() -> abort_computation(?BASIC_CHAIN_NEGATIVE_RESULT) end},
      {"Spawn worker and compute negative fork signalling result",
       fun() -> perform_computation(?BASIC_CHAIN_NEGATIVE_RESULT, false) end},
      {"Spawn worker and compute positive fork signalling result",
       fun() -> perform_computation(?BASIC_CHAIN_POSITIVE_RESULT, true) end}
     ]}.

chain_test_() ->
    {foreach,
     fun() ->
             aec_test_utils:mock_genesis_and_forks(),
             aec_test_utils:mock_governance(),
             ?TEST_MODULE:start_link(),
             aec_test_utils:start_chain_db(),
             aec_test_utils:aec_keys_setup()
     end,
     fun(TmpDir) ->
             aec_test_utils:aec_keys_cleanup(TmpDir),
             aec_test_utils:stop_chain_db(),
             ?TEST_MODULE:stop(),
             aec_test_utils:unmock_governance(),
             aec_test_utils:unmock_genesis_and_forks()
     end,
     [{"Basic chain negative result test, no restart",
       fun() -> basic_chain(no_restart, ?BASIC_CHAIN_NEGATIVE_RESULT, false) end},
      {"Basic chain positive result test, no restart",
       fun() -> basic_chain(no_restart, ?BASIC_CHAIN_POSITIVE_RESULT, true) end},
      {"Basic chain negative result, restart at last signalling block",
       fun() -> basic_chain(restart_at_last_signalling_block, ?BASIC_CHAIN_NEGATIVE_RESULT, false) end},
      {"Basic chain positive result, restart at last signalling block",
       fun() -> basic_chain(restart_at_last_signalling_block, ?BASIC_CHAIN_POSITIVE_RESULT, true) end},
      {"Basic chain negative result, restart between last signalling block and last block before fork",
       fun() -> basic_chain(restart_between_last_signalling_block_and_last_block_before_fork, ?BASIC_CHAIN_NEGATIVE_RESULT, false) end},
      {"Basic chain positive result, restart between last signalling block and last block before fork",
       fun() -> basic_chain(restart_between_last_signalling_block_and_last_block_before_fork, ?BASIC_CHAIN_POSITIVE_RESULT, true) end},
      {"Basic chain negative result, restart at last block before fork",
       fun() -> basic_chain(restart_at_last_block_before_fork, ?BASIC_CHAIN_NEGATIVE_RESULT, false) end},
      {"Basic chain positive result, restart at last block before fork",
       fun() -> basic_chain(restart_at_last_block_before_fork, ?BASIC_CHAIN_POSITIVE_RESULT, true) end},
      {"Fork chain at signalling start block",
       fun() -> fork_chain(fork_at_signalling_start_block) end}
     ]}.

abort_computation(BlockCfgs) ->
    LastSigBlockHeight = ?SIGNALLING_END_HEIGHT - 1,

    prepare_chain(?FORK_HEIGHT, BlockCfgs),
    B5 = read_block_from_chain(LastSigBlockHeight),
    BH5 = block_hash(B5),

    WorkerPid = spawn(?TEST_MODULE, worker_process, [self(), B5, BH5, ?FORK_CFG]),

    %% The worker is spawned, it searches for HE - 1 block and asks server if
    %% should continue with computation.
    ?assertEqual({ok, {check_result, BH5, BH5}}, receive_worker_msg(WorkerPid)),
    %% Server can response with compute | abort.
    send_server_msg(WorkerPid, abort),

    ?assertEqual({ok, dead}, ensure_worker_dead(WorkerPid)),
    ok.

perform_computation(BlockCfgs, ExpectedForkResult) ->
    LastSigBlockHeight = ?SIGNALLING_END_HEIGHT - 1,

    prepare_chain(?FORK_HEIGHT, BlockCfgs),
    B5 = read_block_from_chain(LastSigBlockHeight),
    BH5 = block_hash(B5),

    WorkerPid = spawn(?TEST_MODULE, worker_process, [self(), B5, BH5, ?FORK_CFG]),

    %% The worker is spawned, it searches for HE - 1 block and asks server if
    %% should continue with computation.
    ?assertEqual({ok, {check_result, BH5, BH5}}, receive_worker_msg(WorkerPid)),
    %% Server can response with compute | abort.
    send_server_msg(WorkerPid,  compute),
    %% Worker responds with fork result
    ?assertEqual({ok, {add_result, BH5, ExpectedForkResult}}, receive_worker_msg(WorkerPid)),

    ?assertEqual({ok, dead}, ensure_worker_dead(WorkerPid)),
    ok.

%% [0] --- [1] --- [2] --- [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---
%%                 H1      HS            HE - 1    HE    H2 - 1    H2
%%
%% no_restart - the node is not restarted between HE - 1 and H2 - 1.
%% HE - 1 - async worker is started to compute the fork result;
%% HE     - as there is already a worker or maybe already a result for HE - 1,
%%          there is no need starting another worker;
%% H2 - 1 - the same as for HE
basic_chain(no_restart, BlockCfgs, ExpectedForkResult) ->
    %% FORK_HEIGHT blocks are needed to be generated (including the genesis),
    %% so the chain ends up at H2 - 1 height where it's possible to find out
    %% the fork signalling result.
    Chain = [B0, B1, B2, B3, B4, B5, B6, B7] = aec_test_utils:gen_blocks_only_chain(?FORK_HEIGHT, BlockCfgs),
    [BH0, BH1, BH2, BH3, BH4, BH5, BH6, BH7] = [block_hash(B) || B <- Chain],

    ok = insert_block(B0),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B0, BH0, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B0, BH0, ?FORK_CFG)),

    ok = insert_block(B1),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B1, BH1, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B1, BH1, ?FORK_CFG)),

    ok = insert_block(B2),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B2, BH2, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B2, BH2, ?FORK_CFG)),

    ok = insert_block(B3),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B3, BH3, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B3, BH3, ?FORK_CFG)),

    ok = insert_block(B4),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B4, BH4, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B4, BH4, ?FORK_CFG)),

    ok = insert_block(B5),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B5, BH5, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B5, BH5, ?FORK_CFG)),

    ok = insert_block(B6),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B6, BH6, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B6, BH6, ?FORK_CFG)),

    ok = insert_block(B7),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B7, BH7, ?FORK_CFG)),
    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ok;

%% [0] --- [1] --- [2] --- [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---
%%                 H1      HS            HE - 1    HE    H2 - 1    H2
%%
%% restart_at_last_signalling_block - the node is restarted at HE - 1. There
%% is no worker computing the fork signalling result and there is no fork
%% signalling result available.
%% HE - 1 - after the following block is inserted (HE), async worker is
%% started. It looks for HE - 1 and then starts the fork signalling result
%% computation.
basic_chain(restart_at_last_signalling_block, BlockCfgs, ExpectedForkResult) ->
    [B0, B1, B2, B3, B4, B5, B6, B7] = aec_test_utils:gen_blocks_only_chain(?FORK_HEIGHT, BlockCfgs),
    [BH5, BH6, BH7] = [block_hash(B) || B <- [B5, B6, B7]],

    write_blocks_to_chain([B0, B1, B2, B3, B4, B5]),

    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B5, BH5, ?FORK_CFG)),

    ok = insert_block(B6),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B6, BH6, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B6, BH6, ?FORK_CFG)),

    ok = insert_block(B7),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B7, BH7, ?FORK_CFG)),
    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ok;

%% [0] --- [1] --- [2] --- [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---
%%                 H1      HS            HE - 1    HE    H2 - 1    H2
%%
%% restart_between_last_signalling_block_and_last_block_before_fork - the node
%% is restarted at HE. There is no no worker computing the fork signalling
%% result and there is no fork signalling result available.
%% HE - after the following block is inserted (H2 - 1), async worker is
%% started. It looks for HE - 1 and then starts the fork signalling result
%% computation.
basic_chain(restart_between_last_signalling_block_and_last_block_before_fork, BlockCfgs, ExpectedForkResult) ->
    [B0, B1, B2, B3, B4, B5, B6, B7] = aec_test_utils:gen_blocks_only_chain(?FORK_HEIGHT, BlockCfgs),
    [BH6, BH7] = [block_hash(B) || B <- [B6, B7]],

    write_blocks_to_chain([B0, B1, B2, B3, B4, B5, B6]),

    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B6, BH6, ?FORK_CFG)),

    ok = insert_block(B7),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(B7, BH7, ?FORK_CFG)),
    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ok;

%% [0] --- [1] --- [2] --- [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---
%%                 H1      HS            HE - 1    HE    H2 - 1    H2
%%
%% restart_at_last_block_before_fork - the node is restarted at H2 - 1. There
%% is no worker computing the fork signalling result and there is no fork
%% signalling result available.
%% H2 - 1 - async worker is started, first it walks the chain back until HE - 1
%% is found, then starts computing the fork signalling result.
basic_chain(restart_at_last_block_before_fork, BlockCfgs, ExpectedForkResult) ->
    [B0, B1, B2, B3, B4, B5, B6, B7] = aec_test_utils:gen_blocks_only_chain(?FORK_HEIGHT, BlockCfgs),
    BH7 = block_hash(B7),

    write_blocks_to_chain([B0, B1, B2, B3, B4, B5, B6, B7]),

    ?assertEqual({ok, ExpectedForkResult}, await_result(B7, BH7, ?FORK_CFG)),

    ok.

%%                         [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---  <- High difficulty
%%                          |
%% [0] --- [1] --- [2] --- [3] --- [4] --- [5] --- [6] --- [7] --- [8] ---  <- Low difficulty
%%                 H1      HS            HE - 1    HE    H2 - 1    H2
%%
%% Low difficulty chain has fork signalling result false.
%% Hight difficulty chain has fork signalling result true.
fork_chain(fork_at_signalling_start_block) ->
    CommonChain = aec_test_utils:gen_block_chain_with_state(3, ?FORK_CHAIN_COMMON),
    CommonChainBlocks = [B0, B1, B2] = aec_test_utils:blocks_only_chain(CommonChain),
    [BH0, BH1, BH2] = [block_hash(B) || B <- CommonChainBlocks],

    MinerAccount = aec_blocks:miner(B1),

    ok = insert_block(B0),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B0, BH0, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B0, BH0, ?FORK_CFG)),

    ok = insert_block(B1),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B1, BH1, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B1, BH1, ?FORK_CFG)),

    ok = insert_block(B2),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(B2, BH2, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(B2, BH2, ?FORK_CFG)),

    %% Easy chain fork
    EasyChainBlockCfgs = maps:merge(?FORK_CHAIN_COMMON, ?FORK_CHAIN_LOW_DIFFICULTY_NEGATIVE_RESULT),
    EasyChain =
        aec_test_utils:extend_block_chain_with_key_blocks(CommonChain, 5, MinerAccount, MinerAccount, EasyChainBlockCfgs),
    EasyChainBlocks = [EB3, EB4, EB5, EB6, EB7] = aec_test_utils:blocks_only_chain(EasyChain),
    [EBH3, EBH4, EBH5, EBH6, EBH7] = [block_hash(B) || B <- EasyChainBlocks],

    ok = insert_block(EB3),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(EB3, EBH3, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(EB3, EBH3, ?FORK_CFG)),

    ok = insert_block(EB4),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(EB4, EBH4, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(EB4, EBH4, ?FORK_CFG)),

    ok = insert_block(EB5),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(EB5, EBH5, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(EB5, EBH5, ?FORK_CFG)),

    ok = insert_block(EB6),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(EB6, EBH6, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(EB6, EBH6, ?FORK_CFG)),

    ok = insert_block(EB7),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(EB7, EBH7, ?FORK_CFG)),
    ?assertEqual({ok, false}, await_result(EB7, EBH7, ?FORK_CFG)),

    %% Hard chain fork
    HardChainBlockCfgs = maps:merge(?FORK_CHAIN_COMMON, ?FORK_CHAIN_HIGH_DIFFICULTY_POSITIVE_RESULT),
    HardChain =
        aec_test_utils:extend_block_chain_with_key_blocks(CommonChain, 5, MinerAccount, MinerAccount, HardChainBlockCfgs),
    HardChainBlocks = [HB3, HB4, HB5, HB6, HB7] = aec_test_utils:blocks_only_chain(HardChain),
    [HBH3, HBH4, HBH5, HBH6, HBH7] = [block_hash(B) || B <- HardChainBlocks],

    ok = insert_block(HB3),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(HB3, HBH3, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(HB3, HBH3, ?FORK_CFG)),

    ok = insert_block(HB4),
    ?assertEqual(skipped, ?TEST_MODULE:compute_fork_result(HB4, HBH4, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(HB4, HBH4, ?FORK_CFG)),

    ok = insert_block(HB5),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(HB5, HBH5, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(HB5, HBH5, ?FORK_CFG)),

    ok = insert_block(HB6),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(HB6, HBH6, ?FORK_CFG)),
    ?assertEqual({error, not_last_block_before_fork}, ?TEST_MODULE:get_fork_result(HB6, HBH6, ?FORK_CFG)),

    ok = insert_block(HB7),
    ?assertEqual(ok, ?TEST_MODULE:compute_fork_result(HB7, HBH7, ?FORK_CFG)),
    ?assertEqual({ok, true}, await_result(HB7, HBH7, ?FORK_CFG)),

    %% The current top is a block from the hard chain
    ?assertEqual(HBH7, aec_chain:top_block_hash()),
    ok.

prepare_chain(Length, BlockCfgs) ->
    Blocks = aec_test_utils:gen_blocks_only_chain(Length, BlockCfgs),
    ok = write_blocks_to_chain(Blocks).

write_blocks_to_chain([H | T]) ->
    ok = insert_block(H),
    write_blocks_to_chain(T);
write_blocks_to_chain([]) ->
    ok.

read_block_from_chain(Height) ->
    {ok, B} = aec_chain:get_key_block_by_height(Height),
    B.

insert_block(Block) ->
    insert_block_ret(aec_chain_state:insert_block(Block)).

insert_block_ret({ok,_}     ) -> ok;
insert_block_ret({pof,Pof,_}) -> {pof,Pof};
insert_block_ret(Other      ) -> Other.

receive_worker_msg(WorkerPid) ->
    receive
        {worker_msg, WorkerPid, Msg} ->
            {ok, Msg}
    after
        5000 ->
            {error, timeout}
    end.

send_server_msg(WorkerPid, Msg) ->
    WorkerPid ! {server_msg, self(), Msg}.

ensure_worker_dead(WorkerPid) ->
    ensure_worker_dead(WorkerPid, 5).

ensure_worker_dead(WorkerPid, Retries) when Retries > 0 ->
    case is_process_alive(WorkerPid) of
        true ->
            timer:sleep(500),
            ensure_worker_dead(WorkerPid, Retries - 1);
        false ->
            {ok, dead}
    end;
ensure_worker_dead(_WorkerPid, 0) ->
    {error, worker_process_alive}.

make_info(X) when is_integer(X) ->
    <<X:?OPTIONAL_INFO_BYTES/unit:8>>.

await_result(Block, BlockHash, Fork) ->
    await_result(Block, BlockHash, Fork, 5).

await_result(Block, BlockHash, Fork, Retries) when Retries > 0 ->
    case ?TEST_MODULE:get_fork_result(Block, BlockHash, Fork) of
        {ok, pending_protocol} ->
            timer:sleep(500),
            await_result(Block, BlockHash, Fork, Retries - 1);
        {ok, Result} when is_boolean(Result) ->
            {ok, Result}
    end;
await_result(_Block, _BlockHash, _Fork, 0) ->
    {error, exhausted_retries}.

block_hash(Block) ->
    {ok, H} = aec_blocks:hash_internal_representation(Block),
    H.
