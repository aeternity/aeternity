%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Utils for (mocking tests).
%%% @end
%%%=============================================================================

-module(aec_test_utils).

-export([ mock_time/0
        , unmock_time/0
        , mock_difficulty_as_target/0
        , unmock_difficulty_as_target/0
        , mock_block_target_validation/0
        , unmock_block_target_validation/0
        , mock_fast_cuckoo_pow/0
        , mock_fast_and_deterministic_cuckoo_pow/0
        , wait_for_it/2
        , extend_block_chain/2
        , aec_keys_setup/0
        , aec_keys_cleanup/1
        , gen_block_chain/1
        , gen_block_chain/2
        , genesis_block/0
        , preset_accounts/0
        , create_state_tree/0
        , create_state_tree_with_account/1
        , create_state_tree_with_accounts/1
        , create_temp_key_dir/0
        , remove_temp_key_dir/1
        , copy_genesis_dir/2
        , signed_coinbase_tx/0
        , signed_spend_tx/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-ifdef(DEBUG).
-define(ifDebugFmt(Str, Args), ?debugFmt(Str, Args)).
-else.
-define(ifDebugFmt(Str, Args), ok).
-endif.

-define(PRESET_ACCOUNTS, [{<<"my_public_key">>, 100}]).

preset_accounts() ->
  ?PRESET_ACCOUNTS.

mock_time() ->
    meck:new(aeu_time, [passthrough]),
    TS = spawn(fun() -> receive F -> F(F, 1000) end end),
    TS ! fun(F, T) ->
                 receive {time, R, From}  -> From ! {R,T}
                 end,
                 F(F, T + 1000)
         end,
    GetTime = fun () -> TS ! {time, R = make_ref(), self()},
                        receive {R, T} -> T end
              end,

    meck:expect(aeu_time, now_in_msecs,
                fun() ->
                        GetTime()
                end),
    ok.

unmock_time() ->
    meck:unload(aeu_time).

mock_fast_cuckoo_pow() ->
    mock_fast_cuckoo_pow({"mean16s-generic", "-t 5", 16}).

mock_fast_and_deterministic_cuckoo_pow() ->
    mock_fast_cuckoo_pow({"mean16s-generic", "", 16}).

mock_fast_cuckoo_pow({_MinerBin, _MinerExtraArgs, _NodeBits} = Cfg) ->
    meck:expect(application, get_env, 3,
                fun
                    (aecore, aec_pow_cuckoo, _) ->
                       Cfg;
                    (App, Key, Def) ->
                       meck:passthrough([App, Key, Def])
               end).

wait_for_it(Fun, Value) ->
    wait_for_it(Fun, Value, 0).

wait_for_it(Fun, Value, Sleep) ->
    case Fun() of
        Value ->
            Value;
        _Other ->
            ?ifDebugFmt("Waiting for ~p got ~p~n",[Value,_Other]),
            timer:sleep(Sleep),
            wait_for_it(Fun, Value, Sleep + 10)
    end.

%%%=============================================================================
%%% Chain related util functions
%%%=============================================================================

%% @doc Meck difficulty as target. And startup keys server

%% Make mecked difficulty calculation realistic:
%% - when target increases, difficulty decreases
%% - when target decreases, difficulty increases
-define(FACTOR, 1000000000).
mock_difficulty_as_target() ->
    meck:new(aec_headers, [passthrough]),
    meck:new(aec_blocks, [passthrough]),
    meck:expect(aec_headers, difficulty,
                fun(#header{target = T}) when is_integer(T) ->
                        float(trunc(?FACTOR / T))
                end),
    meck:expect(aec_blocks, difficulty,
                fun(#block{target = T}) when is_integer(T) ->
                        float(trunc(?FACTOR / T))
                end).

unmock_difficulty_as_target() ->
    meck:unload(aec_headers),
    meck:unload(aec_blocks).


mock_block_target_validation() ->
    meck:new(aec_governance, [passthrough]),
    meck:new(aec_target, [passthrough]),
    meck:expect(aec_governance, blocks_to_check_difficulty_count, 0, 1),
    meck:expect(aec_target, verify, 2, ok).

unmock_block_target_validation() ->
    meck:unload(aec_governance),
    meck:unload(aec_target).


genesis_block() ->
    aec_block_genesis:genesis_block(?PRESET_ACCOUNTS).

%% Generic blockchain with only coinbase transactions
gen_block_chain(Length) ->
    gen_block_chain(Length, ?PRESET_ACCOUNTS).
  
gen_block_chain(Length, PresetAccounts) when Length > 0 ->
    {ok, MinerAccount} = aec_keys:wait_for_pubkey(),
    gen_block_chain(Length, MinerAccount, PresetAccounts, []).


gen_block_chain(0,_MinerAccount, _PresetAccounts, Acc) -> lists:reverse(Acc);
gen_block_chain(N, MinerAccount, PresetAccounts, []) ->
    gen_block_chain(N - 1, MinerAccount, PresetAccounts, [aec_block_genesis:genesis_block(PresetAccounts)]);
gen_block_chain(N, MinerAccount, PresetAccounts, [PreviousBlock|_] = Acc) ->
    Trees = aec_blocks:trees(PreviousBlock),
    Txs = [signed_coinbase_tx(MinerAccount)],
    B = aec_blocks:new(PreviousBlock, Txs, Trees),
    gen_block_chain(N - 1, MinerAccount, PresetAccounts, [B|Acc]).

extend_block_chain(PrevBlock, Data) ->
    {ok, MinerAccount} = aec_keys:wait_for_pubkey(),
    Targets    = maps:get(targets, Data),
    Nonce      = maps:get(nonce, Data, 12345),
    Timestamps = maps:get(timestamps, Data, lists:duplicate(length(Targets), undefined)),
    extend_block_chain(PrevBlock, Targets, Timestamps, Nonce, MinerAccount, []).


extend_block_chain(_, [], _, _, _, Chain) ->
    lists:reverse(Chain);
extend_block_chain(PrevBlock, [Tgt | Tgts], [Ts | Tss], Nonce, MinerAcc, Chain) ->
    Block = next_block(PrevBlock, Tgt, Ts, Nonce, MinerAcc),
    extend_block_chain(Block, Tgts, Tss, Nonce, MinerAcc, [Block | Chain]).

next_block(PrevBlock, Target, Time0, Nonce, MinerAcc) ->
    Trees = aec_blocks:trees(PrevBlock),
    B = aec_blocks:new(PrevBlock, [signed_coinbase_tx(MinerAcc)], Trees),
    B#block{ target = Target, nonce  = Nonce,
             time   = case Time0 of undefined -> B#block.time; _ -> Time0 end }.

signed_coinbase_tx() ->
    {ok, MinerAccount} = aec_keys:wait_for_pubkey(),
    signed_coinbase_tx(MinerAccount).

%% In order to find the secret key to sign with, we use the aec_key process
signed_coinbase_tx(Account) ->
    {ok, Tx} = aec_coinbase_tx:new(#{account => Account}),
    {ok, STx} = aec_keys:sign(Tx),
    STx.

signed_spend_tx(ArgsMap) ->
    {ok, SenderAccount} = aec_keys:wait_for_pubkey(),
    ArgsMap1 = maps:put(sender, SenderAccount, ArgsMap),
    {ok, SpendTx} = aec_spend_tx:new(ArgsMap1),
    {ok, SSTx} = aec_keys:sign(SpendTx),
    SSTx.

%% function to setup the .genesis file for test SUITE-s
%% SourceGenesisDir is the test release directory from which to take the .genesis
%% DestRelDir is the release being set up
copy_genesis_dir(SourceRelDir, DestRelDir) ->
    GenesisDir = aec_genesis_block_settings:dir(), % ex data/aecore/.genesis
    GenesisName = filename:basename(GenesisDir),
    DataAecoreRoot = filename:dirname(GenesisDir), % ex. data/aecore
    DataRoot = filename:dirname(DataAecoreRoot), % ex. data
    AecoreName = filename:basename(DataAecoreRoot),
    DataRootName = filename:basename(DataRoot),
    DestGenesisDir = filename:join([DestRelDir, DataRootName, AecoreName, GenesisName]),
    ok = filelib:ensure_dir(DestGenesisDir ++ "/"),
    SourceGenesisDir = filename:join([SourceRelDir, DataRootName, AecoreName, GenesisName]),
    {ok, AllFiles} = file:list_dir_all(SourceGenesisDir),
    lists:foreach(
        fun(FileName) ->
            {ok, _} = file:copy(filename:join(SourceGenesisDir, FileName),
                                filename:join(DestGenesisDir, FileName))
        end,
        AllFiles).



%%%=============================================================================
%%% Key server setup/teardown
%%%=============================================================================

aec_keys_setup() ->
    TmpKeysDir = create_temp_key_dir(),
    ok = application:ensure_started(crypto),
    {ok, _} = aec_keys:start_link([<<"mypassword">>, TmpKeysDir]),
    wait_for_it(fun() -> whereis(aec_keys) =/= undefined end, true),
    TmpKeysDir.

aec_keys_cleanup(TmpKeysDir) ->
    ok = aec_keys:stop(),
    ok = application:stop(crypto),
    remove_temp_key_dir(TmpKeysDir).

remove_temp_key_dir(TmpKeysDir) ->
    {ok, KeyFiles} = file:list_dir(TmpKeysDir),
    %% Expect two filenames - private and public keys.
    [_KF1, _KF2] = KeyFiles,
    lists:foreach(
      fun(F) ->
              AbsF = filename:absname_join(TmpKeysDir, F),
              {ok, _} = {file:delete(AbsF), {F, AbsF}}
      end,
      KeyFiles),
    ok = file:del_dir(TmpKeysDir).

create_temp_key_dir() ->
    mktempd(os:type()).

mktempd({unix, _}) ->
    lib:nonl(?cmd("mktemp -d")).

%%%=============================================================================
%%% State trees setup
%%%=============================================================================

create_state_tree() ->
    {ok, AccountsTree} = aec_accounts:empty(),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree).

create_state_tree_with_account(Account) ->
    create_state_tree_with_accounts([Account]).

create_state_tree_with_accounts(Accounts) ->
    {ok, AccountsTree0} = aec_accounts:empty(),
    AccountsTree1 = lists:foldl(
                      fun(Account, Tree0) ->
                              {ok, Tree} = aec_accounts:put(Account, Tree0),
                              Tree
                      end, AccountsTree0, Accounts),
    StateTrees0 = #trees{},
    aec_trees:set_accounts(StateTrees0, AccountsTree1).
