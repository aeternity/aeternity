%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Utils for (mocking tests).
%%% @end
%%%=============================================================================

-module(aec_test_utils).

-export([ running_apps/0
        , loaded_apps/0
        , restore_stopped_and_unloaded_apps/2
        , mock_time/0
        , unmock_time/0
        , mock_difficulty_as_target/0
        , unmock_difficulty_as_target/0
        , mock_governance/0
        , unmock_governance/0
        , mock_fast_cuckoo_pow/0
        , mock_fast_and_deterministic_cuckoo_pow/0
        , mock_prebuilt_cuckoo_pow/1
        , mock_genesis_and_forks/0
        , mock_genesis_and_forks/1
        , mock_genesis_and_forks/2
        , unmock_genesis_and_forks/0
        , ensure_not_mocked/1
        , ensure_no_mocks/0
        , wait_for_it/2
        , wait_for_it_or_timeout/3
        , wait_for_pred_or_timeout/3
        , exec_with_timeout/2
        , start_chain_db/0
        , start_chain_db/1
        , stop_chain_db/0
        , extend_block_chain_with_key_blocks/2
        , extend_block_chain_with_key_blocks/5
        , extend_block_chain_with_micro_blocks/2
        , extend_block_chain_with_micro_blocks/3
        , extend_block_chain_with_state/2
        , aec_keys_setup/0
        , aec_keys_cleanup/1
        , aec_keys_bare_setup/0
        , aec_keys_bare_cleanup/1
        , gen_blocks_only_chain/1
        , gen_blocks_only_chain/2
        , gen_blocks_only_chain/3
        , gen_block_chain_with_state/1
        , gen_block_chain_with_state/2
        , blocks_only_chain/1
        , genesis_block/0
        , genesis_block_with_state/0
        , genesis_block_with_state/1
        , genesis_accounts/0
        , genesis_accounts_balances/1
        , create_keyblock_with_state/2
        , create_keyblock_with_state/3
        , create_keyblock_with_state/4
        , create_state_tree/0
        , create_state_tree_with_account/1
        , create_state_tree_with_accounts/1
        , create_state_tree_with_accounts/2
        , create_temp_key_dir/0
        , remove_temp_key_dir/1
        , copy_forks_dir/2
        , sign_micro_block/2
        , sign_tx/2
        , co_sign_tx/2
        , substitute_innermost_tx/2
        , sign_tx/3
        , sign_tx/4
        , sign_tx_hash/2
        , sign_pay_for_inner_tx/2
        , signed_spend_tx/1
        , wait_for_pubkey/0
        , min_gas_price/0
        , dev_reward_setup/3
        , run_throughput_test/3
        ]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("blocks.hrl").

-ifdef(DEBUG).
-define(ifDebugFmt(Str, Args), ?debugFmt(Str, Args)).
-else.
-define(ifDebugFmt(Str, Args), ok).
-endif.

-define(GENESIS_ACCOUNTS, [{<<"_________my_public_key__________">>, 100}]).

running_apps() ->
    [A || {A,_,_} <- application:which_applications()].

loaded_apps() ->
    [A || {A,_,_} <- application:loaded_applications()].

restore_stopped_and_unloaded_apps(OldRunningApps, OldLoadedApps) ->
    BadRunningApps = running_apps() -- OldRunningApps,
    lists:foreach(fun(A) -> ok = application:stop(A) end, BadRunningApps),
    BadLoadedApps = loaded_apps() -- OldLoadedApps,
    lists:foreach(fun(A) -> ok = application:unload(A) end, BadLoadedApps),
    {_, OldRunningApps} = {OldRunningApps, running_apps()},
    {_, OldLoadedApps} = {OldLoadedApps, loaded_apps()},
    ok.

genesis_accounts() ->
  ?GENESIS_ACCOUNTS.

genesis_accounts_balances(PresetAccounts) ->
    %% NG : No beneficiary reward for Genesis (yet?)
    PresetAccounts.
    %% [{aec_block_genesis:beneficiary(), aec_governance:block_mine_reward()}
    %%  | PresetAccounts].

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
    mock_cuckoo_pow({15, [{<<"mean15-generic">>, <<"-t 5">>, false, 10, [0, 1], <<"aecuckoo">>}]}).

mock_fast_and_deterministic_cuckoo_pow() ->
    mock_cuckoo_pow({15, [{<<"mean15-generic">>, <<>>, false, 10, undefined, <<"aecuckoo">>}]}).

mock_prebuilt_cuckoo_pow(MinerBin) ->
    mock_cuckoo_pow({15, [{MinerBin, <<>>, false, 1, undefined, <<"aecuckooprebuilt">>}]}).

mock_cuckoo_pow({_EdgeBits, _Miners} = Cfg) ->
    meck:expect(aeu_env, get_env, 3,
                fun
                    (aecore, aec_mining, _) ->
                       Cfg;
                    (App, Key, Def) ->
                       meck:passthrough([App, Key, Def])
               end).

mock_genesis_and_forks() ->
    mock_genesis_and_forks(genesis_accounts()).

mock_genesis_and_forks(PresetAccounts) ->
    mock_genesis_and_forks(PresetAccounts, #{}).

mock_genesis_and_forks(PresetAccounts, Whitelist) ->
    meck:new(aec_fork_block_settings, [passthrough]),
    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_fork_block_settings, minerva_accounts, 0, []),
    meck:expect(aec_fork_block_settings, fortuna_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_extra_accounts, 0, []),
    meck:expect(aec_fork_block_settings, lima_contracts, 0, []),
    meck:expect(aec_fork_block_settings, block_whitelist, 0, Whitelist),
    meck:new(aec_resilience, [passthrough]),
    meck:expect(aec_resilience, fork_resistance_active, 0, no),
    meck:expect(aec_resilience, fork_resistance_configured, 0, no),
    aec_consensus:set_consensus(),
    aec_consensus:set_genesis_hash(),
    ok.

unmock_genesis_and_forks() ->
    meck:unload(aec_fork_block_settings),
    meck:unload(aec_resilience),
    ok.

ensure_not_mocked(Mods) when is_list(Mods) ->
    case intersection(Mods, registered_mocks()) of
        [] ->
            ok;
        AlreadyMocked ->
            ct:pal("Need to mock these modules, but already mocked: ~p",
                   [AlreadyMocked]),
            ct:fail({already_mocked, AlreadyMocked})
    end.

ensure_no_mocks() ->
    case registered_mocks() of
        [] ->
            ok;
        Regs ->
            ct:pal("UNEXPECTED MOCKS: ~p", [Regs]),
            ct:fail({unexpected_mocks, Regs})
    end.

registered_mocks() ->
    lists:foldr(fun meck_name/2, [], registered()).

meck_name(N, Acc) ->
    case re:split(atom_to_binary(N, latin1), <<"_meck$">>) of
        [_] ->
            Acc;
        [Mod, _] ->
            [binary_to_existing_atom(Mod, latin1) | Acc]
    end.

intersection(A, B) when is_list(A), is_list(B) ->
    ordsets:intersection(ordsets:from_list(A),
                         ordsets:from_list(B)).

wait_for_pubkey() ->
    wait_for_pubkey(1).

wait_for_pubkey(Sleep) ->
    case aec_keys:pubkey() of
        {error, key_not_found} ->
            timer:sleep(Sleep),
            wait_for_pubkey(Sleep+10);
        {ok, Pub} ->
            {ok, Priv} = aec_keys:sign_privkey(),
            {ok, Pub, Priv}
    end.


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

wait_for_pred(Fun, Pred) ->
    wait_for_pred(Fun, Pred, 0).

wait_for_pred(Fun, Pred, Sleep) ->
    Value = Fun(),
    case Pred(Value) of
        true ->
            Value;
        false ->
            ?ifDebugFmt("Waiting for predicate got ~p~n",[Value]),
            timer:sleep(Sleep),
            wait_for_pred(Fun, Pred, Sleep + 10)
    end.

wait_for_it_or_timeout(Fun, Value, Timeout) ->
    exec_with_timeout(fun() -> wait_for_it(Fun, Value) end, Timeout).

wait_for_pred_or_timeout(Fun, Pred, Timeout) ->
    exec_with_timeout(fun() -> wait_for_pred(Fun, Pred) end, Timeout).

exec_with_timeout(Fun, Timeout)  when is_function(Fun, 0) ->
    Pid = self(),
    spawn(
        fun() ->
            Res = Fun(),
            Pid ! {exec_result, Res}
        end),
    receive
        {exec_result, Res} ->
            {ok, Res}
    after
        Timeout ->
            {error, timeout}
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
                fun(H) ->
                        T = aec_headers:target(H),
                        true = is_integer(T),
                        float(trunc(?FACTOR / T))
                end),
    meck:expect(aec_blocks, difficulty,
                fun(B) ->
                        T = aec_blocks:target(B),
                        true = is_integer(T),
                        float(trunc(?FACTOR / T))
                end).

unmock_difficulty_as_target() ->
    meck:unload(aec_headers),
    meck:unload(aec_blocks).


mock_governance() ->
    meck:new(aec_governance, [passthrough]),
    meck:new(aec_target, [passthrough]),
    meck:expect(aec_governance, key_blocks_to_check_difficulty_count, 0, 1),
    meck:expect(aec_target, verify, 2, ok).

unmock_governance() ->
    meck:unload(aec_governance),
    meck:unload(aec_target).

start_chain_db() ->
    start_chain_db(ram).

start_chain_db(ram) ->
    persistent_term:put({?MODULE, db_mode}, ram),
    ok = mnesia:start(),
    ok = aec_db:initialize_db(ram),
    Tabs = [Tab || {Tab, _} <- aec_db:tables(ram)],
    ok = mnesia:wait_for_tables(Tabs, 5000),
    aec_db:prepare_mnesia_bypass(),
    ok;

start_chain_db(disc) ->
    Persist = application:get_env(aecore, persist),
    persistent_term:put({?MODULE, db_mode}, {disc, Persist}),
    application:set_env(aecore, persist, true),
    aec_db:check_db(),
    aec_db:prepare_mnesia_bypass(),
    aec_db:clear_db(),
    ok = meck:new(mnesia_rocksdb_lib, [passthrough]).

stop_chain_db() ->
    stop_chain_db(persistent_term:get({?MODULE, db_mode})).
stop_chain_db(ram) ->
    application:stop(mnesia);
stop_chain_db({disc, Persist}) ->
    application:stop(mnesia),
    application:set_env(aecore, persist, Persist),
    ok = meck:unload(mnesia_rocksdb_lib),
    ok = mnesia:delete_schema([node()]).

genesis_block() ->
    {B, _} = genesis_block_with_state(),
    B.

genesis_block_with_state() ->
    genesis_block_with_state(?GENESIS_ACCOUNTS).

genesis_block_with_state(PresetAccounts) ->
    aec_block_genesis:genesis_block_with_state(#{preset_accounts => PresetAccounts}).

%% Generic blockchain without transactions
gen_blocks_only_chain(Length) ->
    blocks_only_chain(gen_block_chain_with_state(Length, ?GENESIS_ACCOUNTS, #{})).

gen_blocks_only_chain(Length, PresetAccounts) when is_list(PresetAccounts) ->
    blocks_only_chain(gen_block_chain_with_state(Length, PresetAccounts, #{}));
gen_blocks_only_chain(Length, BlockCfgs) when is_map(BlockCfgs) ->
    blocks_only_chain(gen_block_chain_with_state(Length, ?GENESIS_ACCOUNTS, BlockCfgs)).

gen_blocks_only_chain(Length, PresetAccounts, BlockCfgs) ->
    blocks_only_chain(gen_block_chain_with_state(Length, PresetAccounts, BlockCfgs)).

gen_block_chain_with_state(Length) ->
    gen_block_chain_with_state(Length, ?GENESIS_ACCOUNTS, #{}).

gen_block_chain_with_state(Length, PresetAccounts) when is_list (PresetAccounts) ->
    gen_block_chain_with_state(Length, PresetAccounts, #{});
gen_block_chain_with_state(Length, BlockCfgs) when is_map(BlockCfgs) ->
    gen_block_chain_with_state(Length, ?GENESIS_ACCOUNTS, BlockCfgs).

gen_block_chain_with_state(Length, PresetAccounts, BlockCfgs) when Length > 0 ->
    {ok, MinerAccount, _} = wait_for_pubkey(),
    gen_block_chain_with_state(Length, MinerAccount, PresetAccounts, BlockCfgs).

gen_block_chain_with_state(Length, MinerAccount, PresetAccounts, BlockCfgs) ->
    BlockCfgs1 = prepare_block_configs(0, Length, BlockCfgs, []),
    gen_block_chain_with_state(Length, MinerAccount, PresetAccounts, BlockCfgs1, []).

gen_block_chain_with_state(N, MinerAccount, PresetAccounts, [undefined | BlockCfgs], []) when N > 0 ->
    {B, S} = aec_block_genesis:genesis_block_with_state(#{preset_accounts => PresetAccounts}),
    gen_block_chain_with_state(N - 1, MinerAccount, PresetAccounts, BlockCfgs, [{B, S}]);
gen_block_chain_with_state(N, MinerAccount, PresetAccounts, [BlockCfg | BlockCfgs], Acc) when N > 0 ->
    {B, S} = create_keyblock_with_state(Acc, MinerAccount, MinerAccount, BlockCfg),
    gen_block_chain_with_state(N - 1, MinerAccount, PresetAccounts, BlockCfgs, [{B, S} | Acc]);
gen_block_chain_with_state(0, _MinerAccount, _PresetAccounts, _BlockCfgs, Acc) ->
    lists:reverse(Acc).

prepare_block_configs(0, Length, BlockCfgs, []) ->
    prepare_block_configs(1, Length, BlockCfgs, [undefined]);
prepare_block_configs(Height, Length, BlockCfgs, Acc) when Height < Length ->
    case maps:get(Height, BlockCfgs, undefined) of
        BlockCfg when BlockCfg =/= undefined ->
            prepare_block_configs(Height + 1, Length, BlockCfgs, [BlockCfg | Acc]);
        undefined ->
            prepare_block_configs(Height + 1, Length, BlockCfgs, [hd(Acc) | Acc])
    end;
prepare_block_configs(Height, Length, _BlockCfgs, Acc) when Height =:= Length ->
    lists:reverse(Acc).

grant_fees(FromHeight, Chain, TreesIn, BeneficiaryAccount) ->
    {Fees, Beneficiary1, Beneficiary2} = fees_at_height(FromHeight, Chain, 0, BeneficiaryAccount),
    Beneficiary1Reward = round(0.4 * Fees),
    BlockReward = aec_governance:block_mine_reward(FromHeight + 1),
    Beneficiary2Reward = Fees - Beneficiary1Reward + BlockReward,
    Protocol = aec_hard_forks:protocol_effective_at_height(FromHeight),
    {{Benefits1, Benefits2}, BenefitsProto} =
        case aec_dev_reward:enabled() andalso aec_dev_reward:activated(Protocol) of
            true ->
                AllocShares = 100,
                TotalShares = 1000,
                AbsContrib1 = Beneficiary1Reward * AllocShares div TotalShares,
                AbsContrib2 = Beneficiary2Reward * AllocShares div TotalShares,
                DevContrib  = AbsContrib1 + AbsContrib2,
                {{Beneficiary1Reward - AbsContrib1, Beneficiary2Reward - AbsContrib2}, DevContrib};
            false ->
                {{Beneficiary1Reward, Beneficiary2Reward}, 0}
        end,
    [{DevRewardPK, _} | _] = aec_dev_reward:beneficiaries(),
    grant_fees_iter({Benefits1, Beneficiary1}, {Benefits2, Beneficiary2},
                    {BenefitsProto, DevRewardPK}, FromHeight, TreesIn).

grant_fees_iter(_, {Benefits2, Beneficiary2}, {0,_}, 0, TreesIn) ->
    aec_trees:grant_fee(Beneficiary2, TreesIn, Benefits2);
grant_fees_iter(_, {Benefits2, Beneficiary2}, {BenefitsProto, BeneficiaryProto}, 0, TreesIn) ->
    Trees1 = aec_trees:grant_fee(Beneficiary2, TreesIn, Benefits2),
    aec_trees:grant_fee(BeneficiaryProto, Trees1, BenefitsProto);
grant_fees_iter({Benefits1, Beneficiary1}, {Benefits2, Beneficiary2}, {0,_BeneficiaryProto}, _, TreesIn) ->
    Trees1 = aec_trees:grant_fee(Beneficiary2, TreesIn, Benefits2),
    aec_trees:grant_fee(Beneficiary1, Trees1, Benefits1);
grant_fees_iter({Benefits1, Beneficiary1}, {Benefits2, Beneficiary2}, {BenefitsProto,BeneficiaryProto}, _, TreesIn) ->
    Trees1 = aec_trees:grant_fee(Beneficiary2, TreesIn, Benefits2),
    Trees2 = aec_trees:grant_fee(BeneficiaryProto, Trees1, BenefitsProto),
    aec_trees:grant_fee(Beneficiary1, Trees2, Benefits1).

fees_at_height(N, [{B, S} | Chain], Acc, Beneficiary) ->
    Height = aec_blocks:height(B),
    if
        Height =:= N ->
            case aec_blocks:type(B) of
                key ->
                    {Acc, aec_blocks:beneficiary(B), Beneficiary};
                micro ->
                    TxFees = lists:foldl(
                          fun(SignedTx, AccFee) ->
                                  Fee = aetx:deep_fee(aetx_sign:tx(SignedTx), S),
                                  AccFee + Fee
                          end, 0, aec_blocks:txs(B)),
                    fees_at_height(N, Chain, Acc + TxFees, Beneficiary)
            end;
        Height =:= N + 1 ->
            case aec_blocks:type(B) of
                key ->
                    GasFees =
                        case Chain of
                            [{_, S1}|_] ->
                                aec_chain_state:calculate_gas_fee(aec_trees:calls(S1));
                            [] ->
                                0
                        end,
                    fees_at_height(N, Chain, Acc + GasFees, aec_blocks:beneficiary(B));
                micro ->
                    fees_at_height(N, Chain, Acc, Beneficiary)
            end;
        Height > N + 1 ->
            fees_at_height(N, Chain, Acc, Beneficiary)
    end.

create_keyblock_with_state(Chain, MinerAccount) ->
    create_keyblock_with_state(Chain, MinerAccount, MinerAccount, #{}).

create_keyblock_with_state(Chain, MinerAccount, BeneficiaryAccount) ->
    create_keyblock_with_state(Chain, MinerAccount, BeneficiaryAccount, #{}).

create_keyblock_with_state([{PrevBlock, TreesIn} | _] = Chain, MinerAccount, BeneficiaryAccount, BlockCfg) ->
    {ok, PrevBlockHash} = aec_blocks:hash_internal_representation(PrevBlock),
    Height = aec_blocks:height(PrevBlock) + 1,
    Protocol = get_config(version, BlockCfg,
                          fun() -> aec_hard_forks:protocol_effective_at_height(Height) end),
    TxEnv = aetx_env:tx_env(Height, Protocol),
    PrevProtocol = aec_blocks:version(PrevBlock),
    Trees1 = aec_trees:perform_pre_transformations(TreesIn, TxEnv, PrevProtocol),
    Delay = aec_governance:beneficiary_reward_delay(),
    PrevKeyHash = case aec_blocks:type(PrevBlock) of
                      micro -> aec_blocks:prev_key_hash(PrevBlock);
                      key   -> PrevBlockHash
                  end,
    Target = get_config(target, BlockCfg, fun() -> adjust_target(Chain) end),
    Info = get_config(info, BlockCfg, fun() -> default end),
    Timestamp = get_config(timestamp, BlockCfg, fun() -> adjust_timestamp(Chain) end),
    %% Dummy block to calculate the fees.
    Block = aec_blocks:new_key(Height, PrevBlockHash, PrevKeyHash, aec_trees:hash(TreesIn),
                               Target, 0, Timestamp, Info, Protocol,
                               MinerAccount, BeneficiaryAccount),
    Trees2 = case Height > Delay of
                 true ->
                     grant_fees(Height - Delay - 1, [{Block, TreesIn}|Chain],
                                Trees1, BeneficiaryAccount);
                 false ->
                     Trees1
             end,
    Block1 = aec_blocks:new_key(Height, PrevBlockHash, PrevKeyHash, aec_trees:hash(Trees2),
                                Target, 0, Timestamp, Info, Protocol,
                                MinerAccount, BeneficiaryAccount),
    {Block1, Trees2}.

adjust_target([{PrevBlock, _} | _Rest] = Chain) ->
    Height = aec_blocks:height(PrevBlock) + 1,
    N = aec_governance:key_blocks_to_check_difficulty_count() + 1,
    case Height =< N of
        true ->
            aec_block_genesis:target();
        false ->
            Headers = get_n_key_headers(Chain, N, []),
            aec_target:recalculate(Headers)
    end;
adjust_target([]) ->
    aec_block_genesis:target().

pick_prev_target([{Block, _} | Rest]) ->
    case aec_blocks:type(Block) of
        key   -> aec_blocks:target(Block);
        micro -> pick_prev_target(Rest)
    end;
pick_prev_target([]) ->
    aec_block_genesis:target().

get_n_key_headers([{Block, _} | Rest], N, Acc) when length(Acc) < N ->
    case aec_blocks:type(Block) of
        key ->
            Header = aec_blocks:to_header(Block),
            get_n_key_headers(Rest, N, [Header | Acc]);
        micro ->
            get_n_key_headers(Rest, N,  Acc)
    end;
get_n_key_headers(_Chain, N, Acc) when length(Acc) =:= N ->
    lists:reverse(Acc).

adjust_timestamp([{PrevBlock, _} | _Rest]) ->
    case aec_blocks:height(PrevBlock) + 1 of
        1 -> aeu_time:now_in_msecs();
        _N -> aec_blocks:time_in_msecs(PrevBlock) + expected_mine_rate()
    end.

expected_mine_rate() ->
    case aeu_env:get_env(aecore, expected_mine_rate, undefined) of
        ExpectedMineRate when ExpectedMineRate =/= undefined ->
            ExpectedMineRate;
        undefined ->
            100
    end.

extend_block_chain_with_state(Chain, Data) ->
    {ok, Pubkey, PrivKey} = wait_for_pubkey(),
    Targets    = maps:get(targets, Data),
    TxsFun     = maps:get(txs_by_height_fun, Data),
    Nonce      = maps:get(nonce, Data, 12345),
    Timestamps = maps:get(timestamps, Data, lists:duplicate(length(Targets), undefined)),
    Miners     = maps:get(miners, Data, lists:duplicate(length(Targets), {Pubkey, PrivKey})),
    Beneficiaries = maps:get(beneficiaries, Data, lists:duplicate(length(Targets), Pubkey)),
    extend_block_chain_with_state(lists:reverse(Chain), Targets, Timestamps,
                                  Miners, Beneficiaries, TxsFun, Nonce).

extend_block_chain_with_state(Chain, [], _, _, _, _, _) ->
    lists:reverse(Chain);
extend_block_chain_with_state(Chain,
                              [Tgt | Tgts],
                              [Ts | Tss],
                              [{PubKey, PrivKey}|Miners],
                              [BeneficiaryPubKey | Beneficiaries],
                              TxsFun, Nonce) ->
    NewChain = next_block_with_state(Chain, Tgt, Ts, TxsFun, Nonce, PubKey, PrivKey, BeneficiaryPubKey),
    extend_block_chain_with_state(NewChain, Tgts, Tss, Miners, Beneficiaries, TxsFun, Nonce).

extend_block_chain_with_key_blocks(Chain, Length) ->
    {ok, MinerAccount, _} = wait_for_pubkey(),
    extend_block_chain_with_key_blocks(Chain, Length, MinerAccount, MinerAccount, #{}).

extend_block_chain_with_key_blocks(Chain, Length, MinerAccount, BeneficiaryAccount, BlockCfgs) ->
    [{B, S}| _] = lists:reverse(Chain),
    StartHeight = aec_blocks:height(B),
    BlockCfgs1 = prepare_block_configs(0, StartHeight + Length + 1, BlockCfgs, []),
    {_, BlockCfgs2} = lists:split(StartHeight + 1, BlockCfgs1),
    extend_block_chain_with_key_blocks1(Length, MinerAccount, BeneficiaryAccount, BlockCfgs2, [{B, S}]).

extend_block_chain_with_key_blocks1(N, MinerAccount, BeneficiaryAccount, [BlockCfg | BlockCfgs], Acc) when N > 0 ->
    {B, S} = create_keyblock_with_state(Acc, MinerAccount, BeneficiaryAccount, BlockCfg),
    extend_block_chain_with_key_blocks1(N - 1, MinerAccount, BeneficiaryAccount, BlockCfgs, [{B, S} | Acc]);
extend_block_chain_with_key_blocks1(0, _MinerAccount, _BeneficiaryAccount, [], Acc) ->
    tl(lists:reverse(Acc)).

extend_block_chain_with_micro_blocks(Chain, Txs) ->
    extend_block_chain_with_micro_blocks(Chain, Txs, 0).

extend_block_chain_with_micro_blocks(Chain, Txs, PrevMicroBlocksCount) ->
    {ok, PrivKey} = aec_keys:sign_privkey(),
    Delay = 1 + PrevMicroBlocksCount * aec_governance:micro_block_cycle(),
    RevChain = lists:reverse(Chain),
    [{PrevKeyBlock, _}|_] = lists:dropwhile(fun({B, _}) -> aec_blocks:type(B) =:= micro end,
                                            RevChain),
    lists:reverse(create_micro_blocks(RevChain, PrevKeyBlock, PrivKey, Txs, Delay)).

blocks_only_chain(Chain) ->
    lists:map(fun({B, _S}) -> B end, Chain).

create_micro_block(PrevBlock, PrevKeyBlock, PrivKey, Txs, Trees, Offset) ->
    {Block1, Trees1} =
        aec_block_micro_candidate:create_with_state(PrevBlock, PrevKeyBlock, Txs, Trees),
    Block2 = aec_blocks:set_time_in_msecs(Block1, Offset + aec_blocks:time_in_msecs(Block1)),
    SignedMicroBlock = sign_micro_block(Block2, PrivKey),
    {SignedMicroBlock, Trees1}.

sign_micro_block(MicroBlock, PrivKey) ->
    Bin = aec_headers:serialize_to_signature_binary(aec_blocks:to_header(MicroBlock)),
    Signature = enacl:sign_detached(Bin, PrivKey),
    aec_blocks:set_signature(MicroBlock, Signature).

next_block_with_state([{PB,_PBS} | _] = Chain, Target, Time0, TxsFun, Nonce,
                      PubKey, PrivKey, BeneficiaryPubKey) ->
    Height = aec_blocks:height(PB) + 1,
    Txs = TxsFun(Height),
    %% NG: if a block X used to have Txs, now put them in micro-blocks after
    %% the key-block at height X. Every transaction is put in a separate micro-block.
    BlockCfg = #{target => pick_prev_target(Chain),
                 timestamp => aeu_time:now_in_msecs()},
    {B, S} = create_keyblock_with_state(Chain, PubKey, BeneficiaryPubKey, BlockCfg),
    Chain1 = [begin
                  B1 = aec_blocks:set_target(B, Target),
                  B2 = aec_blocks:set_nonce(B1, Nonce),
                  Time = case Time0 of
                             undefined -> aec_blocks:time_in_msecs(B);
                             _ -> Time0
                         end,
                  {aec_blocks:set_time_in_msecs(B2, Time), S}
              end
              | Chain],
    create_micro_blocks(Chain1, PrivKey, Txs).

create_micro_blocks(Chain, PrivKey, Txs) ->
    [{PrevKeyBlock, _}|_] = lists:dropwhile(fun({B, _}) -> aec_blocks:type(B) =:= micro end,
                                            Chain),
    create_micro_blocks(Chain, PrevKeyBlock, PrivKey, Txs, 1).

create_micro_blocks(Chain, _PrevKeyBlock, _PrivKey, [], _Offset) ->
    Chain;
create_micro_blocks([{PB, PBS} | _] = Chain, PrevKeyBlock, PrivKey, [Tx | Rest], Offset) ->
    Chain1 = [create_micro_block(PB, PrevKeyBlock, PrivKey, [Tx], PBS, Offset) | Chain],
    create_micro_blocks(Chain1, PrevKeyBlock, PrivKey, Rest, Offset + aec_governance:micro_block_cycle()).


sign_tx(Tx, PrivKey) ->
    sign_tx(Tx, PrivKey, false).

sign_tx_hash(Tx, PrivKey) ->
    sign_tx(Tx, PrivKey, true).

sign_pay_for_inner_tx(Tx, PrivKey) ->
    sign_tx(Tx, PrivKey, true, <<"inner_tx">>).

-define(VALID_PRIVK(K), byte_size(K) =:= 64).

sign_tx(Tx, PrivKey, SignHash) ->
    sign_tx(Tx, PrivKey, SignHash, undefined).

sign_tx(Tx, PrivKey, SignHash, AdditionalPrefix) when is_binary(PrivKey) ->
    sign_tx(Tx, [PrivKey], SignHash, AdditionalPrefix);
sign_tx(Tx, PrivKeys, SignHash, AdditionalPrefix) when is_list(PrivKeys) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    Bin1 =
        case SignHash of
            true  -> aec_hash:hash(signed_tx, Bin0);
            false -> Bin0
        end,
    Bin =
        case AdditionalPrefix of
            undefined -> Bin1;
            _ ->
              <<"-", AdditionalPrefix/binary, Bin1/binary>>
        end,
    BinForNetwork = aec_governance:add_network_id(Bin),
    case lists:filter(fun(PrivKey) -> not (?VALID_PRIVK(PrivKey)) end, PrivKeys) of
        [_|_]=BrokenKeys -> erlang:error({invalid_priv_key, BrokenKeys});
        [] -> pass
    end,
    Signatures = [ enacl:sign_detached(BinForNetwork, PrivKey) || PrivKey <- PrivKeys ],
    aetx_sign:new(Tx, Signatures).

co_sign_tx(SignedTx, Priv) ->
    Tx = aetx_sign:innermost_tx(SignedTx),
    [NewSignature] = aetx_sign:signatures(aec_test_utils:sign_tx(Tx, [Priv])),
    add_signature_to_innermost_signed_tx(SignedTx, NewSignature).

add_signature_to_innermost_signed_tx(TopSignedTx, Signature) ->
    modify_innermost_tx(TopSignedTx,
        fun(SignedTx) ->
            aetx_sign:add_signatures(SignedTx, [Signature])
        end).

substitute_innermost_tx(TopSignedTx, NewInnermostTx) ->
    modify_innermost_tx(TopSignedTx,
        fun(SignedTx) ->
            aetx_sign:set_tx(SignedTx, NewInnermostTx)
        end).

modify_innermost_tx(SignedTx, ModFun) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of % topmost tx
        {aega_meta_tx, Meta} ->
            NewInnerTx =
                % recursively go to inner layers until the innermost tx is
                % reached so its singed transaction is updated
                % note: not tail recursive
                modify_innermost_tx(aega_meta_tx:tx(Meta),
                                    ModFun),
            UpdatedTx = aega_meta_tx:set_tx(NewInnerTx, Meta),
            aetx_sign:new(aetx:new(aega_meta_tx, UpdatedTx), []);
        {_, _InnerMostTx} ->
            ModFun(SignedTx)
    end.

signed_spend_tx(ArgsMap) ->
    {ok, SenderAccount, Privkey} = wait_for_pubkey(),
    ArgsMap1 = maps:put(sender_id, aeser_id:create(account, SenderAccount), ArgsMap),
    {ok, SpendTx} = aec_spend_tx:new(ArgsMap1),
    sign_tx(SpendTx, Privkey).

%% function to setup the .genesis file for test SUITE-s
%% SourceGenesisDir is the test release directory from which to take the .genesis
%% DestRelDir is the release being set up
copy_forks_dir(SourceRelDir, DestRelDir) ->
    copy_fork_dir(SourceRelDir, DestRelDir, ?ROMA_PROTOCOL_VSN),
    copy_fork_dir(SourceRelDir, DestRelDir, ?MINERVA_PROTOCOL_VSN),
    copy_fork_dir(SourceRelDir, DestRelDir, ?FORTUNA_PROTOCOL_VSN),
    copy_fork_dir(SourceRelDir, DestRelDir, ?LIMA_PROTOCOL_VSN).


copy_fork_dir(SourceRelDir, DestRelDir, Release) ->
    GenesisDir = aec_fork_block_settings:dir(Release), % ex data/aecore/.genesis
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

get_config(Key, Cfg, DefaultFun) when is_map(Cfg) ->
    case maps:get(Key, Cfg, undefined) of
        Result when Result =/= undefined ->
            Result;
        undefined ->
            DefaultFun()
    end;
get_config(_Key, undefined, DefaultFun) ->
    DefaultFun().


%%%=============================================================================
%%% Key server setup/teardown
%%%=============================================================================

aec_keys_setup() ->
    ok = application:ensure_started(crypto),
    aec_keys_bare_setup().

aec_keys_cleanup(TmpKeysDir) ->
    aec_keys_bare_cleanup(TmpKeysDir),
    ok = application:stop(crypto).

aec_keys_bare_setup() ->
    TmpKeysDir = create_temp_key_dir(),
    {ok, _} = aec_keys:start_link([<<"secret">>, TmpKeysDir]),
    wait_for_it(fun() -> whereis(aec_keys) =/= undefined end, true),
    TmpKeysDir.

aec_keys_bare_cleanup(TmpKeysDir) ->
    ok = aec_keys:stop(),
    remove_temp_key_dir(TmpKeysDir).

remove_temp_key_dir(TmpKeysDir) ->
    {ok, KeyFiles} = file:list_dir(TmpKeysDir),
    %% Expect two filenames - private and public peer keys
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
    nonl(?cmd("mktemp -d")).

nonl([$\n]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].


%%%=============================================================================
%%% State trees setup
%%%=============================================================================

create_state_tree() ->
    aec_trees:new().

create_state_tree_with_account(Account) ->
    create_state_tree_with_accounts([Account]).

create_state_tree_with_accounts(Accounts) ->
    create_state_tree_with_accounts(Accounts, with_backend).

create_state_tree_with_accounts(Accounts, Backend) ->
    StateTrees0 =
        case Backend of
            with_backend -> create_state_tree();
            no_backend -> aec_trees:new_without_backend()
        end,
    AccountsTree0 = aec_trees:accounts(StateTrees0),
    AccountsTree1 = lists:foldl(fun aec_accounts_trees:enter/2,
                                AccountsTree0, Accounts),
    aec_trees:set_accounts(StateTrees0, AccountsTree1).

min_gas_price() ->
    Protocol = aec_hard_forks:protocol_effective_at_height(1),
    max(aec_governance:minimum_gas_price(Protocol),
        aec_tx_pool:minimum_miner_gas_price()).


%%%=============================================================================
%%% Dev reward setup
%%%=============================================================================

dev_reward_setup(Enabled, Activated, BeneficiaryShare) ->
    #{public := PubKeyProtocol} = enacl:sign_keypair(),
    application:set_env(aecore, dev_reward_enabled, Enabled),
    application:set_env(aecore, dev_reward_activated, Activated),
    application:set_env(aecore, dev_reward_allocated_shares, BeneficiaryShare),
    application:set_env(aecore, dev_reward_beneficiaries, [{PubKeyProtocol, BeneficiaryShare}]).


%%%=============================================================================
%%% Throughput test
%%%=============================================================================

run_throughput_test(TestFun, Blocks, Opts) ->
    InsertBlockFun = fun(B0, AccTime) ->
                             T0 = erlang:system_time(microsecond),
                             TestFun(B0),
                             T1 = erlang:system_time(microsecond),
                             [T1 - T0 | AccTime]
                     end,

    %% Run timings
    Timings = lists:foldl(InsertBlockFun, [], Blocks),

    %% Prepare and print data
    TotalRuntime = lists:sum(Timings),
    [Min | _] = TimingsSorted = lists:sort(Timings),
    Max = lists:last(TimingsSorted),
    Range = Max - Min,
    Mean = TotalRuntime div length(Timings),
    Median = lists:nth(ceil(length(Timings) / 2), TimingsSorted),
    Counts = lists:foldl(
        fun(N, Acc) ->
            case lists:keyfind(N, 1, Acc) of
                false ->
                    [{N, 1} | Acc];
                {N, Count} ->
                    lists:keyreplace(N, 1, Acc, {N, Count + 1})
            end
        end, [], Timings),
    [{Mode, _} | _] = lists:keysort(2, Counts),

    DbInfo = case maps:get(db_mode, Opts, ram) of
                 ram  -> "in ram";
                 disc -> "on disc"
             end,

    {Mod, Fun} = maps:get(test_fun, Opts),
    TestFunInfo = atom_to_list(Mod) ++ [$:] ++ atom_to_list(Fun),
    BlockInfo = case maps:get(block_type, Opts) of
                    key -> "key";
                    micro ->
                        TxsPerBlock = maps:get(txs_per_block, Opts),
                        "micro (txs per block: " ++ integer_to_list(TxsPerBlock) ++ ")"
                end,

    io:format(user,
        "~nThroughput testing results (in microseconds) " ++ DbInfo ++ "~n~n"
        "Tested function\t\t= " ++ TestFunInfo ++ "~n"
        "Block type inserted\t= " ++ BlockInfo ++ "~n"
        "# of blocks inserted\t= ~p~n"
        "Total runtime\t\t= ~p~n~n"
        "Min\t= ~p~n"
        "Max\t= ~p~n"
        "Mean\t= ~p~n"
        "Mode\t= ~p~n"
        "Range\t= ~p~n"
        "Median\t= ~p~n",
        [length(Blocks), TotalRuntime, Min, Max, Mean, Mode, Range, Median]
    ),
    ok.
