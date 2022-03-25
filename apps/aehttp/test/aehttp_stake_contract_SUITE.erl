-module(aehttp_stake_contract_SUITE).

-import(aecore_suite_utils, [http_request/4, internal_address/0, external_address/0,
                             rpc/3, rpc/4]).

-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

-export([mine_and_sync/1,
         spend_txs/1,
         simple_withdraw/1,
         change_leaders/1,
         verify_fees/1
        ]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/test/include/aect_sophia_vsn.hrl").

-define(NETWORK_ID, <<"ae_smart_contract_test">>).
-define(STAKING_CONTRACT, "MainStaking").
-define(REWARD_DELAY, 10).
-define(NODE1, dev1).
-define(NODE1_NAME, aecore_suite_utils:node_name(?NODE1)).

-define(NODE2, dev2).
-define(NODE2_NAME, aecore_suite_utils:node_name(?NODE2)).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).

-define(PEEK_MSGQ, peek_msgq(?LINE)).

-define(ALICE, {
    <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
      53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
      207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
      188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
      80,196,174,81,239,171,117,158,65,91,102>>}).
%% ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm

-define(BOB, {
    <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
      33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
      62,238,132>>,
    <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
      154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
      73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
      210,210,54,3,122,84,195,62,238,132>>}).
%% ak_nQpnNuBPQwibGpSJmjAah6r3ktAB7pG9JHuaGWHgLKxaKqEvC

-define(CAROL, {
    <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
      190,211,20,112,79,108,85,78,88,181,26,207,191,211,
      40,225,138,154>>,
    <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
      100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
      93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
      85,78,88,181,26,207,191,211,40,225,138,154>>}).


all() -> [{group, all}
         ].

groups() ->
    [ {all, [sequence],
       [ verify_fees
       , mine_and_sync
       , spend_txs
       , simple_withdraw
       , change_leaders
       ]}
    ].

suite() -> [].

init_per_suite(Config0) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> {skip, not_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, not_in_minerva};
        ?FORTUNA_PROTOCOL_VSN -> {skip, not_in_fortuna};
        ?LIMA_PROTOCOL_VSN    -> {skip, not_in_lima};
        ?IRIS_PROTOCOL_VSN    -> {skip, not_in_iris};
        Vsn when Vsn >= ?CERES_PROTOCOL_VSN ->
            {_PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
            ct:log("Patron is ~p", [aeser_api_encoder:encode(account_pubkey, PatronPub)]),
            Pubkey = <<42:32/unit:8>>,
            EncodePub =
                fun(P) ->
                    binary_to_list(aeser_api_encoder:encode(account_pubkey, P))
                end,

              
            #{ <<"pubkey">> := StakingValidatorContract} = C0
                = contract_create_spec("StakingValidator",
                                       [EncodePub(Pubkey)], 0, 1, Pubkey),
            #{ <<"pubkey">> := ConsensusContractPubkey
             , <<"owner_pubkey">> := ContractOwner } = C
                = contract_create_spec(?STAKING_CONTRACT, [binary_to_list(StakingValidatorContract), "\"domat\""], 0, 2, Pubkey),
            {ok, CCId} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                       ConsensusContractPubkey), 
            Call1 = 
                contract_call_spec(CCId, ?STAKING_CONTRACT,
                                   "new_validator", [],
                                   trunc(math:pow(10,21)), pubkey(?ALICE), 1),
            Call2 = 
                contract_call_spec(CCId, ?STAKING_CONTRACT,
                                   "new_validator", [],
                                   trunc(math:pow(10,21)), pubkey(?BOB), 1),
            Call3 = 
                contract_call_spec(CCId, ?STAKING_CONTRACT,
                                   "set_online", [], 0, pubkey(?ALICE), 2),
            Call4 = 
                contract_call_spec(CCId, ?STAKING_CONTRACT,
                                   "set_online", [], 0, pubkey(?BOB), 2),
            %% create a BRI validator in the contract so they can receive
            %% rewards as well
            %% TODO: discuss how we want to tackle this:
            %%  A) require the BRI account to be validator
            %%  B) allow pending stake in the contract that is not allocated
            %%  yet
            %%  C) something else
            {ok, BRIPub} = aeser_api_encoder:safe_decode(account_pubkey, <<"ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8">>),
            Call5 = 
                contract_call_spec(CCId, ?STAKING_CONTRACT,
                                   "new_validator", [],
                                   trunc(math:pow(10,21)), BRIPub, 1),
            %% keep the BRI offline
            AllCalls =  [Call1, Call2, Call3, Call4, Call5],
            BuildConfig =
                fun(PotentialStakers) ->
                    Stakers =
                        lists:map(
                            fun(Who) ->
                                Pub = encoded_pubkey(Who),
                                Priv = aeser_api_encoder:encode(contract_bytearray,
                                                                privkey(Who)), %% TODO: discuss key management
                                #{<<"pub">> => Pub, <<"priv">> => Priv}
                            end,
                            PotentialStakers),
                    #{<<"chain">> =>
                        #{<<"persist">> => false,
                        %% we start from CERES hard fork so we can use all
                        %% the nice things CERES has to offer
                        %% TODO: have a consensus-specific setting in aec_hard_forks
                        <<"hard_forks">> => #{integer_to_binary(?CERES_PROTOCOL_VSN) => 0},
                        <<"consensus">> => 
                            #{<<"0">> => #{<<"name">> => <<"smart_contract">>,
                                            <<"config">> => #{<<"contracts">> => [C0, C],
                                                              <<"calls">> => AllCalls,
                                                              <<"consensus_contract">> => ConsensusContractPubkey,
                                                              <<"contract_owner">> => ContractOwner,
                                                              <<"expected_key_block_rate">> => 2000,
                                                              <<"stakers">> => Stakers}}}},
                  <<"fork_management">> =>
                      #{<<"network_id">> => ?NETWORK_ID},
                  <<"mining">> =>
                      #{<<"micro_block_cycle">> => 1,
                        <<"autostart">> => false,
                        <<"beneficiary_reward_delay">> => ?REWARD_DELAY
                        }}
                end,
            {ok, _StartedApps} = application:ensure_all_started(gproc),
            Config = [{symlink_name, "latest.staking"}, {test_module, ?MODULE}] ++ Config0,
            Config1 = aecore_suite_utils:init_per_suite([?NODE1, ?NODE2],
                                                        BuildConfig([?ALICE,
                                                                     ?BOB]),
                                                        [{add_peers, true}],
                                                        Config),
            aecore_suite_utils:create_config(?NODE2, Config1, BuildConfig([]), []),
            aecore_suite_utils:start_node(?NODE1, Config1),
            aecore_suite_utils:connect(?NODE1_NAME, []),
            aecore_suite_utils:start_node(?NODE2, Config1),
            aecore_suite_utils:connect(?NODE2_NAME, []),
            #{<<"pubkey">> := EncodedContractPubkey} = C,
            {ok, ContractPubkey}   =
            aeser_api_encoder:safe_decode(contract_pubkey, EncodedContractPubkey),
            [{staking_contract, ContractPubkey} | Config1]
    end.

end_per_suite(Config) ->
    [application:stop(A) ||
        A <- lists:reverse(
               proplists:get_value(started_apps, Config, []))],
    aecore_suite_utils:stop_node(?NODE1, Config),
    aecore_suite_utils:stop_node(?NODE2, Config),
    ok.

init_per_group(_Group, Config0) ->
    VM = fate,
    Config1 = aect_test_utils:init_per_group(VM, Config0),
    Config1.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    aect_test_utils:setup_testcase(Config),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

contract_create_spec(Name, Args, Amount, Nonce, Owner) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, Code}   = aect_test_utils:compile_contract(aect_test_utils:sophia_version(), Name),
    Pubkey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    EncodedPubkey   = aeser_api_encoder:encode(contract_pubkey, Pubkey),
    EncodedOwner    = aeser_api_encoder:encode(account_pubkey, Owner),
    EncodedCode     = aeser_api_encoder:encode(contract_bytearray, Code),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, "init", Args),
    EncodedCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    VM = aect_test_utils:vm_version(),
    ABI = aect_test_utils:abi_version(),
    Spec = #{ <<"amount">> => Amount
            , <<"vm_version">> => VM
            , <<"abi_version">> => ABI
            , <<"nonce">> => Nonce
            , <<"code">> => EncodedCode
            , <<"call_data">> => EncodedCallData
            , <<"pubkey">> => EncodedPubkey
            , <<"owner_pubkey">> => EncodedOwner },
    Spec.

contract_call_spec(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {contract_call_tx, CallTx} =
        aetx:specialize_type(contract_call(ContractPubkey, Name, Fun, Args,
                                           Amount, From, Nonce)),
    Spec =
        #{  <<"caller">>          => aeser_api_encoder:encode(account_pubkey,
                                                              aect_call_tx:caller_pubkey(CallTx))
          , <<"nonce">>           => aect_call_tx:nonce(CallTx)
          , <<"contract_pubkey">> => aeser_api_encoder:encode(contract_pubkey,
                                                              aect_call_tx:contract_pubkey(CallTx))
          , <<"abi_version">>     => aect_call_tx:abi_version(CallTx)
          , <<"fee">>             => aect_call_tx:fee(CallTx)
          , <<"amount">>          => aect_call_tx:amount(CallTx)
          , <<"gas">>             => aect_call_tx:gas(CallTx)
          , <<"gas_price">>       => aect_call_tx:gas_price(CallTx)
          , <<"call_data">>       => aeser_api_encoder:encode(contract_bytearray,
                                                              aect_call_tx:call_data(CallTx))},
    Spec.

contract_call(ContractPubkey, Name, Fun, Args, Amount, From) ->
    Nonce = next_nonce(From),
    contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce).

contract_call(ContractPubkey, Name, Fun, Args, Amount, From, Nonce) ->
    {ok, BinSrc} = aect_test_utils:read_contract(aect_test_utils:sophia_version(), Name),
    Src = binary_to_list(BinSrc),
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    ABI = aect_test_utils:abi_version(),
    TxSpec =
        #{  caller_id   => aeser_id:create(account, From)
          , nonce       => Nonce
          , contract_id => aeser_id:create(contract, ContractPubkey)
          , abi_version => ABI
          , fee         => 1000000 * ?DEFAULT_GAS_PRICE
          , amount      => Amount
          , gas         => 1000000
          , gas_price   => ?DEFAULT_GAS_PRICE
          , call_data   => CallData},
    {ok, Tx} = aect_call_tx:new(TxSpec),
    Tx.

contract_call_staking_contract(Who, Fun, Args, Amt, Config) ->
    ContractPubkey = ?config(staking_contract, Config),
    contract_call(ContractPubkey, ?STAKING_CONTRACT, Fun,
                  Args, Amt, pubkey(Who)).

mine_and_sync(_Config) ->
    {ok, [KB0]} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, 1),
    {ok, KB0} = wait_same_top(),
    ok.
    
wait_same_top() ->
    wait_same_top(10).

wait_same_top(Attempts) when Attempts < 1 ->
    {error, run_out_of_attempts};
wait_same_top(Attempts) ->
    case {rpc(?NODE1, aec_chain, top_block, []), rpc(?NODE2, aec_chain, top_block, [])} of
        {KB, KB} -> {ok, KB};
        {KB1, KB2} ->
            ct:log("Node1 top: ~p\nNode2 top: ~p", [KB1, KB2]),
            timer:sleep(500),
            wait_same_top(Attempts - 1)
    end.

spend_txs(_Config) ->
    Top0 = rpc(?NODE1, aec_chain, top_header, []),
    ct:log("Top before posting spend txs: ~p", [aec_headers:height(Top0)]),
    seed_account(pubkey(?ALICE), 100000001 * ?DEFAULT_GAS_PRICE),
    seed_account(pubkey(?BOB), 100000002 * ?DEFAULT_GAS_PRICE),
    seed_account(pubkey(?CAROL), 100000003 * ?DEFAULT_GAS_PRICE),

    lists:foreach(
        fun(GenIndex) ->
            {ok, Gen} = rpc:call(?NODE1_NAME, aec_chain,
                                get_generation_by_height, [GenIndex, forward]),
            ct:log("Generation ~p:\n~p", [GenIndex, Gen])
        end,
        lists:seq(1, 2)),
    ok.

simple_withdraw(Config) ->
    Alice = binary_to_list(encoded_pubkey(?ALICE)),
    stopped = rpc:call(?NODE1_NAME, aec_conductor, get_mining_state, []),
    false = rpc:call(?NODE1_NAME, aec_conductor, is_leader, []),
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),

    {ok, [KB0]} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, 1),
    Top0 = aec_blocks:to_header(KB0),
    Top0 = rpc(?NODE1, aec_chain, top_header, []),
    ct_log_header(Top0),
    InitBalance = balance(pubkey(?ALICE)),
    {ok, _AliceContractBalance} = inspect_staking_contract(?ALICE, {balance, ?ALICE}, Config),
    {ok, _BobContractBalance} = inspect_staking_contract(?ALICE, {balance, ?BOB}, Config),
    {ok,
        #{<<"delegates">> := [[<<"ak_2MGLPW2CHTDXJhqFJezqSwYSNwbZokSKkG7wSbGtVmeyjGfHtm">>, 1000000000000000000000]],
          <<"main_staking_ct">> := <<"ak_LRbi65kmLtE7YMkG6mvG5TxAXTsPJDZjAtsPuaXtRyPA7gnfJ">>,
          <<"shares">> := 1000000000000000000000}} =
        inspect_staking_contract(?ALICE, {get_validator_state, ?ALICE}, Config),
    WithdrawAmount = 1000,
    Fun = "unstake",
    CallTx =
        sign_and_push(
              contract_call_staking_contract( ?ALICE, Fun,
                                              [Alice, integer_to_list(WithdrawAmount)], 0,
                                              Config),
                           ?ALICE),
    {ok, [_]} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    {value, _Acc} = rpc(?NODE1, aec_chain, get_account, [pubkey(?ALICE)]),
    mine_tx(CallTx),
    EndBalance = balance(pubkey(?ALICE)),
    {ok, Call} = call_info(CallTx),
    {ok, _Res} = decode_consensus_result(Call, Fun),
    GasUsed = aect_call:gas_used(Call),
    GasPrice = aect_call:gas_price(Call),
    Fee = aetx:fee(aetx_sign:tx(CallTx)),
    ct:log("Initial balance: ~p, withdrawn: ~p, gas used: ~p, gas price: ~p, fee: ~p, end balance: ~p",
           [InitBalance, WithdrawAmount, GasUsed, GasPrice,
                          Fee, EndBalance]),
%% TODO: adjust rewarded fees
%%    {EndBalance, EndBalance} = {EndBalance, InitBalance + WithdrawAmount -
%%                               TotalSpent},

    {ok, _AliceContractBalance1} = inspect_staking_contract(?ALICE, {balance, ?ALICE}, Config),
%%    {AliceContractBalance, AliceContractBalance} = {AliceContractBalance, AliceContractBalance1 + 1},
%%    {ok, BobContractBalance} = inspect_staking_contract(?ALICE, {balance, ?BOB}, Config),
    Top1 = rpc(?NODE1, aec_chain, top_header, []),
    ct_log_header(Top1),
    TimeInBetween = aec_headers:time_in_msecs(Top1) - aec_headers:time_in_msecs(Top0),
    BlocksInBetween = aec_headers:height(Top1) - aec_headers:height(Top0),
    ct:log("Key blocks: ~p, Time difference = ~p", [BlocksInBetween, TimeInBetween]),

    ok.

change_leaders(Config) ->
    {ok, AliceBalance} = inspect_staking_contract(?ALICE, {balance, ?ALICE}, Config),
    {ok, BobBalance} = inspect_staking_contract(?ALICE, {balance, ?BOB}, Config),
    ct:log("Alice ~p, balance: ~p", [encoded_pubkey(?ALICE), AliceBalance]),
    ct:log("Bob ~p, balance: ~p", [encoded_pubkey(?BOB), BobBalance]),
    Blocks = 10,
    NewLeader =
        fun() ->
            {ok, [KB]} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, 1),
            Beneficiary = aec_blocks:beneficiary(KB),
            Beneficiary = aec_blocks:miner(KB),
            ct_log_block(KB),
            {ok, Leader} = inspect_staking_contract(?ALICE, current_leader, Config),
            {ok, LeaderDecoded} =
                aeser_api_encoder:safe_decode(account_pubkey, Leader),
            Beneficiary = LeaderDecoded, %% assert
            Leader
        end,
    Ls = lists:map(fun(_Idx) -> NewLeader() end, lists:seq(1, Blocks)),
    Stats =
        lists:foldl(
            fun(Leader, Accum) ->
                maps:update_with(Leader, fun(X) -> X + 1 end, 1, Accum)
            end, #{}, Ls),
    ct:log("Leaders: ~p", [Stats]),
    AliceLeaderCnt = maps:get(encoded_pubkey(?ALICE), Stats, 0),
    BobLeaderCnt = maps:get(encoded_pubkey(?BOB), Stats, 0),
    false = AliceLeaderCnt =:= Blocks,
    true  = AliceLeaderCnt > 0,
    false = BobLeaderCnt =:= Blocks,
    true  = BobLeaderCnt > 0,
    {ok, _B} = wait_same_top(),
    ok.
    
verify_fees(Config) ->
    %% start without any tx fees, only a keyblock
    {ok, _} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, ?REWARD_DELAY + 1),
    Test =
        fun() ->
            %% gather balances before reward distribution
            AliceBalance0 = balance(pubkey(?ALICE)),
            BobBalance0 = balance(pubkey(?BOB)),
            {ok, AliceContractBalance0} = inspect_staking_contract(?ALICE, {balance, ?ALICE}, Config),
            {ok, BobContractBalance0} = inspect_staking_contract(?ALICE, {balance, ?BOB}, Config),
            {ok, _} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, 1),
            %% gather balances after reward distribution
            AliceBalance1 = balance(pubkey(?ALICE)),
            BobBalance1 = balance(pubkey(?BOB)),
            {ok, AliceContractBalance1} = inspect_staking_contract(?ALICE, {balance, ?ALICE}, Config),
            {ok, BobContractBalance1} = inspect_staking_contract(?ALICE, {balance, ?BOB}, Config),
            %% inspect who shall receive what reward
            Top = rpc(?NODE1, aec_chain, top_header, []),
            Height = aec_headers:height(Top),
            ct:log("Current top is ~p", [Height]),
            RewardForHeight = Height - ?REWARD_DELAY,
            {ok, PrevH} = rpc(?NODE1, aec_chain, get_key_header_by_height, [RewardForHeight - 1]),
            {ok, RewardH} = rpc(?NODE1, aec_chain, get_key_header_by_height, [RewardForHeight]),
            Beneficiary1 = aec_headers:beneficiary(PrevH),
            Beneficiary2 = aec_headers:beneficiary(RewardH),
            ct:log("Beneficiary1: ~p, Beneficiary2: ~p", [Beneficiary1, Beneficiary2]),
            %% assert account balances do not change; only contract balances change
            {AliceBalance0, AliceBalance0} = {AliceBalance0, AliceBalance1},
            {BobBalance0, BobBalance0} = {BobBalance0, BobBalance1},
            %% calc rewards
            {{AdjustedReward1, AdjustedReward2}, _DevRewards} =
                calc_rewards(RewardForHeight),
            {AliceExpectedRewards, BobExpectedRewards} =
                lists:foldl(
                    fun({Pubkey, Amount}, {AliceRewards0, BobRewards0}) ->
                          case who_by_pubkey(Pubkey) of
                              ?ALICE ->
                                  {AliceRewards0 + Amount, BobRewards0};
                              ?BOB ->
                                  {AliceRewards0, BobRewards0 + Amount}
                          end
                    end,
                    {0, 0},
                    [{Beneficiary1, AdjustedReward1},
                    {Beneficiary2, AdjustedReward2}]),
            AliceReward = AliceContractBalance1 - AliceContractBalance0,
            ct:log("Alice expected rewards: ~p, actual rewards: ~p",
                  [AliceExpectedRewards, AliceReward]),
            {AliceExpectedReward, AliceExpectedReward} =
                {AliceExpectedRewards, AliceReward},
            BobReward = BobContractBalance1 - BobContractBalance0,
            ct:log("Bob expected rewards: ~p, actual rewards: ~p",
                  [BobExpectedRewards, BobReward]),
            {BobExpectedReward, BobExpectedReward} =
                {BobExpectedRewards, BobReward}
        end,
    %% test a couple of empty generations - there are no fees, only block
    %% rewards
    lists:foreach(
        fun(_) -> Test() end,
        lists:seq(1, 10)),
    {ok, SignedTx} = seed_account(pubkey(?ALICE), 1),
    {ok, _} = aecore_suite_utils:mine_key_blocks(?NODE1_NAME, ?REWARD_DELAY - 1),
    ct:log("Test with no transaction", []),
    Test(), %% before fees
    ct:log("Test with a spend transaction", []),
    Test(), %% fees
    ct:log("Test with no transaction", []),
    Test(), %% after fees
    ok.

pubkey({Pubkey, _}) -> Pubkey.

privkey({_, Privkey}) -> Privkey.

who_by_pubkey(Pubkey) ->
    case Pubkey =:= pubkey(?ALICE) of
        true -> ?ALICE;
        false ->
            case Pubkey =:= pubkey(?BOB) of
                true -> ?BOB;
                false -> error(unknown_beneficiary)
            end
    end.

encoded_pubkey(Who) ->
    aeser_api_encoder:encode(account_pubkey, pubkey(Who)).

next_nonce(Pubkey) ->
    case rpc(?NODE1, aec_next_nonce, pick_for_account, [Pubkey, max]) of
        {ok, NextNonce} -> NextNonce;
        {error, account_not_found} -> 1
    end.

sign_and_push(Tx, Who) ->
    SignedTx = sign_tx(Tx, privkey(Who)),
    ok = rpc:call(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
    SignedTx.

%% usually we would use aec_test_utils:sign_tx/3. This function is being
%% executed in the context of the CT test and uses the corresponding
%% network_id. Since the network_id of the HC node is different, we must sign
%% the tx using the test-specific network_id
sign_tx(Tx, Privkey) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    Bin = aec_hash:hash(signed_tx, Bin0), %% since we are in CERES context, we sign th hash
    BinForNetwork = <<?NETWORK_ID/binary, Bin/binary>>,
    Signatures = [ enacl:sign_detached(BinForNetwork, Privkey)],
    aetx_sign:new(Tx, Signatures).

seed_account(RecpipientPubkey, Amount) ->
    %% precondition
    {ok, []} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    ct:log("Seed spend tx", []),
    {PatronPriv, PatronPub} = aecore_suite_utils:sign_keys(?NODE1),
    Nonce = next_nonce(PatronPub),
    Params =
        #{sender_id    => aeser_id:create(account, PatronPub),
          recipient_id => aeser_id:create(account, RecpipientPubkey),
          amount       => Amount,
          fee          => 18000 * ?DEFAULT_GAS_PRICE,
          nonce        => Nonce,
          payload      => <<>>},
    ct:log("Preparing a spend tx: ~p", [Params]),
    {ok, Tx} = aec_spend_tx:new(Params),
    SignedTx = sign_tx(Tx, PatronPriv),
    ok = rpc:call(?NODE1_NAME, aec_tx_pool, push, [SignedTx, tx_received]),
    {ok, [_SpendTx]} = rpc:call(?NODE1_NAME, aec_tx_pool, peek, [infinity]),
    mine_tx(SignedTx),
    {ok, SignedTx}.

mine_tx(SignedTx) ->
    TxHash = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(?NODE1_NAME,
                                                      [TxHash],
                                                      10). %% max keyblocks

balance(Pubkey) ->
    case rpc(?NODE1, aec_chain, get_account, [Pubkey]) of
        {value, Account} -> aec_accounts:balance(Account);
        none -> no_such_account
    end.

inspect_staking_contract(OriginWho, WhatToInspect, Config) ->
    {Fun, Args} =
        case WhatToInspect of
            {balance, Who} ->
                {"balance", [binary_to_list(encoded_pubkey(Who))]};
            current_leader ->
                {"leader", []};
          {get_validator_state, Who} ->
                {"get_validator_state", [binary_to_list(encoded_pubkey(Who))]}
        end,
    Tx = contract_call_staking_contract(OriginWho, Fun, Args, 0, Config),
    {ok, Call} = dry_run(Tx),
    {_Type, _Res} = decode_consensus_result(Call, Fun).

dry_run(Tx) ->
    TopHash = rpc(?NODE1, aec_chain, top_block_hash, []),
    case rpc(?NODE1, aec_dry_run, dry_run, [TopHash, [], [{tx, Tx}]]) of
        {error, _} = Err -> Err;
        {ok, {[{contract_call_tx, {ok, Call}}], _Events}} -> {ok, Call}
    end.


call_info(SignedTx) ->
    Hash = aetx_sign:hash(SignedTx),
    case rpc:call(?NODE1_NAME, aec_chain, find_tx_location, [Hash]) of
        not_found ->  {error, unknown_tx};
        none -> {error, gced_tx};
        mempool -> {error, tx_in_pool};
        MBHash when is_binary(MBHash) -> 
            case rpc:call(?NODE1_NAME, aehttp_helpers, get_info_object_signed_tx,
                          [MBHash, SignedTx]) of
                {ok, Call} -> {ok, Call};
                {error, Reason} -> {error, Reason}
            end
    end.

ct_log_block(Block) ->
    ct_log_header(aec_blocks:to_header(Block)).

ct_log_header(Header) ->
    Time = aec_headers:time_in_msecs(Header),
    DateTime = calendar:system_time_to_universal_time(Time, millisecond),
    Height = aec_headers:height(Header),
    ct:log("Block ~p, Timestamp: ~p (~p)", [Height, DateTime, Time]).

decode_consensus_result(Call, Fun) ->
    ReturnType = aect_call:return_type(Call),
    ReturnValue = aect_call:return_value(Call),
    {ok, BinCode} = aect_test_utils:read_contract(?SOPHIA_CERES_FATE, ?STAKING_CONTRACT),
    Res =
        aect_test_utils:decode_call_result(binary_to_list(BinCode), Fun,
                                          ReturnType, ReturnValue),
    {ReturnType, Res}.

calc_rewards(RewardForHeight) ->
    %% we distribute rewards for the previous 
    {ok, #{key_block := PrevKB,
           micro_blocks := MBs}}
        = rpc(?NODE1, aec_chain, get_generation_by_height,
              [RewardForHeight - 1, forward]),
    PrevGenProtocol = aec_blocks:version(PrevKB),
    Txs = lists:flatten(
            lists:map(
                fun(MB) -> aec_blocks:txs(MB) end,
                MBs)),
    ct:log("Txs: ~p", [Txs]),
    KeyReward = rpc(?NODE1, aec_governance, block_mine_reward,
                    [RewardForHeight]),
    GenerationFees =
        lists:foldl(
            fun(SignTx, Accum) ->
                %% TODO: maybe add support for contract calls:
                %% * contract create
                %% * contract call
                %% * force progress
                %% * meta tx
                Tx = aetx_sign:tx(SignTx),
                Fee = aetx:fee(Tx),
                Accum + Fee
            end,
            0,
            Txs),
    ct:log("Height ~p, Generation fees: ~p, key reward: ~p",
           [RewardForHeight, GenerationFees, KeyReward]),
    BeneficiaryReward1 = GenerationFees * 4 div 10,
    BeneficiaryReward2 = GenerationFees - BeneficiaryReward1 + KeyReward,
   %% TODO: verify devrewards
    {{AdjustedReward1, AdjustedReward2}, _DevRewards} = Res =
        rpc(?NODE1, aec_dev_reward, split,
            [BeneficiaryReward1, BeneficiaryReward2, PrevGenProtocol]),
    ct:log("AdjustedReward1: ~p, AdjustedReward2: ~p",
           [AdjustedReward1, AdjustedReward2]),
    Res.

