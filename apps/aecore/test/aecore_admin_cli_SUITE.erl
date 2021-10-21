-module(aecore_admin_cli_SUITE).


%% Test cases for verifying maintenance mode, etc.

%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [ push_tx/1
   , inspect_tx/1
   , miner_gas_price/1
   ]).

-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).
-define(NODE_NAME, aecore_suite_utils:node_name(dev1)).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).
-define(MINE_RATE, 100).
-define(REWARD_DELAY, 2).
-define(GC_TTL, 5).

all() ->
    [
     {group, tx_pool}
    ].

groups() ->
    [
     {tx_pool, [sequence],
      [ push_tx
      , inspect_tx
      , miner_gas_price]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Config =
        aecore_suite_utils:init_per_suite(
          [?NODE],
          #{ <<"mining">> =>
              #{ <<"expected_mine_rate">> => ?MINE_RATE,
                  %% this is important so beneficiary can spend
                  <<"beneficiary_reward_delay">> => ?REWARD_DELAY},
            <<"mempool">> => #{ <<"tx_ttl">> => ?GC_TTL}
           },
          [ {symlink_name, "latest.admin_cli"}
          , {instant_mining, true}
          , {test_module, ?MODULE}] ++ Config0),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect_wait(?NODE_NAME, aehttp),
    TestDir = "lib.aecore.aecore_admin_cli_SUITE.logs",
    {ok, Subdirs} = file:list_dir([TestDir]),
    [RunDir] = [D || D <- Subdirs, string:slice(D, 0, 4) =:= "run."],
    Executable =
        filename:join([TestDir, RunDir, "log_private",
                       "aecore_admin_cli_SUITE", atom_to_list(?NODE), "bin", "aeternity"]),
    %% ensure it is there:
    Res = os:cmd(Executable ++ " admin --help"),
    ExpectedRes =
        "admin: unrecognised argument: --help\n"
        "usage: admin  {tx_pool}\n\n"
        "Subcommands:\n"
        "  tx_pool Transaction pool commands\n",
    ExpectedRes = Res,
    %% mine keyblocks so the miner receives their first reward and have some
    %% tokens in their account - this creates the account iteself
    {ok, _} = aecore_suite_utils:mine_blocks(?NODE_NAME, ?REWARD_DELAY + 1, ?MINE_RATE, key, #{}),
    [{executable, Executable}, {nodes, [aecore_suite_utils:node_tuple(?NODE)]} | Config].

init_per_group(_, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

push_tx(Config) ->
    Tx = spend(),
    EncTx = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(Tx)),
    OKRes = "The transaction is being put in the pool.\n",
    %% test `tx_pool push <tx>`
    Res = cli(["tx_pool", "push", binary_to_list(EncTx)], Config),
    OKRes = Res,
    %% test `tx_pool size`
    "1" = cli(["tx_pool", "size"], Config),
    "1" = cli(["tx_pool", "size", "--show=all"], Config),
    "1" = cli(["tx_pool", "size", "--show=not_visited"], Config),
    "0" = cli(["tx_pool", "size", "--show=visited"], Config),
    %% test `tx_pool delete <tx_hash>`
    DelRes = cli(["tx_pool", "delete", tx_hash(Tx)], Config),
    DelRes = "Tx deleted from the pool.\n",
    "0" = cli(["tx_pool", "size"], Config),
    "0" = cli(["tx_pool", "size", "--show=all"], Config),
    "0" = cli(["tx_pool", "size", "--show=not_visited"], Config),
    "0" = cli(["tx_pool", "size", "--show=visited"], Config),
    %% delete again fails
    TxNotInDB = "Transaction not present in the tx-pool\n",
    TxNotInDB = cli(["tx_pool", "delete", tx_hash(Tx)], Config),
    %% test `tx_pool push <tx>`
    ErrRes = cli(["tx_pool", "push", EncTx], Config),
    ErrRes = "Transaction push failed: already_known\n",
    "0" = cli(["tx_pool", "size"], Config),
    "0" = cli(["tx_pool", "size", "--show=all"], Config),
    "0" = cli(["tx_pool", "size", "--show=not_visited"], Config),
    "0" = cli(["tx_pool", "size", "--show=visited"], Config),
    %% test `tx_pool push --f <tx>`
    Res = cli(["tx_pool", "push -f", EncTx], Config),
    OKRes = Res,
    %% test `tx_pool size`
    "1" = cli(["tx_pool", "size"], Config),
    "1" = cli(["tx_pool", "size", "--show=all"], Config),
    "1" = cli(["tx_pool", "size", "--show=not_visited"], Config),
    "0" = cli(["tx_pool", "size", "--show=visited"], Config),
    %% test `tx_pool delete <tx_hash>` deleted tx
    DelRes = cli(["tx_pool", "delete", tx_hash(Tx)], Config),
    TxNotInDB = cli(["tx_pool", "delete", tx_hash(Tx)], Config),
    "0" = cli(["tx_pool", "size"], Config),
    "0" = cli(["tx_pool", "size", "--show=all"], Config),
    "0" = cli(["tx_pool", "size", "--show=not_visited"], Config),
    "0" = cli(["tx_pool", "size", "--show=visited"], Config),
    ok.

inspect_tx(Config) ->
    #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
    Height0 = rpc:call(?NODE_NAME, aec_chain, top_height, []),
    %% this is a new account that does not have enough funds to cover the tx
    Tx = spend(Pub, Pub, 1, Priv, 1),
    EncTx = aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(Tx)),
    OKRes = "The transaction is being put in the pool.\n",
    %% test `tx_pool push <tx>`
    Res = cli(["tx_pool", "push", EncTx], Config),
    InspectRes = cli(["tx_pool", "inspect", tx_hash(Tx)], Config),
    ExpectedRes = io_lib:format("Failures: 0\nVisited : false\nTTL     : ~B\n",
                                [Height0 + ?GC_TTL]),
    {InspectRes, InspectRes} = {ExpectedRes, InspectRes},
    %% the TLL stays the same after some nodes are mined
    {ok, _} = aecore_suite_utils:mine_blocks(?NODE_NAME, 1, ?MINE_RATE, key, #{}),
    InspectRes = cli(["tx_pool", "inspect", tx_hash(Tx)], Config),
    %% pushing again the same tx does not change anything
    ErrRes = cli(["tx_pool", "push", EncTx], Config),
    ErrRes = "Transaction push failed: already_known\n",
    InspectRes = cli(["tx_pool", "inspect", tx_hash(Tx)], Config),
    %% force pushed does not change TTL as well
    Res = cli(["tx_pool", "push -f", EncTx], Config),
    InspectRes = cli(["tx_pool", "inspect", tx_hash(Tx)], Config),
    OKRes = Res,
    %% check the attempts are counted correctly
    Generations = 2,
    {error, max_reached} =
        aecore_suite_utils:mine_blocks_until_txs_on_chain(?NODE_NAME, [EncTx],
                                                      Generations),
    ExpectedRes2 = io_lib:format("Failures: ~B\nVisited : false\nTTL     : ~B\n",
                                [Generations, Height0 + ?GC_TTL]),
    InspectRes2 = cli(["tx_pool", "inspect", tx_hash(Tx)], Config),
    {InspectRes2, InspectRes2} = {ExpectedRes2, InspectRes2},
    _DelRes = cli(["tx_pool", "delete", tx_hash(Tx)], Config),
    ok.

miner_gas_price(Config) ->
    DefaultGasPrice = cli(["tx_pool", "miner_gas_price", "get"], Config),
    NewGasPrice = 1234567890,
    NewGasPriceStr = integer_to_list(NewGasPrice),
    true = DefaultGasPrice =/= NewGasPriceStr,
    ResOK = cli(["tx_pool", "miner_gas_price", "set", NewGasPrice], Config),
    ExpectedResOK = io_lib:format("Updated to ~B\n", [NewGasPrice]),
    {ResOK, ResOK} = {ExpectedResOK, ResOK},
    NewGasPriceStr = cli(["tx_pool", "miner_gas_price", "get"], Config),
    ExpectedResOK2 = io_lib:format("Updated to ~B\n", [list_to_integer(DefaultGasPrice)]),
    ResOK2 = cli(["tx_pool", "miner_gas_price", "set", DefaultGasPrice], Config),
    {ResOK2, ResOK2} = {ExpectedResOK2, ResOK2},
    DefaultGasPrice = cli(["tx_pool", "miner_gas_price", "get"], Config),
    ok.

cli(Commands0, Config) ->
    Executable = ?config(executable, Config),
    ToStrFun =
        fun(I) when is_integer(I) -> integer_to_list(I);
           (B) when is_binary(B) -> binary_to_list(B);
           (Str) when is_list(Str) -> Str
        end,
    Commands = [ToStrFun(E) || E <- Commands0],
    Cmd = string:join([Executable, "admin" | Commands], " "),
    ct:log(string:join(["admin" | Commands], " "), []),
    Res = os:cmd(Cmd),
    ct:log("Result: ~p", [Res]),
    Res.

spend() ->
    {Priv, Pub} = aecore_suite_utils:sign_keys(?NODE),
    spend(Pub, Pub, 1, Priv).

spend(From, To, Amt, FromPriv) ->
    {ok, Nonce} = rpc:call(?NODE_NAME, aec_next_nonce, pick_for_account, [From]),
    spend(From, To, Amt, FromPriv, Nonce).

spend(From, To, Amt, FromPriv, Nonce) ->
    Params = #{sender_id    => aeser_id:create(account, From),
               recipient_id => aeser_id:create(account, To),
               amount       => Amt,
               fee          => ?SPEND_FEE,
               nonce        => Nonce,
               payload      => <<"foo">>},
    {ok, Tx} = aec_spend_tx:new(Params),
    aec_test_utils:sign_tx(Tx, FromPriv, false).

tx_hash(Tx) ->
    binary_to_list(aeser_api_encoder:encode(tx_hash, aetx_sign:hash(Tx))).
