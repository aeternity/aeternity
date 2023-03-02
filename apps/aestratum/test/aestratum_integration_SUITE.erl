-module(aestratum_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include_lib("aecontract/include/hard_forks.hrl").

-include("aestratum.hrl").

-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1, init_per_group/2, end_per_group/2, init_per_testcase/2,
         end_per_testcase/2]).

-export([session_in_authorized_phase/1,
         mining_stratum_block/1,
         rewarding_participants/1,
         client_target_change/1]).


-define(STRATUM_SERVER_NODE, dev1).
-define(MINING_NODE, dev2).
-define(CLIENT1_NODE, aestratum_client1).

-define(POOL_BENEFICIARY1_ACCOUNT, <<"ak_2hJJGh3eJA2v9yLz73To7P8LvoHdz3arku3WXvgbCfwQyaL4nK">>).
-define(POOL_BENEFICIARY2_ACCOUNT, <<"ak_241xf1kQiexbSvWKfn5uve7ugGASjME93zDbr6SGQzYSCMTeQS">>).

-define(CLIENT1_ACCOUNT, <<"ak_5c8wGi7E7VjMPoSt7USS3k2FzSC6Nj5gutj2hJ5RATN36aQBA">>).

-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).

-define(STRATUM_PUBKEY_FILE, "sign_key.pub").
-define(STRATUM_PRIVKEY_FILE, "sign_key").

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence],
      [{group, single_client}]},

     {single_client, [sequence],
      [session_in_authorized_phase,
       mining_stratum_block,
       rewarding_participants]}].

suite() ->
    [].

init_per_suite(Cfg) ->
    case aect_test_utils:latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN -> {skip, stratum_payout_account_unsupported_in_roma};
        ?MINERVA_PROTOCOL_VSN -> {skip, stratum_payout_account_unsupported_in_minerva};
        PreIris when PreIris =:= ?FORTUNA_PROTOCOL_VSN;
                     PreIris =:= ?LIMA_PROTOCOL_VSN ->
            init_per_suite_(Cfg);
        FromIrisOn when FromIrisOn =:= ?IRIS_PROTOCOL_VSN;
                        FromIrisOn =:= ?CERES_PROTOCOL_VSN ->
            {skip, stratum_not_yet_working_in_iris_and_ceres}
    end.

init_per_suite_(Cfg) ->
    %% Setup aeternity nodes.
    Cfg1 = make_shortcut([{symlink_name, "latest.aestratum"}, {test_module, ?MODULE}] ++ Cfg),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    aecore_suite_utils:create_config(?STRATUM_SERVER_NODE, Cfg1, stratum_server_node_config(false),
                                     [{add_peers, true}]),
    aecore_suite_utils:create_config(?MINING_NODE, Cfg1, mining_node_config(maps:get(pubkey, new_keypair())),
                                     [{add_peers, true}]),
    aecore_suite_utils:make_multi(Cfg1, [?STRATUM_SERVER_NODE, ?MINING_NODE]),
    Cfg2 = write_stratum_keys("stratum_test_keys", [{stratum_keypair, new_keypair()} | Cfg1]),

    %% Setup stratum client node.
    Client1NodeCfg = client_node_config(?CLIENT1_ACCOUNT),
    Cfg3 = aestratum_client_suite_utils:setup_nodes([?CLIENT1_NODE], Client1NodeCfg, Cfg2),

    [{nodes, [aecore_suite_utils:node_tuple(?STRATUM_SERVER_NODE),
              aecore_suite_utils:node_tuple(?MINING_NODE),
              aestratum_client_suite_utils:node_tuple(?CLIENT1_NODE)]} | Cfg3].

end_per_suite(Cfg) ->
    del_stratum_keys(Cfg),
    ok.

init_per_group(single_client, Cfg) ->
    Nodes = ?config(nodes, Cfg),
    SNode = proplists:get_value(?STRATUM_SERVER_NODE, Nodes),
    MNode = proplists:get_value(?MINING_NODE, Nodes),
    C1Node = proplists:get_value(?CLIENT1_NODE, Nodes),
    #{pubkey := StratumPubKey} = StratumKeyPair = ?config(stratum_keypair, Cfg),

    aecore_suite_utils:start_node(?MINING_NODE, Cfg),
    aecore_suite_utils:connect(MNode),
    await_app_started(?MINING_NODE, aecore),
    ok = aecore_suite_utils:check_for_logs([?MINING_NODE], Cfg),

    %% Send some tokens to the Stratum account so the account exists when the
    %% Stratum server starts.
    Fee = 20000 * aec_test_utils:min_gas_price(),
    {ok, Tx} = add_spend_tx(?MINING_NODE, 1000000000000 * aec_test_utils:min_gas_price(), Fee, 1, 10, aecore_suite_utils:patron(), StratumPubKey),
    {ok, ContractTx, ContractPK} = deploy_payout_contract(StratumKeyPair, 1),
    {ok, _} = aecore_suite_utils:mine_blocks_until_txs_on_chain(MNode, [Tx, ContractTx], aec_governance:key_blocks_to_check_difficulty_count() - 2),

    %% The stratum app requires the account to exist otherwise it won't start.
    %% So the node is started with stratum disabled, chain is synced, node
    %% is stopped and started again with stratum enabled (this time the stratum
    %% account exists).
    aecore_suite_utils:start_node(?STRATUM_SERVER_NODE, Cfg),
    aecore_suite_utils:connect(SNode),
    await_app_started(?STRATUM_SERVER_NODE, aestratum),
    ok = aecore_suite_utils:check_for_logs([?STRATUM_SERVER_NODE], Cfg),
    TopBlock = rpc(?MINING_NODE, aec_chain, top_block, []),
    true = await_top_block(?STRATUM_SERVER_NODE, TopBlock),
    aecore_suite_utils:stop_node(?STRATUM_SERVER_NODE, Cfg),

    StratumServerNodeCfg = stratum_server_node_config(true),
    aecore_suite_utils:create_config(?STRATUM_SERVER_NODE, Cfg, StratumServerNodeCfg, [{add_peers, true}]),
    aecore_suite_utils:start_node(?STRATUM_SERVER_NODE, Cfg),
    aecore_suite_utils:connect(SNode),
    await_app_started(?STRATUM_SERVER_NODE, aestratum),

    rpc(?STRATUM_SERVER_NODE, aestratum_env, set,
        [#{contract_pubkey       => ContractPK,
           reward_keyblock_delay => 0,
           payout_keyblock_delay => 0}]),

    ok = aecore_suite_utils:check_for_logs([?STRATUM_SERVER_NODE], Cfg),
    true = await_top_block(?STRATUM_SERVER_NODE, TopBlock),

    aestratum_client_suite_utils:start_node(?CLIENT1_NODE, Cfg),
    aestratum_client_suite_utils:connect(C1Node),
    await_app_started(?CLIENT1_NODE, aestratum_client),
    Cfg;
init_per_group(_Group, Cfg) ->
    Cfg.

end_per_group(single_client, Cfg) ->
    aestratum_client_suite_utils:stop_node(?CLIENT1_NODE, Cfg),

    del_node_db(?STRATUM_SERVER_NODE),
    del_node_db(?MINING_NODE),
    aecore_suite_utils:stop_node(?STRATUM_SERVER_NODE, Cfg),
    aecore_suite_utils:stop_node(?MINING_NODE, Cfg),
    ok;
end_per_group(_Group, _Cfg) ->
    ok.

init_per_testcase(_Case, Cfg) ->
    Cfg.

end_per_testcase(_Case, _Cfg) ->
    ok.

session_in_authorized_phase(_Cfg) ->
    await_server_status(?STRATUM_SERVER_NODE, #{session => #{account => ?CLIENT1_ACCOUNT,
                                                phase => authorized}}),
    await_client_status(?CLIENT1_NODE, #{session => #{phase => authorized}}),
    ok.

mining_stratum_block(Cfg) ->
    Nodes = ?config(nodes, Cfg),
    MNode = proplists:get_value(?MINING_NODE, Nodes),

    ct:log("Connected peers on Mining node: ~p",
       [rpc:call(?MINING_NODE, aec_peers, connected_peers, [], 5000)]),
    ct:log("Connected peers on Stratum node: ~p",
       [rpc:call(?STRATUM_SERVER_NODE, aec_peers, connected_peers, [], 5000)]),

    #{session := #{jobs := Jobs}} =
        server_account_status(?STRATUM_SERVER_NODE, ?CLIENT1_ACCOUNT),

    %% Mining node mines a new key block which is synced on the Stratum server
    %% node and a new key block candidate is generated. A notify notification
    %% is then sent to the Stratum client and a new job is added to the job
    %% queue on the Stratum server node.
    aecore_suite_utils:mine_key_blocks(MNode, 1),
    TopBlock = rpc(?MINING_NODE, aec_chain, top_block, []),
    TopBlockHeight = aec_blocks:height(TopBlock),

    %% The first 17 key blocks, the block target stays the same, then it gets
    %% recalculated (aec_block_key_candidate module). The initial share target
    %% is set to the target of the first 17 key blocks, so when there is a
    %% solution it will be a solution that will be accepted by the chain.
    ?assert(TopBlockHeight < aec_governance:key_blocks_to_check_difficulty_count()),

    %% NOTE: await_top_block is not here as the client could have sent a
    %% solution at this point and the TopBlock would be one block behind
    %% the top block.
    true = await_block(?STRATUM_SERVER_NODE, TopBlock),
    true = await_new_job(?STRATUM_SERVER_NODE, ?CLIENT1_ACCOUNT, Jobs),

    #{session := #{jobs := Jobs1}} =
        server_account_status(?STRATUM_SERVER_NODE, ?CLIENT1_ACCOUNT),

    %% There is a new job in the job queue on the server after a job
    %% notification was sent to the client. The client now mines a solution,
    %% once it finds one, it's submitted back to the server. The server
    %% adds the share to a share list for the given job.
    [#{id := JobId} = NewJob] = Jobs1 -- Jobs,
    true = await_new_job_share(?STRATUM_SERVER_NODE, ?CLIENT1_ACCOUNT, NewJob),

    #{session := #{jobs := Jobs2}} =
        server_account_status(?STRATUM_SERVER_NODE, ?CLIENT1_ACCOUNT),
    #{shares := [#{validity := valid_block}]} = find_job(JobId, Jobs2),

    %% The solution submitted by the client is sent to the chain where it's
    %% included on top. Both the stratum server and the mining node have
    %% this block included in the chain.
    true = await_block_height(?STRATUM_SERVER_NODE, TopBlockHeight + 1),
    true = await_block_height(?MINING_NODE, TopBlockHeight + 1),

    STopBlock = rpc(?STRATUM_SERVER_NODE, aec_chain, top_block, []),
    MTopBlock = rpc(?MINING_NODE, aec_chain, top_block, []),

    ?assertEqual(STopBlock, MTopBlock),

    %% ensure TX left mempool
    timer:sleep(1000),

    %% payment for stratum operators was already sent by calling the contract
    [#aestratum_payment{absmap = #{?POOL_BENEFICIARY1_ACCOUNT := _,
                                   ?POOL_BENEFICIARY2_ACCOUNT := _},
                        tx_hash = <<_/binary>>}] =
        rpc(?STRATUM_SERVER_NODE, aestratum, sent_payments, []),

    %% payment for miners still sits in the queue
    [#aestratum_payment{relmap = #{?CLIENT1_ACCOUNT := _},
                        tx_hash = undefined}] =
            rpc(?STRATUM_SERVER_NODE, aestratum, pending_payments, []),

    ok.


rewarding_participants(Cfg) ->
    Nodes = ?config(nodes, Cfg),
    MNode = proplists:get_value(?MINING_NODE, Nodes),

    aecore_suite_utils:mine_key_blocks(MNode, 1),

    TopBlock1 = rpc(?MINING_NODE, aec_chain, top_block, []),
    true = await_block(?STRATUM_SERVER_NODE, TopBlock1),

    %% stratum operators transaction was paid, but not removed yet
    [#aestratum_payment{id = {Height, 0}, tx_hash = TxHash0, absmap = AbsMap0}] =
        rpc(?STRATUM_SERVER_NODE, aestratum, sent_payments, []),

    #{} = DbKeys = rpc(?STRATUM_SERVER_NODE, aestratum, db_keys, [Height]),

    %% ensure there are necessary objects in DB representing this mining epoch
    #{?ROUNDS_TAB   := [_ | _]} = DbKeys,
    #{?SHARES_TAB   := [_]} = DbKeys,
    #{?HASHES_TAB   := [_]} = DbKeys,
    #{?REWARDS_TAB  := [_]} = DbKeys,
    #{?PAYMENTS_TAB := [_, _]} = DbKeys,

    %% we can find the call tx executing the payment in the chain
    <<_/binary>> = rpc(?STRATUM_SERVER_NODE, aec_chain, find_tx_location, [TxHash0]),

    %% also accounts of the stratum operators exist in the chain, holding a balance
    [begin
         {account_pubkey, PK} = aeser_api_encoder:decode(Account),
         {value, _} = rpc(?STRATUM_SERVER_NODE, aec_chain, get_account, [PK])
     end || Account <- maps:keys(AbsMap0)],

    %% payment for client/miner didn't get through yet, account for miner isn't in chain yet
    {account_pubkey, ClientPK} = aeser_api_encoder:decode(?CLIENT1_ACCOUNT),
    none = rpc(?STRATUM_SERVER_NODE, aec_chain, get_account, [ClientPK]),

    %% to avoid extensive timer:sleep here, we kick in the check of the operators' payout tx,
    %% so it can be removed from payment queue so the miners payment can get in
    rpc(?STRATUM_SERVER_NODE, erlang, send, [aestratum, chain_payment_tx_check]),

    %% the same, to avoid timer:sleep we kick in the check which looks if there's no active
    %% and unconfirmed payment in the queue, and if there isn't it pushes next payment
    %% to the tx pool (and updates db). (moves pending payment to sent payment)
    rpc(?STRATUM_SERVER_NODE, erlang, send, [aestratum, payout_check]),

    %% give it some room so that the DB can be updated
    %% (pending/sent_payments calls don't go via gen_server)
    timer:sleep(1000),

    %% clients/miners payment was sent
    [#aestratum_payment{id = {Height, 1}, tx_hash = TxHash1, absmap = #{?CLIENT1_ACCOUNT := _}}] =
        rpc(?STRATUM_SERVER_NODE, aestratum, sent_payments, []),
    <<_/binary>> = TxHash1,

    retry(fun() ->
                  aecore_suite_utils:mine_key_blocks(MNode, 1),

                  TopBlock2 = rpc(?MINING_NODE, aec_chain, top_block, []),
                  true = await_block(?STRATUM_SERVER_NODE, TopBlock2),

                  %% give it time so that the TX appears in DB
                  timer:sleep(2000),

                  %% after blocks were mined, we can find the payout call tx in chain
                  case rpc(?STRATUM_SERVER_NODE, aec_chain, find_tx_location, [TxHash1]) of
                      <<_/binary>> ->
                          true;
                      X ->
                          {false, X}
                  end
          end,
          {?LINE, await_payout, TxHash1}),

    %% client/miner has account with balance in the chain
    {value, _} = rpc(?STRATUM_SERVER_NODE, aec_chain, get_account, [ClientPK]),

    %% lets remove the DB records for this payment
    rpc(?STRATUM_SERVER_NODE, erlang, send, [aestratum, chain_payment_tx_check]),

    %% give it some room so that the DB can be updated
    timer:sleep(1000),

    %% check the DB record for this epoch are not present in DB anymore
    maps:fold(
      fun (Tab, Ks, ok) ->
              [?assertEqual([], rpc(?STRATUM_SERVER_NODE,
                                    mnesia, dirty_read, [Tab, K])) || K <- Ks],
              ok
      end, ok, DbKeys),

    ok.

%%%% This test is off by default since it mostly fails, the miner can't find solution
%%%% in the limited time
client_target_change(Cfg) ->
    %% Make sure the top block height hasn't reached the height where the
    %% target is recalculated.
    %% NOTE: there must be at least 3 jobs in the job queue on the server
    %% in order to recalculate target for a given client!
    TopBlock = rpc(?MINING_NODE, aec_chain, top_block, []),
    TopBlock = rpc(?STRATUM_SERVER_NODE, aec_chain, top_block, []),
    TopBlockHeight = aec_blocks:height(TopBlock),
    %% Key block height 19 is the first with lower target.
    NewBlockHeight = aec_governance:key_blocks_to_check_difficulty_count() + 2,
    ?assert(TopBlockHeight < NewBlockHeight),

    #{session := #{target := Target}} = client_status(?CLIENT1_NODE),

    %% The previous test case caused that the client mined a valid block
    %% which was submitted to the chain. It also caused a new key block
    %% candidate was generated, but this was skipped (no job notification
    %% was sent to the client as skip_num_blocks == 1). The next key
    %% block candidate will be sent to the client.
    mine_to_height(TopBlockHeight, NewBlockHeight, Cfg),

    %% Stratum node has the latest top now. Let's sync the mining node.
    TopBlock1 = rpc(?STRATUM_SERVER_NODE, aec_chain, top_block, []),
%    true = await_top_block(?MINING_NODE, TopBlock1),

    %% The client should have a different target now.
    #{session := #{target := Target1}} = client_status(?CLIENT1_NODE),
    ?assertNotEqual(Target, Target1), %% TODO: maybe convert to int and check it's lower.

    p(?STRATUM_SERVER_NODE, TopBlock1),

    ok.

%% client_node_stop(_Cfg) ->
%%     %% The client's connection handler is stopped and so the client is
%%     %% disconnected from the server.
%%     %% TODO
%%     %rpc(?CLIENT1_NODE, aestratum_client, stop, []),
%%     %?assertEqual([], rpc(?STRATUM_SERVER_NODE, aestratum, status, [])),
%%     ok.

%%

p(Node, B) ->
    case {aec_blocks:type(B), aec_blocks:height(B)} of
        {key, N} when N > 0 ->
            ct:pal(">>> ~p: ~p", [N, aec_blocks:target(B)]),
            PH = aec_blocks:prev_hash(B),
            {ok, PB} = rpc(Node, aec_chain, get_block, [PH]),
            p(Node, PB);
        {key, 0} ->
            ct:pal(">>> ~p: ~p", [0, aec_blocks:target(B)]);
        {_, _} ->
            PH = aec_blocks:prev_hash(B),
            {ok, PB} = rpc(Node, aec_chain, get_block, [PH]),
            p(Node, PB)
    end.

stratum_server_node_config(StratumEnabled) ->
    %% The first 17 blocks the target is fixed. Stratum client's target
    %% (the first 17 blocks) doesn't change either.
    Target = aeminer_pow:scientific_to_integer(aec_block_genesis:target()),
    #{<<"mining">> =>
        #{<<"autostart">> => false},
          <<"chain">> =>
              #{<<"persist">> => true},
          <<"stratum">> =>
              #{<<"enabled">> => StratumEnabled,
                <<"connection">> =>
                    #{<<"port">> => 9999,
                      <<"max_connections">> => 1024,
                      <<"num_acceptors">> => 100,
                      <<"transport">> => <<"tcp">>},
                <<"session">> =>
                    #{<<"extra_nonce_bytes">> => 4,
                      <<"skip_num_blocks">> => 1,
                      <<"initial_share_target">> => Target,
                      <<"max_share_target">> => Target,
                      <<"desired_solve_time">> => 30,
                      <<"max_solve_time">> => 60,
                      <<"share_target_diff_threshold">> => 1.0,
                      <<"edge_bits">> => 15,
                      <<"max_jobs">> => 20,
                      <<"max_workers">> => 10,
                      <<"msg_timeout">> => 500},
                <<"reward">> =>
                    #{<<"reward_last_rounds">> => 2,
                      <<"beneficiaries">> =>
                          [<<?POOL_BENEFICIARY1_ACCOUNT/binary, ":3.3">>,
                           <<?POOL_BENEFICIARY2_ACCOUNT/binary, ":2.2">>],
                      <<"keys">> => #{<<"dir">> => <<"stratum_test_keys">>}}
               }
     }.

mining_node_config(BeneficiaryPubKey) ->
    #{<<"mining">> =>
        #{<<"autostart">> => false,
          <<"beneficiary">> => aeser_api_encoder:encode(account_pubkey, BeneficiaryPubKey)},
      <<"stratum">> =>
        #{<<"enabled">> => false}
     }.

client_node_config(Account) ->
    #{<<"connection">> =>
        #{<<"transport">> => <<"tcp">>,
          <<"host">> => <<"localhost">>,
          <<"port">> => 9999,
          <<"req_timeout">> => 15,
          <<"req_retries">> => 3},
      <<"user">> =>
        #{<<"account">> => Account,
          <<"worker">> => <<"worker1">>},
      <<"miners">> =>
        [#{<<"exec">> => <<"mean15-generic">>,
            <<"exec_group">> => <<"aecuckoo">>,
            <<"extra_args">> => <<"">>,
            <<"hex_enc_hdr">> => false,
            <<"repeats">> => 1000,
            <<"edge_bits">> => 15}]
     }.

make_shortcut(Cfg) ->
    Cfg1 = [{top_dir, aecore_suite_utils:top_dir(?config(data_dir, Cfg))} | Cfg],
    aecore_suite_utils:make_shortcut(Cfg1),
    Cfg1.

write_stratum_keys(Dir, Cfg) ->
    #{pubkey := PubKey, privkey := PrivKey} = ?config(stratum_keypair, Cfg),
    MNodeTopDir = aecore_suite_utils:node_shortcut(?STRATUM_SERVER_NODE, Cfg),
    StratumTopDir = latest_version(MNodeTopDir),
    ct:log("StratumTopDir = ~p", [StratumTopDir]),
    StratumKeysDir = filename:join([StratumTopDir, "priv", Dir]),
    filelib:ensure_dir(filename:join([StratumKeysDir, "foo"])),
    file:write_file(filename:join(StratumKeysDir, ?STRATUM_PRIVKEY_FILE), PrivKey),
    file:write_file(filename:join(StratumKeysDir, ?STRATUM_PUBKEY_FILE), PubKey),
    [{stratum_top_dir, StratumTopDir},
     {stratum_keys_dir, StratumKeysDir} | Cfg].

latest_version(Top) ->
    Vsns = filelib:wildcard(filename:join([Top, "lib", "aestratum-*"])),
    lists:last(lists:sort(fun vsn_cmp/2, Vsns)).

vsn_cmp(A, B) ->
    sortable_vsn(A) =< sortable_vsn(B).

sortable_vsn(Path) ->
    {match, VsnStr} = re:run(Path, "-([a-zA-Z0-9\\.]+)$", [{capture,[1],list}]),
    Parts = re:split(VsnStr, "\\.", [{return, list}]),
    [leading_num(V) || V <- Parts].

leading_num(V) ->
    case re:run(V, "([0-9]+)([^0-9].*)", [{capture,[1,2],list}]) of
        nomatch ->
            [list_to_integer(V)];
        {match, [N,A]} ->
            [list_to_integer(N), A]
    end.

del_stratum_keys(Cfg) ->
    StratumKeysDir = ?config(stratum_keys_dir, Cfg),
    file:delete(filename:join(StratumKeysDir, ?STRATUM_PRIVKEY_FILE)),
    file:delete(filename:join(StratumKeysDir, ?STRATUM_PUBKEY_FILE)),
    file:del_dir(StratumKeysDir).

del_node_db(Node) ->
    RpcFun = fun(M, F, A) -> rpc(Node, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg).

mine_to_height(TopBlockHeight, NewBlockHeight, Cfg) ->
    Nodes = ?config(nodes, Cfg),
    MNode = proplists:get_value(?MINING_NODE, Nodes),
    case TopBlockHeight < NewBlockHeight of
        true ->
            %% The mining node mines a new block, and when synced with the
            %% stratum server node, it creates a new candidate which is sent
            %% to be mined by the client. The client sends a valid solution
            %% back, a new block is added on top of the chain.
            aecore_suite_utils:mine_key_blocks(MNode, 1),
            true = await_block_height(?STRATUM_SERVER_NODE, TopBlockHeight + 2),
            mine_to_height(TopBlockHeight + 2, NewBlockHeight, Cfg);
        false ->
            ok
    end.

await_app_started(Node, App) ->
    retry(fun() -> await_app_started_(Node, App) end,
          {?LINE, await_app_started, Node, App}).

await_app_started_(Node, App) ->
    case rpc(Node, application, which_applications, []) of
        Apps when is_list(Apps) ->
            Apps1 = [A || {A, _D, _V} <- Apps],
            lists:member(App, Apps1);
        _Other ->
            false
    end.

await_server_status(Node, ExpStatus) ->
    retry(fun() -> await_server_status_(Node, ExpStatus) end,
          {?LINE, await_server_status, Node, ExpStatus}).

%% TODO: make it work for more clients
await_server_status_(Node, #{session := #{account := Account}} = ExpStatus) ->
    case server_account_status(Node, Account) of
        S when is_map(S) -> ExpStatus =:= expected_map(ExpStatus, S);
        _Other           -> false
    end.

await_client_status(Node, ExpStatus) ->
    retry(fun() -> await_client_status_(Node, ExpStatus) end,
          {?LINE, await_client_status, Node, ExpStatus}).

await_client_status_(Node, ExpStatus) ->
    case client_status(Node) of
        S when is_map(S) -> ExpStatus =:= expected_map(ExpStatus, S);
        _Other           -> false
    end.

await_top_block(Node, Block) ->
    retry(fun() -> await_top_block_(Node, Block) end,
          {?LINE, await_top_block, Node, Block}).

await_top_block_(Node, Block) ->
    TopBlock = rpc(Node, aec_chain, top_block, []),
    case Block =:= TopBlock of
        true  -> true;
        false -> {false, TopBlock}
    end.

await_block(Node, Block) ->
    retry(fun() -> await_block_(Node, Block) end,
          {?LINE, await_block, Node, Block}).

await_block_(Node, Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    case rpc(Node, aec_chain, get_block, [Hash]) of
        {ok, Block} -> true;
        _Other      -> false
    end.

await_block_height(Node, Height) ->
    retry(fun() -> await_block_height_(Node, Height) end,
          {?LINE, await_block_height, Node, Height}).

await_block_height_(Node, Height) ->
    TopBlock = rpc(Node, aec_chain, top_block, []),
    aec_blocks:height(TopBlock) =:= Height.

await_new_job(Node, Account, OldJobs) ->
    retry(fun() -> await_new_job_(Node, Account, OldJobs) end,
          {?LINE, await_new_job, Node, Account, OldJobs}).

await_new_job_(Node, Account, OldJobs) ->
    case rpc(Node, aestratum, status, [Account]) of
        #{session := #{jobs := Jobs}} when length(Jobs) =:= (length(OldJobs) + 1) ->
            true;
        _Other ->
            false
    end.

await_new_job_share(Node, Account, Job) ->
    retry(fun() -> await_new_job_share_(Node, Account, Job) end,
          1000,
          {?LINE, await_new_job_share, Node, Account, Job}).

await_new_job_share_(Node, Account, #{id := JobId}) ->
    #{session := #{jobs := Jobs}} = rpc(Node, aestratum, status, [Account]),
    [#{shares := Shares}] = [J || J <- Jobs, maps:get(id, J) =:= JobId],
    length(Shares) > 0.

find_job(Id, Jobs) ->
    [Job] = [J || J <- Jobs, maps:get(id, J) =:= Id],
    Job.

server_account_status(Node, Account) ->
    rpc(Node, aestratum, status, [Account]).

client_status(Node) ->
    rpc(Node, aestratum_client, status, []).

add_spend_tx(Node, Amount, Fee, Nonce, TTL, Sender, Recipient) ->
    SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{sender_id    => SenderId,
               recipient_id => RecipientId,
               amount       => Amount,
               nonce        => Nonce,
               ttl          => TTL,
               payload      => <<>>,
               fee          => Fee},
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
    Res = rpc(Node, aec_tx_pool, push, [STx]),
    {Res, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_keypair() ->
    #{public := PK, secret := SK} = enacl:sign_keypair(),
    #{pubkey => PK, privkey => SK}.

rpc(Node, Mod, Fun, Args) when Node =:= dev1; Node =:= dev2; Node =:= dev3 ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000);
rpc(Node, Mod, Fun, Args) ->
    rpc:call(aestratum_client_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

%% ExpMap is a map that we expect. We take keys from the ExpMap and values from
%% Map and then we can compare them. This function works with nested maps.
expected_map(ExpMap, Map) ->
    maps:fold(
      fun(K, V, Acc) when is_map(V) ->
              Acc#{K => expected_map(V, maps:get(K, Map, #{}))};
         (K, _V, Acc) ->
              Acc#{K => maps:get(K, Map, #{})}
      end, #{}, ExpMap).

retry(Test, Info) ->
    retry(Test, 10, Info).

retry(Test, Retries, Info) ->
    retry_(Test, #{prev => undefined,
                   retries => Retries,
                   tries => Retries,
                   info => Info}).

retry_(Test, #{tries := Tries} = S) when Tries > 0 ->
    case Test() of
        true ->
            true;
        false ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries - 1});
        {false, V} ->
            timer:sleep(1000),
            retry_(Test, S#{tries => Tries - 1, prev => V})
    end;
retry_(_, S) ->
    ct:log("exhausted retries (~p)", [S]),
    ct:fail({retry_exhausted, S}).


deploy_payout_contract(#{pubkey := PubKey, privkey := PrivKey}, Nonce) ->
    {ok, BinSrc} = aect_test_utils:read_contract(payout_contract),
    {ok, CCode}  = aect_test_utils:compile_contract(payout_contract),
    {ok, CallData} = aect_test_utils:encode_call_data(BinSrc, "init", []),

    {ok, WrappedTx}   =
        aect_create_tx:new(#{owner_id    => aeser_id:create(account, PubKey),
                             nonce       => Nonce,
                             code        => CCode,
                             vm_version  => aect_test_utils:latest_sophia_vm_version(),
                             abi_version => aect_test_utils:latest_sophia_abi_version(),
                             deposit     => 0,
                             amount      => 0,
                             gas         => 100000,
                             gas_price   => ?DEFAULT_GAS_PRICE,
                             fee         => 1400000 * ?DEFAULT_GAS_PRICE,
                             call_data   => CallData}),
    {_, CreateTx} = aetx:specialize_type(WrappedTx),
    CtPubkey = aect_create_tx:contract_pubkey(CreateTx),
    BinaryTx = aec_governance:add_network_id(aetx:serialize_to_binary(WrappedTx)),
    SignedTx = aetx_sign:new(WrappedTx, [enacl:sign_detached(BinaryTx, PrivKey)]),
    TxHash   = aeser_api_encoder:encode(tx_hash, aetx_sign:hash(SignedTx)),
    ok       = rpc(?MINING_NODE, aec_tx_pool, push, [SignedTx]),
    {ok, TxHash, CtPubkey}.

