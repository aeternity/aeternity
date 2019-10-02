-module(aest_db_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([ all/0
        , groups/0
        , init_per_suite/1
        , init_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        , end_per_group/2
        , end_per_suite/1]).

% Test cases
-export([ mining/1
        , force_progress_mempool/1
        , sc_leave_upgrade_reestablish_same_node_upgrade/1
        , sc_leave_upgrade_reestablish_different_nodes_full_upgrade/1
        , sc_leave_upgrade_reestablish_different_nodes_partial_upgrade/1
        , sc_leave_upgrade_reestablish_different_nodes_partial_to_full_upgrade/1
        ]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(STARTUP_TIMEOUT, 20000).
-define(MINING_TIMEOUT,   3000).
-define(GRACEFUL_STOP_TIMEOUT, 60000).

%=== RECORDS ===================================================================

-record(db_reuse_test_spec, {create,     % Node spec.
                             populate =  % DB insertion.
                                 fun populate_db/2,
                             reuse,      % Node spec.
                             assert =    % DB assertion.
                                 fun assert_db_reused/3
                            }).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [{group, minerva_compatibility},
          {group, lima_compatibility},
          {group, local_compatibility}
         ].

groups() ->
    [{minerva_compatibility, [sequence], [
        force_progress_mempool
    ]},
    {lima_compatibility, [sequence], [
        {group, leave_upgrade_reestablish}
    ]},
    {local_compatibility, [sequence], [
        mining
    ]},
    {leave_upgrade_reestablish, [sequence],
        [ sc_leave_upgrade_reestablish_same_node_upgrade
        , sc_leave_upgrade_reestablish_different_nodes_full_upgrade
        , sc_leave_upgrade_reestablish_different_nodes_partial_upgrade
        , sc_leave_upgrade_reestablish_different_nodes_partial_to_full_upgrade
        ]
    }].

init_per_suite(Config) ->
    Config.

init_per_group(minerva_compatibility, Config) ->
    [{tx_mempool_vsn, "v2.3.0"} | Config];
init_per_group(lima_compatibility, Config) ->
    %%[{state_channels_vsn, "v5.1.0"} | Config];
    {skip, no_backwards_compatibility_until_5_1};
init_per_group(local_compatibility, Config) ->
    [{block_mining_vsn, "local"} | Config];
init_per_group(_TG, Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_group(_TC, _Config) ->
    ok.

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

mining(Cfg) ->
    OldVersion = proplists:get_value(block_mining_vsn, Cfg),
    Test = #db_reuse_test_spec{
        create = fun(NodeName, DbHostPath) -> node_spec(NodeName, OldVersion, DbHostPath, true) end,
        reuse = fun(NodeName, DBHostPath) -> node_spec(NodeName, "local", DBHostPath, false) end
    },
    node_can_reuse_db_of_other_node_(Test, Cfg).

force_progress_mempool(Cfg) ->
    tx_mempool_compatibility(fun populate_db_with_channels_force_progress_tx/2, Cfg).

sc_leave_upgrade_reestablish_same_node_upgrade(Cfg) ->
    sc_leave_upgrade_reestablish({{node1, old}, {node1, old}}, {{node2, new}, {node2, new}}, Cfg).

sc_leave_upgrade_reestablish_different_nodes_full_upgrade(Cfg) ->
    sc_leave_upgrade_reestablish({{alice1, old}, {bob1, old}}, {{alice2, new}, {bob2, new}}, Cfg).

sc_leave_upgrade_reestablish_different_nodes_partial_upgrade(Cfg) ->
    sc_leave_upgrade_reestablish({{alice1, old}, {bob1, old}}, {{alice2, old}, {bob2, new}}, Cfg).

sc_leave_upgrade_reestablish_different_nodes_partial_to_full_upgrade(Cfg) ->
    sc_leave_upgrade_reestablish({{alice1, new}, {bob1, old}}, {{alice2, new}, {bob2, new}}, Cfg).

%=== INTERNAL FUNCTIONS ========================================================

tx_mempool_compatibility(PopulateF, Cfg) ->
    OldVersion = proplists:get_value(tx_mempool_vsn, Cfg),
    Test = #db_reuse_test_spec{
        create = fun(NodeName, DbHostPath) -> node_spec(NodeName, OldVersion, DbHostPath, false) end,
        populate = PopulateF,
        reuse = fun(NodeName, DBHostPath) -> node_spec(NodeName, "local", DBHostPath, false) end,
        assert = fun assert_db_with_tx_reused/3
    },
    node_can_reuse_db_of_other_node_(Test, Cfg).

node_can_reuse_db_of_other_node_(T = #db_reuse_test_spec{}, Cfg)
  when is_function(T#db_reuse_test_spec.create, 2),
       is_function(T#db_reuse_test_spec.populate, 2),
       is_function(T#db_reuse_test_spec.reuse, 2),
       is_function(T#db_reuse_test_spec.assert, 3) ->
    DbHostPath = node_db_host_path(node1, Cfg),
    N1 = (T#db_reuse_test_spec.create)(node1, DbHostPath),
    aest_nodes:setup_nodes([N1], Cfg),
    start_and_wait_node(node1, ?STARTUP_TIMEOUT, Cfg),
    DbFingerprint = (T#db_reuse_test_spec.populate)(node1, Cfg),
    aest_nodes:stop_node(node1, ?GRACEFUL_STOP_TIMEOUT, Cfg),

    N2 = (T#db_reuse_test_spec.reuse)(node2, DbHostPath),
    aest_nodes:setup_nodes([N2], Cfg),
    start_and_wait_node(node2, ?STARTUP_TIMEOUT, Cfg),
    ok = (T#db_reuse_test_spec.assert)(node2, DbFingerprint, Cfg),
    ok.

sc_leave_upgrade_reestablish( {{BeforeNodeNameI, BeforeNodeTypeI}, {BeforeNodeNameR, BeforeNodeTypeR}}
                            , {{AfterNodeNameI, AfterNodeTypeI}, {AfterNodeNameR, AfterNodeTypeR}}
                            , Cfg) ->
    HostPathI = node_db_host_path(BeforeNodeNameI, Cfg),
    HostPathR = node_db_host_path(BeforeNodeNameR, Cfg),

    OldVersion = proplists:get_value(state_channels_vsn, Cfg),
    OldNodeF =
        fun(NodeName, DbHostPath) ->
            node_spec(NodeName, OldVersion, DbHostPath, is_mining_node(NodeName))
        end,
    NewNodeF =
        fun(NodeName, DbHostPath) ->
            node_spec(NodeName, "local", DbHostPath, is_mining_node(NodeName))
        end,
    SpecF = fun(old) -> OldNodeF; (new) -> NewNodeF end,

    %% Create node specs
    BeforeSpecI = (SpecF(BeforeNodeTypeI))(BeforeNodeNameI, HostPathI),
    BeforeSpecR = (SpecF(BeforeNodeTypeR))(BeforeNodeNameR, HostPathR),
    AfterSpecI = (SpecF(AfterNodeTypeI))(AfterNodeNameI, HostPathI),
    AfterSpecR = (SpecF(AfterNodeTypeR))(AfterNodeNameR, HostPathR),
    BeforeNames = lists:usort([BeforeNodeNameI, BeforeNodeNameR]),
    AfterNames = lists:usort([AfterNodeNameI, AfterNodeNameR]),

    %% Setup "Before" nodes
    case BeforeNodeNameI of
        BeforeNodeNameR ->
            HostPathI = HostPathR,
            BeforeNodeTypeI = BeforeNodeTypeR,
            AfterNodeNameI = AfterNodeNameR,
            AfterNodeTypeI = AfterNodeTypeR,
            true = AfterNodeNameI /= BeforeNodeNameI,
            aest_nodes:setup_nodes([BeforeSpecI], Cfg);
        _ ->
            true = AfterNodeNameI /= AfterNodeNameR,
            aest_nodes:setup_nodes([BeforeSpecI#{peers => [BeforeNodeNameR]}, BeforeSpecR#{peers => [BeforeNodeNameI]}], Cfg)
    end,

    %% Create channel and leave
    start_and_wait_nodes(BeforeNames, ?STARTUP_TIMEOUT, Cfg),
    ReestablishOpts = aest_channels_SUITE:create_state_channel_perform_operations_leave({BeforeNodeNameI, BeforeNodeNameR}, Cfg),
    [aest_nodes:stop_node(Node, ?GRACEFUL_STOP_TIMEOUT, Cfg) || Node <- BeforeNames],

    %% Setup "After" nodes
    case AfterNodeNameI of
        AfterNodeNameR ->
            aest_nodes:setup_nodes([AfterSpecI], Cfg);
        _ ->
            aest_nodes:setup_nodes([AfterSpecI#{peers => [AfterNodeNameR]}, AfterSpecR#{peers => [AfterNodeNameI]}], Cfg)
    end,

    %% Try reestablishing
    start_and_wait_nodes(AfterNames, ?STARTUP_TIMEOUT, Cfg),
    timer:sleep(1000),
    ok = aest_channels_SUITE:reestablish_state_channel_perform_operations_close({AfterNodeNameI, AfterNodeNameR}, ReestablishOpts, Cfg),
    ok.

populate_db(NodeName, Cfg) ->
    TargetHeight = 3,
    aest_nodes:wait_for_value({height, TargetHeight}, [NodeName], TargetHeight * ?MINING_TIMEOUT, Cfg),
    #{hash := BlockHash} = aest_nodes:get_block(NodeName, TargetHeight),
    _DbFingerprint = {TargetHeight, BlockHash}.

assert_db_reused(NodeName, {TargetHeight, BlockHash} = _DbFingerprint, Cfg) ->
    aest_nodes:wait_for_value({height, TargetHeight}, [NodeName], ?STARTUP_TIMEOUT, Cfg),
    ?assertMatch({ok, 200, _}, get_block_by_hash(NodeName, BlockHash)),
    ok.

get_block_by_hash(NodeName, Hash) ->
    aest_nodes:request(NodeName, 'GetKeyBlockByHash', #{hash => Hash}).

start_and_wait_node(NodeName, Timeout, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    aest_nodes:wait_for_value({height, 0}, [NodeName], Timeout, Cfg),
    %% Hardcode expectation that node picks user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(NodeName),
    ok.

start_and_wait_nodes(NodeNames, Timeout, Cfg) ->
    [aest_nodes:start_node(NodeName, Cfg) || NodeName <- NodeNames],
    %% Mine some blocks so the peers may sync with each other
    aest_nodes:wait_for_value({height, 4}, NodeNames, Timeout, Cfg),
    %% Hardcode expectation that node picks user config
    [#{network_id := <<"ae_system_test">>} = aest_nodes:get_status(NodeName) || NodeName <- NodeNames],
    ok.

node_db_host_path(NodeName, Config) ->
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    filename:join(PrivDir, format("~s_db", [NodeName])).

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

node_spec(Name, Version, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:" ++ Version},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => Mining},
                                genesis_accounts => genesis_accounts()}).

genesis_accounts() ->
    %% have all nodes share the same accounts_test.json
    PatronPubkey = maps:get(pubkey, patron()),
    PatronAddress = aeser_api_encoder:encode(account_pubkey, PatronPubkey),
    [{PatronAddress, 123400000000000000000000000000}].

patron() ->
    %% This is the same account as in aest_channels_SUITE.erl
    #{ pubkey => <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,85,78,88,181,26,207,191,211,40,225,138,154>>
     , privkey => <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,85,78,88,181,26,207,191,211,40,225,138,154>>
     }.

populate_db_with_channels_force_progress_tx(NodeName, Cfg) ->
    %% This function actually uses the latest code to populate a possibly old database
    %% The state channel fsm sets its environment to be able to encode data using old
    %% protocols, but in this case, we're not producing the tx inside the fsm, so we
    %% must simulate the relevant part of the environment.
    #{tx_hash := TxHash} =
        aest_nodes:post_force_progress_state_channel_tx(
          NodeName,
          patron(),
          aeser_id:create(channel, <<42:32/unit:8>>),
          #{nonce => 1}),
    _DbFingerprint = TxHash.

assert_db_with_tx_reused(NodeName, TxHash = _DbFingerprint, _Cfg) ->
    aest_nodes:wait_for_value({txs_on_node, [TxHash]}, [NodeName], ?STARTUP_TIMEOUT, []), %% Uses GetTransactionByHash
    ok.

is_mining_node(Bob) when Bob =:= bob1;
                         Bob =:= bob2 ->
    false;
is_mining_node(_) ->
    true.
