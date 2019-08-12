-module(aest_db_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
         node_can_reuse_db_of_other_node/1,
         roma_node_can_reuse_db_of_other_roma_node/1,
         minerva_node_with_epoch_db_can_reuse_db_of_roma_node/1,
         node_can_reuse_db_of_roma_node/1,
         node_can_reuse_db_of_minerva_node_with_epoch_db/1,
         minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node/1,
         minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node_with_force_progress_tx/1,
         node_can_reuse_db_of_minerva_node_with_channels_update_as_tuple_with_force_progress_tx/1,
         %% TODO: Shouldn't the names be longer ;)
         node_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish/1,
         node_and_fortuna_major_release_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish/1,
         node_can_reuse_state_channel_db_of_node_and_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish/1
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
                             pre_reuse = % DB transformation.
                                 fun(_,_,_) -> ok end,
                             reuse,      % Node spec.
                             assert =    % DB assertion.
                                 fun assert_db_reused/3
                            }).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
          node_can_reuse_db_of_other_node,
          roma_node_can_reuse_db_of_other_roma_node,
          minerva_node_with_epoch_db_can_reuse_db_of_roma_node,
          node_can_reuse_db_of_roma_node,
          node_can_reuse_db_of_minerva_node_with_epoch_db,
          minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node,
          minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node_with_force_progress_tx,
          node_can_reuse_db_of_minerva_node_with_channels_update_as_tuple_with_force_progress_tx
          %% TODO: The state channel state does not currently survive restarts :( - when this is fixed uncomment and adjust the version in tests
          %%node_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish,
          %%node_and_fortuna_major_release_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish,
          %%node_can_reuse_state_channel_db_of_node_and_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish
         ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

node_can_reuse_db_of_other_node(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun node_mining_spec/2,
              reuse = fun node_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

roma_node_can_reuse_db_of_other_roma_node(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun roma_node_mining_spec/2,
              reuse = fun roma_node_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

minerva_node_with_epoch_db_can_reuse_db_of_roma_node(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun roma_node_mining_spec/2,
              reuse = fun minerva_with_epoch_name_in_db_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

node_can_reuse_db_of_roma_node(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun roma_node_mining_spec/2,
              pre_reuse = fun run_rename_db_script/3,
              reuse = fun node_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

node_can_reuse_db_of_minerva_node_with_epoch_db(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun minerva_with_epoch_name_in_db_mining_spec/2,
              pre_reuse = fun run_rename_db_script/3,
              reuse = fun node_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun minerva_node_with_channels_update_as_tuple_mining_spec/2,
              reuse = fun minerva_node_with_channels_update_as_tuple_spec/2},
    node_can_reuse_db_of_other_node_(Test, Cfg).

minerva_node_with_channels_update_as_tuple_can_reuse_db_of_analogous_node_with_force_progress_tx(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun minerva_node_with_channels_update_as_tuple_spec/2,
              populate = fun populate_db_with_channels_force_progress_tx/2,
              reuse = fun minerva_node_with_channels_update_as_tuple_spec/2,
              assert = fun assert_db_with_tx_reused/3},
    node_can_reuse_db_of_other_node_(Test, Cfg).

node_can_reuse_db_of_minerva_node_with_channels_update_as_tuple_with_force_progress_tx(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun minerva_node_with_channels_update_as_tuple_spec/2,
              populate = fun populate_db_with_channels_force_progress_tx/2,
              reuse = fun node_spec/2,
              assert = fun assert_db_with_tx_reused/3},
    node_can_reuse_db_of_other_node_(Test, Cfg).

node_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun fortuna_major_release_mining_spec/2,
              populate = fun aest_channels_SUITE:create_state_channel_perform_operations_leave/2,
              reuse = fun node_mining_spec/2,
              assert = fun aest_channels_SUITE:reestablish_state_channel_perform_operations/3},
    node_can_reuse_state_channel_db_of_other_node_(Test, Cfg).

node_and_fortuna_major_release_can_reuse_state_channel_db_of_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun fortuna_major_release_mining_spec/2,
              populate = fun aest_channels_SUITE:create_state_channel_perform_operations_leave/2,
              reuse = fun alice_latest_bob_fortuna_mining_spec/2,
              assert = fun aest_channels_SUITE:reestablish_state_channel_perform_operations/3},
    node_can_reuse_state_channel_db_of_other_node_(Test, Cfg).

node_can_reuse_state_channel_db_of_node_and_fortuna_major_release_with_offchain_and_onchain_updates_using_leave_reestablish(Cfg) ->
    Test = #db_reuse_test_spec{
              create = fun alice_latest_bob_fortuna_mining_spec/2,
              populate = fun aest_channels_SUITE:create_state_channel_perform_operations_leave/2,
              reuse = fun node_mining_spec/2,
              assert = fun aest_channels_SUITE:reestablish_state_channel_perform_operations/3},
    node_can_reuse_state_channel_db_of_other_node_(Test, Cfg).

%=== INTERNAL FUNCTIONS ========================================================

node_can_reuse_db_of_other_node_(T = #db_reuse_test_spec{}, Cfg)
  when is_function(T#db_reuse_test_spec.create, 2),
       is_function(T#db_reuse_test_spec.populate, 2),
       is_function(T#db_reuse_test_spec.pre_reuse, 3),
       is_function(T#db_reuse_test_spec.reuse, 2),
       is_function(T#db_reuse_test_spec.assert, 3) ->
    DbHostPath = node_db_host_path(node1, Cfg),
    N1 = (T#db_reuse_test_spec.create)(node1, DbHostPath),
    aest_nodes:setup_nodes([N1], Cfg),
    start_and_wait_node(node1, ?STARTUP_TIMEOUT, Cfg),
    DbFingerprint = (T#db_reuse_test_spec.populate)(node1, Cfg),
    aest_nodes:stop_node(node1, ?GRACEFUL_STOP_TIMEOUT, Cfg),

    ok = (T#db_reuse_test_spec.pre_reuse)(node3, DbHostPath, Cfg),

    N2 = (T#db_reuse_test_spec.reuse)(node2, DbHostPath),
    aest_nodes:setup_nodes([N2], Cfg),
    start_and_wait_node(node2, ?STARTUP_TIMEOUT, Cfg),
    ok = (T#db_reuse_test_spec.assert)(node2, DbFingerprint, Cfg),
    ok.

node_can_reuse_state_channel_db_of_other_node_(T = #db_reuse_test_spec{}, Cfg)
  when is_function(T#db_reuse_test_spec.create, 2),
       is_function(T#db_reuse_test_spec.populate, 2),
       is_function(T#db_reuse_test_spec.reuse, 2),
       is_function(T#db_reuse_test_spec.assert, 3) ->
    AliceDbHostPath = node_db_host_path(alice1, Cfg),
    BobDbHostPath = node_db_host_path(bob1, Cfg),
    A1 = (T#db_reuse_test_spec.create)(alice1, AliceDbHostPath),
    B1 = (T#db_reuse_test_spec.create)(bob1, BobDbHostPath),
    aest_nodes:setup_nodes([A1#{peers => [bob1]}, B1#{peers => [alice1]}], Cfg),
    start_and_wait_nodes([alice1, bob1], ?STARTUP_TIMEOUT, Cfg),
    DbFingerprint = (T#db_reuse_test_spec.populate)({alice1, bob1}, Cfg),
    aest_nodes:stop_node(alice1, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    aest_nodes:stop_node(bob1, ?GRACEFUL_STOP_TIMEOUT, Cfg),

    %% pre_reuse not used here

    A2 = (T#db_reuse_test_spec.reuse)(alice2, AliceDbHostPath),
    B2 = (T#db_reuse_test_spec.reuse)(bob2, BobDbHostPath),
    aest_nodes:setup_nodes([A2#{peers => [bob2]}, B2#{peers => [alice2]}], Cfg),
    start_and_wait_nodes([alice2, bob2], ?STARTUP_TIMEOUT, Cfg),
    ok = (T#db_reuse_test_spec.assert)({alice2, bob2}, DbFingerprint, Cfg),
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

run_rename_db_script(UnusedNodeName, DbHostPath, Cfg) when is_atom(UnusedNodeName) ->
    {ok, DbSchema} = file:read_file(filename:join(DbHostPath, "schema.DAT")),
    {error, _} = file:read_file(filename:join(DbHostPath, "schema.DAT.backup")),
    N3 = node_spec_custom_entrypoint(UnusedNodeName, DbHostPath),
    aest_nodes:setup_nodes([N3], Cfg),
    aest_nodes:start_node(UnusedNodeName, Cfg),

    {0, _} = aest_nodes:run_cmd_in_node_dir(UnusedNodeName, ["bin/aeternity", "rename_db", "data"], #{timeout => 5000}, Cfg),

    aest_nodes:stop_container(UnusedNodeName, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    {ok, DbSchemaRenamed} = file:read_file(filename:join(DbHostPath, "schema.DAT")),
    {ok, DbSchemaBackup} = file:read_file(filename:join(DbHostPath, "schema.DAT.backup")),
    ?assertNotEqual(DbSchema, DbSchemaRenamed),
    ?assertEqual(DbSchema, DbSchemaBackup),
    ok.

node_mining_spec(Name, DbHostPath) ->
    node_spec(Name, DbHostPath, true).
node_spec(Name, DbHostPath) ->
    node_spec(Name, DbHostPath, false).
node_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:local"},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => Mining},
                                genesis_accounts => genesis_accounts()}).

node_spec_custom_entrypoint(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:local"},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => false},
                                entrypoint => [<<"sleep">>],
                                custom_command => [<<"98127308917209371890273">>]}).

roma_node_mining_spec(Name, DbHostPath) ->
    roma_node_spec(Name, DbHostPath, true).
roma_node_spec(Name, DbHostPath) ->
    roma_node_spec(Name, DbHostPath, false).
%% Last Roma release.
roma_node_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v1.4.0"},
                                db_path => {DbHostPath, DbGuestPath},
                                config_guest_path => "/home/aeternity/.epoch/epoch/epoch.yaml",
                                mining => #{autostart => Mining},
                                genesis_accounts => genesis_accounts()}).

minerva_with_epoch_name_in_db_mining_spec(Name, DbHostPath) ->
    minerva_with_epoch_name_in_db_spec(Name, DbHostPath, true).
minerva_with_epoch_name_in_db_spec(Name, DbHostPath) ->
    minerva_with_epoch_name_in_db_spec(Name, DbHostPath, false).
%% Minerva release using old epoch@localhost node name in the db.
minerva_with_epoch_name_in_db_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v2.1.0"},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => Mining},
                                genesis_accounts => genesis_accounts()}).

minerva_node_with_channels_update_as_tuple_mining_spec(Name, DbHostPath) ->
    minerva_node_with_channels_update_as_tuple_spec(Name, DbHostPath, true).
minerva_node_with_channels_update_as_tuple_spec(Name, DbHostPath) ->
    minerva_node_with_channels_update_as_tuple_spec(Name, DbHostPath, false).
%% https://github.com/aeternity/aeternity/blob/v2.3.0/apps/aechannel/src/aesc_offchain_update.erl#L15-L17
minerva_node_with_channels_update_as_tuple_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v2.3.0"},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => Mining},
                                genesis_accounts => genesis_accounts()}).

fortuna_major_release_mining_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v4.0.0"},
                                db_path => {DbHostPath, DbGuestPath},
                                mining => #{autostart => true},
                                genesis_accounts => genesis_accounts()}).

alice_latest_bob_fortuna_mining_spec(Name, DbHostPath) when Name =:= alice1; Name =:= alice2 ->
    node_mining_spec(Name, DbHostPath);
alice_latest_bob_fortuna_mining_spec(Name, DbHostPath) when Name =:= bob1; Name =:= bob2 ->
    fortuna_major_release_mining_spec(Name, DbHostPath).

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

populate_db_with_channels_force_progress_tx(NodeName, _Cfg) ->
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
