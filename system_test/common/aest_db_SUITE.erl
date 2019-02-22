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
    minerva_node_with_epoch_db_can_reuse_db_of_roma_node/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(STARTUP_TIMEOUT, 20000).
-define(MINING_TIMEOUT,   3000).
-define(GRACEFUL_STOP_TIMEOUT, 60000).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    node_can_reuse_db_of_other_node,
    roma_node_can_reuse_db_of_other_roma_node,
    minerva_node_with_epoch_db_can_reuse_db_of_roma_node
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
    node_can_reuse_db_of_other_node_(fun node_spec/2, Cfg).

roma_node_can_reuse_db_of_other_roma_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun roma_node_spec/2, Cfg).

minerva_node_with_epoch_db_can_reuse_db_of_roma_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun roma_node_spec/2, fun minerva_with_epoch_name_in_db_spec/2, Cfg).

%=== INTERNAL FUNCTIONS ========================================================

node_can_reuse_db_of_other_node_(NodeSpecFun, Cfg) ->
    node_can_reuse_db_of_other_node_(NodeSpecFun, NodeSpecFun, Cfg).

node_can_reuse_db_of_other_node_(CreateDbNodeSpecFun, ReuseDbNodeSpecFun, Cfg) ->
    DbHostPath = node_db_host_path(node1, Cfg),
    N1 = CreateDbNodeSpecFun(node1, DbHostPath),
    N2 = ReuseDbNodeSpecFun(node2, DbHostPath),
    aest_nodes:setup_nodes([N1, N2], Cfg),
    start_and_wait_node(node1, ?STARTUP_TIMEOUT, Cfg),
    TargetHeight = 3,
    aest_nodes:wait_for_value({height, TargetHeight}, [node1], TargetHeight * ?MINING_TIMEOUT, Cfg),
    #{hash := BlockHash} = aest_nodes:get_block(node1, TargetHeight),
    aest_nodes:stop_node(node1, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    start_and_wait_node(node2, ?STARTUP_TIMEOUT, Cfg),
    aest_nodes:wait_for_value({height, TargetHeight}, [node2], ?STARTUP_TIMEOUT, Cfg),
    ?assertMatch({ok, 200, _}, get_block_by_hash(node2, BlockHash)),
    ok.

get_block_by_hash(NodeName, Hash) ->
    aest_nodes:request(NodeName, 'GetKeyBlockByHash', #{hash => Hash}).

start_and_wait_node(NodeName, Timeout, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    aest_nodes:wait_for_value({height, 0}, [NodeName], Timeout, Cfg),
    %% Hardcode expectation that node picks user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(NodeName),
    ok.

node_db_host_path(NodeName, Config) ->
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    filename:join(PrivDir, format("~s_db", [NodeName])).

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

node_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:local"},
                                db_path => {DbHostPath, DbGuestPath},
                                genesis_accounts => genesis_accounts()}).

%% Last Roma release.
roma_node_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v1.4.0"},
                                db_path => {DbHostPath, DbGuestPath},
                                config_guest_path => "/home/aeternity/.epoch/epoch/epoch.yaml",
                                genesis_accounts => genesis_accounts()}).

%% Minerva release still using epoch@localhost node name in the db.
minerva_with_epoch_name_in_db_spec(Name, DbHostPath) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v2.0.0-rc.1"}, db_path => {DbHostPath, DbGuestPath}}).

genesis_accounts() ->
    %% have all nodes share the same accounts_test.json
    PatronPubkey = <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
    PatronAddress = aehttp_api_encoder:encode(account_pubkey, PatronPubkey),
    [{PatronAddress, 123400000000000000000000000000}].
