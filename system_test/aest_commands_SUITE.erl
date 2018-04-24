-module(aest_commands_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([epoch_commands/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT, 2000).
-define(STARTUP_TIMEOUT, 5000).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/epoch:local"}
}).

-define(EPOCH_VERSION_OUTPUT_RX, "Installed versions:[\r\n]*\\*[ \t]*[0-9.]*[ \t]*permanent[\r\n]*").
-define(EPOCH_CHECK_OUTPUT_RX, "OK[\r\n]*").
-define(EPOCH_PID_OUTPUT_RX, "1[\r\n]*").
-define(EPOCH_PING_OUTPUT_RX, "pong[\r\n]*").


%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    epoch_commands
].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).


%=== TEST CASES ================================================================

epoch_commands(Cfg) ->
    setup_nodes([?NODE1], Cfg),
    start_node(node1, Cfg),
    wait_for_value({height, 0}, [node1], ?STARTUP_TIMEOUT, Cfg),
    wait_for_value({height, 10}, [node1], ?MINING_TIMEOUT * 10, Cfg),
    {0, Output1} = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "versions"], Cfg),
    ?assertMatch({match, _}, re:run(Output1, ?EPOCH_VERSION_OUTPUT_RX)),

    {0, Output2} = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "peer_key"], Cfg),
    {ok, PeerKey} = aec_base58c:safe_decode(peer_pubkey, list_to_binary(Output2)),
    ExpPeerKey = aest_nodes:get_node_pubkey(node1, Cfg),
    ?assertEqual(ExpPeerKey, PeerKey),

    {0, Output3} = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "check_config", "epoch.yaml"], Cfg),
    ?assertMatch({match, _}, re:run(Output3, ?EPOCH_CHECK_OUTPUT_RX)),

    {0, Output4} = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "pid"], Cfg),
    ?assertMatch({match, _}, re:run(Output4, ?EPOCH_PID_OUTPUT_RX)),

    {0, Output5} = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "ping"], Cfg),
    ?assertMatch({match, _}, re:run(Output5, ?EPOCH_PING_OUTPUT_RX)),

    Result6 = aest_nodes:run_cmd_in_node_dir(node1, ["bin/epoch", "status"], Cfg),
    ?assertMatch({0, _}, Result6),

    ok.


%=== INTERNAL FUNCTIONS ========================================================
