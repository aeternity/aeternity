-module(aest_commands_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([commands/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT, 2000).
-define(STARTUP_TIMEOUT, 20000).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).


%=== COMMON TEST FUNCTIONS =====================================================

% Please note: this module is part of of the smoke-test target. The combined
% runtime should be kept below 10 minutes.
all() -> [
    commands
].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

commands(Cfg) ->
    commands_("bin/aeternity", Cfg).

commands_(OpsBin, Cfg) ->
    setup_nodes([?NODE1], Cfg),
    start_node(node1, Cfg),
    wait_for_value({height, 0}, [node1], ?STARTUP_TIMEOUT, Cfg),
    {0, Output1} = run_cmd_in_node_dir(node1, [OpsBin, "versions"], Cfg),
    ?assertMatch({match, _}, re:run(Output1,
        "Installed versions:[\r\n]*\\*[ \t]*[0-9\\.\\-a-z]*[0-9\\.\\+a-z]*[ \t]*permanent[\r\n]*")),

    {0, Output2} = run_cmd_in_node_dir(node1, [OpsBin, "peer_key"], Cfg),
    {ok, PeerKey} = aeser_api_encoder:safe_decode(peer_pubkey, list_to_binary(string:trim(Output2, trailing, "\n"))),
    ExpPeerKey = aest_nodes:get_node_pubkey(node1, Cfg),
    ?assertEqual(ExpPeerKey, PeerKey),

    {0, Output3} = run_cmd_in_node_dir(node1,
        [OpsBin, "check_config", "../aeternity.yaml"], Cfg),
    ?assertMatch({match, _}, re:run(Output3, "OK[\r\n]*")),

    {0, Output4} = run_cmd_in_node_dir(node1, [OpsBin, "pid"], Cfg),
    ?assertMatch({match, _}, re:run(Output4, "1[\r\n]*")),

    {0, Output5} = run_cmd_in_node_dir(node1, [OpsBin, "ping"], Cfg),
    ?assertMatch({match, _}, re:run(Output5, "pong[\r\n]*")),

    Result6 = run_cmd_in_node_dir(node1, [OpsBin, "status"], Cfg),
    ?assertMatch({0, _}, Result6),

    {0, Output7} = run_cmd_in_node_dir(node1, [OpsBin, "keys_gen", "secret password"], Cfg),
    ?assertMatch({match, _}, re:run(Output7, "Generated keypair with encoded pubkey:[\r\n]*")),
    {match, [EncodedPubKey]} = re:run(Output7, "ak\\_[A-Za-z0-9]*", [{capture, first, binary}]),
    ?assertMatch({ok, _}, aeser_api_encoder:safe_decode(account_pubkey, EncodedPubKey)),

    {HostPath, GuestPath} = aest_nodes:shared_temp_file(node1, "chain.dlog"),
    {0, _Output8} = run_cmd_in_node_dir(node1, [OpsBin, "export", GuestPath], Cfg),
    ?assert(filelib:is_regular(HostPath)),

    ok.


%=== INTERNAL FUNCTIONS ========================================================

run_cmd_in_node_dir(NodeName, Cmd, Ctx) ->
    aest_nodes:run_cmd_in_node_dir(NodeName, Cmd, #{timeout => 5000, stderr => false}, Ctx).
