-module(aest_genesis_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_node_starts_with_30k_accounts/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    post_spend_tx/5
]).

%=== INCLUDES ==================================================================

%=== MACROS ====================================================================

-define(MIKE, #{
    pubkey => <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
                190,211,20,112,79,108,85,78,88,181,26,207,191,211,
                40,225,138,154>>,
    privkey => <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
                 100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
                 93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
                 85,78,88,181,26,207,191,211,40,225,138,154>>
}).

-define(ALICE, #{
    pubkey => <<177,181,119,188,211,39,203,57,229,94,108,2,107,214, 167,74,27,
                53,222,108,6,80,196,174,81,239,171,117,158,65,91,102>>,
    privkey => <<145,69,14,254,5,22,194,68,118,57,0,134,66,96,8,20,124,253,238,
                 207,230,147,95,173,161,192,86,195,165,186,115,251,177,181,119,
                 188,211,39,203,57,229,94,108,2,107,214,167,74,27,53,222,108,6,
                 80,196,174,81,239,171,117,158,65,91,102>>
}).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_node_starts_with_30k_accounts
].

init_per_suite(Config) ->
    [
        {node_startup_time, 200000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_node_starts_with_30k_accounts(Cfg) ->
    MikeAddress = aeser_api_encoder:encode(account_pubkey, maps:get(pubkey, ?MIKE)),
    Alice = maps:get(pubkey, ?ALICE),
    NodeConfig = #{ beneficiary      => MikeAddress},
    GenesisAccounts =
        lists:map(
            fun({PK, Amount}) ->
                Address = aeser_api_encoder:encode(account_pubkey, PK),
                {Address, Amount}
            end,
            [{Alice, 1234} | random_accounts(_Cnt = 30000)]),
    setup([?NODE1], NodeConfig, GenesisAccounts, Cfg),
    start_node(node1, Cfg),
    wait_for_startup([node1], 1, Cfg),
    wait_for_value({balance, Alice, 1234}, [node1], 10000, Cfg),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, GenesisAccounts, Cfg) ->
    UpdatedNodeSpecs =
        lists:map(
            fun(NodeSpec) ->
                maps:merge(NodeSpec, #{config => Config,
                                       genesis_accounts => GenesisAccounts})
            end,
            NodeSpecs),
    setup_nodes(UpdatedNodeSpecs, Cfg).

random_accounts(Cnt) ->
   random_accounts(Cnt, []).

random_accounts(Cnt, Accum) when Cnt < 1 ->
    Accum;
random_accounts(Cnt, Accum) ->
    random_accounts(Cnt - 1, [{random_pubkey(), rand:uniform(100000000)} | Accum]).

random_pubkey() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).
