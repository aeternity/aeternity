-module(aest_compatibility_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_mining_algorithms_compatibility/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    get_block/2
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).

-define(NODE1, #{
    name    => node1,
    peers   => [],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).

-define(NODE2, #{
    name    => node2,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aeternity/aeternity:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    test_mining_algorithms_compatibility
].

init_per_suite(Config) ->
    [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_mining_algorithms_compatibility(Cfg) ->
    MinLength = 20,
    ExtraStep = 5,
    MaxSteps = 10,

    #{ public := PubKey1 } = enacl:sign_keypair(),
    #{ public := PubKey2 } = enacl:sign_keypair(),
    ?assertNotEqual(PubKey1, PubKey2),
    Beneficiary1 = aeser_api_encoder:encode(account_pubkey, PubKey1),
    Beneficiary2 = aeser_api_encoder:encode(account_pubkey, PubKey2),

    %% Setup nodes
    setup([?NODE1], #{ beneficiary => Beneficiary1, algorithm => <<"mean15-generic">> }, Cfg),
    setup([?NODE2], #{ beneficiary => Beneficiary2, algorithm => <<"lean15-generic">> }, Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_startup([node1, node2], 0, Cfg),

    wait_for_value({height, MinLength + 1}, [node1, node2],
                   (MinLength + 1) * ?MINING_TIMEOUT, Cfg),
    wait_for_beneficiaries(node1, node2, [Beneficiary1, Beneficiary2],
                           MinLength, ExtraStep, MaxSteps,
                           Cfg),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).

get_block_beneficiary(NodeName, Height) ->
    #{ beneficiary := Beneficiary } = get_block(NodeName, Height),
    Beneficiary.

get_chain_beneficiaries(NodeName, Height) when is_integer(Height),
                                               Height >= 1 ->
    lists:usort([ get_block_beneficiary(NodeName, H) || H <- lists:seq(1, Height) ]).

wait_for_beneficiaries(Node1, Node2, ExpectedBeneficiaries,
                       CurrentLength, Step, MaxSteps,
                       Cfg)
  when is_integer(MaxSteps), MaxSteps > 0 ->
    B1 = get_block(Node1, CurrentLength),
    B2 = get_block(Node2, CurrentLength),
    ?assertNotEqual(undefined, B1),
    ?assertEqual(B1, B2),

    % Be sure both nodes created some blocks
    ActualBeneficiaries = get_chain_beneficiaries(Node1, CurrentLength),
    case { lists:sort(ExpectedBeneficiaries)
         , lists:sort(ActualBeneficiaries)
         } of
        {X, X} -> ok;
        {_, _} ->
            NewLength = Step + CurrentLength,
            wait_for_value({height, NewLength + 1}, [Node1, Node2],
                           Step * ?MINING_TIMEOUT, Cfg),
            wait_for_beneficiaries(Node1, Node2, ExpectedBeneficiaries,
                                   NewLength, Step, MaxSteps - 1,
                                   Cfg)
    end.

