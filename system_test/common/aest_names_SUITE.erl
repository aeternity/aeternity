-module(aest_names_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    test_name_registration/1,
    test_name_transfer/1
]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    wait_for_value/4,
    wait_for_startup/3,
    request/3,
    post_spend_tx/5,
    post_name_preclaim_tx/4,
    post_name_claim_tx/3,
    post_name_update_tx/4,
    post_name_transfer_tx/5,
    post_name_revoke_tx/4
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT,   3000).
-define(SYNC_TIMEOUT,      100).

-define(MIKE, #{
    pubkey => <<200,171,93,11,3,93,177,65,197,27,123,127,177,165,
                190,211,20,112,79,108,85,78,88,181,26,207,191,211,
                40,225,138,154>>,
    privkey => <<237,12,20,128,115,166,32,106,220,142,111,97,141,104,201,130,56,
                 100,64,142,139,163,87,166,185,94,4,159,217,243,160,169,200,171,
                 93,11,3,93,177,65,197,27,123,127,177,165,190,211,20,112,79,108,
                 85,78,88,181,26,207,191,211,40,225,138,154>>
}).

-define(RICHARD, #{
    pubkey => <<103,28,85,70,70,73,69,117,178,180,148,246,81,104,
                33,113,6,99,216,72,147,205,210,210,54,3,122,84,195,
                62,238,132>>,
    privkey => <<59,130,10,50,47,94,36,188,50,163,253,39,81,120,89,219,72,88,68,
                 154,183,225,78,92,9,216,215,59,108,82,203,25,103,28,85,70,70,
                 73,69,117,178,180,148,246,81,104,33,113,6,99,216,72,147,205,
                 210,210,54,3,122,84,195,62,238,132>>
}).

-define(ROBERT, #{
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
    test_name_registration,
    test_name_transfer
].

init_per_suite(Config) ->
    [
        {node_startup_time, 20000}, %% Time may take to get the node to respond to http
        {node_shutdown_time, 20000}, %% Time it may take to stop node cleanly
        {gas_price, aest_nodes:gas_price()}
    | Config].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

test_name_registration(Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    MPubKey = maps:get(pubkey, ?MIKE),
    RPubKey = maps:get(pubkey, ?RICHARD),
    EncMPubKey = aeser_api_encoder:encode(account_pubkey, MPubKey),
    EncRPubKey = aeser_api_encoder:encode(account_pubkey, RPubKey),

    %% Setup nodes
    NodeConfig = #{ beneficiary => EncMPubKey },
    setup([?NODE1], NodeConfig, Cfg),
    start_node(node1, Cfg),
    wait_for_startup([node1], Height = 4, Cfg),

    Name = aens_test_utils:fullname(<<"richard-no-auction-for-long-name">>, Height),  %% richard-no-auction
    NameSalt = 36346245,
    EncNameId = aeser_api_encoder:encode(name, aens_hash:name_hash(Name)),

    %% Generate tokens for Mike
    wait_for_value({balance, MPubKey, 10000000 * GasPrice}, [node1], 10000, Cfg),

    %% Give some tokens to the registrar account
    post_spend_tx(node1, ?MIKE, ?RICHARD, 1, #{ amount => 6000000 * GasPrice }),
    wait_for_value({balance, RPubKey, 200}, [node1], 10000, Cfg),

    {ok, 404, #{ reason := <<"Name not found">> }} =
        request(node1, 'GetNameEntryByName', #{ name => Name }),

    %% Preclaime the name
    CommitmentHash = aens_hash:commitment_hash(Name, NameSalt),
    #{ tx_hash := PreClaimTxHash } = post_name_preclaim_tx(node1, ?RICHARD, CommitmentHash, #{
        nonce => 1,
        fee   => 20000 * GasPrice
    }),
    aest_nodes:wait_for_value({txs_on_chain, [PreClaimTxHash]}, [node1], 10000, []),

    {ok, 404, #{ reason := <<"Name not found">> }} =
        request(node1, 'GetNameEntryByName', #{ name => Name }),

    %% Claim the name
    #{ tx_hash := ClaimTxHash } = post_name_claim_tx(node1, ?RICHARD, #{
        nonce     => 2,
        name      => Name,
        name_salt => NameSalt,
        fee       => 20000 * GasPrice,
        name_fee  => 500000 * GasPrice
    }),
    aest_nodes:wait_for_value({txs_on_chain, [ClaimTxHash]}, [node1], 10000, []),

    {ok, 200, NameQuery1} =
        request(node1, 'GetNameEntryByName', #{ name => Name }),
    ?assertMatch(#{
        id := EncNameId,
        pointers := [],
        ttl := _
    }, NameQuery1),

    %% Update the registration
    Pointers = [aens_pointer:new(<<"account_pubkey">>, aeser_id:create(account, RPubKey))],
    #{ tx_hash := UpdateTxHash } = post_name_update_tx(node1, ?RICHARD, Name, #{
        nonce      => 3,
        name_ttl   => 100,
        pointers   => Pointers,
        client_ttl => 100,
        fee        => 20000 * GasPrice
    }),
    aest_nodes:wait_for_value({txs_on_chain, [UpdateTxHash]}, [node1], 10000, []),

    {ok, 200, NameQuery2} =
        request(node1, 'GetNameEntryByName', #{ name => Name }),
    ?assertMatch(#{
        id := EncNameId,
        pointers := [#{
            key := <<"account_pubkey">>,
            id := EncRPubKey
        }],
        ttl := _
    }, NameQuery2),

    %% Revoke the name
    #{ tx_hash := RevokeTxHash } = post_name_revoke_tx(node1, ?RICHARD, Name, #{
        nonce => 4,
        fee   => 20000 * GasPrice
    }),
    aest_nodes:wait_for_value({txs_on_chain, [RevokeTxHash]}, [node1], 10000, []),

    {ok,404, #{reason := <<"Name revoked">>}} =
        request(node1, 'GetNameEntryByName', #{ name => Name }),

    ok.

test_name_transfer(Cfg) ->
    GasPrice = proplists:get_value(gas_price, Cfg),
    MPubKey = maps:get(pubkey, ?MIKE),
    RPubKey1 = maps:get(pubkey, ?RICHARD),
    RPubKey2 = maps:get(pubkey, ?ROBERT),
    EncMPubKey = aeser_api_encoder:encode(account_pubkey, MPubKey),

    %% Setup nodes
    NodeConfig = #{ beneficiary => EncMPubKey },
    setup([?NODE1], NodeConfig, Cfg),
    start_node(node1, Cfg),
    wait_for_startup([node1], Height = 4, Cfg),

    Name = aens_test_utils:fullname(<<"richard-no-auction-and-long-name">>, Height),
    NameSalt = 36346245,

    %% Generate tokens for Mike
    wait_for_value({balance, MPubKey, 2000000 * GasPrice}, [node1], 10000, Cfg),

    %% Give some tokens to the registrar accounts
    post_spend_tx(node1, ?MIKE, ?RICHARD, 1, #{ amount => 600000 * GasPrice }),
    post_spend_tx(node1, ?MIKE, ?ROBERT, 2, #{ amount => 600000 * GasPrice }),
    wait_for_value({balance, RPubKey1, 200}, [node1], 10000, Cfg),
    wait_for_value({balance, RPubKey2, 200}, [node1], 10000, Cfg),

    Fee = 20000 * GasPrice,
    %% Preclaime the name
    CommitmentHash = aens_hash:commitment_hash(Name, NameSalt),
    #{ tx_hash := PreClaimTxHash } = post_name_preclaim_tx(node1, ?RICHARD, CommitmentHash, #{
        nonce => 1,
        fee   => Fee
    }),
    aest_nodes:wait_for_value({txs_on_chain, [PreClaimTxHash]}, [node1], 10000, []),

    %% Claim the name
    #{ tx_hash := ClaimTxHash } = post_name_claim_tx(node1, ?RICHARD, #{
        nonce     => 2,
        name      => Name,
        name_salt => NameSalt,
        fee       => Fee,
        name_fee  => 20*Fee
    }),
    aest_nodes:wait_for_value({txs_on_chain, [ClaimTxHash]}, [node1], 10000, []),

    %% Transfer the name to robert
    #{ tx_hash := TransferTxHash } = post_name_transfer_tx(node1, ?RICHARD, ?ROBERT, Name, #{
        nonce => 3,
        fee   => Fee
    }),
    aest_nodes:wait_for_value({txs_on_chain, [TransferTxHash]}, [node1], 10000, []),

    %% Now robert can update the name
    #{ tx_hash := UpdateTxHash } = post_name_update_tx(node1, ?ROBERT, Name, #{
        nonce      => 1,
        name_ttl   => 100,
        pointers   => [],
        client_ttl => 100,
        fee        => Fee
    }),
    aest_nodes:wait_for_value({txs_on_chain, [UpdateTxHash]}, [node1], 10000, []),

    %% And revoke it
    #{ tx_hash := RevokeTxHash } = post_name_revoke_tx(node1, ?ROBERT, Name, #{
        nonce => 2,
        fee   => Fee
    }),
    aest_nodes:wait_for_value({txs_on_chain, [RevokeTxHash]}, [node1], 10000, []),

    ok.

%=== INTERNAL FUNCTIONS ========================================================

setup(NodeSpecs, Config, Cfg) ->
    setup_nodes([maps:put(config, Config, N) || N <- NodeSpecs], Cfg).
