-module(aest_heavy_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0, suite/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
         many_spend_txs/1
        ]).

-import(aest_nodes, [
    cluster/2,
    setup_nodes/2,
    get_node_config/2,
    start_node/2,
    stop_node/3,
    connect_node/3, disconnect_node/3,
    wait_for_value/4,
    wait_for_startup/3,
    get_block/2,
    get_top/1,
    get_mempool/1,
    post_spend_tx/5,
    request/3,
    assert_in_sync/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT, 300*1000).


%=== COMMON TEST FUNCTIONS =====================================================

suite() ->
    [{timetrap, {minutes, 30}}].

%% Hard to switch mining on and off. Therefore a test that starts with first filling the mempool
%% and then start mining is really difficult to write in this framework.
all() ->
    [ many_spend_txs ].

init_per_suite(Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    %% timers must be less than gen_server:call timeout.
    [ {node_startup_time, 20000}, %% Time may take to get the node to respond to http
      {node_shutdown_time, 20000} %% Time it may take to stop node cleanly
    | Config].

init_per_testcase(quick_start_stop, Config) ->
    aest_nodes:ct_setup([{verify_logs, false}|Config]);
init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.


%=== TEST CASES ================================================================

%% This is keypair of an account set in genesis config file
%% (see https://github.com/aeternity/epoch/blob/master/data/aecore/.genesis/accounts_test.json),
%% so beneficiary configuration in aeternity.yaml (mining > beneficiary param) does not matter.
patron() ->
    #{ pubkey => <<206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>,
       privkey => <<230,169,29,99,60,119,207,87,113,50,157,51,84,179,188,239,27,197,224,50,196,61,112,182,211,90,249,35,206,30,183,77,206,167,173,228,112,201,249,157,157,78,64,8,128,168,111,29,73,187,68,75,98,241,26,158,187,100,187,207,235,115,254,243>>
     }.

many_spend_txs(Cfg) ->
    setup_nodes([#{ name    => node1,
                    peers   => [node2, node3],
                    backend => aest_docker,
                    source  => {pull, "aeternity/aeternity:local"},
                    config  => #{persist => false}
                  },
                 #{ name    => node2,
                    peers   => [node1],
                    backend => aest_docker,
                    source  => {pull, "aeternity/aeternity:local"},
                    config  => #{persist => true}
                  },
                 #{ name    => node3,
                    peers   => [node1],
                    backend => aest_docker,
                    source  => {pull, "aeternity/aeternity:local"},
                    config  => #{persist => true},
                    mining  => #{autostart => false}
                  }], Cfg),

    %% Load many transactions to node3 and then sync transactions to a mining node.
    %% Transactions sync is faster that transactions posting and limit on http clients no issue

    %% This means we always create micro blocks in first generation (after genesis block)
    %% That is a sligtly unrealistic situation, but if we first mine a few key blocks,
    %% then this test takes ages.

    %% Compute gas for a simple spend
    {ok, FakeTx} = aec_spend_tx:new(#{ sender_id => aeser_id:create(account, maps:get(pubkey, patron()))
                                     , recipient_id => aeser_id:create(account, maps:get(pubkey, patron()))
                                     , amount => 1 * aest_nodes:gas_price()
                                     , fee => 1 * aest_nodes:gas_price()
                                     , ttl => 10000000
                                     , nonce => 1000
                                     , payload => <<"node3">>}),
    Height = 0,
    Protocol = aec_hard_forks:protocol_effective_at_height(0),
    Gas = aetx:gas_limit(FakeTx, Height, Protocol),

    TxsPerMB = aec_governance:block_gas_limit() div Gas,
    ct:log("We can put approx ~p Txs in a micro block (gas per spend ~p, ~p)\n", [TxsPerMB, Gas, FakeTx]),

    start_node(node3, Cfg),
    wait_for_startup([node3], 0, Cfg),

    Patron = patron(),

    %% We assume 300 spend per Micro block. 2700 txs takes 9 micro blocks and should be under
    %% 9*3 seconds (half a minute). Expected to have at most 10 micro blocks, otherwise
    %% we are not efficient enough
    TxHashes = add_spend_txs(node3, Patron, 2700, 1),

    start_node(node1, Cfg),
    wait_for_startup([node1], 0, Cfg),
    LastTxHash = lists:last(TxHashes),

    %% Check that the valid transactions made it to the chain.
    #{node1 := [{_Hash, Height}]} =
        wait_for_value({txs_on_chain, [LastTxHash]},
                   [node1], 10 * ?MINING_TIMEOUT, [{key_blocks, 0} | Cfg]),
                                                  %% don't wait for extra key blocks
    ct:log("On chain at height = ~p", [Height]),


    %% Start 2nd node and let it sync with node1 and node3
    start_node(node2, Cfg),

    FoundNode3 = find_txs(node3, TxHashes, #{}, erlang:system_time(seconds) + 120),
    ct:log("MicroBlock distribution ~p", [FoundNode3]),

    %% It's to be expected that at least one micro block contains almost max nr of transactions
    ?assert(abs(TxsPerMB - lists:max(maps:values(FoundNode3))) =< 2),

    %% If we have synced one more key block, all transactions are present!
    wait_for_startup([node2], Height, Cfg),

    FoundNode2 = find_txs(node2, TxHashes, #{}, erlang:system_time(seconds) + 200),  %% around 10 micro blocks
    case FoundNode2 == FoundNode3 of
        true -> ok;
        false ->
            %% may be different if we had a fork!
            {skip, unlikely_but_we_have_mined_a_fork}
    end.


%% helper functions

find_txs(_Node, [], Found, _Deadline) ->
    Found;
find_txs(Node, [TxHash | TxHashes], Found, Deadline) ->
    case Deadline - erlang:system_time(seconds) > 0 of
        true ->
             case aest_nodes:request(Node, 'GetTransactionByHash', #{hash => TxHash}) of
                 {ok, 200, #{ block_hash := MBHash}} ->
                     Prev = maps:get(MBHash, Found, 0),
                     find_txs(Node, TxHashes, maps:put(MBHash, Prev + 1, Found), Deadline);
                 _ ->
                     find_txs(Node, TxHashes ++ [TxHash], Found, Deadline)
             end;
        false ->
            error({timeout, Found})
    end.



add_spend_txs(Node, SenderAcct, N, NonceStart) ->
    add_many_spend_tx(Node, SenderAcct, lists:seq(NonceStart, NonceStart + N - 1), []).

add_many_spend_tx(_Node, _SenderAcct, [], Acc) ->
    [ Hash || {_, Hash} <- lists:sort(Acc)];
add_many_spend_tx(Node, SenderAcct, [Nonce|Nonces], Acc) ->
    case (catch add_spend_tx(Node, SenderAcct, Nonce)) of
        {'EXIT', _} ->
            %% temporarily overloaded http server
            timer:sleep(100),
            add_many_spend_tx(Node, SenderAcct, Nonces ++ [Nonce], Acc);
        Res ->
            add_many_spend_tx(Node, SenderAcct, Nonces, [Res|Acc])
    end.


add_spend_tx(Node, Sender, Nonce) ->
    %% create new receiver
    GasPrice = aest_nodes:gas_price(),
    #{ public := RecvPubKey, secret := _RecvSecKey } =  enacl:sign_keypair(),
    #{ tx_hash := TxHash} = post_spend_tx(Node, Sender, #{pubkey => RecvPubKey}, Nonce,
                                          #{amount => 1, fee => 20000 * GasPrice}),
    {Nonce, TxHash}.
