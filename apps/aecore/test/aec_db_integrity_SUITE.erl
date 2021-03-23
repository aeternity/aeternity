-module(aec_db_integrity_SUITE).

%% common_test exports
-export([all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Tests
-export([ abort_large_insertion/1
        , fault_injection/1 ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence], [abort_large_insertion, fault_injection]}].

suite() ->
    [].

init_per_suite(Config0) ->
    DefCfg = #{
        <<"chain">> => #{
                <<"persist">> => true
        },
        <<"mining">> => #{
            <<"micro_block_cycle">> => 100,
            <<"beneficiary_reward_delay">> => 2
        }},
    Accounts = [new_pubkey() || _ <- lists:seq(1, 128)],
    B = os:getenv("AETERNITY_TESTCONFIG_DB_BACKEND"),
    try
        os:set_env_var("AETERNITY_TESTCONFIG_DB_BACKEND", "rocksdb"),
        Config1 = aecore_suite_utils:init_per_suite([dev1], DefCfg, [{symlink_name, "latest.db_integrity"}, {test_module, ?MODULE}] ++ Config0),
        aecore_suite_utils:start_node(dev1, Config1),
        aecore_suite_utils:connect(aecore_suite_utils:node_name(dev1)),
        ok = aecore_suite_utils:check_for_logs([dev1], Config1),
        [{nodes, [aecore_suite_utils:node_tuple(dev1)]}, {accounts, Accounts} | Config1]
    catch E:R:S ->
        ct:fail("Setup failed ~p ~p ~p", [E, R, S])
    after
        os:set_env_var("AETERNITY_TESTCONFIG_DB_BACKEND", B),
        Config0
    end.

end_per_suite(Config) ->
    [catch begin
               %%{ok, DbCfg} = node_db_cfg(X),
               aecore_suite_utils:stop_node(X, Config)
               %% We need the DB logs for inspection
               %%aecore_suite_utils:delete_node_db_if_persisted(DbCfg)
           end || X <- [dev1]].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
   ok.

init_per_testcase(_Case, Config) ->
    ct:log("testcase pid: ~p", [self()]),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

abort_large_insertion(Config) ->
    %% This is the easy case as the uncommited data is present in the ETS transaction store
    N = aecore_suite_utils:node_name(dev1),
    Accounts = ?config(accounts, Config),
    %% Mine 10 keyblocks
    aecore_suite_utils:mine_key_blocks(N, 10),
    %% Now create a heavy microblock
    {KB0, Trees0} = rpc:call(N, aec_chain, top_block_with_state, []),
    true = aec_blocks:is_key_block(KB0),
    Fee = 1500000 * aec_test_utils:min_gas_price(),
    TxOk = [element(1, create_spend_tx(10, Fee, Nonce, 100, PK)) || {Nonce, PK} <- lists:zip(lists:seq(1, length(Accounts)), Accounts)],
    {MB0, Trees1} = rpc:call(N, aec_block_micro_candidate, create_with_state, [KB0, KB0, TxOk, Trees0]),
    %% MB0 is now valid - let's add one more TX to ensure its not valid but will process the transactions
    {BadTx, _} = create_spend_tx(10, Fee, 2, 100, hd(Accounts)), %% It's not valid as the nonce is reused
    Txs = TxOk ++ [BadTx],
    TxsTree = aec_txs_trees:from_txs(Txs),
    TxsRootHash = aec_txs_trees:pad_empty(aec_txs_trees:root_hash(TxsTree)),
    MB1 = aec_blocks:set_txs_hash(aec_blocks:set_txs(MB0, Txs), TxsRootHash),
    %% Sign the almost valid microblock
    {ok, MB1S} = rpc:call(N, aec_keys, sign_micro_block, [MB1]),
    %% Snapshot most of the mnesia tables
    State = lists:usort(rpc:call(N, mnesia, dirty_all_keys, [aec_account_state])),
    CheckFun = fun() ->
        %% Did mnesia do it's job properly?
        {atomic, State} = rpc:call(N, mnesia, transaction, [fun() -> lists:usort(mnesia:all_keys(aec_account_state)) end])
    end,
    MutatorFun = fun() ->
                        %% This should do a lot of accesses to the state trees and then trigger an abort
                        {error,invalid_transactions_in_block} = rpc:call(N, aec_conductor, post_block, [MB1S])
                 end,
    %% Race it!
    spawn_link(fun() -> [MutatorFun() || _ <- lists:seq(1, 100)] end),
    [CheckFun() || _ <- lists:seq(1, 100)],
    ok.

fault_injection(Config) ->
    %% This is the funny part!
    %% Try injecting some faults to the storage layer and see what happens :)
    N = aecore_suite_utils:node_name(dev1),
    Accounts = ?config(accounts, Config),
    %% Mine 10 keyblocks
    aecore_suite_utils:mine_key_blocks(N, 10),
    %% Now create a heavy microblock
    {KB0, Trees0} = rpc:call(N, aec_chain, top_block_with_state, []),
    true = aec_blocks:is_key_block(KB0),
    Fee = 1500000 * aec_test_utils:min_gas_price(),
    TxOk = [element(1, create_spend_tx(10, Fee, Nonce, 100, PK)) || {Nonce, PK} <- lists:zip(lists:seq(1, length(Accounts)), Accounts)],
    {MB0, _} = rpc:call(N, aec_block_micro_candidate, create_with_state, [KB0, KB0, TxOk, Trees0]),
    {ok, MB0S} = rpc:call(N, aec_keys, sign_micro_block, [MB0]),
    %% Get the initial table state
    State0 = lists:usort(rpc:call(N, mnesia, dirty_all_keys, [aec_account_state])),
    %% Now figure out what the final state should look like
    {aborted, {throw, State1}} = rpc:call(N, mnesia, transaction, [
        fun() ->
            aec_chain_state:insert_block(MB0S),
            throw(lists:usort(mnesia:all_keys(aec_account_state)))
        end]),
    false = State0 =:= State1,
    %% Now inject some faults to rocksdb :)
    {rocksdb, #{aec_account_state := Handle}} = rpc:call(N, persistent_term, get, [{aec_db, mnesia_bypass}, no_bypass]),
    Me = self(),
    Pid = rpc:call(N, erlang, spawn, [
        fun() ->
            meck:new(aec_db_lib, []),
            meck:expect(aec_db_lib, rocksdb_write, fun (Ref, _, _) when Ref =:= Handle -> {error, better_luck_next_time};
                                                       (Ref, Batch, Opts) -> meck:passthrough([Ref, Batch, Opts])
                                                   end),
            Me ! {self(), started},
            receive
                {done, P} ->
                    meck:unload(aec_db_lib),
                    P ! done,
                    ok
            end
        end]),
    %% Ensure we don't have a race between the mocking and the db access
    receive
        {Pid, started} -> ok
    after 5000 ->
            error(timeout)
    end,
    State0 = lists:usort(rpc:call(N, mnesia, dirty_all_keys, [aec_account_state])),
    {error, {io_error, {aec_account_state, better_luck_next_time}}} = rpc:call(N, aec_chain_state, insert_block, [MB0S]),
    Pid ! {done, self()},
    receive
        done ->
            false = rpc:call(N, erlang, is_process_alive, [Pid]),
            ok
    end,
    %% Check if the DB is consistent at this point :(
    case lists:usort(rpc:call(N, mnesia, dirty_all_keys, [aec_account_state])) of
        State0 ->
            ok;
        _ ->
            ct:fail("DB INCONSISTENT!!!")
    end,
    %% Now check that when the error went away the insertion worked as intended
    {ok, _} = rpc:call(N, aec_chain_state, insert_block, [MB0S]),
    case lists:usort(rpc:call(N, mnesia, dirty_all_keys, [aec_account_state])) of
        State1 ->
            ok;
        _ ->
            ct:fail("DB INCONSISTENT!!!")
    end,
    ok.

%% Helpers
create_spend_tx(Amount, Fee, Nonce, TTL, Recipient) ->
    Sender = aecore_suite_utils:patron(),
    SenderId = aeser_id:create(account, maps:get(pubkey, Sender)),
    RecipientId = aeser_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    STx = aec_test_utils:sign_tx(Tx, maps:get(privkey, Sender)),
    {STx, aeser_api_encoder:encode(tx_hash, aetx_sign:hash(STx))}.

new_pubkey() ->
    #{public := PubKey} = enacl:sign_keypair(),
    PubKey.

node_db_cfg(Node) ->
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(
                    fun (M, F, A)->
                            rpc:call(aecore_suite_utils:node_name(Node), M, F, A, 5000)
                    end),
    {ok, DbCfg}.
