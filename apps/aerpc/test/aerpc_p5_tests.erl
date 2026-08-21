%%%-------------------------------------------------------------------
%%% @doc Unit coverage for address-index maintenance from the state
%%% delta.
%%%
%%% The measured defect these cases pin down: a pubkey that no
%%% transaction field names -- a `Chain.spend' recipient, a
%%% `Chain.create' child contract -- was never indexed after the
%%% backfill, so `eth_getBalance' answered `0x0' and `eth_getCode'
%%% answered `0x' for addresses that were live on chain. Restarting the
%%% node repaired it and fresh traffic broke it again, because the
%%% backfill reads the tries and the incremental path read transaction
%%% fields.
%%%
%%% So every case here indexes through a block that names NOTHING in a
%%% transaction field, and asserts the pubkey resolves anyway. A case
%%% that passed by adding a transaction would be testing the path that
%%% already worked.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_p5_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(CHAIN, aerpc_p5_chain).   %% {block_hash, Hash} | {Hash, Trees}

%% Two hashes are enough for most cases: a parent state and a new one.
-define(H_OLD, <<1:32/unit:8>>).
-define(H_NEW, <<2:32/unit:8>>).
-define(H_KEY, <<3:32/unit:8>>).

%% ===================================================================
%% Fixture
%% ===================================================================

delta_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"a pubkey no transaction names is indexed from the state delta",
       fun learns_an_account_no_tx_names/0},
      {"a contract created from inside a call is indexed, and its store "
       "does not leak extra entries",
       fun learns_a_contract_and_not_its_store/0},
      {"a contract whose own entry changed is indexed even when its "
       "store did not",
       fun learns_a_contract_whose_store_is_unchanged/0},
      {"an unchanged state indexes nothing and reads nothing",
       fun unchanged_state_is_free/0},
      {"a key block delta covers the generation it closed",
       fun key_block_covers_the_generation/0},
      {"transaction-field coverage still works, and a missing state is "
       "counted rather than silent",
       fun tx_fields_survive_a_delta_miss/0},
      {"a delta too large to run re-walks instead of answering partially",
       fun oversized_delta_re_walks/0},
      {"an oversized contracts-trie delta re-walks too, rather than "
       "crashing the index",
       fun oversized_contract_delta_re_walks/0},
      {"the re-walk keeps what is already known",
       fun re_walk_keeps_known_entries/0},
      {"the backfill reads the contracts trie rather than materialising "
       "it, and a store does not become entries of its own",
       fun backfill_walks_contracts_without_materialising/0}]}.

%% ===================================================================
%% Cases
%% ===================================================================

%% The `Chain.spend' shape: the recipient exists in the accounts trie of
%% the new state and in no transaction field anywhere.
learns_an_account_no_tx_names() ->
    Patron    = pk(10),
    Recipient = pk(11),
    Old = trees([Patron], []),
    New = trees([Patron, Recipient], []),
    put_state(?H_OLD, Old),
    put_state(?H_NEW, New),
    ?assertEqual(unknown, resolve(Recipient)),

    micro_event(?H_NEW, ?H_OLD),

    ?assertEqual({ok, Recipient}, resolve(Recipient)),
    ?assertEqual({ok, Patron}, resolve(Patron)),
    ?assert(maps:get(deltas, status()) >= 1).

%% The `Chain.create' shape: a contract whose pubkey appears in no
%% `contract_create_tx', because a factory made it. Its store lives in
%% the same trie under its own 32-byte prefix, which is why the walk
%% stops at 32 bytes -- otherwise a contract that wrote a thousand store
%% slots would cost a thousand lookups and could mint entries for keys
%% that are not pubkeys at all.
learns_a_contract_and_not_its_store() ->
    Owner = pk(20),
    {Child, CtTree} = contract_tree(Owner, 1),
    Old = trees([Owner], []),
    New = trees_with_contracts([Owner], CtTree),
    put_state(?H_OLD, Old),
    put_state(?H_NEW, New),
    ?assertEqual(unknown, resolve(Child)),
    Before = maps:get(indexed, status()),

    micro_event(?H_NEW, ?H_OLD),

    ?assertEqual({ok, Child}, resolve(Child)),
    %% Exactly one new entry: the contract. Its store id is a longer key
    %% under the same 32 bytes and must not become an entry of its own.
    ?assertEqual(Before + 1, maps:get(indexed, status())).

%% The awkward shape in the contracts trie, and the reason the walk keys
%% on bits rather than whole bytes. `unfold/3' follows an extension node
%% before emitting anything, so a contract's own node at 64 nibbles is
%% never handed back: its children arrive at 65 nibbles. With the store
%% byte-identical, every one of those children prunes as unchanged -- so
%% a walk that only recognised whole-byte paths would descend past this
%% contract and index nothing at all.
learns_a_contract_whose_store_is_unchanged() ->
    Owner = pk(35),
    {Child, OldCt} = contract_tree(Owner, 3, <<"code-v1">>),
    {Child, NewCt} = contract_tree(Owner, 3, <<"code-v2">>),
    put_state(?H_OLD, trees_with_contracts([Owner], OldCt)),
    put_state(?H_NEW, trees_with_contracts([Owner], NewCt)),
    ?assertEqual(unknown, resolve(Child)),

    micro_event(?H_NEW, ?H_OLD),

    ?assertEqual({ok, Child}, resolve(Child)).

unchanged_state_is_free() ->
    Patron = pk(30),
    Same   = trees([Patron], []),
    put_state(?H_OLD, Same),
    put_state(?H_NEW, Same),
    Before = status(),

    micro_event(?H_NEW, ?H_OLD),

    %% Identical roots: nothing walked, nothing indexed, no delta run.
    ?assertEqual(maps:get(indexed, Before), maps:get(indexed, status())),
    ?assertEqual(maps:get(deltas, Before), maps:get(deltas, status())).

%% A micro block whose own event was never seen -- a sync burst, a reorg
%% -- is still covered, because the key block diffs from the previous key
%% block's state rather than from its own parent.
key_block_covers_the_generation() ->
    Beneficiary = pk(40),
    Missed      = pk(41),
    put_state(?H_OLD, trees([Beneficiary], [])),
    put_state(?H_KEY, trees([Beneficiary, Missed], [])),
    ?assertEqual(unknown, resolve(Missed)),

    %% No micro event for the block that created `Missed' ever arrived.
    key_event(?H_KEY, ?H_OLD, Beneficiary),

    ?assertEqual({ok, Missed}, resolve(Missed)),
    ?assertEqual({ok, Beneficiary}, resolve(Beneficiary)).

%% The tx-field pass is not replaced by the delta, it runs in front of
%% it. Here the state is unreadable, so the only thing that can index the
%% recipient is the transaction field -- and the miss is counted rather
%% than passed off as an empty delta.
tx_fields_survive_a_delta_miss() ->
    Sender    = pk(50),
    Recipient = pk(51),
    STx       = spend_tx(Sender, Recipient),
    put_block(?H_NEW, [STx]),
    %% Deliberately no state for either hash.
    Misses = maps:get(delta_misses, status()),

    micro_event(?H_NEW, ?H_OLD),

    ?assertEqual({ok, Recipient}, resolve(Recipient)),
    ?assertEqual({ok, Sender}, resolve(Sender)),
    ?assertEqual(Misses + 1, maps:get(delta_misses, status())),
    %% And the process is still alive to serve the next block.
    ?assertEqual(complete, maps:get(backfill, status())).

%% The safety valve. A delta bigger than the ceiling is never applied
%% half-way: the work goes to the chunked backfill, which is the only
%% path in this module that yields.
oversized_delta_re_walks() ->
    Patron = pk(60),
    Fresh  = [pk(N) || N <- lists:seq(61, 70)],
    put_state(?H_OLD, trees([Patron], [])),
    put_state(?H_NEW, trees([Patron | Fresh], [])),
    %% The new top is where the re-walk reads from.
    put_top(?H_NEW),
    application:set_env(aerpc, addr_delta_max_nodes, 1),

    micro_event(?H_NEW, ?H_OLD),

    ?assertEqual(1, maps:get(resyncs, status())),
    wait_for_backfill(complete, 100),
    %% Every pubkey resolves once the re-walk lands -- no partial answer
    %% was ever served as a complete one.
    [?assertEqual({ok, PK}, resolve(PK)) || PK <- [Patron | Fresh]].

%% The same valve, reached through the second trie. Worth its own case
%% because the accounts trie is diffed first: any test whose accounts
%% also changed overflows there and never exercises this return path at
%% all. Here the accounts roots are identical, so only the contracts
%% diff can overflow -- and the index has to survive it.
oversized_contract_delta_re_walks() ->
    Owner = pk(90),
    Cs    = [contract(Owner, N, <<"code">>) || N <- lists:seq(1, 6)],
    CtTree = lists:foldl(fun aect_state_tree:insert_contract/2,
                         aect_state_tree:empty(), Cs),
    put_state(?H_OLD, trees([Owner], [])),
    put_state(?H_NEW, trees_with_contracts([Owner], CtTree)),
    put_top(?H_NEW),
    application:set_env(aerpc, addr_delta_max_nodes, 1),
    Pid = whereis(aerpc_addr_index),

    micro_event(?H_NEW, ?H_OLD),

    %% Survived: an overflow in the second trie is a re-walk, not a crash.
    ?assertEqual(Pid, whereis(aerpc_addr_index)),
    ?assertEqual(1, maps:get(resyncs, status())),
    wait_for_backfill(complete, 100),
    [?assertEqual({ok, aect_contracts:pubkey(C)},
                  resolve(aect_contracts:pubkey(C))) || C <- Cs].

%% A re-walk is not a `rebuild/0': dropping the table would make every
%% address already served answer `incomplete' for the whole walk.
re_walk_keeps_known_entries() ->
    Known  = pk(80),
    Patron = pk(81),
    ok = aerpc_addr_index:index_pubkey(Known),
    put_state(?H_OLD, trees([Patron], [])),
    put_state(?H_NEW, trees([Patron, pk(82)], [])),
    put_top(?H_NEW),
    application:set_env(aerpc, addr_delta_max_nodes, 1),

    micro_event(?H_NEW, ?H_OLD),

    %% The re-walk really is under way -- otherwise the assertions below
    %% would hold for a run that simply never touched the table.
    ?assertEqual(1, maps:get(resyncs, status())),
    ?assertEqual(running, maps:get(backfill, status())),
    %% Mid-walk, before it can possibly have reached anything: the entry
    %% that was already there still answers.
    ?assertEqual({ok, Known}, resolve(Known)),
    wait_for_backfill(complete, 100),
    %% `Known' is in no trie the walk reads, so it can only still be here
    %% because the re-walk added to the table rather than replacing it.
    ?assertEqual({ok, Known}, resolve(Known)).

%% The mainnet-scale defect, measured at height 1,314,674: the contracts
%% half of the backfill read the whole trie -- every store key of every
%% contract -- into one list through `aect_state_tree:to_list/1', spending
%% 10m37s of a 12m26s walk and a 2.12 GiB process heap to insert nothing
%% the accounts walk had not already inserted.
%%
%% So what this case pins is that `to_list/1' is never called. A purely
%% functional assertion would have passed against the old code too: it was
%% correct, and only ruinous.
backfill_walks_contracts_without_materialising() ->
    Owner  = pk(100),
    Cs     = [contract(Owner, N, <<"code">>) || N <- lists:seq(1, 5)],
    CtTree = lists:foldl(fun aect_state_tree:insert_contract/2,
                         aect_state_tree:empty(), Cs),
    put_state(?H_NEW, trees_with_contracts([Owner], CtTree)),
    put_top(?H_NEW),
    ok = meck:new(aect_state_tree, [passthrough, no_link]),
    ok = meck:expect(aect_state_tree, to_list,
                     fun(_Tree) -> error(backfill_must_not_materialise) end),
    try
        ok = aerpc_addr_index:rebuild(),
        wait_for_backfill(complete, 100),

        ?assertEqual({ok, Owner}, resolve(Owner)),
        [?assertEqual({ok, aect_contracts:pubkey(C)},
                      resolve(aect_contracts:pubkey(C))) || C <- Cs],
        %% One owner and five contracts. Each contract carries a store
        %% under its own 32 bytes, and none of those keys is an entry.
        ?assertEqual(6, maps:get(indexed, status()))
    after
        meck:unload(aect_state_tree)
    end.

%% ===================================================================
%% Driving the index
%% ===================================================================

%% Send the event, then block on a call. Messages are handled in order,
%% so the call returning proves the event was fully handled -- no sleep,
%% no polling, no flake.
micro_event(Hash, PrevHash) ->
    event(#{block_hash => Hash, block_type => micro, prev_hash => PrevHash}).

key_event(Hash, PrevKeyHash, Beneficiary) ->
    put_header(Hash, Beneficiary, PrevKeyHash),
    event(#{block_hash => Hash, block_type => key, prev_hash => PrevKeyHash}).

event(Info) ->
    aerpc_addr_index ! {gproc_ps_event, top_changed, #{info => Info}},
    sync().

sync() ->
    {error, unknown_call} = gen_server:call(aerpc_addr_index, sync_barrier),
    ok.

resolve(Pubkey) ->
    aerpc_addr_index:resolve(aerpc_addr_index:to_addr20(Pubkey)).

status() -> aerpc_addr_index:status().

%% ===================================================================
%% Chain doubles
%% ===================================================================

setup() ->
    ?CHAIN = ets:new(?CHAIN, [set, named_table, public]),
    put_top(<<0:32/unit:8>>),
    put_state(<<0:32/unit:8>>, aec_trees:new_without_backend()),
    application:unset_env(aerpc, addr_delta_max_nodes),
    ok = meck:new(aec_chain, [passthrough, no_link]),
    ok = meck:expect(aec_chain, top_block_hash, fun() -> get_top() end),
    ok = meck:expect(aec_chain, get_block_state_partial,
                     fun(Hash, _Elements) -> lookup(Hash) end),
    ok = meck:expect(aec_chain, get_block,
                     fun(Hash) -> lookup({block, Hash}) end),
    ok = meck:expect(aec_chain, get_header,
                     fun(Hash) -> lookup({header, Hash}) end),
    ok = meck:expect(aec_chain, get_generation_by_hash,
                     fun(_Hash, _Dir) -> error end),
    ok = meck:new(aec_blocks, [passthrough, no_link]),
    ok = meck:expect(aec_blocks, txs,
                     fun({fake_block, Txs}) -> Txs;
                        (Block)             -> meck:passthrough([Block])
                     end),
    ok = meck:new(aec_headers, [passthrough, no_link]),
    ok = meck:expect(aec_headers, beneficiary,
                     fun({fake_header, B, _P}) -> B;
                        (H)                    -> meck:passthrough([H])
                     end),
    ok = meck:expect(aec_headers, prev_key_hash,
                     fun({fake_header, _B, P}) -> P;
                        (H)                    -> meck:passthrough([H])
                     end),
    {ok, Pid} = aerpc_addr_index:start_link(),
    wait_for_backfill(complete, 100),
    Pid.

teardown(Pid) ->
    unlink(Pid),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 -> ok
    end,
    ok = meck:unload(aec_headers),
    ok = meck:unload(aec_blocks),
    ok = meck:unload(aec_chain),
    application:unset_env(aerpc, addr_delta_max_nodes),
    ets:delete(?CHAIN),
    ok.

put_state(Hash, Trees)  -> ets:insert(?CHAIN, {Hash, {ok, Trees}}).
put_block(Hash, Txs)    -> ets:insert(?CHAIN, {{block, Hash},
                                               {ok, {fake_block, Txs}}}).
put_header(Hash, B, P)  -> ets:insert(?CHAIN, {{header, Hash},
                                               {ok, {fake_header, B, P}}}).
put_top(Hash)           -> ets:insert(?CHAIN, {top, Hash}).

get_top() -> [{top, Hash}] = ets:lookup(?CHAIN, top), Hash.

lookup(Key) ->
    case ets:lookup(?CHAIN, Key) of
        [{_, Value}] -> Value;
        []           -> error
    end.

%% ===================================================================
%% Trees
%% ===================================================================

%% Distinct in the FIRST 20 bytes, because that is what the index keys
%% on. A big-endian integer would leave every pubkey here sharing one
%% address and the test would be measuring collision handling instead.
pk(N) -> binary:copy(<<N:8>>, 32).

trees(Accounts, Contracts) ->
    trees_with_contracts(Accounts,
                         lists:foldl(fun(C, T) ->
                                         aect_state_tree:insert_contract(C, T)
                                     end, aect_state_tree:empty(), Contracts)).

trees_with_contracts(Accounts, CtTree) ->
    T0 = aec_trees:new_without_backend(),
    AccTree = lists:foldl(
                fun(PK, Tree) ->
                    aec_accounts_trees:enter(aec_accounts:new(PK, 1), Tree)
                end, aec_trees:accounts(T0), Accounts),
    aec_trees:set_contracts(aec_trees:set_accounts(T0, AccTree), CtTree).

%% A contract carries a store, so inserting one puts more than one key
%% under its 32-byte prefix -- which is exactly the shape the walk has to
%% collapse back to a single pubkey.
contract_tree(Owner, Nonce) ->
    contract_tree(Owner, Nonce, <<"code">>).

%% The pubkey is derived from owner and nonce, so holding those fixed and
%% varying the code gives the same contract with a different serialised
%% entry -- and a byte-identical store.
contract_tree(Owner, Nonce, Code) ->
    C = contract(Owner, Nonce, Code),
    Tree = aect_state_tree:insert_contract(C, aect_state_tree:empty()),
    {aect_contracts:pubkey(C), Tree}.

contract(Owner, Nonce, Code) ->
    aect_contracts:new(Owner, Nonce, #{vm => 5, abi => 3}, Code, 0).

spend_tx(Sender, Recipient) ->
    {ok, Tx} = aec_spend_tx:new(
                 #{sender_id    => aeser_id:create(account, Sender),
                   recipient_id => aeser_id:create(account, Recipient),
                   amount       => 1,
                   fee          => 20000,
                   nonce        => 1,
                   payload      => <<>>,
                   ttl          => 0}),
    aetx_sign:new(Tx, []).

wait_for_backfill(_Want, 0) ->
    ?assert(false);
wait_for_backfill(Want, N) ->
    case maps:get(backfill, aerpc_addr_index:status()) of
        Want   -> ok;
        _Other -> timer:sleep(20), wait_for_backfill(Want, N - 1)
    end.

-endif.
