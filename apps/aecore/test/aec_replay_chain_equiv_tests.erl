%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Full block-sequence root-equivalence test: builds one diverse chain via
%%%    the leader/candidate path (spends with hot re-touched keys, a contract
%%%    with repeated stateful store writes, an oracle lifecycle, and a name
%%%    lifecycle with explicit revoke and a natural short-TTL expiry), then
%%%    replays every block oldest-first through the real follower/validator
%%%    path on a fresh DB. Each block's recomputed root must equal the root
%%%    its header declares; a leader/follower disagreement fails loudly at the
%%%    exact divergent block.
%%% @end
%%%=============================================================================
-module(aec_replay_chain_equiv_tests).

%% Exported so the suite can also be driven standalone (e.g. from an
%% escript), not just under EUnit.
-export([setup/0, teardown/1, replay_full_sequence/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(GENESIS_TARGET, 553713663).
-define(TEST_HEIGHT, 24).

%%%===================================================================
%%% Fixture
%%%===================================================================

replay_full_sequence_test_() ->
    {timeout, 300,
     {foreach,
      fun setup/0,
      fun teardown/1,
      [ {"batched leader-path blocks replay byte-identical through the "
         "real follower path, full diverse sequence",
         fun replay_full_sequence/0}
      ]}}.

setup() ->
    aec_test_utils:mock_difficulty_as_target(),
    aec_test_utils:mock_governance(),
    aec_test_utils:mock_genesis_and_forks(),
    aec_consensus_bitcoin_ng:load_whitelist(),
    meck:new(aec_events, [passthrough]),
    meck:expect(aec_events, publish, fun(tx_received, _Tx) -> ok end),
    aec_test_utils:aec_keys_setup().

teardown(TmpDir) ->
    %% Defensive: clean the chain-db even if the body crashed, so it can't
    %% leak into the next test as {already_exists, aec_blocks}.
    catch aec_test_utils:stop_chain_db(),
    aec_test_utils:aec_keys_cleanup(TmpDir),
    aec_test_utils:unmock_genesis_and_forks(),
    aec_test_utils:unmock_governance(),
    aec_test_utils:unmock_difficulty_as_target(),
    meck:unload(aec_events),
    ok.

%%%===================================================================
%%% The test
%%%===================================================================

replay_full_sequence() ->
    %% The scripted chain uses a name_fee claim + FATE contracts (Lima+);
    %% skip cleanly on pre-Lima lanes (roma/minerva/fortuna).
    case lists:max(maps:keys(aec_hard_forks:protocols())) >= ?LIMA_PROTOCOL_VSN of
        false ->
            ?debugFmt("skip replay_full_sequence: needs Lima+, lane max = ~p",
                      [lists:max(maps:keys(aec_hard_forks:protocols()))]),
            ok;
        true ->
            do_replay_full_sequence()
    end.

do_replay_full_sequence() ->
    %% actors
    #{public := Alice, secret := AliceSk} = enacl:sign_keypair(),
    #{public := Bob,   secret := BobSk}   = enacl:sign_keypair(),
    #{public := Carol, secret := CarolSk} = enacl:sign_keypair(),
    #{public := Dave,  secret := DaveSk}  = enacl:sign_keypair(),

    NameFee = 40000000000000000000,
    PresetAmount = 20 * NameFee,
    PresetAccounts = [{Alice, PresetAmount}, {Bob, PresetAmount},
                       {Carol, PresetAmount}, {Dave, PresetAmount}],
    Fee = 100000 * aec_test_utils:min_gas_price(),

    %% contract: create + repeated stateful store-writing calls
    {ok, ContractSrc} = aect_test_utils:read_contract(counter),
    {ok, Code} = aect_test_utils:compile_contract(counter),
    {ok, InitCallData} = aect_test_utils:encode_call_data(ContractSrc, <<"init">>, ["0"]),
    {ok, TickCallData} = aect_test_utils:encode_call_data(ContractSrc, <<"tick">>, []),
    ContractNonce = 2,
    ContractPubkey = aect_contracts:compute_contract_pubkey(Alice, ContractNonce),

    %% names: one full lifecycle with revoke, one short-TTL for natural expiry
    Salt1 = 111, Salt2 = 222,
    NameAlpha = <<"alpha-batch-mpt-replay-test.chain">>,
    NameBeta  = <<"beta-batch-mpt-replay-test.chain">>,

    QId = aeo_query:id(Carol, 1, Bob),
    QId2 = aeo_query:id(Carol, 2, Bob),

    TxsFun =
        fun(1) ->
                [ sign(make_spend(Alice, 1, Bob, Fee), AliceSk)
                , sign(make_spend(Bob, 1, Carol, Fee), BobSk)
                ];
           (2) ->
                [ sign(make_contract_create(Alice, ContractNonce, Code, InitCallData, Fee), AliceSk) ];
           (3) ->
                [ sign(make_contract_call(Alice, 3, ContractPubkey, TickCallData, Fee), AliceSk) ];
           (4) ->
                [ sign(make_contract_call(Alice, 4, ContractPubkey, TickCallData, Fee), AliceSk) ];
           (5) ->
                [ sign(make_oracle_register(Bob, 2, Fee), BobSk) ];
           (6) ->
                [ sign(make_oracle_query(Carol, 1, Bob, Fee), CarolSk) ];
           (7) ->
                [ sign(make_oracle_response(Bob, 3, QId, Fee), BobSk) ];
           (8) ->
                [ sign(make_oracle_query(Carol, 2, Bob, Fee), CarolSk) ];
           (9) ->
                [ sign(make_oracle_response(Bob, 4, QId2, Fee), BobSk) ];
           (10) ->
                [ sign(make_preclaim(Dave, 1, NameAlpha, Salt1, Fee), DaveSk) ];
           (12) ->
                [ sign(make_claim(Dave, 2, NameAlpha, Salt1, Fee, NameFee), DaveSk) ];
           (13) ->
                [ sign(make_update(Dave, 3, NameAlpha, 1000, [ptr(<<"a">>, Dave)], Fee), DaveSk) ];
           (14) ->
                [ sign(make_transfer(Dave, 4, NameAlpha, Alice, Fee), DaveSk) ];
           (15) ->
                [ sign(make_update(Alice, 5, NameAlpha, 1000, [ptr(<<"a">>, Alice)], Fee), AliceSk) ];
           (16) ->
                [ sign(make_revoke(Alice, 6, NameAlpha, Fee), AliceSk) ];
           (17) ->
                [ sign(make_preclaim(Carol, 3, NameBeta, Salt2, Fee), CarolSk) ];
           (19) ->
                [ sign(make_claim(Carol, 4, NameBeta, Salt2, Fee, NameFee), CarolSk) ];
           (20) ->
                %% Short TTL (expires at height 22) so the name is pruned
                %% by natural expiry before the chain ends.
                [ sign(make_update(Carol, 5, NameBeta, 2, [ptr(<<"b">>, Carol)], Fee), CarolSk) ];
           (21) ->
                [ sign(make_contract_call(Alice, 7, ContractPubkey, TickCallData, Fee), AliceSk) ];
           (22) ->
                [ sign(make_spend(Bob, 5, Dave, Fee), BobSk) ];
           (_) ->
                []
        end,

    meck:expect(aec_fork_block_settings, genesis_accounts, 0, PresetAccounts),
    meck:expect(aec_governance, beneficiary_reward_delay, 0, 1000),
    aec_consensus:set_genesis_hash(),

    %% build the chain on the leader/candidate path
    ok = aec_test_utils:start_chain_db(),
    Targets = lists:duplicate(?TEST_HEIGHT, ?GENESIS_TARGET),
    Chain = gen_chain(PresetAccounts, Targets, 111, TxsFun),
    %% Ensure the leader path actually included every scripted tx
    %% (none silently dropped), else the chain proves nothing.
    NonEmptyMicro = length([B || B <- Chain, aec_blocks:type(B) =:= micro,
                                  aec_blocks:txs(B) =/= []]),
    ?assertEqual(21, NonEmptyMicro),
    ok = aec_test_utils:stop_chain_db(),

    %% replay every block through the real follower path on a fresh DB
    ok = aec_test_utils:start_chain_db(),
    Report = aec_replay_harness:replay_insert(Chain),
    aec_replay_harness:print_report(Report),
    ok = aec_test_utils:stop_chain_db(),

    case Report of
        {ok, Entries} ->
            ?assertEqual(length(Chain), length(Entries)),
            ?assert(lists:all(fun(#{status := S}) -> S =:= ok end, Entries));
        {error, Bad, _Good} ->
            ?debugFmt("REPLAY DIVERGENCE at ~p", [Bad]),
            ?assert(false)
    end,
    ok.

%%%===================================================================
%%% Chain building — thin wrapper over aec_test_utils' exported
%%% genesis_block_with_state/1 + extend_block_chain_with_state/2.
%%%===================================================================

gen_chain(PresetAccounts, Targets, Nonce, TxsFun) ->
    {B0, S0} = aec_test_utils:genesis_block_with_state(PresetAccounts),
    Data = #{targets => Targets, txs_by_height_fun => TxsFun, nonce => Nonce},
    Chain = aec_test_utils:extend_block_chain_with_state([{B0, S0}], Data),
    [B || {B, _S} <- Chain].

%%%===================================================================
%%% Tx builders
%%%===================================================================

sign(Tx, Sk) -> aec_test_utils:sign_tx(Tx, [Sk]).

make_spend(Sender, Nonce, Recipient, Fee) ->
    {ok, Tx} = aec_spend_tx:new(#{sender_id => aeser_id:create(account, Sender),
                                  recipient_id => aeser_id:create(account, Recipient),
                                  amount => 1,
                                  fee => Fee,
                                  nonce => Nonce,
                                  payload => <<>>}),
    Tx.

make_contract_create(Owner, Nonce, Code, CallData, Fee) ->
    ABI = aect_test_utils:latest_sophia_abi_version(),
    VM  = aect_test_utils:latest_sophia_vm_version(),
    {ok, Tx} = aect_create_tx:new(#{owner_id => aeser_id:create(account, Owner),
                                    nonce => Nonce,
                                    code => Code,
                                    abi_version => ABI,
                                    vm_version => VM,
                                    deposit => 10,
                                    amount => 0,
                                    gas => 100000,
                                    gas_price => aec_test_utils:min_gas_price(),
                                    call_data => CallData,
                                    fee => Fee}),
    Tx.

make_contract_call(Caller, Nonce, ContractPubkey, CallData, Fee) ->
    ABI = aect_test_utils:latest_sophia_abi_version(),
    {ok, Tx} = aect_call_tx:new(#{caller_id => aeser_id:create(account, Caller),
                                  nonce => Nonce,
                                  contract_id => aeser_id:create(contract, ContractPubkey),
                                  abi_version => ABI,
                                  fee => Fee,
                                  amount => 0,
                                  gas => 100000,
                                  gas_price => aec_test_utils:min_gas_price(),
                                  call_data => CallData}),
    Tx.

make_oracle_register(Pubkey, Nonce, Fee) ->
    {ok, Tx} = aeo_register_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                     nonce => Nonce,
                                     query_format => <<>>,
                                     response_format => <<>>,
                                     query_fee => 100000,
                                     oracle_ttl => {delta, 500},
                                     abi_version => ?ABI_NO_VM,
                                     fee => Fee}),
    Tx.

make_oracle_query(Pubkey, Nonce, OraclePubkey, Fee) ->
    {ok, Tx} = aeo_query_tx:new(#{sender_id => aeser_id:create(account, Pubkey),
                                  nonce => Nonce,
                                  oracle_id => aeser_id:create(oracle, OraclePubkey),
                                  query => <<"What is your name?">>,
                                  query_fee => 100000,
                                  query_ttl => {delta, 100},
                                  response_ttl => {delta, 100},
                                  fee => Fee}),
    Tx.

make_oracle_response(OraclePubkey, Nonce, QueryId, Fee) ->
    {ok, Tx} = aeo_response_tx:new(#{oracle_id => aeser_id:create(oracle, OraclePubkey),
                                     nonce => Nonce,
                                     query_id => QueryId,
                                     response_ttl => {delta, 100},
                                     response => <<"I am Nemo">>,
                                     fee => Fee}),
    Tx.

make_preclaim(Pubkey, Nonce, Name, Salt, Fee) ->
    Commitment = aens_hash:commitment_hash(Name, Salt),
    {ok, Tx} = aens_preclaim_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                      nonce => Nonce,
                                      commitment_id => aeser_id:create(commitment, Commitment),
                                      fee => Fee}),
    Tx.

make_claim(Pubkey, Nonce, Name, Salt, Fee, Bid) ->
    {ok, Tx} = aens_claim_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                   nonce => Nonce,
                                   name => Name,
                                   name_salt => Salt,
                                   name_fee => Bid,
                                   fee => Fee}),
    Tx.

make_update(Pubkey, Nonce, Name, DeltaTTL, Pointers, Fee) ->
    NameHash = aens_hash:name_hash(Name),
    {ok, Tx} = aens_update_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                    nonce => Nonce,
                                    name_id => aeser_id:create(name, NameHash),
                                    name_ttl => DeltaTTL,
                                    pointers => Pointers,
                                    client_ttl => 3600,
                                    fee => Fee}),
    Tx.

make_transfer(Pubkey, Nonce, Name, RecipientPubkey, Fee) ->
    NameHash = aens_hash:name_hash(Name),
    {ok, Tx} = aens_transfer_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                      nonce => Nonce,
                                      name_id => aeser_id:create(name, NameHash),
                                      recipient_id => aeser_id:create(account, RecipientPubkey),
                                      fee => Fee}),
    Tx.

make_revoke(Pubkey, Nonce, Name, Fee) ->
    NameHash = aens_hash:name_hash(Name),
    {ok, Tx} = aens_revoke_tx:new(#{account_id => aeser_id:create(account, Pubkey),
                                    nonce => Nonce,
                                    name_id => aeser_id:create(name, NameHash),
                                    fee => Fee}),
    Tx.

ptr(Key, Pubkey) -> aens_pointer:new(Key, aeser_id:create(account, Pubkey)).
