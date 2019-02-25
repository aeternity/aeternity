-module(aehttp_dryrun_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-export([ spend_txs/1
        , identity_contract/1
        , accounts/1
        ]).

-import(aehttp_contracts_SUITE,
        [http_request/4, internal_address/0, external_address/0, new_account/1, rpc/4]).

-define(NODE, dev1).

all() ->
    [ {group, dry_run}
    ].

groups() ->
    [ {dry_run, [],
        [ spend_txs
        , identity_contract
        , accounts
        ]}
    ].

suite() -> [].

init_per_suite(Config) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> => #{<<"persist">> => true,
                                <<"hard_forks">> => Forks}},
    Config1 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, [{symlink_name, "latest.http_dryrun"}, {test_module, ?MODULE}] ++ Config),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(_, Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ aecore_suite_utils:mine_key_blocks(NodeName, ToMine) || ToMine > 0 ],

    %% Prepare accounts, Alice, Bert, Carl and Diana.

    StartAmt = 25000000,
    {APubkey, APrivkey, STx1} = new_account(StartAmt),
    {BPubkey, BPrivkey, STx2} = new_account(StartAmt),
    {CPubkey, CPrivkey, STx3} = new_account(StartAmt),
    {DPubkey, DPrivkey, STx4} = new_account(StartAmt),

    {ok, KBs} = aecore_suite_utils:mine_blocks_until_txs_on_chain(
                                    NodeName, [STx1, STx2, STx3, STx4], 5),

    Top = lists:last(KBs),

    %% Save account information.
    Accounts = #{acc_a => #{pub_key => APubkey,
                            priv_key => APrivkey,
                            start_amt => StartAmt},
                 acc_b => #{pub_key => BPubkey,
                            priv_key => BPrivkey,
                            start_amt => StartAmt},
                 acc_c => #{pub_key => CPubkey,
                            priv_key => CPrivkey,
                            start_amt => StartAmt},
                 acc_d => #{pub_key => DPubkey,
                            priv_key => DPrivkey,
                            start_amt => StartAmt}},
    {ok, TopHash} = aec_blocks:hash_internal_representation(Top),
    [{top_hash, TopHash}, {accounts, Accounts}, {node_name, NodeName} | Config].

end_per_group(_Group, Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok.

init_per_testcase(_Case, Config) ->
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_,N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

spend_txs(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    #{ public := EPub } = enacl:sign_keypair(),

    Tx1 = create_spend_tx(APub, EPub, 100000, 20000, 1, 100),
    Tx2 = create_spend_tx(EPub, APub, 100, 20000, 1, 100),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [Tx1, Tx2]),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [Tx2, Tx1]),

    ok.

identity_contract(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),

    {ok, Code}   = aect_test_utils:compile_contract("contracts/identity.aes"),

    InitCallData = make_call_data(Code, list_to_binary(
                        [ "contract MakeCall = \n"
                        , "  function init : () => _\n"
                        , "  function __call() = init()" ])),

    CallCallData = make_call_data(Code, list_to_binary(
                        [ "contract MakeCall = \n"
                        , "  function main : int => int\n"
                        , "  function __call() = main(42)" ])),

    CreateTx  = create_contract_tx(APub, 1, Code, InitCallData),
    CPub      = contract_id(CreateTx),
    CallTx    = call_contract_tx(APub, CPub, 2, CallCallData),
    BadCallTx = call_contract_tx(APub, CPub, 1, CallCallData),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [CreateTx, CallTx]),

    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [BadCallTx, CreateTx]),

    ok.

accounts(Config) ->
    #{acc_a := #{pub_key := APub}} = proplists:get_value(accounts, Config),
    TopHash = proplists:get_value(top_hash, Config),
    GenHash = get_genesis_hash(),

    #{ public := EPub } = enacl:sign_keypair(),

    Tx1 = create_spend_tx(APub, EPub, 100000, 20000, 1, 100),
    Tx2 = create_spend_tx(EPub, APub, 100, 20000, 1, 100),

    %% Should work on TopHash
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [Tx1, Tx2]),

    %% Should not work on GenHash
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"error">> }] }} =
        dry_run(GenHash, [Tx1, Tx2]),

    %% Should work on GenHash with APub
    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"ok">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [Tx1, Tx2], [#{ pub_key => APub, amount => 100000000}]),


    {ok, 200, #{ <<"results">> := [#{ <<"result">> := <<"error">> }, #{ <<"result">> := <<"ok">> }] }} =
        dry_run(TopHash, [Tx2, Tx1]),

    ok.

%% --- Internal functions ---

make_call_data(ContractCode, Call) ->
    {ok, CallData} = aect_sophia:encode_call_data(ContractCode, <<>>, Call),
    CallData.

contract_id(Tx) ->
    {_, CTx} = aetx:specialize_callback(Tx),
    aect_create_tx:contract_pubkey(CTx).

dry_run(TopHash, Txs) ->
    dry_run(TopHash, Txs, []).

dry_run(TopHash, Txs, Accounts) ->
    http_request(internal_address(), post, "debug/transactions/dry-run",
                 #{ top => aehttp_api_encoder:encode(key_block_hash, TopHash),
                    accounts => [ A#{pub_key => aehttp_api_encoder:encode(account_pubkey, PK)}
                                  || A = #{pub_key := PK } <- Accounts ],
                    txs => [aehttp_api_encoder:encode(transaction, aetx:serialize_to_binary(Tx)) || Tx <- Txs] }).

get_genesis_hash() ->
    {ok, 200, #{<<"genesis_key_block_hash">> := EncGenesisHash}} = get_status(),
    {ok, GenesisHash} = aehttp_api_encoder:safe_decode(key_block_hash, EncGenesisHash),
    GenesisHash.

get_status() ->
    http_request(external_address(), get, "status", #{}).

create_spend_tx(Sender, Recipient, Amount, Fee, Nonce, TTL) ->
    SenderId = aec_id:create(account, Sender),
    RecipientId = aec_id:create(account, Recipient),
    Params = #{ sender_id    => SenderId,
                recipient_id => RecipientId,
                amount       => Amount,
                nonce        => Nonce,
                ttl          => TTL,
                payload      => <<>>,
                fee          => Fee },
    {ok, Tx} = aec_spend_tx:new(Params),
    Tx.

create_contract_tx(Owner, Nonce, Code, CallData) ->
    OwnerId = aec_id:create(account, Owner),
    Params = #{ owner_id => OwnerId,
                code => Code,
                call_data => CallData,
                vm_version => aect_test_utils:latest_sophia_vm_version(),
                abi_version => aect_test_utils:latest_sophia_abi_version(),
                deposit => 0,
                amount => 0,      %Initial balance
                gas => 100000,     %May need a lot of gas
                gas_price => 1,
                fee => 1400000,
                nonce => Nonce },
    {ok, Tx} = aect_create_tx:new(Params),
    Tx.

call_contract_tx(Caller, Contract, Nonce, CallData) ->
    CallerId = aec_id:create(account, Caller),
    ContractId = aec_id:create(contract, Contract),
    Params = #{ caller_id => CallerId,
                contract_id => ContractId,
                call_data => CallData,
                abi_version => aect_test_utils:latest_sophia_abi_version(),
                amount => 0,
                gas => 100000,     %May need a lot of gas
                gas_price => 1,
                fee => 800000,
                nonce => Nonce },
    {ok, Tx} = aect_call_tx:new(Params),
    Tx.


