-module(aehttp_rosetta_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").

-import(aecore_suite_utils, [http_request/4, httpc_request/4, process_http_return/1]).
-import(aecore_suite_utils,
        [internal_address/0,
         external_address/0,
         rosetta_address/0,
         rosetta_offline_address/0,
         rpc/3,
         rpc/4]).

%% common_test exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([network_status/1, network_options/1, network_list/1]).
-export([block_genesis/1, block_key_only/1, block_spend_tx/1,
         block_create_contract_tx/1]).
-export([block_create_channel_tx/1]).
-export([construction_flow/1]).
-export([construction_rosetta_cli/1]).
%% for external use
-export([assertBalanceChanges/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(NODE, dev1).
-define(SPEND_FEE, 20000 * aec_test_utils:min_gas_price()).

%% These funded accounts must also be included in rosetta/rosetta.cfg
-define(FUNDED_PUBKEY,
        <<184, 46, 221, 75, 166, 126, 195, 164, 224, 106, 80, 86, 58, 196, 56, 149, 36, 43, 124,
          67, 52, 167, 116, 213, 32, 233, 130, 33, 11, 67, 246, 248>>).
-define(FUNDED_PRIVKEY,
        <<197, 127, 76, 110, 192, 197, 4, 189, 106, 225, 212, 178, 106, 62, 168, 51, 195, 28, 223,
          90, 210, 159, 38, 185, 217, 37, 82, 64, 89, 53, 120, 195, 184, 46, 221, 75, 166, 126, 195,
          164, 224, 106, 80, 86, 58, 196, 56, 149, 36, 43, 124, 67, 52, 167, 116, 213, 32, 233, 130,
          33, 11, 67, 246, 248>>).
-define(FUNDED_PUBKEY2,
        <<59, 21, 39, 47, 165, 14, 39, 153, 244, 204, 123, 53, 145, 239, 0, 38, 27, 6, 124, 50,
          149, 253, 228, 170, 53, 66, 224, 229, 126, 95, 204, 139>>).
-define(FUNDED_PRIVKEY2,
        <<117, 194, 27, 249, 50, 137, 80, 68, 225, 170, 175, 196, 97, 176, 80, 140, 25, 58, 234,
          156, 243, 232, 53, 126, 216, 29, 118, 4, 206, 89, 177, 133, 59, 21, 39, 47, 165, 14, 39,
          153, 244, 204, 123, 53, 145, 239, 0, 38, 27, 6, 124, 50, 149, 253, 228, 170, 53, 66, 224,
          229, 126, 95, 204, 139>>).

all() ->
    [{group, all}].

groups() ->
    [{all, [sequence], [{group, rosetta_read}, {group, rosetta_construction}]},
     {rosetta_read,
      [sequence],
      %% /network/*
      [{group, network_endpoint},
       {group, block_basic_endpoint},
       {group, block_contract_endpoint},
       {group, block_channels_endpoint}]},
     %% /network/*
     {network_endpoint, [], [network_list, network_options, network_status]},
     {block_basic_endpoint, [], [block_genesis, block_key_only, block_spend_tx]},
     {block_contract_endpoint, [], [block_create_contract_tx]},
     {block_channels_endpoint, [], [block_create_channel_tx]},
     %% /construction
     {rosetta_construction, [sequence], [{group, rosetta_flow}, {group, rosetta_cli}]},
     {rosetta_flow, [], [construction_flow]},
     {rosetta_cli, [], [construction_rosetta_cli]}].

suite() ->
    [].

init_per_suite(Config) ->
    DefCfg =
        #{<<"chain">> => #{<<"persist">> => false},
          <<"mining">> =>
              #{<<"micro_block_cycle">> => 1,
                <<"name_claim_bid_timeout">> => 0}}, %% NO name auctions
    {ok, StartedApps} = application:ensure_all_started(gproc),
    Config1 =
        aecore_suite_utils:init_per_suite([?NODE],
                                          DefCfg,
                                          [{instant_mining, true},
                                           {symlink_name, "latest.http_endpoints"},
                                           {test_module, ?MODULE}]
                                          ++ Config),
    Config2 =
        [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}, {started_apps, StartedApps}] ++ Config1,
    aecore_suite_utils:start_node(?NODE, Config2),
    Node = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:connect(Node, []),
    [{node, Node} | Config2].

end_per_suite(Config) ->
    aecore_suite_utils:stop_node(?NODE, Config),
    [application:stop(A)
     || A
            <- lists:reverse(
                   proplists:get_value(started_apps, Config, []))],
    ok.

init_per_group(network_endpoint, Config) ->
    Config;
init_per_group(block_basic_endpoint, Config) ->
    Config;
init_per_group(rosetta_cli, Config) ->
    ProtoVsn = aect_test_utils:latest_protocol_version(),
    case os:cmd("rosetta-cli version") of
        _ when ProtoVsn < ?IRIS_PROTOCOL_VSN ->
            {skip, rosetta_not_before_iris};
        "v" ++ _ ->
            Config;
        _ ->
            {skip, rosetta_cli_executable_not_found}
    end;
init_per_group(_Group, Config) ->
    case aect_test_utils:latest_protocol_version() of
        Vsn when Vsn < ?IRIS_PROTOCOL_VSN ->
            {skip, rosetta_not_before_iris};
        _ ->
            Config
    end.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    init_per_testcase_all(Config).

init_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    aecore_suite_utils:use_rosetta(),
    [{tc_start, os:timestamp()} | Config].

end_per_testcase(_Case, Config) ->
    end_per_testcase_all(Config).

end_per_testcase_all(Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p",
           [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
             || {_, N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% External helper for adding Rosetta checks to other test suites
%% ============================================================

%% Assert a list of {Account, Delta} pairs for a Tx matches what
%% we get from Rosetta
assertBalanceChanges(TxHash, ExpectedChanges) ->
    %% This is designed for use within other non rosetta suites, so we need to restore
    %% the api_prefix when we are done
    Prefix = get(api_prefix),

    aecore_suite_utils:use_swagger(oas3),
    {ok, 200, #{<<"block_height">> := Height, <<"block_hash">> := MBHash}} =
        aehttp_integration_SUITE:get_transactions_by_hash_sut(TxHash),

    {ok, 200, #{<<"prev_key_hash">> := KeyBlockHash}} =
        aehttp_integration_SUITE:get_micro_blocks_header_by_hash_sut(MBHash),

    aecore_suite_utils:use_rosetta(),
    {ok,
     200,
     #{<<"transaction">> :=
           #{<<"transaction_identifier">> := #{<<"hash">> := TxHash}, <<"operations">> := Ops}}} =
        get_block_transaction_sut(KeyBlockHash, Height, TxHash),

    ct:pal("Ops = ~p~n", [Ops]),
    ct:pal("Expected = ~p~n", [ExpectedChanges]),

    matchBalanceChanges(Ops, ExpectedChanges, 0),

    %% Restore original setup of calling SUITE
    case Prefix of
        undefined ->
            erase(api_prefix);
        _ ->
            put(api_prefix, Prefix)
    end,
    ok.

matchBalanceChanges([Op | Ops], [{ExpectedAccount, ExpectedDelta} | Es], Index) ->
    #{<<"account">> := #{<<"address">> := Account},
      <<"amount">> := #{<<"value">> := AmountBin},
      <<"operation_identifier">> := #{<<"index">> := Ix}} =
        Op,
    Delta = binary_to_integer(AmountBin),
    %% Don't use ?assertMatch here because we need the stacktrace logged to know
    %% who called this.
    ExpectedAccount = Account,
    true = ExpectedDelta == variable orelse ExpectedDelta == Delta,
    Index = Ix,
    matchBalanceChanges(Ops, Es, Index + 1);
matchBalanceChanges([], [], _) ->
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% /network/list
network_list(_Config) ->
    {ok,
     200,
     #{<<"network_identifiers">> :=
           [#{<<"blockchain">> := Chain, <<"network">> := Network}]}} =
        get_list_sut(),
    ?assertMatch(<<"aeternity">>, Chain),
    ExpectedNwId = aec_governance:get_network_id(),
    ?assertMatch(ExpectedNwId, Network),
    ok.

get_list_sut() ->
    Host = rosetta_address(),
    Body = #{metadata => #{}},
    http_request(Host, post, "network/list", Body).

%% /network/options

%% %% Official semver regex https://semver.org/#is-there-a-suggested-regular-expression-regex-to-check-a-semver-string
-define(SEMVER_RE,
        "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(?:-((?:0|[1-9]\\d*|\\"
        "d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-"
        "Z-]*))*))?(?:\\+([0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$").

network_options(_Config) ->
    {ok,
     200,
     #{<<"version">> :=
           #{<<"rosetta_version">> := RosettaVsn, <<"node_version">> := NodeVsn}}} =
        get_options_sut(),
    ?assertMatch(<<"1.4.10">>, RosettaVsn),
    {ok, Mp} = re:compile(?SEMVER_RE),
    case re:run(NodeVsn, Mp) of
        {match, _} ->
            ok;
        _ ->
            ct:fail("Node version is not semver")
    end,
    ok.

get_options_sut() ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          metadata => #{}},
    http_request(Host, post, "network/options", Body).

%% /network/status
network_status(Config) ->
     [ {_NodeId, Node} | _ ] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    {ok,
     200,
     #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash},
       <<"current_block_timestamp">> := CurrentBlockTimestamp,
       <<"genesis_block_identifier">> := #{<<"hash">> := GenesisKeyBlockHash},
       <<"sync_status">> := #{<<"synced">> := Synced},
       <<"peers">> := PeersFormatted}} =
        get_status_sut(),

    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(GenesisKeyBlockHash)),
    ?assertMatch(X when is_boolean(X), Synced),
    ?assertMatch(true, is_integer(CurrentBlockTimestamp)),
    ?assertMatch([], PeersFormatted),
    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(TopKeyBlockHash)),
    ok.

get_status_sut() ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          metadata => #{}},
    http_request(Host, post, "network/status", Body).

%% /block
block_genesis(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, 200, #{<<"block">> := #{<<"block_identifier">> := #{<<"index">> := 0}}}} =
        get_block_at_height_sut(0).

%% Test we can fetch an empty keyblock
block_key_only(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),
    {ok, Hash} =
        aec_headers:hash_header(
            aec_blocks:to_header(KeyBlock)),
    KeyHash = aeapi:format_key_block_hash(Hash),
    {ok,
     200,
     #{<<"block">> :=
           #{<<"block_identifier">> := #{<<"hash">> := KeyBlockHash, <<"index">> := _Height},
             <<"timestamp">> := CurrentBlockTimestamp,
             <<"parent_block_identifier">> := #{<<"hash">> := ParentKeyBlockHash},
             <<"transactions">> := Transactions}}} =
        get_block_sut(KeyHash),
    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(KeyBlockHash)),
    ?assertMatch(KeyHash, KeyBlockHash),
    ?assertMatch(true, is_integer(CurrentBlockTimestamp)),
    ?assertMatch([], Transactions),
    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(ParentKeyBlockHash)),
    ok.

%% Test fetch of SpendTx
block_spend_tx(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),

    %% Create To and From accounts and the SpendTx using the non rosetta API for now
    %% Uses process dictionary to set the path prefix
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    StartBalance = 100000000 * aec_test_utils:min_gas_price(),
    {FromPubKey, FromPrivKey} = aehttp_integration_SUITE:initialize_account(StartBalance),
    {ToPubKey, _ToPrivKey} = aehttp_integration_SUITE:initialize_account(StartBalance),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [FromPubKey]),
    {ok, SpendTx} =
        aec_spend_tx:new(#{sender_id => aeser_id:create(account, FromPubKey),
                           recipient_id => aeser_id:create(account, ToPubKey),
                           amount => 1,
                           fee => ?SPEND_FEE,
                           nonce => Nonce,
                           payload => <<"foo">>}),

    SignedSpendTx = sign_tx(SpendTx, FromPrivKey),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [SpendTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    aecore_suite_utils:use_rosetta(),

    FromPubKeyEnc = aeapi:format_account_pubkey(FromPubKey),
    ToPubKeyEnc = aeapi:format_account_pubkey(ToPubKey),

    %% Test Rosetta /account/balance API
    {ok,
     200,
     #{<<"balances">> :=
           [#{<<"currency">> := #{<<"decimals">> := 18, <<"symbol">> := <<"AE">>},
              <<"value">> := FromBalance}]}} =
        get_balance_sut(FromPubKeyEnc),

    ?assertEqual(integer_to_binary(StartBalance - 1 - ?SPEND_FEE), FromBalance),

    %% Seems that mine_blocks_until_txs_on_chain always stops at the block
    %% containing the Tx. Or maybe this is a race condition??
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash}}} =
        get_status_sut(),
    {ok,
     200,
     #{<<"block">> :=
           #{<<"block_identifier">> := #{<<"hash">> := KeyBlockHash, <<"index">> := Height},
             <<"timestamp">> := CurrentBlockTimestamp,
             <<"parent_block_identifier">> := #{<<"hash">> := ParentKeyBlockHash},
             <<"transactions">> := Transactions}}} =
        get_block_sut(TopKeyBlockHash),

    ?assertMatch([_, _], Transactions),
    {ok,
     200,
     #{<<"balances">> :=
           [#{<<"currency">> := #{<<"decimals">> := 18, <<"symbol">> := <<"AE">>},
              <<"value">> := FromBalance}]}} =
        get_balance_at_hash_sut(FromPubKeyEnc, KeyBlockHash),
    {ok,
     200,
     #{<<"balances">> :=
           [#{<<"currency">> := #{<<"decimals">> := 18, <<"symbol">> := <<"AE">>},
              <<"value">> := FromBalance}]}} =
        get_balance_at_height_sut(FromPubKeyEnc, Height),

    %% Expect a Reward, then Fee, and the two balance changes
    [_, #{<<"operations">> := [FeeOp, FromOp, ToOp]}] = Transactions,
    #{<<"operation_identifier">> := #{<<"index">> := 0}, <<"type">> := <<"Spend.fee">>} =
        FeeOp,
    #{<<"operation_identifier">> := #{<<"index">> := 1}, <<"type">> := <<"Spend.amount">>} =
        FromOp,
    #{<<"operation_identifier">> := #{<<"index">> := 2}, <<"type">> := <<"Spend.amount">>} =
        ToOp,

    %% Also check we can get the same Tx via the "fetch individual Tx" Rosetta API
    %% This is a bit inefficient, because to be sure the Tx starts with the correct state
    %% we have to run all the Txs in the microblock containing the Tx until the one we want
    {ok,
     200,
     #{<<"transaction">> :=
           #{<<"transaction_identifier">> := #{<<"hash">> := SpendTxHash},
             <<"operations">> := [FeeOp1, FromOp1, ToOp1]}}} =
        get_block_transaction_sut(TopKeyBlockHash, Height, SpendTxHash),

    ?assertEqual(FromOp, FromOp1),
    ?assertEqual(ToOp, ToOp1),
    ?assertEqual(FeeOp, FeeOp1),

    FromPubKeyEnc = aeapi:format_account_pubkey(FromPubKey),
    ToPubKeyEnc = aeapi:format_account_pubkey(ToPubKey),
    assertBalanceChanges(SpendTxHash,
                         [{FromPubKeyEnc, -?SPEND_FEE}, {FromPubKeyEnc, -1}, {ToPubKeyEnc, 1}]),

    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(KeyBlockHash)),
    ?assertMatch(true, is_integer(CurrentBlockTimestamp)),
    ?assertMatch({ok, _}, aeapi:decode_key_block_hash(ParentKeyBlockHash)),
    ok.

block_create_contract_tx(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),

    %% Use the native http api for the operations not yet implemented in Rosetta
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {OwnerPubKey, OwnerPrivKey} =
        aehttp_integration_SUITE:initialize_account(100000000 * aec_test_utils:min_gas_price()),
    {ToPubKey, _ToPrivKey} =
        aehttp_integration_SUITE:initialize_account(200000000 * aec_test_utils:min_gas_price()),

    OwnerAccountPubKey = aeapi:format_account_pubkey(OwnerPubKey),
    {ok, 200, #{<<"balance">> := OwnerBalance}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAccountPubKey),
    ToAccountPubKey = aeapi:format_account_pubkey(ToPubKey),
    {ok, 200, #{<<"balance">> := ToBalance}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ToAccountPubKey),
    %% ------------------ Contract Create ---------------------------
    %% Check mempool is empty before we start
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [OwnerPubKey]),

    %% Create and post spend_test.aes
    EncodedCode = contract_byte_code(spend_test),
    {ok, EncodedInitCallData} = encode_call_data(spend_test, "init", []),
    CreateAmount = 20000,
    CreateDeposit = 200,
    CreateGas = 100000,
    CreateGasPrice = aec_test_utils:min_gas_price(),
    CreateFee = 400000 * aec_test_utils:min_gas_price(),
    ValidEncoded =
        #{owner_id => aeapi:format_account_pubkey(OwnerPubKey),
          nonce => Nonce,
          code => EncodedCode,
          vm_version => aect_test_utils:latest_sophia_vm_version(),
          abi_version => aect_test_utils:latest_sophia_abi_version(),
          deposit => CreateDeposit,
          amount => CreateAmount,
          gas => CreateGas,
          gas_price => CreateGasPrice,
          call_data => EncodedInitCallData,
          fee => CreateFee},

    %% prepare the contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx}} =
        aehttp_integration_SUITE:get_contract_create(ValidEncoded),

    ContractCreateTxHash =
        aehttp_integration_SUITE:sign_and_post_tx(EncodedUnsignedContractCreateTx, OwnerPrivKey),

    %% Mine the contract Tx so it is on chain when we try to retrieve it
    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ContractCreateTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check everyone's balance after the Contract Create is mined and on chain
    {ok, 200, #{<<"balance">> := OwnerBalanceAfterCreate}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAccountPubKey),

    {ok, 200, CO} = aehttp_integration_SUITE:get_contract_call_object(ContractCreateTxHash),
    #{<<"call_info">> :=
          #{<<"gas_used">> := CreateGasUsed,
            <<"gas_price">> := CreateGasPrice,
            <<"contract_id">> := ContractPubKeyEnc}} =
        CO,

    %% Check Owner Balance using the call object as a cross check
    TotalAmount = CreateAmount + CreateDeposit + CreateFee + CreateGas * CreateGasPrice,
    Refund = (CreateGas - CreateGasUsed) * CreateGasPrice,
    ExpectedBalance = OwnerBalance - TotalAmount + Refund,
    ?assertMatch(ExpectedBalance, OwnerBalanceAfterCreate),

    %% Finally reached a place where we can switch to using our new rosetta API
    aecore_suite_utils:use_rosetta(),
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, Transaction]}}} =
        get_block_sut(TopKeyBlockHash),

    %% Check the the listed balance changing operations contain the right things
    %% and deliver the right answer.
    #{<<"operations">> := [AmountOp, ToContractOp, RefundOp]} = Transaction,
    #{<<"amount">> := #{<<"value">> := AmountDelta},
      <<"account">> := #{<<"address">> := AmountAcc}} =
        AmountOp,
    #{<<"amount">> := #{<<"value">> := RefundDelta},
      <<"account">> := #{<<"address">> := RefundAcc}} =
        RefundOp,

    ?assertEqual(OwnerAccountPubKey, AmountAcc),
    ?assertEqual(OwnerAccountPubKey, RefundAcc),
    ?assertEqual(OwnerBalanceAfterCreate,
                 OwnerBalance + binary_to_integer(AmountDelta) + binary_to_integer(RefundDelta)),

    %% Convert the contract id into the matching account id
    {_, ContractPubKey} = aeapi:decode(ContractPubKeyEnc),
    ContractAccountPubKey = aeapi:format_account_pubkey(ContractPubKey),

    %% Fetch the contract Balance
    %% Temp switch to the native http api until we have the balance Rosetta Op
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    {ok, 200, #{<<"balance">> := ContractBalanceAfterCreate}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ContractAccountPubKey),
    aecore_suite_utils:use_rosetta(),
    #{<<"amount">> := #{<<"value">> := ContractDelta},
      <<"account">> := #{<<"address">> := ContractAcc}} =
        ToContractOp,
    ?assertEqual(ContractAcc, ContractAccountPubKey),
    ?assertEqual(ContractBalanceAfterCreate, 0 + binary_to_integer(ContractDelta)),

    %% ------------------ Contract Call ---------------------------
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    %% Call the contract, sending 15000 of the 20000 balance to the To Account
    %% This should generate a Chain.spend trace within the contract execution
    Args = [aeapi:format_account_pubkey(ToPubKey), "15000"],
    {ok, CallData} = encode_call_data(spend_test, "spend", Args),
    ContractCallEncoded =
        #{caller_id => aeapi:format_account_pubkey(OwnerPubKey),
          contract_id => ContractPubKeyEnc,
          call_data => CallData,
          abi_version => aect_test_utils:latest_sophia_abi_version(),
          amount => 0,      %% FIXME: Use a payable entry point to test this non 0
          gas => 100000,    %May need a lot of gas
          gas_price => aec_test_utils:min_gas_price(),
          fee => 800000 * aec_test_utils:min_gas_price(),
          nonce => Nonce + 1},

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} =
        aehttp_integration_SUITE:get_contract_call(ContractCallEncoded),
    ContractCallTxHash =
        aehttp_integration_SUITE:sign_and_post_tx(EncodedUnsignedContractCallTx, OwnerPrivKey),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ContractCallTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := OwnerBalanceAfterCall}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAccountPubKey),
    {ok, 200, #{<<"balance">> := ContractBalanceAfterCall}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ContractAccountPubKey),
    {ok, 200, #{<<"balance">> := ToBalanceAfterCall}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ToAccountPubKey),

    %% Switch back to the Rosetta API root path
    aecore_suite_utils:use_rosetta(),

    %% Seems that mine_blocks_until_txs_on_chain always stops at the block
    %% containing the Tx. Or maybe this is a race condition??
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash1}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, CallTransaction]}}} =
        get_block_sut(TopKeyBlockHash1),

    %% Expect
    %% CallerAccount -= Fees
    %% ContractAccount -= 15000
    %% ToAccount += 15000
    #{<<"operations">> :=
          [CallerFeeOp, ContractAmountOp, FromContractOp, ToOp, CallRefundOp]} =
        CallTransaction,
    #{<<"amount">> := #{<<"value">> := ContractCallDelta},
      <<"account">> := #{<<"address">> := ContractAcc}} =
        FromContractOp,
    ?assertEqual(ContractAccountPubKey, ContractAcc),
    ?assertEqual(ContractBalanceAfterCall,
                 ContractBalanceAfterCreate + binary_to_integer(ContractCallDelta)),

    #{<<"amount">> := #{<<"value">> := ToDelta}, <<"account">> := #{<<"address">> := ToAcc}} =
        ToOp,
    ?assertEqual(ToAccountPubKey, ToAcc),
    ?assertEqual(ToBalanceAfterCall, ToBalance + binary_to_integer(ToDelta)),
    #{<<"amount">> := #{<<"value">> := CallerFeeDelta},
      <<"account">> := #{<<"address">> := CallerFeeAcc}} =
        CallerFeeOp,
    #{<<"amount">> := #{<<"value">> := _ContractAmountOpDelta},
      <<"account">> := #{<<"address">> := ContractAmountOpAcc}} =
        ContractAmountOp,
    #{<<"amount">> := #{<<"value">> := CallRefundOpDelta},
      <<"account">> := #{<<"address">> := CallRefundOpAcc}} =
        CallRefundOp,
    ?assertEqual(OwnerAccountPubKey, CallerFeeAcc),
    ?assertEqual(ContractAccountPubKey, ContractAmountOpAcc),
    ?assertEqual(OwnerAccountPubKey, CallRefundOpAcc),
    ?assertEqual(OwnerBalanceAfterCall,
                 OwnerBalanceAfterCreate
                 + binary_to_integer(CallerFeeDelta)
                 + binary_to_integer(CallRefundOpDelta)),

    %% ------------------ Errored Contract Call ---------------------------
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),
    %% Call the contract, attempting to send amount 500 to a non payable entry point
    %% This should just consume all the fees but not take the amount
    ErrArgs = [aeapi:format_account_pubkey(ToPubKey), "2000"],
    {ok, CallDataErr} = encode_call_data(spend_test, "spend", ErrArgs),
    ContractCallErrEncoded =
        #{caller_id => aeapi:format_account_pubkey(OwnerPubKey),
          contract_id => ContractPubKeyEnc,
          call_data => CallDataErr,
          abi_version => aect_test_utils:latest_sophia_abi_version(),
          amount => 500,
          gas => 100000,    %May need a lot of gas
          gas_price => aec_test_utils:min_gas_price(),
          fee => 800000 * aec_test_utils:min_gas_price(),
          nonce => Nonce + 2},

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallErrTx}} =
        aehttp_integration_SUITE:get_contract_call(ContractCallErrEncoded),
    ContractCallErrTxHash =
        aehttp_integration_SUITE:sign_and_post_tx(EncodedUnsignedContractCallErrTx, OwnerPrivKey),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ContractCallErrTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := OwnerBalanceAfterErrCall}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(OwnerAccountPubKey),

    %% Switch back to the Rosetta API root path
    aecore_suite_utils:use_rosetta(),

    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash2}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, ErrCallTransaction]}}} =
        get_block_sut(TopKeyBlockHash2),

    %% Expect
    %% CallerAccount -= Fees
    #{<<"operations">> := [ErrCallerOp]} = ErrCallTransaction,
    #{<<"amount">> := #{<<"value">> := ErrCallerDelta},
      <<"account">> := #{<<"address">> := CallerAcc}} =
        ErrCallerOp,
    ?assertEqual(OwnerAccountPubKey, CallerAcc),
    ?assertEqual(OwnerBalanceAfterErrCall,
                 OwnerBalanceAfterCall + binary_to_integer(ErrCallerDelta)).

block_create_channel_tx(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),

    %% Use the native http api for the operations not yet implemented in Rosetta
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {InitiatorPubKey, InitiatorPrivKey} =
        aehttp_integration_SUITE:initialize_account(1000000000 * aec_test_utils:min_gas_price()),
    {ResponderPubKey, ResponderPrivKey} =
        aehttp_integration_SUITE:initialize_account(2000000000 * aec_test_utils:min_gas_price()),

    InitiatorAccountPubKey = aeapi:format_account_pubkey(InitiatorPubKey),
    {ok, 200, #{<<"balance">> := InitiatorBalance}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(InitiatorAccountPubKey),
    ResponderAccountPubKey = aeapi:format_account_pubkey(ResponderPubKey),
    {ok, 200, #{<<"balance">> := ResponderBalance}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ResponderAccountPubKey),

    InitiatorId = aeser_id:create(account, InitiatorPubKey),
    ResponderId = aeser_id:create(account, ResponderPubKey),

    %% ------------------ Channel Create ---------------------------
    %% Check mempool is empty before we start
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [InitiatorPubKey]),
    Accounts =
        [aec_accounts:new(Pubkey, Balance)
         || {Pubkey, Balance} <- [{InitiatorPubKey, 30}, {ResponderPubKey, 50}]],
    Trees = aec_test_utils:create_state_tree_with_accounts(Accounts, no_backend),
    StateHash = aec_trees:hash(Trees),
    Delegates =
        case aecore_suite_utils:latest_protocol_version() >= ?IRIS_PROTOCOL_VSN of
            true ->
                {[], []};
            false ->
                []
        end,
    {ok, ChannelCreateTx} =
        aesc_create_tx:new(#{initiator_id => InitiatorId,
                             initiator_amount => 3000000 * aec_test_utils:min_gas_price(),
                             responder_id => ResponderId,
                             responder_amount => 5000000 * aec_test_utils:min_gas_price(),
                             channel_reserve => 6,
                             lock_period => 5,
                             fee => ?SPEND_FEE,
                             state_hash => StateHash,
                             nonce => Nonce,
                             delegate_ids => Delegates}),

    ChannelId =
        aesc_create_tx:channel_id(
            aetx:tx(ChannelCreateTx)),
    % ChannelPubKeyEnc = aeapi:format_id(ChannelId),
    % {_, ChannelPubKey} = aeapi:decode(ChannelPubKeyEnc),
    % ChannelAccountPubKey = aeapi:format_account_pubkey(ChannelPubKey),
    SignedChannelCreateTx =
        aec_test_utils:sign_tx(ChannelCreateTx, [InitiatorPrivKey, ResponderPrivKey]),
    EncTx =
        aeapi:format_transaction(
            aetx_sign:serialize_to_binary(SignedChannelCreateTx)),
    {ok, 200, #{<<"tx_hash">> := ChannelCreateTxHash}} = post_tx(EncTx),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ChannelCreateTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := InitiatorBalanceAfterCreate}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(InitiatorAccountPubKey),
    {ok, 200, #{<<"balance">> := ResponderBalanceAfterCreate}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ResponderAccountPubKey),
    % {ok, 200, #{<<"balance">> := ChannelBalanceAfterCreate}} =
    %     aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ChannelAccountPubKey),
    %% Switch back to the Rosetta API root path
    aecore_suite_utils:use_rosetta(),

    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash}}} =
        get_status_sut(),

    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, CreateTransaction]}}} =
        get_block_sut(TopKeyBlockHash),

    %% Expect
    %% Initiator -= Fees
    %% Initiator -= initiator_amount
    %% Responder -= responder_amount
    %% Channel += initiator_amount + responder_amount
    #{<<"operations">> := [FeesOp, InitiatorOp, ResponderOp]} = CreateTransaction,
    #{<<"amount">> := #{<<"value">> := FeesDelta},
      <<"account">> := #{<<"address">> := FeesAcc}} =
        FeesOp,
    ?assertEqual(InitiatorAccountPubKey, FeesAcc),
    #{<<"amount">> := #{<<"value">> := InitiatorDelta},
      <<"account">> := #{<<"address">> := InitiatorAcc}} =
        InitiatorOp,
    ?assertEqual(InitiatorAccountPubKey, InitiatorAcc),
    ?assertEqual(InitiatorBalanceAfterCreate,
                 InitiatorBalance
                 + binary_to_integer(FeesDelta)
                 + binary_to_integer(InitiatorDelta)),
    #{<<"amount">> := #{<<"value">> := ResponderDelta},
      <<"account">> := #{<<"address">> := ResponderAcc}} =
        ResponderOp,
    ?assertEqual(ResponderAccountPubKey, ResponderAcc),
    ?assertEqual(ResponderBalanceAfterCreate,
                 ResponderBalance + binary_to_integer(ResponderDelta)),

    % FIXME: Do state channels actually have an on chain account or not?
    % #{<<"amount">> := #{<<"value">> := ChannelDelta},
    %   <<"account">> := #{<<"address">> := ChannelAcc}} =
    %     ChannelOp,
    % ?assertEqual(ChannelAccountPubKey, ChannelAcc),
    % ?assertEqual(ChannelBalanceAfterCreate,
    %               binary_to_integer(ChannelDelta)),
    %% ------------------ Channel deposit ---------------------------
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {ok, ChannelDepositTx} =
        aesc_deposit_tx:new(#{channel_id => ChannelId,
                              from_id => InitiatorId,
                              amount => 700000 * aec_test_utils:min_gas_price(),
                              fee => ?SPEND_FEE,
                              state_hash => StateHash,
                              nonce => Nonce + 1,
                              round => 2}),

    SignedChannelDepositTx =
        aec_test_utils:sign_tx(ChannelDepositTx, [InitiatorPrivKey, ResponderPrivKey]),
    EncDTx =
        aeapi:format_transaction(
            aetx_sign:serialize_to_binary(SignedChannelDepositTx)),
    {ok, 200, #{<<"tx_hash">> := ChannelDepositTxHash}} = post_tx(EncDTx),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ChannelDepositTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := InitiatorBalanceAfterDeposit}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(InitiatorAccountPubKey),

    aecore_suite_utils:use_rosetta(),
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash1}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, DepositTransaction]}}} =
        get_block_sut(TopKeyBlockHash1),

    %% Expect
    %% Initiator -= Fees
    %% Initiator -= initiator_amount
    #{<<"operations">> := [DepositFeesOp, DepositInitiatorOp]} = DepositTransaction,
    #{<<"amount">> := #{<<"value">> := DepositFeesDelta},
      <<"account">> := #{<<"address">> := DepositFeesAcc}} =
        DepositFeesOp,
    ?assertEqual(InitiatorAccountPubKey, DepositFeesAcc),
    #{<<"amount">> := #{<<"value">> := DepositInitiatorDelta},
      <<"account">> := #{<<"address">> := DepositInitiatorAcc}} =
        DepositInitiatorOp,
    ?assertEqual(InitiatorAccountPubKey, DepositInitiatorAcc),
    ?assertEqual(InitiatorBalanceAfterDeposit,
                 InitiatorBalanceAfterCreate
                 + binary_to_integer(DepositFeesDelta)
                 + binary_to_integer(DepositInitiatorDelta)),

    %% ------------------ Channel withdraw ---------------------------
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    ChannelId =
        aesc_create_tx:channel_id(
            aetx:tx(ChannelCreateTx)),

    {ok, ChannelWithdrawTx} =
        aesc_withdraw_tx:new(#{channel_id => ChannelId,
                               to_id => InitiatorId,
                               amount => 100000 * aec_test_utils:min_gas_price(),
                               fee => ?SPEND_FEE,
                               state_hash => StateHash,
                               nonce => Nonce + 2,
                               round => 3}),

    SignedChannelWithdrawTx =
        aec_test_utils:sign_tx(ChannelWithdrawTx, [InitiatorPrivKey, ResponderPrivKey]),
    EncWTx =
        aeapi:format_transaction(
            aetx_sign:serialize_to_binary(SignedChannelWithdrawTx)),
    {ok, 200, #{<<"tx_hash">> := ChannelWithdrawTxHash}} = post_tx(EncWTx),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ChannelWithdrawTxHash], 2),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := InitiatorBalanceAfterWithdraw}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(InitiatorAccountPubKey),

    aecore_suite_utils:use_rosetta(),
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash2}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, WithdrawTransaction]}}} =
        get_block_sut(TopKeyBlockHash2),

    %% Expect
    %% Initiator -= Fees
    %% Initiator -= initiator_amount
    #{<<"operations">> := [WithdrawFeesOp, WithdrawInitiatorOp]} = WithdrawTransaction,
    #{<<"amount">> := #{<<"value">> := WithdrawFeesDelta},
      <<"account">> := #{<<"address">> := WithdrawFeesAcc}} =
        WithdrawFeesOp,
    ?assertEqual(InitiatorAccountPubKey, WithdrawFeesAcc),
    #{<<"amount">> := #{<<"value">> := WithdrawInitiatorDelta},
      <<"account">> := #{<<"address">> := WithdrawInitiatorAcc}} =
        WithdrawInitiatorOp,
    ?assertEqual(InitiatorAccountPubKey, WithdrawInitiatorAcc),
    ?assertEqual(InitiatorBalanceAfterWithdraw,
                 InitiatorBalanceAfterDeposit
                 + binary_to_integer(WithdrawFeesDelta)
                 + binary_to_integer(WithdrawInitiatorDelta)),

    %% ------------------ Channel close mutual ---------------------------
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {ok, ChannelCloseMutualTx} =
        aesc_close_mutual_tx:new(#{channel_id => ChannelId,
                                   from_id => InitiatorId,
                                   initiator_amount_final => 90,
                                   responder_amount_final => 50,
                                   fee => ?SPEND_FEE,
                                   nonce => Nonce + 3}),

    SignedChannelCloseMutualTx =
        aec_test_utils:sign_tx(ChannelCloseMutualTx, [InitiatorPrivKey, ResponderPrivKey]),
    EncFPTx =
        aeapi:format_transaction(
            aetx_sign:serialize_to_binary(SignedChannelCloseMutualTx)),
    {ok, 200, #{<<"tx_hash">> := ChannelCloseMutualTxHash}} = post_tx(EncFPTx),

    {ok, [_]} = rpc(aec_tx_pool, peek, [infinity]),
    aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [ChannelCloseMutualTxHash], 4),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    {ok, 200, #{<<"balance">> := ResponderBalanceAfterCloseMutual}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(ResponderAccountPubKey),
    {ok, 200, #{<<"balance">> := InitiatorBalanceAfterCloseMutual}} =
        aehttp_integration_SUITE:get_accounts_by_pubkey_sut(InitiatorAccountPubKey),

    aecore_suite_utils:use_rosetta(),
    {ok, 200, #{<<"current_block_identifier">> := #{<<"hash">> := TopKeyBlockHash3}}} =
        get_status_sut(),
    {ok, 200, #{<<"block">> := #{<<"transactions">> := [_, CloseMutualTransaction]}}} =
        get_block_sut(TopKeyBlockHash3),

    %% Expect
    %% Initiator -= Fees
    %% Initiator += initiator_amount_final
    %% Responder += responder_amount_final
    #{<<"operations">> := [CloseMutualInitiatorOp, CloseMutualResponderOp, _LockFundsOp]} =
        CloseMutualTransaction,
    #{<<"amount">> := #{<<"value">> := CloseMutualInitiatorDelta},
      <<"account">> := #{<<"address">> := CloseMutualInitiatorAcc}} =
        CloseMutualInitiatorOp,
    ?assertEqual(InitiatorAccountPubKey, CloseMutualInitiatorAcc),

    ?assertEqual(InitiatorBalanceAfterCloseMutual,
                 InitiatorBalanceAfterWithdraw + binary_to_integer(CloseMutualInitiatorDelta)),
    #{<<"amount">> := #{<<"value">> := CloseMutualResponderDelta},
      <<"account">> := #{<<"address">> := CloseMutualResponderAcc}} =
        CloseMutualResponderOp,
    ?assertEqual(ResponderAccountPubKey, CloseMutualResponderAcc),
    ?assertEqual(ResponderBalanceAfterCloseMutual,
                 ResponderBalanceAfterCreate + binary_to_integer(CloseMutualResponderDelta)).

construction_flow(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),

    %% Use the native http api for the operations not yet implemented in Rosetta
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {FromPubKey, FromPrivKey} =
        aehttp_integration_SUITE:initialize_account(1000000000 * aec_test_utils:min_gas_price()),
    FromHexKey = list_to_binary(aeu_hex:bin_to_hex(FromPubKey)),
    FromPubKeyExt = aeapi:format_account_pubkey(FromPubKey),

    {ToPubKey, _ToPrivKey} =
        aehttp_integration_SUITE:initialize_account(2000000000 * aec_test_utils:min_gas_price()),
    ToPubKeyExt = aeapi:format_account_pubkey(ToPubKey),

    aecore_suite_utils:use_rosetta(),

    %% The standard rosetta construction flow (create and post a SpendTx) must follow all 9 steps in this test case:
    %%
    %% 1. /construction/derive - turn client generated priv key into account name
    {ok, 200, DeriveResp} = construction_derive_sut(FromHexKey),
    #{<<"address">> := Account, <<"account_identifier">> := #{<<"address">> := Account}} =
        DeriveResp,
    ?assertEqual(FromPubKeyExt, Account),

    %% 2. /construction/preprocess - turn sequence of ops into what is needed for metadata request (in our case we just need the from address)
    [From, To] = rosetta_ops(FromPubKeyExt, ToPubKeyExt, 10000),
    {ok, 200, PreprocessResp} = construction_preprocess_sut([From, To]),
    #{<<"options">> := #{<<"from">> := FromAcc}} = PreprocessResp,
    ?assertEqual(FromPubKeyExt, FromAcc),

    %% 3. /construction/metadata - query for metadata (fetch next nonce for the from address) (online)
    {ok, 200, MetadataResp} = construction_metadata_sut(FromPubKeyExt),
    #{<<"suggested_fee">> :=
          [#{<<"value">> := SuggestedFee,
             <<"currency">> := #{<<"symbol">> := <<"AE">>, <<"decimals">> := 18}}],
      <<"metadata">> := #{<<"nonce">> := NextNonce, <<"fee">> := SuggestedFee}} =
        MetadataResp,
    ?assertEqual(FromPubKeyExt, FromAcc),

    %% 4. /construction/payloads - create unsigned tx from sequence of ops and metadata
    {ok, 200, PayloadsResp} = construction_payloads_sut(NextNonce, SuggestedFee, [From, To]),
    #{<<"payloads">> := [Signer], <<"unsigned_transaction">> := UnsignedTx} = PayloadsResp,
    #{<<"account_identifier">> := #{<<"address">> := FromPubKeyExt},
      <<"hex_bytes">> := HexBytes,
      <<"signature_type">> := <<"ed25519">>} =
        Signer,

    %% 5. /construction/parse - turn unsigned tx back into sequence of ops (so we the client can check it's correct)
    {ok, 200, ParseUnsignedResp} = construction_parse_sut(UnsignedTx, false),
    #{<<"operations">> := [_FromOp, _ToOp], <<"account_identifier_signers">> := _Signers} =
        ParseUnsignedResp,

    %% 6. /construction/combine - combine client generated signature with the unsigned tx to create a signed tx object
    %% We are the client here, so we sign and send the signature to Rosetta
    HexBytesBin = aeu_hex:hex_to_bin(HexBytes),
    SignedHexBytesBin = enacl:sign_detached(HexBytesBin, FromPrivKey),
    {ok, 200, CombineResp} =
        construction_combine_sut(SignedHexBytesBin, FromPubKey, FromPubKeyExt, UnsignedTx),
    #{<<"signed_transaction">> := SignedTx} = CombineResp,

    %% 7. /construction/parse - turn signed tx back into sequence of ops (so client can check it's correct)
    {ok, 200, ParseSignedResp} = construction_parse_sut(SignedTx, true),
    #{<<"operations">> := [_FromOpBin, _ToOpBin], <<"account_identifier_signers">> := [#{<<"address">> := FromPubKeyExt}]} =
        ParseSignedResp,

    %% 8. /construction/hash - turn transaction into tx hash (so client can track progress of tx)
    {ok, 200, HashResp} = construction_hash_sut(SignedTx),
    #{<<"transaction_identifier">> := #{<<"hash">> := Hash}} = HashResp,

    %% 9. /construction/submit - post signed tx to chain (online)
    {ok, 200, SubmitResp} = construction_submit_sut(SignedTx),
    #{<<"transaction_identifier">> := #{<<"hash">> := Hash}} = SubmitResp,

    {ok, N} = aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [Hash], 10),
    ?assert(N > 0).

construction_rosetta_cli(Config) ->
    [{_NodeId, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:reinit_with_ct_consensus(?NODE),
    ToMine = max(2, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_key_blocks(Node, ToMine),
    {ok, [KeyBlock]} = aecore_suite_utils:mine_key_blocks(Node, 1),
    true = aec_blocks:is_key_block(KeyBlock),

    %% Use the native http api for the operations not yet implemented in Rosetta
    SwaggerVsn = proplists:get_value(swagger_version, Config, oas3),
    aecore_suite_utils:use_swagger(SwaggerVsn),

    {_Pubkey, _Privkey} =
        aehttp_integration_SUITE:initialize_account(1000000000 * aec_test_utils:min_gas_price(),
                                                    {?FUNDED_PUBKEY, ?FUNDED_PRIVKEY}),
    {_Pubkey2, _Privkey2} =
        aehttp_integration_SUITE:initialize_account(1000000000 * aec_test_utils:min_gas_price(),
                                                    {?FUNDED_PUBKEY2, ?FUNDED_PRIVKEY2}),

    ConfigFile = filename:join([code:lib_dir(aehttp), "test", "rosetta", "rosetta.cfg"]),
    Cmd = "rosetta-cli --result-file ros-res --configuration-file " ++ ConfigFile ++ " check:construction",
    Pid = spawn(fun() -> mine_many(Node) end),
    Result = os:cmd(Cmd),
    ct:log("rosetta-cli result ~s", [Result]),
    exit(Pid, kill),
    ?assert(string:str(Result, "Success") > 0).

mine_many(Node) ->
    timer:sleep(2000),
    aecore_suite_utils:mine_micro_blocks(Node, 1),
    aecore_suite_utils:mine_key_blocks(Node, 1),
    mine_many(Node).

%% ============================================================
%% Internal
%% ============================================================
get_block_sut(Hash) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          block_identifier => #{hash => Hash}},
    http_request(Host, post, "block", Body).

get_block_at_height_sut(Height) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          block_identifier => #{index => Height}},
    http_request(Host, post, "block", Body).

get_block_transaction_sut(KeyBlockHash, Height, TxHash) ->
    Host = rosetta_address(),
    Body =
        #{block_identifier => #{hash => KeyBlockHash, index => Height},
          network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          transaction_identifier => #{hash => TxHash}},
    http_request(Host, post, "block/transaction", Body).

get_balance_sut(AccountPubKey) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          account_identifier => #{address => AccountPubKey}},
    http_request(Host, post, "account/balance", Body).

get_balance_at_hash_sut(AccountPubKey, Hash) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          block_identifier => #{hash => Hash},
          account_identifier => #{address => AccountPubKey}},
    http_request(Host, post, "account/balance", Body).

get_balance_at_height_sut(AccountPubKey, Height) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          block_identifier => #{index => Height},
          account_identifier => #{address => AccountPubKey}},
    http_request(Host, post, "account/balance", Body).

construction_derive_sut(HexBytes) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          public_key => #{curve_type => <<"edwards25519">>, hex_bytes => HexBytes}},
    http_request(Host, post, "construction/derive", Body).

construction_preprocess_sut([From, To]) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          operations => [From, To]},
    http_request(Host, post, "construction/preprocess", Body).

construction_metadata_sut(FromPubKeyExt) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          options => #{from => FromPubKeyExt}},
    http_request(Host, post, "construction/metadata", Body).

construction_payloads_sut(Nonce, Fee, [From, To]) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          metadata => #{nonce => Nonce, fee => Fee},
          operations => [From, To]},
    http_request(Host, post, "construction/payloads", Body).

construction_parse_sut(TxBin, Signed) when is_boolean(Signed) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          signed => Signed,
          transaction => TxBin},
    http_request(Host, post, "construction/parse", Body).

construction_hash_sut(SignedTx) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
        signed_transaction => SignedTx},
    http_request(Host, post, "construction/hash", Body).

construction_submit_sut(SignedTx) ->
    Host = rosetta_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
        signed_transaction => SignedTx},
    http_request(Host, post, "construction/submit", Body).

construction_combine_sut(Signature, PubKey, From, UnsignedTx) ->
    Host = rosetta_offline_address(),
    Body =
        #{network_identifier =>
              #{blockchain => <<"aeternity">>, network => aec_governance:get_network_id()},
          signatures =>
              [#{hex_bytes => list_to_binary(aeu_hex:bin_to_hex(Signature)),
                 public_key =>
                     #{curve_type => <<"edwards25519">>,
                       hex_bytes => list_to_binary(aeu_hex:bin_to_hex(PubKey))},
                 signature_type => <<"ed25519">>,
                 signing_payload =>
                     #{account_identifier => #{address => From},
                       address => From,
                       hex_bytes => <<>>,
                       signature_type => <<"ed25519">>}}],
          unsigned_transaction => UnsignedTx},
    http_request(Host, post, "construction/combine", Body).

rosetta_ops(From, To, Amount) ->
    FromOp =
        #{account => #{address => From},
          amount =>
              #{currency => #{decimals => 18, symbol => <<"AE">>},
                value => integer_to_binary(-Amount)},
          operation_identifier => #{index => 0},
          type => <<"Spend.amount">>},
    ToOp =
        #{account => #{address => To},
          amount =>
              #{currency => #{decimals => 18, symbol => <<"AE">>},
                value => integer_to_binary(Amount)},
          operation_identifier => #{index => 1},
          type => <<"Spend.amount">>},
    [FromOp, ToOp].

sign_tx(Tx, Privkey) ->
    STx = aec_test_utils:sign_tx(Tx, [Privkey]),
    aeapi:format_transaction(
        aetx_sign:serialize_to_binary(STx)).

post_tx(Tx) ->
    aehttp_integration_SUITE:post_transactions_sut(Tx).

contract_byte_code(ContractName) ->
    {ok, BinCode} = aect_test_utils:compile_contract(ContractName),
    aeapi:format_contract_bytearray(BinCode).

contract_code(ContractName) ->
    {ok, BinSrc} = aect_test_utils:read_contract(ContractName),
    BinSrc.

encode_call_data(Name, Fun, Args) when is_atom(Name) ->
    encode_call_data(contract_code(Name), Fun, Args);
encode_call_data(Src, Fun, Args) ->
    {ok, CallData} = aect_test_utils:encode_call_data(Src, Fun, Args),
    {ok, aeapi:format_contract_bytearray(CallData)}.

%% NOTES from a real world Tx
%% Balance of SpendTx to self of 20000 aettos at height.
%% 581191: {"balance":6542226359999999997,"id":"ak_wTPFpksUJFjjntonTvwK4LJvDw11DPma7kZBneKbumb8yPeFq","kind":"basic","nonce":7327720,"payable":true}
%% 581192: {"balance":6542168419999999997,"id":"ak_wTPFpksUJFjjntonTvwK4LJvDw11DPma7kZBneKbumb8yPeFq","kind":"basic","nonce":7327723,"payable":true}
%% Difference: 6542226359999999997 - 6542168419999999997 = 57940000000000
%% Fee per Tx is around 0.00001932 AE or 19320000000000 aetto
%% So this account must have had 3 Spend TX in this generation
%% Confirmed by https://explorer.aeternity.io/generations/581191
