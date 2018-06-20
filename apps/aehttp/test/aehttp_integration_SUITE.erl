-module(aehttp_integration_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("stdlib/include/assert.hrl").
%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% Endpoint calls
-export([]).

%% off chain endpoints
-export(
   [test_encode_decode_sophia_data/1,
    test_encode_decode_sophia_data2/1,
    broken_decode_sophia_data/1
   ]).

%% test case exports
%% external endpoints
-export(
   [
    % get block-s
    get_top_empty_chain/1,
    get_top_non_empty_chain/1,
    block_genesis/1,
    block_pending/1,
    block_latest/1,
    block_by_height/1,
    block_not_found_by_height/1,
    block_by_hash/1,
    block_not_found_by_broken_hash/1,
    block_not_found_by_hash/1,

    % non signed txs
    contract_transactions/1,
    contract_create_transaction_init_error/1,
    oracle_transactions/1,
    nameservice_transactions/1,
    spend_transaction/1,
    state_channels_onchain_transactions/1,
    unknown_atom_in_spend_tx/1,

    get_transaction/1,

    % sync gossip
    pending_transactions/1,
    post_correct_blocks/1,
    post_broken_blocks/1,
    post_correct_tx/1,
    post_broken_tx/1,
    post_broken_base58_tx/1,

    % balances
    all_accounts_balances/1,
    all_accounts_balances_empty/1,
    all_accounts_balances_disabled/1,
    balance/1,
    balance_negative_cases/1,

    % infos
    version/1,
    info_disabled/1,
    info_empty/1,
    info_more_than_30/1,
    info_less_than_30/1,

    peer_pub_key/1
   ]).

%%
%% test case exports
%% internal endpoints
-export(
   [
    broken_spend_tx/1,
    miner_pub_key/1,

    %% requested Endpoints
    block_number/1,

    block_txs_count_by_height/1,
    block_txs_count_by_hash/1,
    block_txs_count_genesis/1,
    block_txs_count_latest/1,
    block_txs_count_pending/1,

    block_txs_count_by_height_not_found/1,
    block_txs_count_by_hash_not_found/1,
    block_txs_count_by_broken_hash/1,

    block_tx_index_by_height/1,
    block_tx_index_by_hash/1,
    block_tx_index_latest/1,
    block_tx_index_not_founds/1,

    block_txs_list_by_height/1,
    block_txs_list_by_hash/1,
    block_txs_list_by_height_invalid_range/1,
    block_txs_list_by_hash_invalid_range/1,

    naming_system_manage_name/1,
    naming_system_broken_txs/1,

    list_oracles/1,
    list_oracle_queries/1,

    peers/1
   ]).

%% test case exports
%% for swagger validation errors
-export([
    swagger_validation_body/1,
    swagger_validation_enum/1,
    swagger_validation_required/1,
    swagger_validation_schema/1,
    swagger_validation_types/1
    ]).

%%
%% test case exports
%% wrong http method for all endpoints
-export([
    wrong_http_method_top/1,
    wrong_http_method_contract_create/1,
    wrong_http_method_contract_call/1,
    wrong_http_method_contract_call_compute/1,
    wrong_http_method_spend/1,
    wrong_http_method_oracle_register/1,
    wrong_http_method_oracle_extend/1,
    wrong_http_method_oracle_query/1,
    wrong_http_method_oracle_response/1,
    wrong_http_method_name_preclaim/1,
    wrong_http_method_name_claim/1,
    wrong_http_method_name_update/1,
    wrong_http_method_name_transfer/1,
    wrong_http_method_name_revoke/1,
    wrong_http_method_block_by_height/1,
    wrong_http_method_block_by_hash/1,
    wrong_http_method_header_by_hash/1,
    wrong_http_method_transactions/1,
    wrong_http_method_tx_id/1,
    wrong_http_method_spend_tx/1,
    wrong_http_method_name_preclaim_tx/1,
    wrong_http_method_name_claim_tx/1,
    wrong_http_method_name_update_tx/1,
    wrong_http_method_name_transfer_tx/1,
    wrong_http_method_name_revoke_tx/1,
    wrong_http_method_commitment_hash/1,
    wrong_http_method_name/1,
    wrong_http_method_balance/1,
    wrong_http_method_block/1,
    wrong_http_method_tx/1,
    wrong_http_method_all_accounts_balances/1,
    wrong_http_method_miner_pub_key/1,
    wrong_http_method_version/1,
    wrong_http_method_info/1,
    wrong_http_method_block_number/1,
    wrong_http_method_block_latest/1,
    wrong_http_method_block_genesis/1,
    wrong_http_method_block_pending/1,
    wrong_http_method_block_txs_count_by_height/1,
    wrong_http_method_block_txs_count_by_hash/1,
    wrong_http_method_block_txs_count_latest/1,
    wrong_http_method_block_txs_count_genesis/1,
    wrong_http_method_block_txs_count_pending/1,
    wrong_http_method_block_tx_by_index_height/1,
    wrong_http_method_block_tx_by_index_hash/1,
    wrong_http_method_block_tx_by_index_latest/1,
    wrong_http_method_block_txs_list_by_height/1,
    wrong_http_method_block_txs_list_by_hash/1,
    wrong_http_method_list_oracles/1,
    wrong_http_method_list_oracle_queries/1,
    wrong_http_method_peers/1
    ]).

%%
%% test case exports
%% websocket endpoints
-export(
   [ws_get_genesis/1,
    ws_request_tag/1,
    ws_block_mined/1,
    ws_refused_on_limit_reached/1,
    ws_tx_on_chain/1,
    ws_oracles/1
   ]).

%% channel websocket endpoints
-export(
   [sc_ws_timeout_open/1,
    sc_ws_open/1,
    sc_ws_update/1,
    sc_ws_update_fails_and_close/1,
    sc_ws_send_messages_and_close/1,
    sc_ws_conflict_and_close/1,
    sc_ws_close/1,
    sc_ws_close_mutual_inititator/1,
    sc_ws_close_mutual_responder/1
   ]).


-include_lib("common_test/include/ct.hrl").
-define(NODE, dev1).
-define(DEFAULT_TESTS_COUNT, 5).
-define(WS, aehttp_ws_test_utils).
-define(BOGUS_STATE_HASH, <<42:32/unit:8>>).

all() ->
    [
     {group, all_endpoints}
    ].

groups() ->
    [
     {all_endpoints, [sequence], [{group, off_chain_endpoints},
                                  {group, external_endpoints},
                                  {group, internal_endpoints},
                                  {group, swagger_validation},
                                  {group, wrong_http_method_endpoints},
                                  {group, websocket},
                                  {group, naming},
                                  {group, channel_websocket}
                                  ]},
     {off_chain_endpoints, [],
      [
       test_encode_decode_sophia_data,
       test_encode_decode_sophia_data2,
       broken_decode_sophia_data
      ]},
     {external_endpoints, [sequence],
      [
        % get block-s
        get_top_empty_chain,
        get_top_non_empty_chain,
        block_genesis,
        block_pending,
        block_latest,

        block_by_height,
        block_not_found_by_height,
        block_by_hash,
        block_not_found_by_hash,

        % non signed txs
        contract_transactions,
        contract_create_transaction_init_error,
        oracle_transactions,
        nameservice_transactions,
        spend_transaction,
        state_channels_onchain_transactions,
        unknown_atom_in_spend_tx,

        get_transaction,

        % sync gossip
        pending_transactions,
        post_correct_blocks,
        post_broken_blocks,
        post_correct_tx,
        post_broken_tx,
        post_broken_base58_tx,

        % balances
        all_accounts_balances,
        all_accounts_balances_empty,
        all_accounts_balances_disabled,
        balance,
        balance_negative_cases,

        % infos
        version,
        info_disabled,
        info_empty,
        info_more_than_30,
        info_less_than_30,

        peer_pub_key
      ]},
     {internal_endpoints, [sequence],
      [
        broken_spend_tx,
        naming_system_broken_txs,
        miner_pub_key,

        %% requested Endpoints
        block_number,

        block_txs_count_by_height,
        block_txs_count_by_hash,
        block_txs_count_genesis,
        block_txs_count_latest,
        block_txs_count_pending,

        block_txs_count_by_height_not_found,
        block_txs_count_by_hash_not_found,
        block_txs_count_by_broken_hash,

        block_tx_index_by_height,
        block_tx_index_by_hash,
        block_tx_index_latest,
        block_tx_index_not_founds,

        block_txs_list_by_height,
        block_txs_list_by_hash,
        block_txs_list_by_height_invalid_range,
        block_txs_list_by_hash_invalid_range,

        list_oracles,
        list_oracle_queries,

        peers
      ]},
     {swagger_validation, [], [
        swagger_validation_body,
        swagger_validation_enum,
        swagger_validation_required,
        swagger_validation_schema,
        swagger_validation_types
      ]},
     {wrong_http_method_endpoints, [], [
        wrong_http_method_top,
        wrong_http_method_contract_create,
        wrong_http_method_contract_call,
        wrong_http_method_contract_call_compute,
        wrong_http_method_spend,
        wrong_http_method_oracle_register,
        wrong_http_method_oracle_extend,
        wrong_http_method_oracle_query,
        wrong_http_method_oracle_response,
        wrong_http_method_name_preclaim,
        wrong_http_method_name_claim,
        wrong_http_method_name_update,
        wrong_http_method_name_transfer,
        wrong_http_method_name_revoke,
        wrong_http_method_block_by_height,
        wrong_http_method_block_by_hash,
        wrong_http_method_header_by_hash,
        wrong_http_method_transactions,
        wrong_http_method_tx_id,
        wrong_http_method_spend_tx,
        wrong_http_method_name_preclaim_tx,
        wrong_http_method_name_claim_tx,
        wrong_http_method_name_update_tx,
        wrong_http_method_name_transfer_tx,
        wrong_http_method_name_revoke_tx,
        wrong_http_method_commitment_hash,
        wrong_http_method_name,
        wrong_http_method_balance,
        wrong_http_method_block,
        wrong_http_method_tx,
        wrong_http_method_all_accounts_balances,
        wrong_http_method_miner_pub_key,
        wrong_http_method_version,
        wrong_http_method_info,
        wrong_http_method_block_number,
        wrong_http_method_block_latest,
        wrong_http_method_block_genesis,
        wrong_http_method_block_pending,
        wrong_http_method_block_txs_count_by_height,
        wrong_http_method_block_txs_count_by_hash,
        wrong_http_method_block_txs_count_latest,
        wrong_http_method_block_txs_count_genesis,
        wrong_http_method_block_txs_count_pending,
        wrong_http_method_block_tx_by_index_height,
        wrong_http_method_block_tx_by_index_hash,
        wrong_http_method_block_tx_by_index_latest,
        wrong_http_method_block_txs_list_by_height,
        wrong_http_method_block_txs_list_by_hash,
        wrong_http_method_list_oracles,
        wrong_http_method_list_oracle_queries,
        wrong_http_method_peers
     ]},
     {naming, [sequence],
      [naming_system_manage_name
      ]},
     {websocket, [sequence],
      [ws_get_genesis,
       ws_request_tag,
       ws_block_mined,
       ws_refused_on_limit_reached,
       ws_tx_on_chain,
       ws_oracles
      ]},
     {channel_websocket, [sequence],
      [sc_ws_timeout_open,
       sc_ws_open,
       sc_ws_update,
       sc_ws_close,
      % ensure port is reusable
       sc_ws_open,
       sc_ws_update_fails_and_close,
       sc_ws_open,
       sc_ws_send_messages_and_close,
       sc_ws_open,
       sc_ws_conflict_and_close,
      % initiator can start close mutual
       sc_ws_open,
       sc_ws_update,
       sc_ws_close_mutual_inititator,
       % responder can start close mutual
       sc_ws_open,
       sc_ws_update,
       sc_ws_close_mutual_responder
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    ok = application:ensure_started(erlexec),
    DataDir = ?config(data_dir, Config),
    TopDir = aecore_suite_utils:top_dir(DataDir),
    Config1 = [{symlink_name, "latest.http_endpoints"},
               {top_dir, TopDir},
               {test_module, ?MODULE}] ++ Config,
    aecore_suite_utils:make_shortcut(Config1),
    ct:log("Environment = ~p", [[{args, init:get_arguments()},
                                 {node, node()},
                                 {cookie, erlang:get_cookie()}]]),
    Forks = aecore_suite_utils:forks(),

    aecore_suite_utils:create_configs(Config1, #{<<"chain">> =>
                                                 #{<<"persist">> => true,
                                                   <<"hard_forks">> => Forks}}),
    aecore_suite_utils:make_multi(Config1, [?NODE]),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config1.

end_per_suite(_Config) ->
    ok.

init_per_group(all_endpoints, Config) ->
    Config;
init_per_group(channel_websocket, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    {ok, 404, _} = get_balance_at_top(),
    %% prepare participants
    {IPubkey, IPrivkey} = generate_key_pair(),
    {RPubkey, RPrivkey} = generate_key_pair(),
    IStartAmt = 50,
    RStartAmt = 50,
    Fee = 1,
    BlocksToMine = 1,

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), BlocksToMine),

    {ok, 200, _} = post_spend_tx(IPubkey, IStartAmt, Fee),
    {ok, 200, _} = post_spend_tx(RPubkey, RStartAmt, Fee),
    {ok, [Block]} = aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    [_Spend1, _Spend2] = aec_blocks:txs(Block),
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),
    Participants = #{initiator => #{pub_key => IPubkey,
                                    priv_key => IPrivkey,
                                    start_amt => IStartAmt},
                     responder => #{pub_key => RPubkey,
                                    priv_key => RPrivkey,
                                    start_amt => RStartAmt}},
    [{participants, Participants} | Config];
init_per_group(_Group, Config) ->
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    ToMine = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ToMine),
    Config.

end_per_group(all_endpoints, _Config) ->
    ok;
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

%% off chain endpoints
test_encode_decode_sophia_data(_Config) ->
    CallEncodedInteger42 = encode_data(<<"(42,2)">>),
    Decoded = decode_data(<<"(string, (int, int))">>,
                          CallEncodedInteger42),
    #{<<"data">> :=
          #{<<"type">> := <<"tuple">>,
            <<"value">> :=
                [#{<<"type">>  := <<"string">>,
                   <<"value">> := <<"foo">>},
                 #{<<"type">>  := <<"tuple">>,
                   <<"value">> :=
                       [#{<<"type">> := <<"word">>,
                          <<"value">> := 42},
                        _]}
                ]
           }
     }
        = Decoded,

    ok.

test_encode_decode_sophia_data2(_Config) ->
    CD =            <<"(\"Hello\", [1,2,3], Some(true))">>,
    Type = <<"(string, (string, list(int), option(bool)))">>,
    CallEncoded = encode_data(CD),
    Decoded = decode_data(Type, CallEncoded),
    #{<<"data">> :=
          #{ <<"type">> := <<"tuple">>
           , <<"value">> :=
                 [#{<<"type">>   := <<"string">>
                   , <<"value">> := <<"foo">> }
                 , #{ <<"type">>  := <<"tuple">>
                    , <<"value">> :=
                          [ #{ <<"type">> := <<"string">>
                             , <<"value">> := <<"Hello">> }
                          , #{ <<"type">> := <<"list">>
                             , <<"value">> :=
                                   [_,_,_]}
                          , #{ <<"type">> := <<"option">>
                             , <<"value">> :=
                                   #{ <<"type">> := <<"word">>
                                    , <<"value">> := 1}}]}
                  ]
           }
     } = Decoded,

    ok.

broken_decode_sophia_data(_Config) ->
    %% Sanity check on happy path.
    D = encode_data(<<"bar">>, <<"42">>),
    T = <<"(string, int)">>,
    {ok, 200, #{<<"data">> := _}} = get_contract_decode_data(#{'sophia-type' => T, data => D}),
    %% Missing field.
    lists:foreach(
      fun({Req, ExpMissingField}) ->
              {ok, 400, Body} = get_contract_decode_data(Req),
              ?assertMatch(
                 #{<<"reason">> := <<"validation_error">>,
                   <<"parameter">> := <<"body">>,
                   <<"info">> := #{<<"error">> := <<"missing_required_property">>,
                                   <<"data">> := ActMissingField}
                  } when ActMissingField =:= ExpMissingField,
                 Body)
      end,
      [ { #{data => D}, <<"sophia-type">>}
      , { #{'sophia-type' => T}, <<"data">>}
      , { #{type => T, data => D}, <<"sophia-type">>}
      ]),
    %% Field invalid according to schema.
    lists:foreach(
      fun({Req, ExpWrongField, ExpWrongValue}) ->
              {ok, 400, Body} = get_contract_decode_data(Req),
              ?assertMatch(
                 #{<<"reason">> := <<"validation_error">>,
                   <<"parameter">> := <<"body">>,
                   <<"info">> := #{<<"error">> := <<"wrong_type">>,
                                   <<"path">> := [ActWrongField],
                                   <<"data">> := ActWrongValue}
                  } when (ActWrongField =:= ExpWrongField) andalso
                         (ActWrongValue =:= ExpWrongValue),
                 Body)
      end,
      [ { #{'sophia-type' => 42, data => D}, <<"sophia-type">>, 42}
      , { #{'sophia-type' => T, data => 42}, <<"data">>, 42}
      ]),
    %% Field valid according to schema but invalid for handler.
    {ok, 400, #{<<"reason">> := <<"bad_type">>}} = get_contract_decode_data(#{'sophia-type' => <<"foo">>, data => D}),
    {ok, 400, #{<<"reason">> := <<"bad argument">>}} = get_contract_decode_data(#{'sophia-type' => T, data => <<"foo">>}),
    %% Field valid for both schema and handler, though data
    %% interpreted in a different way than the specified type spec.
    {ok, 200,
     #{<<"data">> :=
           #{<<"type">> := <<"tuple">>,
             <<"value">> :=
                 [#{<<"type">> := <<"string">>, <<"value">> := <<"bar">>},
                  #{<<"type">> := <<"word">>  , <<"value">> := 192} ]}}
    } = get_contract_decode_data(#{'sophia-type' => <<"(string, (int))">>, data => D}),
    {ok, 200,
     #{<<"data">> :=
           #{<<"type">> := <<"tuple">>,
             <<"value">> :=
                 [#{<<"type">> := <<"string">>, <<"value">> := <<"bar">>},
                  #{<<"type">> := <<"word">>, <<"value">> := 192} ]}}
    } = get_contract_decode_data(#{'sophia-type' => T, data => encode_data(<<"bar">>, <<"(42)">>)}),
    ok.


encode_data(Term) ->
    encode_data(<<"foo">>, Term).

encode_data(Function, Term) ->
    {ok, HexData} = aect_sophia:encode_call_data(<<>>,
                                              Function,
                                              Term),
    HexData.

decode_data(Type, EncodedData) ->
    {ok, 200, Data} =
        get_contract_decode_data(#{ 'sophia-type' => Type,
                                    data => EncodedData}),
    Data.


%% enpoints
get_top_empty_chain(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    {ok, 200, HeaderMap} = get_top(),
    ct:log("~p returned header = ~p", [?NODE, HeaderMap]),
    {ok, 200, GenBlockMap} = get_block_by_height(0),
    {ok, GenBlock} = aehttp_api_parser:decode(block, GenBlockMap),
    ExpectedMap = header_to_endpoint_top(aec_blocks:to_header(GenBlock)),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    ?assertEqual(ExpectedMap, HeaderMap),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

get_top_non_empty_chain(_Config) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    ExpectedH = rpc(aec_chain, top_header, []),
    ExpectedMap = header_to_endpoint_top(ExpectedH),
    ct:log("Cleaned top header = ~p", [ExpectedMap]),
    {ok, 200, HeaderMap} = get_top(),
    ?assertEqual(ExpectedMap, HeaderMap),
    #{<<"height">> := Height} = HeaderMap,
    true = Height > 0,
    ok.

block_by_height(_Config) ->
    GetExpectedBlockFun =
        fun(H) -> rpc(aec_chain, get_block_by_height, [H]) end,
    CallApiFun = fun get_block_by_height/2,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

block_not_found_by_height(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    lists:foreach(
        fun(H) ->
            lists:foreach(
                fun(Opt) ->
                    {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_block_by_height(H, Opt)
                end,
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),

    ToMine = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ToMine),
    ok.

block_not_found_by_hash(_Config) ->
    lists:foreach(
        fun(_Height) ->
            lists:foreach(
                fun(Opt) ->
                    H = random_hash(),
                    error = rpc(aec_chain, get_block, [H]),
                    Hash = aec_base58c:encode(block_hash, H),
                    {ok, 404, #{<<"reason">> := <<"Block not found">>}}
                        = get_block_by_hash(Hash, Opt)
                end,
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

block_not_found_by_broken_hash(_Config) ->
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            lists:foreach(
                fun(Opt) ->
                    {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} =
                        get_block_by_hash(BrokenHash, Opt)
                end,
                [default, message_pack, json])
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

block_by_hash(_Config) ->
    GetExpectedBlockFun =
        fun(H) -> rpc(aec_chain, get_block_by_height, [H]) end,
    CallApiFun =
        fun(H, Opts) ->
            {ok, Hash} = block_hash_by_height(H),
            get_block_by_hash(Hash, Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

%% tests the following
%% GET contract_create_tx unsigned transaction
%% GET contract_call_tx unsigned transaction
%% due to complexity of contract_call_tx (needs a contract in the state tree)
%% both positive and negative cases are tested in this test
contract_transactions(_Config) ->    % miner has an account
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    SophiaCode = <<"contract Identity = function main (x:int) = x">>,
    {ok, 200, #{<<"bytecode">> := Code}} = get_contract_bytecode(SophiaCode),

    % contract_create_tx positive test
    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aect_sophia:encode_call_data(Code,
                                     InitFunction,
                                     InitArgument),

    ValidEncoded = #{ owner => MinerAddress,
                      code => Code,
                      vm_version => 1,
                      deposit => 2,
                      amount => 1,
                      gas => 300,
                      gas_price => 1,
                      fee => 1,
                      call_data => EncodedInitCallData},

    ValidDecoded = maps:merge(ValidEncoded,
                              #{owner => MinerPubkey,
                                code => aeu_hex:hexstring_decode(Code),
                                call_data => aeu_hex:hexstring_decode(EncodedInitCallData)}),

    unsigned_tx_positive_test(ValidDecoded, ValidEncoded, fun get_contract_create/1,
                               fun aect_create_tx:new/1, MinerPubkey),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
                <<"contract_address">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aec_base58c:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(MinerAddress, EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 400, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCreateTxHash),

    {ok, 404, #{}} = get_contract_poi(EncodedContractPubKey),

    % mine a block
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get the contract init call object
    {ok, 200, InitCallObject} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_address">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
        maps:get(<<"contract_address">>, InitCallObject)),
    ?assertEqual(maps:get(gas_price, ValidDecoded), maps:get(<<"gas_price">>, InitCallObject)),
    ?assertMatch({Used, Limit} when
        is_integer(Used) andalso
            is_integer(Limit) andalso
            Limit > 0 andalso
            Used =< Limit,
        {maps:get(<<"gas_used">>, InitCallObject), maps:get(gas, ValidDecoded)}
    ),
    ?assertEqual(<<"ok">>, maps:get(<<"return_type">>, InitCallObject)),
    ?assertMatch(_, maps:get(<<"return_value">>, InitCallObject)),

    {ok, 200, #{<<"poi">> := EncPoI}} = get_contract_poi(EncodedContractPubKey),
    {ok, PoIBin} = aec_base58c:safe_decode(poi, EncPoI),
    PoI = aec_trees:deserialize_poi(PoIBin),
    {ok, ContractInPoI} = aec_trees:lookup_poi(contracts, ContractPubKey, PoI),
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ContractInTree = rpc(aect_state_tree, get_contract, [ContractPubKey,
                                                         aec_trees:contracts(Trees)]),
    {ContractInPoI, _} = {ContractInTree, ContractInPoI},

    Function = <<"main">>,
    Argument = <<"42">>,
    {ok, EncodedCallData} =
        aect_sophia:encode_call_data(Code,
                                     Function,
                                     Argument),


    ContractCallEncoded = #{ caller => MinerAddress,
                             contract => EncodedContractPubKey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 1,
                             call_data => EncodedCallData},

    ContractCallDecoded = maps:merge(ContractCallEncoded,
                              #{caller => MinerPubkey,
                                contract => ContractPubKey,
                                call_data => aeu_hex:hexstring_decode(EncodedCallData)}),

    unsigned_tx_positive_test(ContractCallDecoded, ContractCallEncoded,
                               fun get_contract_call/1,
                               fun aect_call_tx:new/1, MinerPubkey),

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallTx}} = get_contract_call(ContractCallEncoded),
    ContractCallTxHash = sign_and_post_tx(MinerAddress, EncodedUnsignedContractCallTx),

    %% Try to get the call object while in mempool
    {ok, 400, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCallTxHash),

    % mine a block
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ?assert(tx_in_chain(ContractCallTxHash)),

    %% Get the call object
    {ok, 200, CallObject} = get_contract_call_object(ContractCallTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_address">>, CallObject, <<>>)),
    ?assertEqual(get_tx_nonce(ContractCallTxHash), maps:get(<<"caller_nonce">>, CallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
                 maps:get(<<"contract_address">>, CallObject, <<>>)),
    ?assertEqual(maps:get(gas_price, ContractCallDecoded), maps:get(<<"gas_price">>, CallObject)),
    ?assertMatch({Used, Limit} when
      is_integer(Used) andalso
      is_integer(Limit) andalso
      Limit > 0 andalso
      Used =< Limit,
      {maps:get(<<"gas_used">>, CallObject), maps:get(gas, ContractCallDecoded)}
      ),
    ?assertEqual(<<"ok">>, maps:get(<<"return_type">>, CallObject)),

    %% Test to call the contract without a transaction.
    {ok, 200, #{<<"out">> := DirectCallResult}} =
        call_contract_directly(#{<<"abi">> => <<"sophia-address">>,
                                 <<"code">> => EncodedContractPubKey,
                                 <<"function">> => Function,
                                 <<"arg">> => Argument}),
    ReturnBin = maps:get(<<"return_value">>, CallObject),
    {ok, DecodedReturnValue} = aehttp_logic:contract_decode_data(<<"int">>, ReturnBin),
    {ok, 200, #{<<"data">> := DecodedCallResult}} =
        get_contract_decode_data(
          #{ 'sophia-type' => <<"int">>,
             data => DirectCallResult}),
    ?assertEqual(DecodedReturnValue, DecodedCallResult),
    #{<<"value">> := 42} = DecodedReturnValue,

    ComputeCCallEncoded = #{ caller => MinerAddress,
                             contract => EncodedContractPubKey,
                             vm_version => 1,
                             amount => 1,
                             gas => 1000,
                             gas_price => 1,
                             fee => 1,
                             function => Function,
                             arguments => Argument},

    {ok, EncodedCallData} = aect_sophia:encode_call_data(Code, Function,
                                                         Argument),
    ComputeCCallDecoded = maps:merge(ComputeCCallEncoded,
                              #{caller => MinerPubkey,
                                contract => ContractPubKey,
                                call_data => aeu_hex:hexstring_decode(EncodedCallData)}),

    unsigned_tx_positive_test(ComputeCCallDecoded, ComputeCCallEncoded,
                               fun get_contract_call_compute/1,
                               fun aect_call_tx:new/1, MinerPubkey),

    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCallComputeTx}} =
        get_contract_call_compute(ComputeCCallEncoded),
    ContractCallComputeTxHash =
        sign_and_post_tx(MinerAddress, EncodedUnsignedContractCallComputeTx),

    % mine a block
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ?assert(tx_in_chain(ContractCallComputeTxHash)),

    %% Get the call object
    {ok, 200, CallObject1} = get_contract_call_object(ContractCallComputeTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_address">>, CallObject1, <<>>)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
                 maps:get(<<"contract_address">>, CallObject1, <<>>)),

    {ok, 200, #{<<"data">> := DecodedCallReturnValue}} =
        get_contract_decode_data(
          #{ 'sophia-type' => <<"int">>,
             data => maps:get(<<"return_value">>, CallObject1)}),
    %% Check that it is also the same as the direct call result
    ?assertEqual(DecodedCallReturnValue, DecodedCallResult),

    %% negative tests
    %% Invalid hashes
    %% invalid owner hash
    <<_, InvalidHash/binary>> = MinerAddress,
    {ok, 400, #{<<"reason">> := <<"Invalid hash: owner">>}} =
        get_contract_create(maps:put(owner, InvalidHash, ValidEncoded)),
    % invalid caller hash
    {ok, 400, #{<<"reason">> := <<"Invalid hash: caller">>}} =
        get_contract_call(maps:put(caller, InvalidHash, ContractCallEncoded)),
    % invalid caller hash
    {ok, 400, #{<<"reason">> := <<"Invalid hash: caller">>}} =
        get_contract_call_compute(maps:put(caller, InvalidHash,
                                           ComputeCCallEncoded)),
    %% account not found
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    RandContractAddress =aec_base58c:encode(contract_pubkey, random_hash()),
    %% owner not found
    {ok, 404, #{<<"reason">> := <<"Account of owner not found">>}} =
        get_contract_create(maps:put(owner, RandAddress, ValidEncoded)),
    %% caller not found
    {ok, 404, #{<<"reason">> := <<"Account of caller not found">>}} =
        get_contract_call(maps:put(caller, RandAddress, ContractCallEncoded)),
    %% contract not found
    {ok, 404, #{<<"reason">> := <<"Contract address for key contract not found">>}} =
        get_contract_call(maps:put(contract, RandContractAddress,
                                   ContractCallEncoded)),
    %% caller not found
    {ok, 404, #{<<"reason">> := <<"Account of caller not found">>}} =
        get_contract_call_compute(maps:put(caller, RandAddress,
                                           ComputeCCallEncoded)),
    %% contract not found
    {ok, 404, #{<<"reason">> := <<"Contract address for key contract not found">>}} =
        get_contract_call_compute(maps:put(contract, RandContractAddress,
                                           ComputeCCallEncoded)),

    %% Invalid hexstrings
    InvalidHex1 = <<"1234">>,
    InvalidHex2 = <<"0xXYZ">>,

    % invalid code
    {ok, 400, #{<<"reason">> := <<"Not hex string: code">>}} =
        get_contract_create(maps:put(code, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: code">>}} =
        get_contract_create(maps:put(code, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex1, ValidEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_create(maps:put(call_data, InvalidHex2, ValidEncoded)),
    % invalid call data
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex1, ContractCallEncoded)),
    {ok, 400, #{<<"reason">> := <<"Not hex string: call_data">>}} =
        get_contract_call(maps:put(call_data, InvalidHex2, ContractCallEncoded)),

    {ok, 400, #{<<"reason">> := <<"Failed to compute call_data, reason: bad argument">>}} =
        get_contract_call_compute(maps:put(arguments, <<"garbadge">>,
                                           ComputeCCallEncoded)),

    %% Call objects
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_spend_tx(MinerPubkey, 1, 1),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 400, #{<<"reason">> := <<"Tx is not a create or call">>}} =
        get_contract_call_object(SpendTxHash),

    ok.

contract_create_transaction_init_error(_Config) ->
    % miner has an account
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),

    % contract_create_tx positive test
    Code = aeu_hex:hexstring_encode(<<"NOT PROPER BYTE CODE">>),

    InitFunction = <<"init">>,
    InitArgument = <<"()">>,
    {ok, EncodedInitCallData} =
        aect_sophia:encode_call_data(Code,
            InitFunction,
            InitArgument),
    ValidEncoded = #{ owner => MinerAddress,
        code => Code,
        vm_version => 1,
        deposit => 2,
        amount => 1,
        gas => 30,
        gas_price => 1,
        fee => 1,
        call_data => EncodedInitCallData},
    ValidDecoded = maps:merge(ValidEncoded,
        #{owner => MinerPubkey,
            code => aeu_hex:hexstring_decode(Code),
            call_data => aeu_hex:hexstring_decode(EncodedInitCallData)}),

    %% prepare a contract_create_tx and post it
    {ok, 200, #{<<"tx">> := EncodedUnsignedContractCreateTx,
        <<"contract_address">> := EncodedContractPubKey}} =
        get_contract_create(ValidEncoded),
    {ok, ContractPubKey} = aec_base58c:safe_decode(contract_pubkey, EncodedContractPubKey),
    ContractCreateTxHash = sign_and_post_tx(MinerAddress, EncodedUnsignedContractCreateTx),

    %% Try to get the contract init call object while in mempool
    {ok, 400, #{<<"reason">> := <<"Tx not mined">>}} =
        get_contract_call_object(ContractCreateTxHash),

    % mine a block
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ?assert(tx_in_chain(ContractCreateTxHash)),

    %% Get the contract init call object
    {ok, 200, InitCallObject} = get_contract_call_object(ContractCreateTxHash),
    ?assertEqual(MinerAddress, maps:get(<<"caller_address">>, InitCallObject)),
    ?assertEqual(get_tx_nonce(ContractCreateTxHash), maps:get(<<"caller_nonce">>, InitCallObject)),
    ?assertEqual(aec_base58c:encode(contract_pubkey, ContractPubKey),
        maps:get(<<"contract_address">>, InitCallObject)),
    ?assertEqual(maps:get(gas_price, ValidDecoded), maps:get(<<"gas_price">>, InitCallObject)),
    ?assertMatch({Used, Limit} when
        is_integer(Used) andalso
            is_integer(Limit) andalso
            Limit > 0 andalso
            Used =< Limit,
        {maps:get(<<"gas_used">>, InitCallObject), maps:get(gas, ValidDecoded)}
    ),
    ?assertEqual(<<"error">>, maps:get(<<"return_type">>, InitCallObject)),
    ?assertMatch(_, maps:get(<<"return_value">>, InitCallObject)),

    ok.

oracle_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    OracleAddress = aec_base58c:encode(oracle_pubkey, MinerPubkey),

    % oracle_register_tx positive test
    RegEncoded = #{account => MinerAddress,
                   query_format => <<"something">>,
                   response_format => <<"something else">>,
                   query_fee => 1,
                   fee => 6,
                   oracle_ttl => #{type => <<"block">>, value => 2000}},
    RegDecoded = maps:merge(RegEncoded,
                            #{account => MinerPubkey,
                              query_spec => <<"something">>,
                              response_spec => <<"something else">>,
                              oracle_ttl => {block, 2000}}),
    unsigned_tx_positive_test(RegDecoded, RegEncoded,
                               fun get_oracle_register/1,
                               fun aeo_register_tx:new/1, MinerPubkey),

    % in order to test a positive case for oracle_extend_tx and
    % oracle_query_tx we first need an actual Oracle on the chain

    {ok, 200, #{<<"tx">> := RegisterTx}} = get_oracle_register(RegEncoded),
    sign_and_post_tx(MinerAddress, RegisterTx),

    % mine blocks to include it
    ct:log("Before oracle registered nonce is ~p", [rpc(aec_next_nonce, pick_for_account, [MinerPubkey])]),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    ct:log("Oracle registered nonce is ~p", [rpc(aec_next_nonce, pick_for_account, [MinerPubkey])]),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty

    % oracle_extend_tx positive test
    ExtEncoded = #{oracle => aec_base58c:encode(oracle_pubkey, MinerPubkey),
                   fee => 2,
                   oracle_ttl => #{type => <<"delta">>, value => 500}},
    ExtDecoded = maps:merge(ExtEncoded,
                            #{oracle => MinerPubkey,
                              oracle_ttl => {delta, 500}}),
    unsigned_tx_positive_test(ExtDecoded, ExtEncoded,
                               fun get_oracle_extend/1,
                               fun aeo_extend_tx:new/1, MinerPubkey),

    % oracle_query_tx positive test
    QueryEncoded = #{sender => MinerAddress,
                     oracle_pubkey => aec_base58c:encode(oracle_pubkey, MinerPubkey),
                     query => <<"Hejsan Svejsan">>,
                     query_fee => 2,
                     fee => 30,
                     query_ttl => #{type => <<"block">>, value => 20},
                     response_ttl => #{type => <<"delta">>, value => 20}},
    QueryDecoded = maps:merge(QueryEncoded,
                              #{sender => MinerPubkey,
                                oracle => MinerPubkey,
                                query_ttl => {block, 20},
                                response_ttl => {delta, 20}}),
    unsigned_tx_positive_test(QueryDecoded, QueryEncoded,
                               fun get_oracle_query/1,
                               fun aeo_query_tx:new/1, MinerPubkey),

    % in order to test a positive case for oracle_response_tx we first need an
    % actual Oracle query on the chain

    {ok, 200, _} = get_balance_at_top(),
    {ok, QueryNonce} = rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
    ct:log("Nonce is ~p", [QueryNonce]),
    QueryId = aeo_query:id(MinerPubkey, QueryNonce, MinerPubkey),
    {ok, 200, #{<<"tx">> := QueryTx}} = get_oracle_query(QueryEncoded),
    sign_and_post_tx(MinerAddress, QueryTx),

    % mine blocks to include it
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty

    ResponseEncoded = #{oracle => OracleAddress,
                        query_id => aec_base58c:encode(oracle_query_id,
                                                       QueryId),
                        response => <<"Hejsan">>,
                        fee => 3},
    ResponseDecoded = maps:merge(ResponseEncoded,
                              #{oracle => MinerPubkey,
                                query_id => QueryId}),
    unsigned_tx_positive_test(ResponseDecoded, ResponseEncoded,
                               fun get_oracle_response/1,
                               fun aeo_response_tx:new/1, MinerPubkey),
    {ok, 200, #{<<"tx">> := ResponseTx}} = get_oracle_response(ResponseEncoded),
    sign_and_post_tx(MinerAddress, ResponseTx),
    % mine a block to include it
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% negative tests

    % broken hash
    <<_, InvalidHash/binary>> = MinerAddress,
    {ok, 400, #{<<"reason">> := <<"Invalid hash: account">>}} =
        get_oracle_register(maps:put(account, InvalidHash, RegEncoded)),

    {ok, 400, #{<<"reason">> := <<"Invalid hash: sender">>}} =
        get_oracle_query(maps:put(sender, InvalidHash, QueryEncoded)),
    {ok, 400, #{<<"reason">> := <<"Invalid hash: oracle_pubkey">>}} =
        get_oracle_query(maps:put(oracle_pubkey, InvalidHash, QueryEncoded)),

    {ok, 400, #{<<"reason">> := <<"Invalid hash: oracle">>}} =
        get_oracle_response(maps:put(oracle, InvalidHash, ResponseEncoded)),

    %% account not found
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    RandOracleAddress = aec_base58c:encode(oracle_pubkey, random_hash()),
    RandQueryID = aec_base58c:encode(oracle_query_id, random_hash()),
    {ok, 404, #{<<"reason">> := <<"Account of account not found">>}} =
        get_oracle_register(maps:put(account, RandAddress, RegEncoded)),

    {ok, 404, #{<<"reason">> := <<"Account of sender not found">>}} =
        get_oracle_query(maps:put(sender, RandAddress, QueryEncoded)),

    {ok, 404, #{<<"reason">> := <<"Account of oracle not found">>}} =
        get_oracle_response(maps:put(oracle, RandOracleAddress, ResponseEncoded)),

    {ok, 404, #{<<"reason">> := <<"Oracle address for key oracle not found">>}} =
        get_oracle_query(maps:put(oracle_pubkey, RandOracleAddress, QueryEncoded)),

    {ok, 404, #{<<"reason">> := <<"Oracle query for key query_id not found">>}} =
        get_oracle_response(maps:put(query_id, RandQueryID, ResponseEncoded)),

    %% broken ttl
    BrokenTTL1 = #{<<"invalid">> => <<"structure">>},
    BrokenTTL2 = #{<<"type">> => <<"hejsan">>, <<"value">> => 20},

    {ok, 400, _} =
        get_oracle_register(maps:put(ttl, BrokenTTL1, RegEncoded)),
    {ok, 400, _} =
        get_oracle_register(maps:put(ttl, BrokenTTL2, RegEncoded)),

    {ok, 400, _} =
        get_oracle_query(maps:put(query_ttl, BrokenTTL1, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(query_ttl, BrokenTTL2, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL1, QueryEncoded)),
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, BrokenTTL2, QueryEncoded)),
    % test non-relative ttl
    {ok, 400, _} =
        get_oracle_query(maps:put(response_ttl, #{type => <<"block">>,
                                                  value => 2}, QueryEncoded)),
    ok.

%% tests the following
%% GET preclaim_tx unsigned transaction
%% GET claim_tx unsigned transaction
%% GET update_tx unsigned transaction
%% GET transfer_tx unsigned transaction
%% GET revoke_tx unsigned transaction
nameservice_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    nameservice_transaction_preclaim(MinerAddress, MinerPubkey),
    nameservice_transaction_claim(MinerAddress, MinerPubkey),
    nameservice_transaction_update(MinerAddress, MinerPubkey),
    nameservice_transaction_transfer(MinerAddress, MinerPubkey),
    nameservice_transaction_revoke(MinerAddress, MinerPubkey),
    ok.

nameservice_transaction_preclaim(MinerAddress, MinerPubkey) ->
    Commitment = random_hash(),
    Encoded = #{account => MinerAddress,
                commitment => aec_base58c:encode(commitment, Commitment),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account => MinerPubkey,
                          commitment => Commitment}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_preclaim/1,
                               fun aens_preclaim_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, account, Encoded, fun get_name_preclaim/1),
    test_invalid_hash(MinerPubkey, commitment, Encoded, fun get_name_preclaim/1),
    test_missing_address(account, Encoded, fun get_name_preclaim/1),
    ok.

test_invalid_hash(CorrectAddress, Key0, Encoded, APIFun) ->
    {Key, Name} =
        case Key0 of
            {_, _} = Pair -> Pair;
            K -> {K, K}
        end,
    <<_, InvalidHash/binary>> = CorrectAddress,
    Msg = list_to_binary("Invalid hash: " ++ atom_to_list(Name)),
    {ok, 400, #{<<"reason">> := Msg}} = APIFun(maps:put(Key, InvalidHash, Encoded)),
    ok.

test_missing_address(Key, Encoded, APIFun) ->
    Msg = list_to_binary("Account of " ++ atom_to_list(Key) ++ " not found"),
    RandAddress = aec_base58c:encode(account_pubkey, random_hash()),
    {ok, 404, #{<<"reason">> := Msg}} =
        APIFun(maps:put(Key, RandAddress, Encoded)),
    ok.

nameservice_transaction_claim(MinerAddress, MinerPubkey) ->
    Name = <<"name.test">>,
    Salt = 1234,

    {ok, 200, #{<<"commitment">> := EncodedCHash}} = get_commitment_hash(Name, Salt),
    {ok, CHash} = aec_base58c:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    {ok, 200, _}                   = post_name_preclaim_tx(CHash, 1),
    {ok, [PreclaimTx0]}            = rpc(aec_tx_pool, peek, [infinity]),
    {name_preclaim_tx, PreclaimTx} = aetx:specialize_type(aetx_sign:tx(PreclaimTx0)),
    CHash                          = aens_preclaim_tx:commitment(PreclaimTx),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    Encoded = #{account => MinerAddress,
                name => aec_base58c:encode(name, Name),
                name_salt => Salt,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account => MinerPubkey,
                          name => Name}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_claim/1,
                               fun aens_claim_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, account, Encoded, fun get_name_claim/1),
    test_invalid_hash(MinerPubkey, name, Encoded, fun get_name_claim/1),
    test_missing_address(account, Encoded, fun get_name_claim/1),

    %% missing registar
    Missing = aec_base58c:encode(name, <<"missing">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: no_registrar">>}} =
        get_name_claim(maps:put(name, Missing, Encoded)),
    MissingReg = aec_base58c:encode(name, <<"missing.reg">>),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_name_claim(maps:put(name, MissingReg, Encoded)),
    ok.

nameservice_transaction_update(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Pointers = [{}],
    Encoded = #{account => MinerAddress,
                name_hash => aec_base58c:encode(name, NameHash),
                name_ttl => 3,
                client_ttl => 2,
                pointers => jsx:encode(Pointers),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account => MinerPubkey,
                          pointers => Pointers,
                          name_hash => NameHash}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_update/1,
                               fun aens_update_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, account, Encoded, fun get_name_update/1),
    test_invalid_hash(MinerPubkey, name_hash, Encoded, fun get_name_update/1),
    test_missing_address(account, Encoded, fun get_name_update/1),
    %% test broken pointers
    TestBrokenPointers =
        fun(P) ->
            {ok, 400, #{<<"reason">> := <<"Invalid pointers">>}} =
                get_name_update(maps:put(pointers, P, Encoded))
        end,
    TestBrokenPointers(<<"not a valid JSON">>),
    TestBrokenPointers(<<"{\"a\":1">>),
    ok.

nameservice_transaction_transfer(MinerAddress, MinerPubkey) ->
    RandAddress = random_hash(),
    NameHash = random_hash(),
    Encoded = #{account => MinerAddress,
                name_hash => aec_base58c:encode(name, NameHash),
                recipient_pubkey => aec_base58c:encode(account_pubkey,
                                                       RandAddress),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account => MinerPubkey,
                          recipient_account => RandAddress,
                          name_hash => NameHash}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_transfer/1,
                               fun aens_transfer_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, account, Encoded, fun get_name_transfer/1),
    test_invalid_hash(MinerPubkey, recipient_pubkey, Encoded, fun get_name_transfer/1),
    test_invalid_hash(MinerPubkey, name_hash, Encoded, fun get_name_transfer/1),
    test_missing_address(account, Encoded, fun get_name_transfer/1),
    ok.

nameservice_transaction_revoke(MinerAddress, MinerPubkey) ->
    NameHash = random_hash(),
    Encoded = #{account => MinerAddress,
                name_hash => aec_base58c:encode(name, NameHash),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{account => MinerPubkey,
                          name_hash => NameHash}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_name_revoke/1,
                               fun aens_revoke_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, account, Encoded, fun get_name_revoke/1),
    test_invalid_hash(MinerPubkey, name_hash, Encoded, fun get_name_revoke/1),
    test_missing_address(account, Encoded, fun get_name_revoke/1),
    ok.

%% tests the following
%% GET channel_create_tx unsigned transaction
%% GET channel_deposit_tx unsigned transaction
%% GET channel_withdraw_tx unsigned transaction
%% GET channel_close_mutual_tx unsigned transaction
%% GET channel_close_solo unsigned transaction
%% GET channel_slash_tx unsigned transaction
%% GET channel_settle_tx unsigned transaction
state_channels_onchain_transactions(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    ParticipantPubkey = random_hash(),
    ok = give_tokens(ParticipantPubkey, 100),
    {ok, AeTx} = state_channels_create(MinerPubkey, ParticipantPubkey),
    ChannelId = state_channel_id(AeTx),
    state_channels_deposit(ChannelId, MinerPubkey),
    state_channels_withdrawal(ChannelId, MinerPubkey),
    state_channels_close_mutual(ChannelId, MinerPubkey),
    state_channels_close_solo(ChannelId, MinerPubkey),
    state_channels_slash(ChannelId, MinerPubkey),
    state_channels_settle(ChannelId, MinerPubkey),
    ok.

state_channel_id(Tx) ->
    {channel_create_tx, ChCTx} = aetx:specialize_type(Tx),
    Initiator = aesc_create_tx:initiator(ChCTx),
    Nonce = aesc_create_tx:nonce(ChCTx),
    Responder = aesc_create_tx:responder(ChCTx),
    aesc_channels:id(Initiator, Nonce, Responder).

state_channels_create(MinerPubkey, ResponderPubkey) ->
    Encoded = #{initiator => aec_base58c:encode(account_pubkey, MinerPubkey),
                initiator_amount => 2,
                responder => aec_base58c:encode(account_pubkey, ResponderPubkey),
                responder_amount => 3,
                push_amount => 5, channel_reserve => 5,
                lock_period => 20,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{initiator => MinerPubkey,
                          responder => ResponderPubkey,
                          state_hash => ?BOGUS_STATE_HASH}),
    {ok, Tx} = unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_create/1,
                               fun aesc_create_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, initiator, Encoded, fun get_channel_create/1),
    test_invalid_hash(ResponderPubkey, responder, Encoded, fun get_channel_create/1),
    test_missing_address(initiator, Encoded, fun get_channel_create/1),
    {ok, Tx}.

state_channels_deposit(ChannelId, MinerPubkey) ->
    MinerAddress = aec_base58c:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from => MinerAddress,
                amount => 2,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => ChannelId,
                          from => MinerPubkey,
                          state_hash => ?BOGUS_STATE_HASH}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_deposit/1,
                               fun aesc_deposit_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash(MinerPubkey, from, Encoded1, fun get_channel_deposit/1),
    ok.

state_channels_withdrawal(ChannelId, MinerPubkey) ->
    MinerAddress = aec_base58c:encode(account_pubkey, MinerPubkey),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                to => MinerAddress,
                amount => 2,
                state_hash => aec_base58c:encode(state, ?BOGUS_STATE_HASH),
                round => 42,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => ChannelId,
                          to => MinerPubkey,
                          state_hash => ?BOGUS_STATE_HASH}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_withdrawal/1,
                               fun aesc_withdraw_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash(MinerPubkey, to, Encoded1, fun get_channel_withdrawal/1),
    ok.

state_channels_close_mutual(ChannelId, InitiatorPubkey) ->
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                initiator_amount => 4,
                responder_amount => 3,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{channel_id => ChannelId}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_close_mutual/1,
                               fun aesc_close_mutual_tx:new/1, InitiatorPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [InitiatorPubkey]),
                            InitiatorPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash(ChannelId, channel_id, Encoded1, fun get_channel_close_mutual/1),
    ok.

state_channels_close_solo(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from => aec_base58c:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from => MinerPubkey,
                          channel_id => ChannelId,
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_close_solo/1,
                               fun aesc_close_solo_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey,  from, Encoded, fun get_channel_close_solo/1),
    ok.

state_channels_slash(ChannelId, MinerPubkey) ->
    PoI = aec_trees:new_poi(aec_trees:new_without_backend()),
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from => aec_base58c:encode(account_pubkey, MinerPubkey),
                payload => <<"hejsan svejsan">>, %%TODO proper payload
                poi => aec_base58c:encode(poi, aec_trees:serialize_poi(PoI)),
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from => MinerPubkey,
                          channel_id => ChannelId,
                          poi => PoI}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_slash/1,
                               fun aesc_slash_tx:new/1, MinerPubkey),
    test_invalid_hash(MinerPubkey, from, Encoded, fun get_channel_slash/1),
    ok.

state_channels_settle(ChannelId, MinerPubkey) ->
    Encoded = #{channel_id => aec_base58c:encode(channel, ChannelId),
                from => aec_base58c:encode(account_pubkey, MinerPubkey),
                initiator_amount => 4,
                responder_amount => 3,
                fee => 1},
    Decoded = maps:merge(Encoded,
                        #{from => MinerPubkey,
                          channel_id => ChannelId}),
    unsigned_tx_positive_test(Decoded, Encoded,
                               fun get_channel_settle/1,
                               fun aesc_settle_tx:new/1, MinerPubkey,
                               _NonceRequred = true),
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [MinerPubkey]),
                            MinerPubkey},
    Encoded1 = maps:put(nonce, NextNonce, Encoded),
    test_invalid_hash(MinerPubkey, from, Encoded1, fun get_channel_settle/1),
    ok.

%% tests the following
%% GET spend_tx unsigned transaction
spend_transaction(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    {ok, MinerPubkey} = aec_base58c:safe_decode(account_pubkey, MinerAddress),
    RandAddress = random_hash(),
    Encoded = #{sender => MinerAddress,
                recipient_pubkey => aec_base58c:encode(account_pubkey,
                                                       RandAddress),
                amount => 2,
                fee => 1,
                ttl => 43,
                payload => <<"hejsan svejsan">>},
    Decoded = maps:merge(Encoded,
                        #{sender => MinerPubkey,
                          recipient => RandAddress}),
    {ok, T} = unsigned_tx_positive_test(Decoded, Encoded,
                                  fun get_spend/1,
                                  fun aec_spend_tx:new/1, MinerPubkey),
    {spend_tx, SpendTx} = aetx:specialize_type(T),
    <<"hejsan svejsan">> = aec_spend_tx:payload(SpendTx),

    test_invalid_hash(MinerPubkey, sender, Encoded, fun get_spend/1),
    test_invalid_hash(MinerPubkey, {recipient_pubkey, recipient}, Encoded, fun get_spend/1),
    test_missing_address(sender, Encoded, fun get_spend/1),
    ok.

%% tests the following
%% GET spend_tx unsigned transaction with an non-present key in request
unknown_atom_in_spend_tx(_Config) ->
    {ok, 200, _} = get_balance_at_top(),
    {ok, 200, #{<<"pub_key">> := MinerAddress}} = get_miner_pub_key(),
    RandAddress = random_hash(),
    Encoded = #{sender => MinerAddress,
                recipient_pubkey => aec_base58c:encode(account_pubkey,
                                                       RandAddress),
                amount => 2,
                fee => 1,
                %% this tests relies on this being an atom unknown to the VM
                %% if someone adds this atom in particular, please modify the
                %% binary below accordingly
                <<"hejsan_svejsan_atom">> => 10,
                payload => <<"hejsan svejsan">>},
    {ok, 400, #{<<"reason">> := <<"Invalid parameter: hejsan_svejsan_atom">>}} = get_spend(Encoded),
    ok.

unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey) ->
    unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey,
                              false).

unsigned_tx_positive_test(Data, Params0, HTTPCallFun, NewFun, Pubkey,
                          NonceRequred) ->
    {{ok, NextNonce}, _} = {rpc(aec_next_nonce, pick_for_account, [Pubkey]),
                            Pubkey},
    Test =
        fun(Nonce, P) ->
            ct:log("PARAMS ~p", [P]),
            {ok, ExpectedTx} = NewFun(maps:put(nonce, Nonce, Data)),
            {ok, 200, #{<<"tx">> := ActualTx}} = HTTPCallFun(P),
            {ok, SerializedTx} = aec_base58c:safe_decode(transaction, ActualTx),
            Tx = aetx:deserialize_from_binary(SerializedTx),
            ct:log("Expected ~p~nActual ~p", [ExpectedTx, Tx]),
            ExpectedTx = Tx,
            Tx
        end,
    Params =
        case NonceRequred of
            true -> maps:put(nonce, NextNonce, Params0);
            false -> Params0
        end,
    Tx = Test(NextNonce, Params),
    RandomNonce = rand:uniform(999) + 1,
    Test(RandomNonce, maps:put(nonce, RandomNonce, Params)),
    {ok, Tx}.

get_transaction(_Config) ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    TxHashes = add_spend_txs(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    Encodings = [default, message_pack, json],
    lists:foreach(
        fun(TxHash) ->
                {ok, 200, #{<<"transaction">> := #{<<"hash">> := TxHash1}}} =
                    get_tx(TxHash, json),
                ?assertEqual(TxHash, TxHash1)
        end,
      TxHashes),

    %% test in mempool
    RandAddress = random_hash(),
    Encoded = #{sender => EncodedPubKey,
                recipient_pubkey => aec_base58c:encode(account_pubkey,
                                                       RandAddress),
                amount => 2,
                fee => 1,
                payload => <<"foo">>},
    {ok, 200, #{<<"tx">> := EncodedSpendTx}} = get_spend(Encoded),
    {ok, SpendTxBin} = aec_base58c:safe_decode(transaction, EncodedSpendTx),
    SpendTx = aetx:deserialize_from_binary(SpendTxBin),
    {ok, SignedSpendTx} = rpc(aec_keys, sign, [SpendTx]),
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedSpendTx)),

    SerializedSpendTx = aetx_sign:serialize_to_binary(SignedSpendTx),
    {ok, 200, _} = post_tx(aec_base58c:encode(transaction, SerializedSpendTx)),
    lists:foreach(
        fun(Encoding) ->
            {ok, 200, #{<<"transaction">> := PendingTx}} = get_tx(TxHash, Encoding),
            E = case Encoding of
                  default -> message_pack;
                  E0 -> E0
                end,
            Expected = aetx_sign:serialize_for_client_pending(E, SignedSpendTx),
            Expected = PendingTx
        end,
        Encodings),

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    ok.

%% Maybe this test should be broken into a couple of smaller tests
%% it currently tests the positive cases for
%% GET externalAPI/transactions
%% POST internalAPI/spend-tx
%% GET externalAPI/account/balance
pending_transactions(_Config) ->
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty
    {ok, 200, []} = get_transactions(),
    InitialBalance =
        case get_balance_at_top() of
            {ok, 404, #{<<"reason">> := <<"Account not found">>}} -> 0;
            {ok, 200, #{<<"balance">> := Bal00}} -> Bal00
        end,
    AmountToSpent = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(AmountToSpent, 1),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),

    {Bal0, _, _} = {InitialBalance + BlocksToMine * MineReward, Bal0,
                 {InitialBalance, BlocksToMine, MineReward, Fee}},
    true = (is_integer(Bal0) andalso Bal0 > AmountToSpent + Fee),

    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % still empty
    {ok, 200, []} = get_transactions(),

    %{ok, SenderPubKey} = rpc:call(?NODE, aec_keys, pubkey, [], 5000),
    ReceiverPubKey = random_hash(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),

    {ok, 200, _} = post_spend_tx(ReceiverPubKey, AmountToSpent, Fee),
    {ok, NodeTxs} = rpc(aec_tx_pool, peek, [infinity]),
    true = length(NodeTxs) =:= 1, % not empty anymore
    {ok, 200, ReturnedTxs} = get_transactions(),
    ExpectedTxs = [#{<<"tx">> => aec_base58c:encode(
                                   transaction,
                                   aetx_sign:serialize_to_binary(T))}
           || T <- NodeTxs],
    true = length(ExpectedTxs) =:= length(ReturnedTxs),
    true = lists:all(fun(Tx) -> lists:member(Tx, ExpectedTxs) end, ReturnedTxs),

    {ok, 200, #{<<"balance">> := Bal0}} = get_balance_at_top(),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
                  get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),


    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty again
    {ok, 200, []} = get_transactions(),

    {ok, 200, #{<<"balance">> := Bal1}} = get_balance_at_top(),
    Bal1 = Bal0 + MineReward + Fee - AmountToSpent - Fee,
    {ok, 200, #{<<"balance">> := AmountToSpent}} =
                 get_balance_at_top(aec_base58c:encode(account_pubkey, ReceiverPubKey)),
    ok.

post_correct_blocks(_Config) ->
    ok = rpc(aec_conductor, stop_mining, []),
    ok = rpc(aec_conductor, reinit_chain, []),
    timer:sleep(200),
    BlocksToPost = max(?DEFAULT_TESTS_COUNT, aecore_suite_utils:latest_fork_height()),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPost),
    Blocks =
        lists:map(
            fun(Height) ->
                {ok, B} = rpc(aec_chain, get_block_by_height, [Height]),
                B
            end,
            lists:seq(1, BlocksToPost)),
    ok = rpc(aec_conductor, reinit_chain, []),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_chain, top_header, []),
    0 = aec_headers:height(GH), %chain is empty
    lists:foreach(
        fun(Block) ->
            {ok, 200, _} = post_block(Block),
            H = rpc(aec_chain, top_header, []),
            {ok, HH} = aec_headers:hash_header(H),
            {ok, BH} = aec_blocks:hash_internal_representation(Block),
            BH = HH % block accepted
        end,
        Blocks),

    ToMine = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ToMine),
    ok.

post_broken_blocks(Config) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   1),
    {ok, CorrectBlock} = rpc(aec_chain, get_block_by_height, [1]),
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(aecore_suite_utils:node_name(?NODE)),
    GH = rpc(aec_chain, top_header, []),
    0 = aec_headers:height(GH), %chain is empty
    CorrectBlockMap =
        aehttp_api_parser:encode(block, CorrectBlock),
    BrokenBlocks =
        lists:map(
            fun({Key, Value}) ->
                BrokenBlock = maps:put(Key, Value, CorrectBlockMap),
                {Key, BrokenBlock}
            end,
        [{<<"prev_hash">>, aec_base58c:encode(block_hash, <<"foo">>)},
         {<<"state_hash">>, aec_base58c:encode(block_state_hash, <<"foo">>)},
         {<<"txs_hash">>, aec_base58c:encode(block_tx_hash, <<"foo">>)},
         {<<"target">>, 42},
         {<<"nonce">>, 42},
         {<<"time">>, 42},
         {<<"version">>, 42},
         {<<"pow">>, lists:seq(1, 42)},
         {<<"transactions">>, [#{<<"tx">> => <<"foo">>}]},
         {<<"miner">>, aec_base58c:encode(account_pubkey, <<"foo">>)}
        ]),

    lists:foreach(
        fun({BrokenField, BlockMap}) ->
            ct:log("Testing with a broken ~p", [BrokenField]),
            {ok, 400, #{<<"reason">> := <<"Block rejected">>}} = post_block_map(BlockMap),
            H = rpc(aec_chain, top_header, []),
            0 = aec_headers:height(H) %chain is still empty
        end,
        BrokenBlocks),
    ok.

%% Even though a tx with a unknown sender pubkey would be accepted, we need
%% a valid account nonce; that's why we mine a while
post_correct_tx(_Config) ->
    Amount = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender => PubKey,
            recipient => random_hash(),
            amount => Amount,
            fee => Fee,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
    ExpectedHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := ExpectedHash}} =
        post_tx(aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx))),
    {ok, [SignedTx]} = rpc(aec_tx_pool, peek, [infinity]), % same tx
    ok.

post_broken_tx(_Config) ->
    Amount = 1,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, 1),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    {ok, SpendTx} =
        aec_spend_tx:new(
          #{sender => PubKey,
            recipient => random_hash(),
            amount => Amount,
            fee => Fee,
            nonce => Nonce,
            payload => <<"foo">>}),
    {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
    SignedTxBin = aetx_sign:serialize_to_binary(SignedTx),
    BrokenTxBin = case SignedTxBin of
                    <<1:1, Rest/bits>> -> <<0:1, Rest/bits>>;
                    <<0:1, Rest/bits>> -> <<1:1, Rest/bits>>
                  end,
    EncodedBrokenTx = aec_base58c:encode(transaction, BrokenTxBin),
    EncodedSignedTx = aec_base58c:encode(transaction, SignedTxBin),
    {ok, 400, #{<<"reason">> := <<"Invalid base58Check encoding">>}} = post_tx(EncodedBrokenTx),
    {ok, 200, _} = post_tx(EncodedSignedTx),
    ok.

post_broken_base58_tx(_Config) ->
    Amount = 1,
    NumberOfChecks = ?DEFAULT_TESTS_COUNT,
    {BlocksToMine, Fee} = minimal_fee_and_blocks_to_mine(Amount, NumberOfChecks),
    {PubKey, Nonce} = prepare_for_spending(BlocksToMine),
    lists:foreach(
        fun(_) ->
            {ok, SpendTx} =
                aec_spend_tx:new(
                  #{sender => PubKey,
                    recipient => random_hash(),
                    amount => Amount,
                    fee => Fee,
                    nonce => Nonce,
                    payload => <<"foo">>}),
            {ok, SignedTx} = rpc(aec_keys, sign, [SpendTx]),
            <<_, BrokenHash/binary>> =
                aec_base58c:encode(transaction,
                                   aetx_sign:serialize_to_binary(SignedTx)),
            {ok, 400, #{<<"reason">> := <<"Invalid base58Check encoding">>}} = post_tx(BrokenHash)
        end,
        lists:seq(1, NumberOfChecks)), % number
    ok.

all_accounts_balances(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisPresetAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    GenesisAccounts = [{aec_block_genesis:miner(), aec_governance:block_mine_reward()} | GenesisPresetAccounts],
    Receivers = ?DEFAULT_TESTS_COUNT,
    AmountToSpent = 1,
    {BlocksToMine0, Fee} = minimal_fee_and_blocks_to_mine(AmountToSpent, Receivers),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    BlocksToMine = max(BlocksToMine0, ForkHeight),

    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ReceiversAccounts = [{random_hash(), AmountToSpent} || _Idx <- lists:seq(1, Receivers)],
    lists:foreach(
        fun({ReceiverPubKey, _}) ->
            {ok, 200, _} = post_spend_tx(ReceiverPubKey, AmountToSpent, Fee)
        end,
        ReceiversAccounts),

    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, Txs} =  get_transactions(aec_base58c:encode(account_pubkey, MinerPubKey)),
    ?assertEqual(Receivers, length(Txs)),

    % mine a block to include the txs
    {ok, [Block]} = aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, #{<<"accounts_balances">> := BalancesMap}} = get_all_accounts_balances(),
    {ok, 200, #{<<"nonce">> := Receivers}} = get_account_nonce(aec_base58c:encode(account_pubkey, MinerPubKey)),
    {ok, MinerBal} = rpc(aec_mining, get_miner_account_balance, []),
    ExpectedBalances = [{MinerPubKey, MinerBal} | GenesisAccounts] ++  ReceiversAccounts,

    % make sure all spend txs are part of the block
    AllTxs = aec_blocks:txs(Block),
    AllTxsCnt = length(AllTxs),
    AllTxsCnt = Receivers,

    Balances =
        lists:map(fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                          {account_pubkey, AccDec} = aec_base58c:decode(PKEncoded),
                          {AccDec, Bal}
                  end, BalancesMap),
    ?assertEqual(lists:sort(ExpectedBalances), lists:sort(Balances)),
    ok.

all_accounts_balances_empty(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    GenesisPresetAccounts = rpc(aec_genesis_block_settings, preset_accounts, []),
    GenesisAccounts = [{aec_block_genesis:miner(), aec_governance:block_mine_reward()} | GenesisPresetAccounts],
    {ok, 200, #{<<"accounts_balances">> := Balances}} = get_all_accounts_balances(),
    true = length(Balances) =:= length(GenesisAccounts),
    true =
        lists:all(
            fun(#{<<"pub_key">> := PKEncoded, <<"balance">> := Bal}) ->
                    {account_pubkey, AccDec} = aec_base58c:decode(PKEncoded),
                Account = {AccDec, Bal},
                lists:member(Account, GenesisAccounts) end,
            Balances),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

all_accounts_balances_disabled(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Balances not enabled">>}} = get_all_accounts_balances(),
    ok.

version(_Config) ->
    {ok, 200, #{<<"version">> := V,
                <<"revision">> := Rev,
                <<"genesis_hash">> := EncodedGH}} = get_version(),
    V0 = rpc(aeu_info, get_version, []),
    Rev0 = rpc(aeu_info, get_revision, []),
    GenHash0 = rpc(aec_chain, genesis_hash, []),
    % asserts
    V = V0,
    Rev = Rev0,
    {block_hash, GenHash0} = aec_base58c:decode(EncodedGH),
    ok.

info_disabled(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Info not enabled">>}} = get_info(),
    ok.

info_empty(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    ExpectedEmpty = #{<<"last_30_blocks_time">> => [ #{<<"difficulty">> => 1.0,
                                                       <<"height">> => 0,
                                                       <<"time">>  => 0}]},
    {ok, 200, ExpectedEmpty} = get_info(),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

info_more_than_30(_Config) ->
    test_info(40).

info_less_than_30(_Config) ->
    test_info(20).

test_info(BlocksToMine) ->
    true = aecore_suite_utils:latest_fork_height() < BlocksToMine,
    ok = rpc(aec_conductor, reinit_chain, []),
    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    ExpectedMineRate = 100,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine, ExpectedMineRate),
    {ok, 200, #{<<"last_30_blocks_time">> := Summary}} = get_info(),
    ExpectedCnt = min(BlocksToMine + 1, 30),
    ExpectedCnt = length(Summary),
    lists:foldl(
        fun(BlockSummary, 0) ->
            #{<<"difficulty">> := 1.0,
              <<"height">> := 0,
              <<"time">>  := 0} = BlockSummary;
        (BlockSummary, ExpectedHeight) ->
            #{<<"height">> := ExpectedHeight} = BlockSummary,
            {ok, 200, BlockMap} = get_block_by_height(ExpectedHeight),
            #{<<"time">> := Time, <<"target">> := Target} = BlockMap,
            #{<<"time">> := Time} = BlockSummary,
            Difficulty = aec_pow:target_to_difficulty(Target),
            #{<<"difficulty">> := Difficulty} = BlockSummary,
            {ok, 200, PreviousBlockMap} = get_block_by_height(ExpectedHeight -1),
            #{<<"time">> := PrevTime} = PreviousBlockMap,
            TimeDelta = Time - PrevTime,
            #{<<"time_delta_to_parent">> := TimeDelta} = BlockSummary,
            ExpectedHeight -1
        end,
        BlocksToMine,
        Summary),

    ok.





%% positive test of spend_tx is handled in pending_transactions test
broken_spend_tx(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
    ReceiverPubKey = random_hash(),
    {ok, 404, _} = post_spend_tx(ReceiverPubKey, 42, 2),

    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

miner_pub_key(_Config) ->
    {ok, MinerPubKey} = rpc(aec_keys, pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    ct:log("MinerPubkey = ~p~nEncodedPubKey = ~p", [MinerPubKey,
                                                    EncodedPubKey]),
    {account_pubkey, MinerPubKey} = aec_base58c:decode(EncodedPubKey),
    ok.

peer_pub_key(_Config) ->
    {ok, PeerPubKey} = rpc(aec_keys, peer_pubkey, []),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_peer_pub_key(),
    ct:log("PeerPubkey = ~p~nEncodedPubKey = ~p", [PeerPubKey,
                                                    EncodedPubKey]),
    {ok, PeerPubKey} = aec_base58c:safe_decode(peer_pubkey, EncodedPubKey),
    ok.

block_number(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    TopHeader = rpc(aec_chain, top_header, []),
    0 = aec_headers:height(TopHeader),
    {ok, 200, #{<<"height">> := 0}} = get_block_number(),
    lists:foreach(
        fun(ExpectedNum) ->
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            {ok, 200, #{<<"height">> := Num}} = get_block_number(),
            ExpectedNum = Num
        end,
        lists:seq(1, max(?DEFAULT_TESTS_COUNT, ForkHeight))),
    ok.

block_genesis(_Config) ->
    GetExpectedBlockFun =
        fun(_H) ->
            GenesisBlock = rpc(aec_chain, genesis_block, []),
            {ok, GenesisBlock}
        end,
    CallApiFun =
        fun(_H, Opts) ->
            get_internal_block_preset("genesis", Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

block_latest(_Config) ->
    GetExpectedBlockFun =
        fun(_H) ->
            TopBlock = rpc(aec_chain, top_block, []),
            {ok, TopBlock}
        end,
    CallApiFun =
        fun(_H, Opts) ->
            get_internal_block_preset("latest", Opts)
        end,
    internal_get_block_generic(GetExpectedBlockFun, CallApiFun).

%% we need really slow mining; since mining speed is not modified for the
%% first X blocks, we need to premine them before the test
block_pending(_Config) ->
    BlocksToPremine = rpc(aec_governance, blocks_to_check_difficulty_count, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPremine),
    lists:foreach(
        fun(Opt) ->
            {ok, 404, #{<<"reason">> := <<"Not mining, no pending block">>}} =
                    get_internal_block_preset("pending", Opt)
        end,
        [default, message_pack, json]),
    ok = rpc(application, set_env, [aecore, expected_mine_rate,
                                    60 * 60 * 1000]), % aim at one block an hour
    add_spend_txs(),
    rpc(aec_conductor, start_mining, []),
    timer:sleep(100),% so the miner is started
    {ok, PendingBlock} = get_pending_block(),
    ExpectedPendingTx = maps:put(<<"data_schema">>,
                <<"BlockWithMsgPackTxs">>,
                block_to_endpoint_map(PendingBlock)),
    ct:log("Expected pending block ~p", [ExpectedPendingTx]),
    GetPending =
        fun(Opt)->
            aec_test_utils:exec_with_timeout(
               fun TryGetting() ->
                   case get_internal_block_preset("pending", Opt) of
                      {ok, 200, B} -> B;
                      {ok, 404, _} ->
                          timer:sleep(100),
                          TryGetting()
                  end
               end,
               10000)
        end,
    {ok, PendingTxDefault} = GetPending(default),
    {ok, PendingTxHashes} = GetPending(message_pack),
    ValidateKeys =
        fun(Map1, Map2, Key) ->
            true = maps:get(Key, Map1, not_found1) =:=
                   maps:get(Key, Map2, not_found2)
        end,
    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTx, PendingTxHashes, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTx, PendingTxHashes, <<"data_schema">>),

    ExpectedPendingTxsObjects = maps:put(<<"data_schema">>,
                <<"BlockWithJSONTxs">>,
                block_to_endpoint_map(PendingBlock, #{tx_encoding => json})),
    ct:log("Expected pending block with tx objects~p",
           [ExpectedPendingTxsObjects]),
    {ok, PendingTxObjects} = GetPending(json),

    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxObjects, <<"data_schema">>),

    % no block should have been mined, so the same prev_hash
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxDefault, <<"prev_hash">>),
    ValidateKeys(ExpectedPendingTxsObjects, PendingTxDefault, <<"data_schema">>),

    rpc(aec_conductor, stop_mining, []),
    ok.

internal_get_block_generic(GetExpectedBlockFun, CallApiFun) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    BlocksToCheck = 4,
    CheckAtHeight =
        fun(Height) ->
            {ok, ExpectedBlock} = GetExpectedBlockFun(Height),
            Specific =
                fun(DataSchema) ->
                    {ok, Hash} =
                        aec_blocks:hash_internal_representation(ExpectedBlock),
                    #{<<"data_schema">> => DataSchema,
                      <<"hash">> => aec_base58c:encode(block_hash, Hash)}
                end,
            ExpectedBlockMap = maps:merge(Specific(<<"BlockWithMsgPackTxs">>),
                block_to_endpoint_map(ExpectedBlock)),
            {ok, 200, BlockMap} = CallApiFun(Height, default),
            ct:log("ExpectedBlockMap ~p, BlockMap: ~p", [ExpectedBlockMap,
                                                         BlockMap]),
            {ok, 200, BlockMap1} = CallApiFun(Height, message_pack),
            true = equal_block_maps(BlockMap1, ExpectedBlockMap),

            ExpectedBlockMapTxsObjects = maps:merge(Specific(<<"BlockWithJSONTxs">>),
                block_to_endpoint_map(ExpectedBlock, #{tx_encoding => json})),
            {ok, 200, BlockMap2} = CallApiFun(Height, json),
            ct:log("ExpectedBlockMapTxsObjects ~p, BlockMap2: ~p",
                   [ExpectedBlockMapTxsObjects, BlockMap2]),
            true = equal_block_maps(BlockMap, ExpectedBlockMapTxsObjects),
            true = equal_block_maps(BlockMap2, ExpectedBlockMapTxsObjects)
        end,
    CheckAtHeight(0), % genesis
    % ensure at least one block mine reward - for later adding spend txs
    MinBlockHeightToCheck =
        case ForkHeight of
            0 ->
                aecore_suite_utils:mine_blocks(
                  aecore_suite_utils:node_name(?NODE), 1),
                1; % from first block with reward
            _ when is_integer(ForkHeight), ForkHeight > 0 ->
                ForkHeight % from latest fork
        end,
    lists:foreach(
        fun(Height) ->
            {ok, 200, #{<<"height">> := Height}} = get_top(),
            CheckAtHeight(Height),
            % prepare the next block
            add_spend_txs(),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1)
        end,
        lists:seq(MinBlockHeightToCheck, ForkHeight + BlocksToCheck)),
    ok.

block_txs_count_by_height(_Config) ->
    generic_counts_test(fun(H) -> rpc(aec_chain, get_block_by_height,
                                     [H]) end,
                        fun get_block_txs_count_by_height/1).

block_txs_count_by_hash(_Config) ->
    CallApiFun =
        fun(H) ->
            {ok, Hash} = block_hash_by_height(H),
            get_block_txs_count_by_hash(Hash)
        end,
    generic_counts_test(fun(H) -> rpc(aec_chain, get_block_by_height,
                                     [H]) end,
                        CallApiFun).

block_txs_count_genesis(_Config) ->
    generic_counts_test(
        fun(_H) -> {ok, rpc(aec_chain, genesis_block, [])} end,
        fun(_) -> get_block_txs_count_preset("genesis") end).

block_txs_count_latest(_Config) ->
    generic_counts_test(
        fun(_H) ->
            TopBlock = rpc(aec_chain, top_block, []),
            {ok, TopBlock}
        end,
        fun(_) -> get_block_txs_count_preset("latest") end).

block_txs_count_pending(_Config) ->
    BlocksToPremine = rpc(aec_governance, blocks_to_check_difficulty_count, []),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToPremine),
        {ok, 404, #{<<"reason">> := <<"Not mining, no pending block">>}} =
                    get_block_txs_count_preset("pending"),
    ok = rpc(application, set_env, [aecore, expected_mine_rate,
                                    60 * 60 * 1000]), % aim at one block an hour
    InsertedTxsCount = length(add_spend_txs()),
    rpc(aec_conductor, start_mining, []),
    GetPending =
        fun()->
            aec_test_utils:exec_with_timeout(
               fun TryGetting() ->
                  case get_block_txs_count_preset("pending") of
                      {ok, 200, B} -> B;
                      {ok, 404, _} ->
                          timer:sleep(100),
                          TryGetting()
                  end
               end,
               10000)
        end,
    {ok, #{<<"count">> := TxsCount}} = GetPending(),
    ct:log("Inserted transactions count ~p, transactions count in the pending block ~p",
           [InsertedTxsCount, TxsCount]),
    % the assert below relies on no block being mined during the test run
    % this is achieved by mining BlocksToPremine number of blocks and setting
    % a high value for expected_mine_rate
    ?assertEqual(InsertedTxsCount, TxsCount),
    rpc(aec_conductor, start_mining, []),
    ok.

generic_counts_test(GetBlock, CallApi) ->
    BlocksToMine = 5,
    {ok, 200, #{<<"height">> := ChainHeight}} = get_top(),

    Check = fun(H) ->
                {ok, B} = GetBlock(H),
                TxsCount = length(aec_blocks:txs(B)),
                {ok, 200, #{<<"count">> := TxsCount}} = CallApi(H)
            end,

    %% Check genesis
    Check(aec_block_genesis:height()),

    %% Check BlocksToMine blocks
    lists:foreach(
        fun(Height) ->
            add_spend_txs(),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            Check(Height + 1)
        end,
        lists:seq(ChainHeight, ChainHeight + BlocksToMine)),
    ok.

block_txs_count_by_height_not_found(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    lists:foreach(
        fun(H) ->
            {ok, 404, #{<<"reason">> := <<"Chain too short">>}}
                        = get_block_txs_count_by_height(H)
        end,
        lists:seq(InitialHeight + 1, InitialHeight + ?DEFAULT_TESTS_COUNT)),
    ok.

block_txs_count_by_hash_not_found(_Config) ->
    lists:foreach(
        fun(_Height) ->
            H = random_hash(),
            error = rpc(aec_chain, get_block, [H]),
            Hash = aec_base58c:encode(block_hash, H),
            {ok, 404, #{<<"reason">> := <<"Block not found">>}}
                = get_block_txs_count_by_hash(Hash)
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

block_txs_count_by_broken_hash(_Config) ->
    lists:foreach(
        fun(_) ->
            <<_, BrokenHash/binary>> = aec_base58c:encode(block_hash, random_hash()),
            {ok, 400, #{<<"reason">> := <<"Invalid hash">>}} =
                get_block_txs_count_by_hash(BrokenHash)
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    ok.

block_tx_index_by_height(_Config) ->
    generic_block_tx_index_test(fun get_block_tx_by_index_height/3).

block_tx_index_by_hash(_Config) ->
    CallApiFun =
        fun(H, Index, Opts) ->
            {ok, Hash} = block_hash_by_height(H),
            get_block_tx_by_index_hash(Hash, Index, Opts)
        end,
    generic_block_tx_index_test(CallApiFun).

block_tx_index_latest(_Config) ->
    generic_block_tx_index_test(
        fun(_, Index, Opts) ->
            get_block_tx_by_index_latest(Index, Opts)
        end).

block_tx_index_not_founds(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    RandomHeight = InitialHeight + rand:uniform(999) + 1, % CurrentTop + 1..1000
    Test =
        fun(Code, ErrMsg, Fun, Cases) ->
            lists:foreach(
                fun({H, I}) ->
                    lists:foreach(
                        fun(Opt) ->
                            {ok, Code, #{<<"reason">> := ErrMsg}} = Fun(H, I, Opt) end,
                        [default, message_pack, json])
                end,
                Cases)
        end,
    Test(404, <<"Chain too short">>, fun get_block_tx_by_index_height/3,
         [{RandomHeight, 0},
          {RandomHeight, 1},
          {RandomHeight + 1, 0},
          {RandomHeight + 1, 1}]),
    Test(404, <<"Block not found">>, fun get_block_tx_by_index_hash/3,
         [{aec_base58c:encode(block_hash, random_hash()), 0},
          {aec_base58c:encode(block_hash, random_hash()), 1}]),
    BlocksToMine = 3,
    lists:foreach(
        fun(Height) ->
            {ok, 200, #{<<"count">> := TxsCount}} = get_block_txs_count_by_height(Height),
            Test(404, <<"Transaction not found">>, fun get_block_tx_by_index_height/3,
                [{Height, TxsCount + 1},
                 {Height, TxsCount + 2},
                 {Height, TxsCount + rand:uniform(1000) + 1}
                ]),
            {ok, Hash} = block_hash_by_height(Height),
            Test(404, <<"Transaction not found">>, fun get_block_tx_by_index_hash/3,
                [{Hash, TxsCount + 1},
                 {Hash, TxsCount + 2},
                 {Hash, TxsCount + rand:uniform(1000) + 1}
                ]),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end, lists:seq(0, BlocksToMine)),

    ok.


generic_block_tx_index_test(CallApi) when is_function(CallApi, 3)->
    ok = rpc(aec_conductor, reinit_chain, []),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    BlocksToMine = ?DEFAULT_TESTS_COUNT,
    lists:foreach(
        fun(Height) ->
            lists:foreach(
                fun({Opts, GetBlockDef, DataSchema}) ->
                    {ok, 200, BlockMap} =
                        get_block_by_height(Height, GetBlockDef),
                    AllTxs = maps:get(<<"transactions">>, BlockMap, []),
                    TxsIdxs =
                        case length(AllTxs) of
                            0 ->
                                [];
                            TxsLength ->
                                lists:seq(1, TxsLength)
                        end,
                    lists:foreach(
                        fun({Tx, Index}) ->
                            ct:log("Index: ~p, Transaction: ~p", [Index, Tx]),
                            {ok, 200, #{<<"data_schema">> := DataSchema,
                                        <<"transaction">> := Tx}} = CallApi(Height,
                                                                            Index,
                                                                            Opts)
                        end,
                        lists:zip(AllTxs, TxsIdxs))
                end,
                [{default, message_pack, <<"SingleTxMsgPack">>},
                 {message_pack, message_pack, <<"SingleTxMsgPack">>},
                 {json, json, <<"SingleTxJSON">>}]),
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            add_spend_txs()
        end,
        lists:seq(ForkHeight, ForkHeight + BlocksToMine)), % from latest fork
    ok.

block_txs_list_by_height(_Config) ->
    generic_range_test(fun get_block_txs_list_by_height/4,
                        fun(H) -> H end).

block_txs_list_by_hash(_Config) ->
    generic_range_test(fun get_block_txs_list_by_hash/4,
                        fun(H) ->
                            {ok, Hash} = block_hash_by_height(H),
                            Hash
                        end).

generic_range_test(GetTxsApi, HeightToKey) ->
    MaximumRange = rpc(aec_chain, max_block_range, []),
    CurrentHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    BlocksToMine = max(2 * MaximumRange - CurrentHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    lists:foreach(
        fun(From) ->
            lists:foreach(
                fun(Length) ->
                    single_range_test(From, From + Length,
                                       GetTxsApi, HeightToKey)
                end,
                [0, 1, MaximumRange - 1, MaximumRange])
        end,
        [0, 1, MaximumRange - 1, MaximumRange]),
    ok.

single_range_test(HeightFrom, HeightTo, GetTxsApi, HeightToKey) ->
    TxEncoding = [default, message_pack, json],
    AllTestedTxTypes = [[<<"spend_tx">>]],
    lists:foreach(
        fun({Encoding, TxTypes}) ->
            From = HeightToKey(HeightFrom),
            To = HeightToKey(HeightTo),

            % all transactions
            {ok, 200, Result} = GetTxsApi(From, To, Encoding, #{}),
            ExpectedResult =
                expected_range_result(HeightFrom, HeightTo, Encoding, all),
            ct:log("Expected Result ~p, Actual result ~p",
                    [ExpectedResult, Result]),
            ExpectedResult = Result,


            {ok, 200, Result1} = GetTxsApi(From, To, Encoding, #{include => TxTypes}),
            ExpectedResult1 =
                expected_range_result(HeightFrom, HeightTo, Encoding, {only, TxTypes}),
            ct:log("Showing only ~p, Expected Result ~p,~n Actual result ~p",
                    [TxTypes, ExpectedResult1, Result1]),
            ExpectedResult1 = Result1,

            {ok, 200, Result2} = GetTxsApi(From, To, Encoding, #{exclude => TxTypes}),
            ExpectedResult2 =
                expected_range_result(HeightFrom, HeightTo, Encoding, {exclude, TxTypes}),
            ct:log("Showing excluded ~p, Expected Result ~p,~n Actual result ~p",
                    [TxTypes, ExpectedResult2, Result2]),
            ExpectedResult2 = Result2,
            ok
        end,
        [{E, TxT} || E <- TxEncoding, TxT <- AllTestedTxTypes]).

expected_range_result(HeightFrom, HeightTo, TxEncoding0, TxTypes) ->
    expected_range_result(HeightFrom, HeightTo, TxEncoding0, TxTypes,
                          fun(_Tx) -> true end, false).

encode_pending_tx(Txs, TxEncoding0) ->
    TxEncoding =
        case TxEncoding0 of
            default -> message_pack;
            Other -> Other
        end,
    lists:map(
        fun(Tx) -> aetx_sign:serialize_for_client_pending(TxEncoding, Tx)
        end,
        Txs).

expected_range_result(HeightFrom, HeightTo, TxEncoding0, TxTypes, Filter,
                      Reverse) ->
    {ok, Blocks} = rpc(aec_chain, get_block_range_by_height, [HeightFrom,
                                                              HeightTo]),
    TxEncoding =
        case TxEncoding0 of
            default -> message_pack;
            Other -> Other
        end,
    SerializedTxs =
        lists:foldl(
            fun(Block, Accum) ->
                AllBlockTxs =
                    case Reverse of
                        false -> aec_blocks:txs(Block);
                        true -> lists:reverse(aec_blocks:txs(Block))
                    end,
                CustomFiltered = lists:filter(Filter, AllBlockTxs),
                FilteredTxs = apply_tx_type_filter(TxTypes, CustomFiltered),
                H = aec_blocks:to_header(Block),
                lists:map(
                    fun(Tx) ->
                        aetx_sign:serialize_for_client(TxEncoding, H, Tx)
                    end,
                    FilteredTxs) ++ Accum
            end,
            [],
            Blocks),
    DataSchema =
        case TxEncoding of
            default ->
                <<"MsgPackTxs">>;
            message_pack ->
                <<"MsgPackTxs">>;
            json ->
                <<"JSONTxs">>
          end,
    #{<<"data_schema">> => DataSchema,
      <<"transactions">> => SerializedTxs}.

apply_tx_type_filter(TxTypes, Txs) ->
    case TxTypes of
        all ->
            Txs;
        {only, Includes} ->
            lists:filter(
                fun(Tx) -> lists:member(tx_type(Tx), Includes) end,
                Txs);
        {exclude, Excludes} ->
            lists:filter(
                fun(Tx) -> not lists:member(tx_type(Tx), Excludes) end,
                Txs)
      end.

tx_type(SignedTx) ->
    Tx = aetx_sign:tx(SignedTx),
    aetx:tx_type(Tx).

block_txs_list_by_height_invalid_range(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    MaximumRange = rpc(aec_chain, max_block_range, []),
    BlocksToMine = max(2 * MaximumRange - InitialHeight, 0),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    Error = get_block_txs_list_by_height(From, To, Opts, #{})
                end,
                [default, message_pack, json])
        end,
    InvalidRange = {ok, 400, #{<<"reason">> => <<"From's height is bigger than To's">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, InvalidRange) end,
        [{1, 0}, {3, 1}]),
    RangeTooBig = {ok, 400, #{<<"reason">> => <<"Range too big">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, RangeTooBig) end,
        [{0, MaximumRange +1}, {1, MaximumRange + 2}]),
    ChainTooShort = {ok, 404, #{<<"reason">> => <<"Chain too short">>}},
    CurrentHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, ChainTooShort) end,
        [{CurrentHeight - 1, CurrentHeight + 1},
         {CurrentHeight, CurrentHeight + 1},
         {CurrentHeight + 1, CurrentHeight + 1}]),
    ok.

block_txs_list_by_hash_invalid_range(_Config) ->
    InitialHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    MaximumRange = rpc(aec_chain, max_block_range, []),
    BlocksToMine = 2 * MaximumRange,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    ValidateError =
        fun(From, To, Error) ->
            lists:foreach(
                fun(Opts) ->
                    case get_block_txs_list_by_hash(From, To, Opts, #{}) of
                      Error -> Error;
                      Other -> error({expected, Error, got, Other})
                    end
                end,
                [default, message_pack, json])
        end,
    {ok, GenBlockHash} = block_hash_by_height(0),
    {ok, Block1Hash} = block_hash_by_height(InitialHeight),
    {ok, Block2Hash} = block_hash_by_height(InitialHeight + 1),
    {ok, BlockX1Hash} = block_hash_by_height(InitialHeight + MaximumRange + 1),
    {ok, BlockX2Hash} = block_hash_by_height(InitialHeight + MaximumRange + 2),
    InvalidRange = {ok, 400, #{<<"reason">> => <<"From's height is bigger than To's">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, InvalidRange) end,
        [{Block1Hash, GenBlockHash},
         {Block2Hash, Block1Hash}]),
    RangeTooBig = {ok, 400, #{<<"reason">> => <<"Range too big">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, RangeTooBig) end,
        [{GenBlockHash, BlockX1Hash},
         {Block1Hash, BlockX2Hash}]),
    ChainTooShort = {ok, 404, #{<<"reason">> => <<"Block not found">>}},
    lists:foreach(
        fun({From, To}) -> ValidateError(From, To, ChainTooShort) end,
        [{GenBlockHash, aec_base58c:encode(block_hash, random_hash())},
         {Block1Hash, aec_base58c:encode(block_hash, random_hash())},
         {aec_base58c:encode(block_hash, random_hash()), Block1Hash},
         {aec_base58c:encode(block_hash, random_hash()), GenBlockHash}
        ]),
    ok.

naming_system_manage_name(_Config) ->
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    PubKeyEnc   = aec_base58c:encode(account_pubkey, PubKey),
    Name        = <<"詹姆斯詹姆斯.test"/utf8>>,
    NameSalt    = 12345,
    NameTTL     = 20000,
    Pointers    = <<"{\"account_pubkey\":\"", PubKeyEnc/binary, "\"}">>,
    TTL         = 10,
    {ok, NHash} = aens:get_name_hash(Name),
    Fee         = 2,
    MineReward  = rpc(aec_governance, block_mine_reward, []),

    %% Mine a block to get some funds
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, #{<<"balance">> := Balance}} = get_balance_at_top(),

    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Get commitment hash to preclaim a name
    {ok, 200, #{<<"commitment">> := EncodedCHash}} = get_commitment_hash(Name, NameSalt),
    {ok, CHash} = aec_base58c:safe_decode(commitment, EncodedCHash),

    %% Submit name preclaim tx and check it is in mempool
    {ok, 200, _}                   = post_name_preclaim_tx(CHash, Fee),
    {ok, [PreclaimTx0]}            = rpc(aec_tx_pool, peek, [infinity]),
    {name_preclaim_tx, PreclaimTx} = aetx:specialize_type(aetx_sign:tx(PreclaimTx0)),
    CHash                          = aens_preclaim_tx:commitment(PreclaimTx),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check fee taken from account, then mine reward and fee added to account
    {ok, 200, #{<<"balance">> := Balance1}} = get_balance_at_top(),
    Balance1 = Balance - Fee + MineReward + Fee,

    %% Submit name claim tx and check it is in mempool
    {ok, 200, _}             = post_name_claim_tx(Name, NameSalt, Fee),
    {ok, [ClaimTx0]}         = rpc(aec_tx_pool, peek, [infinity]),
    {name_claim_tx, ClaimTx} = aetx:specialize_type(aetx_sign:tx(ClaimTx0)),
    Name                     = aens_claim_tx:name(ClaimTx),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check tx fee taken from account, claim fee burned,
    %% then mine reward and fee added to account
    ClaimBurnedFee = rpc(aec_governance, name_claim_burned_fee, []),
    {ok, 200, #{<<"balance">> := Balance2}} = get_balance_at_top(),
    Balance2 = Balance1 - Fee + MineReward + Fee - ClaimBurnedFee,

    %% Check that name entry is present
    EncodedNHash = aec_base58c:encode(name, NHash),
    ExpectedTTL1 = 3 + aec_governance:name_claim_max_expiration(),
    {ok, 200, #{<<"name">>      := Name,
                <<"name_hash">> := EncodedNHash,
                <<"name_ttl">>  := ExpectedTTL1,
                <<"pointers">>  := <<"[]">>}} = get_name(Name),

    %% Submit name updated tx and check it is in mempool
    {ok, 200, _}               = post_name_update_tx(NHash, NameTTL, Pointers, TTL, Fee),
    {ok, [UpdateTx0]}          = rpc(aec_tx_pool, peek, [infinity]),
    {name_update_tx, UpdateTx} = aetx:specialize_type(aetx_sign:tx(UpdateTx0)),
    NameTTL                    = aens_update_tx:name_ttl(UpdateTx),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check that TTL and pointers got updated in name entry
    ExpectedTTL2 = 4 + NameTTL,
    {ok, 200, #{<<"name">>     := Name,
                <<"name_ttl">> := ExpectedTTL2,
                <<"pointers">> := Pointers}} = get_name(Name),

    {ok, 200, #{<<"balance">> := Balance3}} = get_balance_at_top(),
    Host = internal_address(),
    {ok, 200, _} = http_request(Host, post, "spend-tx",
                                #{recipient_pubkey => Name,
                                  amount           => 77,
                                  fee              => 50,
                                  payload          => <<"foo">>}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Nothing gets lost as recipient = sender = miner
    %% This tests 'resolve_name' because recipient is expressed by name label
    %% This tests passes with 1 block confirmation due to lack of miner's reward delay
    FinalBalance = Balance3+MineReward,
    {ok, 200, #{<<"balance">> := FinalBalance}} = get_balance_at_top(),

    %% Submit name transfer tx and check it is in mempool
    {ok, DecodedPubkey}            = aec_base58c:safe_decode(account_pubkey, PubKeyEnc),
    {ok, 200, _}                   = post_name_transfer_tx(NHash, DecodedPubkey, Fee),
    {ok, [TransferTx0]}            = rpc(aec_tx_pool, peek, [infinity]),
    {name_transfer_tx, TransferTx} = aetx:specialize_type(aetx_sign:tx(TransferTx0)),
    DecodedPubkey                  = aens_transfer_tx:recipient_account(TransferTx),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Submit name revoke tx and check it is in mempool
    {ok, 200, _}      = post_name_revoke_tx(NHash, Fee),
    {ok, [_RevokeTx]} = rpc(aec_tx_pool, peek, [infinity]),

    %% Mine a block and check mempool empty again
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Check the name got expired
    {ok, 404, #{<<"reason">> := <<"Name revoked">>}} = get_name(Name),
    ok.

naming_system_broken_txs(_Config) ->
    Name        = <<"fooo.test">>,
    NameSalt    = 12345,
    {ok, NHash} = aens:get_name_hash(Name),
    CHash       = aens_hash:commitment_hash(Name, NameSalt),
    Fee         = 2,

    % these tests require that no accounts are present
    ok = rpc(aec_conductor, reinit_chain, []),
    %% Check mempool empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),

    %% Try to submit txs with empty account

    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_commitment_hash(<<"abcd.badregistrar">>, 123),
    {ok, 400, #{<<"reason">> := <<"Name validation failed with a reason: registrar_unknown">>}} =
        get_name(<<"abcd.badregistrar">>),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        post_name_preclaim_tx(CHash, Fee),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        post_name_claim_tx(Name, NameSalt, Fee),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        post_name_update_tx(NHash, 5, <<"pointers">>, 5, Fee),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        post_name_transfer_tx(NHash, random_hash(), Fee),
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} =
        post_name_revoke_tx(NHash, Fee),

    %% Check mempool still empty
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]),
    ForkHeight = aecore_suite_utils:latest_fork_height(),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   ForkHeight),
    ok.

list_oracles(_Config) ->
    %% Mine a blocks to get some funds
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    KeyPair = fun() ->
                  #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
                  {Pub, Priv}
              end,
    KeyPairs = [ KeyPair() || _ <- lists:seq(1, 5) ],

    %% Transfer some funds to these accounts
    [ post_spend_tx(Receiver, 9, 1) || {Receiver, _} <- KeyPairs ],

    %% Mine a block to effect this
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Now register those accounts as oracles...
    [ register_oracle(6, PubKey, PrivKey, 1, 4, {delta, 50})
      || {PubKey, PrivKey} <- KeyPairs ],

    %% Mine a block to effect the registrations
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Now we can test the oracle listing...
    Oracles = get_list_oracles(5),
    Os1 = lists:sort([aec_base58c:encode(oracle_pubkey, PubKey) ||  {PubKey, _} <- KeyPairs ]),
    Os2 = lists:sort([ maps:get(<<"address">>, O) || O <- Oracles ]),

    ct:log("Os1 = ~p\nOs2 = ~p", [Os1, Os2]),
    Os1 = Os2,

    %% Try pagination
    Oracles1 = [_, O2] = get_list_oracles(2),
    Oracles2 = get_list_oracles(maps:get(<<"address">>, O2), 3),
    Os3 = lists:sort([ maps:get(<<"address">>, O) || O <- Oracles1 ++ Oracles2 ]),

    ct:log("Os3 = ~p\nOs2 = ~p", [Os3, Os2]),
    Os3 = Os2,

    ok.

list_oracle_queries(_Config) ->
    %% Mine a block to get some funds
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    KeyPair = fun() ->
                  #{ public := Pub, secret := Priv } = enacl:sign_keypair(),
                  {Pub, Priv}
              end,
    OKeyPairs = [ KeyPair() || _ <- lists:seq(1, 2) ],

    {APubKey, APrivKey} = KeyPair(),

    %% Transfer some funds to these accounts
    [ post_spend_tx(Receiver, 9, 1) || {Receiver, _} <- OKeyPairs ],
    post_spend_tx(APubKey, 79, 1),

    %% Mine a block to effect this
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Now register both accounts as oracles...
    [ register_oracle(2, PubKey, PrivKey, 1, 3, {delta, 50})
      || {PubKey, PrivKey} <- OKeyPairs ],

    %% Mine a block to effect the registrations
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Query each oracle four times
    [{OPubKey1, _}, {OPubKey2, _}] = OKeyPairs,
    [ query_oracle(3, APubKey, APrivKey, OPubKey1, N, <<"a query">>, {delta, 20}, 3)
      || N <- lists:seq(1, 4) ],
    [ query_oracle(3, APubKey, APrivKey, OPubKey2, N, <<"a query">>, {delta, 20}, 3)
      || N <- lists:seq(5, 8) ],

    QueryIds = [ aeo_query:id(APubKey, N, OPubKey1) || N <- lists:seq(1, 4) ] ++
               [ aeo_query:id(APubKey, N, OPubKey2) || N <- lists:seq(5, 8) ],

    %% Mine a block to effect the queries
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),

    %% Now we can test the oracle query listing...
    Queriess = [ get_list_oracle_queries(OPubKey, 4) || {OPubKey, _} <- OKeyPairs ],

    QS1 = lists:sort([ aec_base58c:encode(oracle_query_id, QId) || QId <- QueryIds ]),
    QS2 = lists:sort([ maps:get(<<"query_id">>, Q) || Qs <- Queriess, Q <- Qs ]),

    ct:log("Qs1 = ~p\nQs2 = ~p", [QS1, QS2]),
    QS1 = QS2,

    %% Try pagination
    Queries1 = [_, Q2] = get_list_oracle_queries(OPubKey1, 2),
    Queries2 = get_list_oracle_queries(OPubKey1, maps:get(<<"query_id">>, Q2), 3),
    Queries3 = [Q4] = get_list_oracle_queries(OPubKey2, 1),
    Queries4 = get_list_oracle_queries(OPubKey2, maps:get(<<"query_id">>, Q4), 3),

    QS3 = lists:sort([ maps:get(<<"query_id">>, Q)
                       || Q <- Queries1 ++ Queries2 ++ Queries3 ++ Queries4 ]),

    ct:log("Qs3 = ~p\nQs2 = ~p", [QS3, QS2]),
    QS3 = QS2,

    ok.

register_oracle(ChainHeight, PubKey, PrivKey, Nonce, QueryFee, TTL) ->
    TTLFee = aeo_utils:ttl_fee(1, aeo_utils:ttl_delta(ChainHeight, TTL)),
    {ok, RegTx} = aeo_register_tx:new(#{account       => PubKey,
                                        nonce         => Nonce,
                                        query_spec    => <<"TODO">>,
                                        response_spec => <<"TODO">>,
                                        query_fee     => QueryFee,
                                        oracle_ttl    => TTL,
                                        fee           => 4 + TTLFee}),
    SignedTx = aetx_sign:sign(RegTx, PrivKey),
    SendTx = aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    post_tx(SendTx).

query_oracle(ChainHeight, PubKey, PrivKey, Oracle, Nonce, Query, TTL, QueryFee) ->
    TTLFee = aeo_utils:ttl_fee(1, aeo_utils:ttl_delta(ChainHeight, TTL)),
    {ok, QueryTx} = aeo_query_tx:new(#{sender        => PubKey,
                                       nonce         => Nonce,
                                       oracle        => Oracle,
                                       query         => Query,
                                       query_fee     => QueryFee,
                                       query_ttl     => TTL,
                                       response_ttl  => {delta, 10},
                                       fee           => QueryFee + 2 + TTLFee }),
    SignedTx = aetx_sign:sign(QueryTx, PrivKey),
    SendTx = aec_base58c:encode(transaction, aetx_sign:serialize_to_binary(SignedTx)),
    {ok, 200, Res} = post_tx(SendTx),
    Res.

%% ============================================================
%% Websocket tests
%% ============================================================

ws_get_genesis(_Config) ->
    {ok, ConnPid} = ws_start_link(),
    {_Tag, #{ <<"block">> := Block }} =
        ws_chain_get(ConnPid, #{height => 0, type => block}),
    {ok, 200, BlockMap} = get_block_by_height(0, message_pack),
    ExpectedBlockMap =
        maps:remove(<<"hash">>, maps:remove(<<"data_schema">>, BlockMap)),
    ?assertEqual(ExpectedBlockMap, Block),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

ws_request_tag(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Test with tag
    {<<"supersecret_tag">>, #{<<"block">> := _Block}} =
        ws_chain_get(ConnPid, supersecret_tag, #{height => 0, type => block}),

    %% And test without tag
    {<<"untagged">>, #{<<"block">> := _Block}} =
        ws_chain_get(ConnPid, #{height => 0, type => block}),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.


ws_block_mined(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Register for mined_block events
    ws_subscribe(ConnPid, #{ type => mined_block }),

    {Height, Hash} = ws_mine_block(ConnPid, ?NODE),

    {_Tag, #{<<"block">> := Block}} = ws_chain_get(ConnPid, #{height => Height, type => block}),
    {_Tag, #{<<"block">> := Block}} = ws_chain_get(ConnPid, #{hash => Hash, type => block}),
    {_Tag, #{<<"header">> := Header}} = ws_chain_get(ConnPid, #{height => Height, type => header}),
    {_Tag, #{<<"header">> := Header}} = ws_chain_get(ConnPid, #{hash => Hash, type => header}),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

%% Currently the websockets are a queue: they have a maximim amount of
%% acceptors. Every WS trying to connect after all acceptoprs are used
%% goes into a queue. When the queue is full - the node starts rejecting
%% any new incoming WS connections
ws_refused_on_limit_reached(_Config) ->
    %% maximum amount of acceptors
    MaxWsCount = rpc(aeu_env, user_config_or_env,
                          [[<<"websocket">>, <<"internal">>, <<"acceptors">>],
                          aehttp, [internal, websocket, handlers], 10]),
    %% Maximum WS connections hanging in the queue
    WSQueueSize = rpc(aehttp_app, ws_handlers_queue_max_size, []),
    WSDieTimeout = 1000,
    ct:log("Websocket acceptors: ~p, websocket acceptor's queue size ~p",
           [MaxWsCount, WSQueueSize]),
    %% assert no WS running on the node
    0 = open_websockets_count(),
    %% start as many WS as needed to consume all acceptors
    WSPids =
        lists:map(
            fun(_) ->
              {ok, ConnPid} = ws_start_link(),
              ConnPid
            end,
            lists:seq(1, MaxWsCount)),
    %% assert expectation for amount of connected WSs
    MaxWsCount = open_websockets_count(),
    WaitingPids =
        lists:map(
            fun(_) ->
                %% try to connect a WS client; assert it does not connect and
                %% is waiting
                {error, still_connecting, WaitingPid} = ws_start_link(),
                MaxWsCount = open_websockets_count(),
                WaitingPid
            end,
            lists:seq(1, WSQueueSize)),

    %% Now both the acceptopr pool and queue are full. Try to connect a new WS
    %% client and assert it fails
    {error, rejected} = ws_start_link(),

    %% split currently connected WS clients into two groups: first with as
    %% many as there are WS clients waiting in the queue and all the rest in a
    %% seperate list. The first group would be used for stopping WS clients
    %% one by one while validating that one of the waiting WS clients connects
    %% for each one stopped
    {FirstWSsPids, OtherWSsPids} = lists:split(WSQueueSize, WSPids),
    lists:foreach(
        fun(Pid) ->
            %% stop one
            ?WS:stop(Pid),
            %% another one connects in its place and it doesn't matter which one
            {ok, _SomePid} = ?WS:wait_for_connect_any(),
            %% total amount of connected WS clients is still the maximum
            MaxWsCount = open_websockets_count()
        end,
        FirstWSsPids),
    %% cleanup
    lists:foreach(fun ?WS:stop/1, OtherWSsPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, WSQueueSize} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              WSQueueSize, WSDieTimeout),
    lists:foreach(fun ?WS:stop/1, WaitingPids),
    timer:sleep(100), % wait for all of them to die out
    {ok, 0} =
        aec_test_utils:wait_for_it_or_timeout(fun open_websockets_count/0,
                                              0, WSDieTimeout),
    ok.

ws_tx_on_chain(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Register for events when a block is mined!
    ws_subscribe(ConnPid, #{ type => mined_block }),

    %% Mine a block to make sure the Pubkey has some funds!
    ws_mine_block(ConnPid, ?NODE),

    %% Fetch the pubkey via HTTP
    {ok, 200, #{ <<"pub_key">> := PK }} = get_miner_pub_key(),

    %% Register an oracle
    RegisterData =
        #{ type => 'OracleRegisterTxObject',
           account => PK,
           query_format => <<"the query spec">>,
           response_format => <<"the response spec">>,
           query_fee => 4,
           oracle_ttl => #{ type => delta, value => 500 },
           fee => 5 },
    #{<<"result">> := <<"ok">>,
      <<"tx_hash">> := TxHash } = ws_do_request(ConnPid, oracle, register, RegisterData),

    %% Subscribe for an event once the Tx goes onto the chain...
    ws_subscribe(ConnPid, #{ type => tx, tx_hash => TxHash }),
    ok = ?WS:register_test_for_event(ConnPid, chain, tx_chain),

    %% Mine a block and check that an event is receieved corresponding to
    %% the Tx.
    ws_mine_block(ConnPid, ?NODE),
    {ok, #{<<"tx_hash">> := TxHash }} = ?WS:wait_for_event(ConnPid, chain, tx_chain),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.

ws_oracles(_Config) ->
    {ok, ConnPid} = ws_start_link(),

    %% Register for events when a block is mined!
    ws_subscribe(ConnPid, #{ type => mined_block }),

    %% Mine a block to make sure the Pubkey has some funds!
    ws_mine_block(ConnPid, ?NODE),

    %% Fetch the pubkey via HTTP
    {ok, 200, #{ <<"pub_key">> := PK }} = get_miner_pub_key(),

    %% The account is already an oracle from an earlier test case.
    {account_pubkey, ActualPK} = aec_base58c:decode(PK),
    OId = aec_base58c:encode(oracle_pubkey, ActualPK),

    %% Mine a block to get the oracle onto the chain
    ws_mine_block(ConnPid, ?NODE),

    %% Register for events when the freshly registered oracle is queried!
    ws_subscribe(ConnPid, #{ type => oracle_query, oracle_id => OId }),
    ok = ?WS:register_test_for_event(ConnPid, chain, new_oracle_query),

    %% Post a query
    QueryData =
        #{ type => 'OracleQueryTxObject',
           oracle_pubkey => OId,
           query_ttl => #{ type => delta, value => 10 },
           response_ttl => #{ type => delta, value => 10 },
           query => <<"How are you doing?">>,
           query_fee => 4,
           fee => 7 },
    #{<<"result">> := <<"ok">>,
      <<"query_id">> := QId } = ws_do_request(ConnPid, oracle, query, QueryData),

    %% Mine a block and check that an event is receieved corresponding to
    %% the query.
    ws_mine_block(ConnPid, ?NODE),
    {ok, #{<<"query_id">> := QId }} = ?WS:wait_for_event(ConnPid, chain, new_oracle_query),

    %% Subscribe to responses to the query.
    ws_subscribe(ConnPid, #{ type => oracle_response, query_id => QId }),
    ok = ?WS:register_test_for_event(ConnPid, chain, new_oracle_response),

    %% Post a response to the query
    ResponseData = #{ type => 'OracleResponseTxObject',
                      query_id => QId,
                      response => <<"I am fine, thank you!">>,
                      fee => 3 },
    #{<<"result">> := <<"ok">>,
      <<"query_id">> := QId } = ws_do_request(ConnPid, oracle, response, ResponseData),

    %% Mine a block and check that an event is received
    ws_mine_block(ConnPid, ?NODE),
    {ok, #{<<"query_id">> := QId }} = ?WS:wait_for_event(ConnPid, chain, new_oracle_response),

    %% Check that we can extend the oracle TTL
    ExtendData =
        #{ type => 'OracleExtendTxObject',
           oracle => OId,
           oracle_ttl => #{ type => delta, value => 50 },
           fee => 2 },
    #{<<"result">> := <<"ok">>,
      <<"tx_hash">> := ExtendTxHash,
      <<"oracle_id">> := OId } = ws_do_request(ConnPid, oracle, extend, ExtendData),

    %% Subscribe for an event once the Tx goes onto the chain...
    ws_subscribe(ConnPid, #{ type => tx, tx_hash => ExtendTxHash }),
    ok = ?WS:register_test_for_event(ConnPid, chain, tx_chain),

    %% Mine a block and check that the extend tx made it onto the chain
    ws_mine_block(ConnPid, ?NODE),
    {ok, #{<<"tx_hash">> := ExtendTxHash }} = ?WS:wait_for_event(ConnPid, chain, tx_chain),

    ok = aehttp_ws_test_utils:stop(ConnPid),
    ok.
%%
%% Channels
%%
assert_balance(Pubkey, ExpectedBalance) ->
    Address = aec_base58c:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := ExpectedBalance}} = get_balance_at_top(Address).

channel_sign_tx(ConnPid, Privkey, Tag) ->
    {ok, Tag, #{<<"tx">> := EncCreateTx}} = ?WS:wait_for_channel_event(ConnPid, sign),
    {ok, CreateBinTx} = aec_base58c:safe_decode(transaction, EncCreateTx),
    Tx = aetx:deserialize_from_binary(CreateBinTx),
    SignedCreateTx = aetx_sign:sign(Tx, Privkey),
    EncSignedCreateTx = aec_base58c:encode(transaction,
                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
    ?WS:send(ConnPid, Tag, #{tx => EncSignedCreateTx}),
    Tx.

sc_ws_open(Config) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),

    {ok, 200, #{<<"balance">> := IStartAmt}} =
                 get_balance_at_top(aec_base58c:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := RStartAmt}} =
                 get_balance_at_top(aec_base58c:encode(account_pubkey, RPubkey)),
    IAmt = 7,
    RAmt = 4,

    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt),
    {ok, IConnPid} = channel_ws_start(initiator,
                                           maps:put(host, <<"localhost">>, ChannelOpts)),
    ok = ?WS:register_test_for_channel_events(IConnPid, [info, sign, on_chain_tx]),

    {ok, RConnPid} = channel_ws_start(responder, ChannelOpts),

    ok = ?WS:register_test_for_channel_events(RConnPid, [info, sign, on_chain_tx]),

    channel_send_conn_open_infos(RConnPid, IConnPid),

    ChannelCreateFee = channel_create(Config, IConnPid, RConnPid),

    %% ensure new balances
    assert_balance(IPubkey, IStartAmt - IAmt - ChannelCreateFee),
    assert_balance(RPubkey, RStartAmt - RAmt),

    % mine min depth
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 4),

    channel_send_locking_infos(IConnPid, RConnPid),

    channel_send_chan_open_infos(RConnPid, IConnPid),

    ChannelClients = #{initiator => IConnPid,
                       responder => RConnPid},
    {save_config, [{channel_clients, ChannelClients} | Config]}.


channel_send_conn_open_infos(RConnPid, IConnPid) ->
    {ok, #{<<"event">> := <<"channel_open">>}} = ?WS:wait_for_channel_event(RConnPid, info),
    {ok, #{<<"event">> := <<"channel_accept">>}} = ?WS:wait_for_channel_event(IConnPid, info).

channel_send_locking_infos(IConnPid, RConnPid) ->
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"own_funding_locked">>}} = ?WS:wait_for_channel_event(RConnPid, info),

    {ok, #{<<"event">> := <<"funding_locked">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"funding_locked">>}} = ?WS:wait_for_channel_event(RConnPid, info).

channel_send_chan_open_infos(RConnPid, IConnPid) ->
    {ok, #{<<"event">> := <<"open">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"open">>}} = ?WS:wait_for_channel_event(RConnPid, info).

channel_participants_balances(IPubkey, RPubkey) ->
    {ok, 200, #{<<"balance">> := BalI}} =
        get_balance_at_top(aec_base58c:encode(account_pubkey, IPubkey)),
    {ok, 200, #{<<"balance">> := BalR}} =
        get_balance_at_top(aec_base58c:encode(account_pubkey, RPubkey)),
    {BalI, BalR}.

channel_create(Config, IConnPid, RConnPid) ->
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants, Config),
    %% initiator gets to sign a create_tx
    {IStartAmt, RStartAmt} = channel_participants_balances(IPubkey, RPubkey),

    CrTx = channel_sign_tx(IConnPid, IPrivkey, <<"initiator_sign">>),
    {ok, #{<<"event">> := <<"funding_created">>}} = ?WS:wait_for_channel_event(RConnPid, info),
    %% responder gets to sign a create_tx
    CrTx = channel_sign_tx(RConnPid, RPrivkey, <<"responder_sign">>),
    {ok, #{<<"event">> := <<"funding_signed">>}} = ?WS:wait_for_channel_event(IConnPid, info),

    %% both of them receive the same co-signed channel_create_tx
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = ?WS:wait_for_channel_event(IConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedCrTx}} = ?WS:wait_for_channel_event(RConnPid, on_chain_tx),

    {ok, SSignedCrTx} = aec_base58c:safe_decode(transaction, EncodedSignedCrTx),
    SignedCrTx = aetx_sign:deserialize_from_binary(SSignedCrTx),
    %% same transaction
    CrTx = aetx_sign:tx(SignedCrTx),

    {channel_create_tx, Tx} = aetx:specialize_type(CrTx),
    IPubkey = aesc_create_tx:initiator(Tx),
    RPubkey = aesc_create_tx:responder(Tx),
    ChannelCreateFee = aesc_create_tx:fee(Tx),

    %% ensure the tx is in the mempool
    ok = wait_for_signed_transaction_in_pool(SignedCrTx),

    %% balances hadn't changed yet
    assert_balance(IPubkey, IStartAmt),
    assert_balance(RPubkey, RStartAmt),

    % mine the create_tx
    ok = wait_for_signed_transaction_in_block(SignedCrTx),

    ChannelCreateFee.

sc_ws_update(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, Config),
    Conns = proplists:get_value(channel_clients, ConfigList),
    lists:foldl(
        fun(Sender, Round) ->
            channel_update(Conns, Sender, Participants, 1, Round),
            Round + 1
        end,
        2, % we start from round 2
        [initiator,
         responder,
         responder,
         initiator,
         responder]),
    {save_config, ConfigList}.

sc_ws_update_fails_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} = Conns =
        proplists:get_value(channel_clients, ConfigList),
    lists:foreach(
        fun(Sender) ->
            {ok, #{<<"reason">> := <<"insufficient_balance">>,
                  <<"request">> := _Request0}} = channel_update_fail(Conns, Sender,
                                                                    Participants, 10000),
            {ok, #{<<"reason">> := <<"negative_amount">>,
                  <<"request">> := _Request1}} = channel_update_fail(Conns, Sender,
                                                                     Participants,
                                                                     -1),
            {ok, #{<<"reason">> := <<"invalid_pubkeys">>,
                  <<"request">> := _Request2}} =
                channel_update_fail(Conns, Sender,
                                    #{initiator => #{pub_key => <<123456>>},
                                      responder => #{pub_key => <<654321>>}},
                                    1),
            ok
        end,
        [initiator, responder]),
    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_send_messages_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} =
        proplists:get_value(channel_clients, ConfigList),

    lists:foreach(
        fun({Sender, Msg}) ->
            {SenderPubkey, ReceiverPubkey, SenderPid, ReceiverPid} =
                case Sender of
                    initiator ->
                        {IPubkey, RPubkey, IConnPid, RConnPid};
                    responder ->
                        {RPubkey, IPubkey, RConnPid, IConnPid}
                end,
            SenderEncodedK = aec_base58c:encode(account_pubkey, SenderPubkey),
            ReceiverEncodedK = aec_base58c:encode(account_pubkey, ReceiverPubkey),
            ok = ?WS:register_test_for_channel_event(ReceiverPid, message),

            ?WS:send(SenderPid, <<"message">>,
                    #{<<"to">> => ReceiverEncodedK,
                      <<"info">> => Msg}),

            {ok, #{<<"message">> := #{<<"from">> := SenderEncodedK,
                                      <<"to">> := ReceiverEncodedK,
                                      <<"info">> := Msg}}}
                = ?WS:wait_for_channel_event(ReceiverPid, message),
            ok = ?WS:unregister_test_for_channel_event(ReceiverPid, message)
        end,
        [ {initiator, <<"hejsan">>}                   %% initiator can send
        , {responder, <<"svejsan">>}                  %% responder can send
        , {initiator, <<"first message in a row">>}   %% initiator can send two messages in a row
        , {initiator, <<"second message in a row">>}
        , {responder, <<"some message">>}             %% responder can send two messages in a row
        , {responder, <<"other message">>}
        ]),

    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

sc_ws_conflict_and_close(Config) ->
    {sc_ws_open, ConfigList} = ?config(saved_config, Config),
    Participants = proplists:get_value(participants, Config),
    #{initiator := IConnPid, responder :=RConnPid} = Conns =
        proplists:get_value(channel_clients, ConfigList),

    lists:foreach(
        fun(FirstSender) ->
            channel_conflict(Conns, FirstSender, Participants, 1, 2)
        end,
        [initiator,
         responder]),

    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.

channel_conflict(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount1, Amount2) ->
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey, RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey, IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, conflict]),

    SignUpdate =
        fun TrySignUpdate(ConnPid, Privkey) ->
            case ?WS:wait_for_channel_event(ConnPid, sign) of
                {ok, <<"update_ack">>, _} -> %% this is not the message we are looking for
                    TrySignUpdate(ConnPid, Privkey);
                {ok, <<"update">>, #{<<"tx">> := EncCreateTx}} ->
                    {ok, CreateBinTx} = aec_base58c:safe_decode(transaction, EncCreateTx),
                    Tx = aetx:deserialize_from_binary(CreateBinTx),
                    SignedCreateTx = aetx_sign:sign(Tx, Privkey),
                    EncSignedCreateTx = aec_base58c:encode(transaction,
                                                  aetx_sign:serialize_to_binary(SignedCreateTx)),
                    ?WS:send(ConnPid, <<"update">>, #{tx => EncSignedCreateTx})
            end
        end,
    %% sender initiates an update
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount1}),

    %% starter signs the new state

    %% acknowledger initiates an update too
    ?WS:send_tagged(AcknowledgerPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount2}),

    SignUpdate(StarterPid, StarterPrivkey),
    SignUpdate(AcknowledgerPid, AcknowledgerPrivkey),

    {ok, _} = ?WS:wait_for_channel_event(StarterPid, conflict),
    {ok, _} = ?WS:wait_for_channel_event(AcknowledgerPid, conflict),

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, conflict]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, conflict]),

    ok.

channel_update(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey,
                                priv_key := IPrivkey},
                 responder := #{pub_key := RPubkey,
                                priv_key := RPrivkey}},
               Amount, Round) ->
    true = undefined =/= process_info(IConnPid),
    true = undefined =/= process_info(RConnPid),
    {StarterPid, AcknowledgerPid, StarterPubkey, StarterPrivkey,
     AcknowledgerPubkey, AcknowledgerPrivkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, IPrivkey,
                                    RPubkey, RPrivkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, RPrivkey,
                                    IPubkey, IPrivkey}
        end,
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, update]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, update]),

    %% sender initiates an update
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount}),

    %% starter signs the new state
    UnsignedStateTx = channel_sign_tx(StarterPid, StarterPrivkey, <<"update">>),
    ct:log("Unsigned state tx ~p", [UnsignedStateTx]),
    %% verify contents
    {channel_offchain_tx, OffchainTx} = aetx:specialize_type(UnsignedStateTx),
    [Update] = aesc_offchain_tx:updates(OffchainTx),
    Expected = aesc_offchain_state:op_transfer(StarterPubkey,
                                               AcknowledgerPubkey, Amount),
    Expected = Update,


    %% acknowledger signs the new state
    {ok, #{<<"event">> := <<"update">>}} = ?WS:wait_for_channel_event(AcknowledgerPid, info),
    UnsignedStateTx = channel_sign_tx(AcknowledgerPid, AcknowledgerPrivkey, <<"update_ack">>),

    {ok, #{<<"state">> := NewState}} = ?WS:wait_for_channel_event(IConnPid,
                                                                 update),
    {ok, #{<<"state">> := NewState}} = ?WS:wait_for_channel_event(RConnPid,
                                                                 update),
    {ok, SignedStateTxBin} = aec_base58c:safe_decode(transaction, NewState),
    SignedStateTx = aetx_sign:deserialize_from_binary(SignedStateTxBin),

    %% validate it is co-signed
    {ok, Trees} = rpc(aec_chain, get_top_state, []),
    ok = rpc(aetx_sign, verify, [SignedStateTx, Trees]), % RPC because of DB
    {UnsignedStateTx, _} = % same transaction that was signed
        {aetx_sign:tx(SignedStateTx), UnsignedStateTx},

    ok = ?WS:unregister_test_for_channel_events(IConnPid, [sign, info, update]),
    ok = ?WS:unregister_test_for_channel_events(RConnPid, [sign, info, update]),

    ok.


channel_update_fail(#{initiator := IConnPid, responder :=RConnPid},
               StarterRole,
               #{initiator := #{pub_key := IPubkey},
                 responder := #{pub_key := RPubkey}},
               Amount) ->
    {StarterPid, _AcknowledgerPid, StarterPubkey, AcknowledgerPubkey} =
        case StarterRole of
            initiator ->
                {IConnPid, RConnPid, IPubkey, RPubkey};
            responder ->
                {RConnPid, IConnPid, RPubkey, IPubkey}
        end,
    ok = ?WS:register_test_for_channel_event(StarterPid, error),

    %% sender initiates an update
    ?WS:send_tagged(StarterPid, <<"update">>, <<"new">>,
        #{from => aec_base58c:encode(account_pubkey, StarterPubkey),
          to => aec_base58c:encode(account_pubkey, AcknowledgerPubkey),
          amount => Amount}),

    {ok, _Payload}= Res = ?WS:wait_for_channel_event(StarterPid, error),


    ok = ?WS:unregister_test_for_channel_event(StarterPid, error),
    Res.

sc_ws_close(Config) ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),

    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),


    ok = ?WS:stop(IConnPid),
    ok = ?WS:stop(RConnPid),
    ok.


sc_ws_close_mutual_inititator(Config) ->
    sc_ws_close_mutual(Config, initiator).

sc_ws_close_mutual_responder(Config) ->
    sc_ws_close_mutual(Config, responder).

sc_ws_close_mutual(Config, Closer) when Closer =:= initiator
                                 orelse Closer =:= responder ->
    {sc_ws_update, ConfigList} = ?config(saved_config, Config),
    #{initiator := #{pub_key := IPubkey,
                    priv_key := IPrivkey},
      responder := #{pub_key := RPubkey,
                    priv_key := RPrivkey}} = proplists:get_value(participants,
                                                                 ConfigList),
    {IStartB, RStartB} = channel_participants_balances(IPubkey, RPubkey),
    #{initiator := IConnPid,
      responder := RConnPid} = proplists:get_value(channel_clients, ConfigList),
    ok = ?WS:register_test_for_channel_events(IConnPid, [sign, info, on_chain_tx]),
    ok = ?WS:register_test_for_channel_events(RConnPid, [sign, info, on_chain_tx]),


    CloseMutual =
        fun(CloserConn, CloserPrivkey, OtherConn, OtherPrivkey) ->
            ?WS:send(CloserConn, <<"shutdown">>, #{}),

            ShTx = channel_sign_tx(CloserConn, CloserPrivkey, <<"shutdown_sign">>),
            ShTx = channel_sign_tx(OtherConn, OtherPrivkey, <<"shutdown_sign_ack">>)
        end,
    ShutdownTx =
        case Closer of
            initiator -> CloseMutual(IConnPid, IPrivkey, RConnPid, RPrivkey);
            responder -> CloseMutual(RConnPid, RPrivkey, IConnPid, IPrivkey)
        end,

    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = ?WS:wait_for_channel_event(IConnPid, on_chain_tx),
    {ok, #{<<"tx">> := EncodedSignedMutualTx}} = ?WS:wait_for_channel_event(RConnPid, on_chain_tx),

    {ok, SSignedMutualTx} = aec_base58c:safe_decode(transaction, EncodedSignedMutualTx),
    SignedMutualTx = aetx_sign:deserialize_from_binary(SSignedMutualTx),
    %% same transaction
    ShutdownTx = aetx_sign:tx(SignedMutualTx),

    {channel_close_mutual_tx, MutualTx} = aetx:specialize_type(ShutdownTx),

    ok = wait_for_signed_transaction_in_pool(SignedMutualTx),

    assert_balance(IPubkey, IStartB),
    assert_balance(RPubkey, RStartB),

    ok = wait_for_signed_transaction_in_block(SignedMutualTx),

    IChange = aesc_close_mutual_tx:initiator_amount(MutualTx),
    RChange = aesc_close_mutual_tx:responder_amount(MutualTx),

    assert_balance(IPubkey, IStartB + IChange),
    assert_balance(RPubkey, RStartB + RChange),

    % ensure tx is not hanging in mempool
    {ok, 200, []} = get_transactions(),
    ok.

wait_for_signed_transaction_in_pool(SignedTx) ->
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    WaitForTx =
        fun Try(0) -> no_transaction;
            Try(Attempts) ->
                case tx_in_mempool(TxHash) of
                    true  -> ok;
                    false ->
                        timer:sleep(10),
                        Try(Attempts - 1)
                end
            end,
    ok = WaitForTx(30). % 30 attempts * 10ms

wait_for_signed_transaction_in_block(SignedTx) ->
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    MineTx =
        fun Try(0) -> did_not_mine;
            Try(Attempts) ->
                aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
                case tx_in_chain(TxHash) of
                    true  -> ok;
                    false -> Try(Attempts - 1)
                end
        end,
    ok = MineTx(5).

sc_ws_timeout_open(Config) ->
    #{initiator := #{pub_key := IPubkey},
      responder := #{pub_key := RPubkey}} = proplists:get_value(participants, Config),

    IAmt = 8,
    RAmt = 4,

    ChannelOpts = channel_options(IPubkey, RPubkey, IAmt, RAmt,
                                  #{timeout_accept => 100}),
    {ok, IConnPid} = channel_ws_start(initiator, maps:put(host, <<"localhost">>, ChannelOpts)),
    ok = ?WS:register_test_for_channel_event(IConnPid, info),
    {ok, #{<<"event">> := <<"died">>}} = ?WS:wait_for_channel_event(IConnPid, info),
    ok.

channel_options(IPubkey, RPubkey, IAmt, RAmt) ->
    channel_options(IPubkey, RPubkey, IAmt, RAmt, #{}).

channel_options(IPubkey, RPubkey, IAmt, RAmt, Other) ->
    maps:merge(#{ port => 12340,
                  initiator => aec_base58c:encode(account_pubkey, IPubkey),
                  responder => aec_base58c:encode(account_pubkey, RPubkey),
                  lock_period => 10,
                  push_amount => 10,
                  initiator_amount => IAmt,
                  responder_amount => RAmt,
                  channel_reserve => 2
                }, Other).

%% changing of another account's balance is checked in pending_transactions test
balance(_Config) ->
    ok = rpc(aec_conductor, reinit_chain, []),
    % height 0, no account
    {ok, 404, #{<<"reason">> := <<"Account not found">>}} = get_balance_at_top(),
    % get to height 1
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    lists:foreach(
        fun(Height) ->
            % get the balance at height=Height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(),
            % mine a block, move the top
            aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 1),
            % same balance at height=Height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{height => Height}),
            {ok, HashStr} = block_hash_by_height(Height),
            Hash = list_to_binary(HashStr),
            % same balance by hash
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{hash => Hash}),
            % same balance by hash and height
            {ok, 200, #{<<"balance">> := Bal}} = get_balance(EncodedPubKey,
                                                             #{hash => Hash,
                                                               height => Height}),
            ok
        end,
        lists:seq(1, ?DEFAULT_TESTS_COUNT)),
    {ok, Bal} = rpc(aec_mining, get_miner_account_balance, []),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(),
    {ok, 200, #{<<"balance">> := Bal}} = get_balance_at_top(EncodedPubKey),
    ok.

balance_negative_cases(_Config) ->
    MaxHeight = aec_blocks:height(rpc(aec_chain, top_block, [])),
    true = MaxHeight > 2,
    % get a random height, where 0 < Height < MaxHeight
    Height = rand:uniform(MaxHeight - 2) + 1,
    {ok, HashStr} = block_hash_by_height(Height),
    BlockHash = list_to_binary(HashStr),
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),

    RandAccount = aec_base58c:encode(account_pubkey, random_hash()),
    <<_, BrokenHash/binary>> = RandAccount,
    TestAccHash =
        fun(Code, ErrReson, H) ->
            Res = {ok, Code, #{<<"reason">> => ErrReson}},
            Res = get_balance_at_top(H),
            Res = get_balance(H, #{height => Height}),
            Res = get_balance(H, #{hash => BlockHash}),
            Res = get_balance(H, #{height => Height,
                                   hash => BlockHash})
        end,
    TestAccHash(400, <<"Invalid account hash">>, BrokenHash),
    TestAccHash(404, <<"Account not found">>, RandAccount),

    % block in the future
    {ok, 404, #{<<"reason">> := <<"Block not found">>}} =
              get_balance(EncodedPubKey, #{height => MaxHeight + 1}),
    % block in the future and valid block hash
    {ok, 404, #{<<"reason">> := <<"Block not found">>}} =
              get_balance(EncodedPubKey, #{height => MaxHeight + 1,
                                           hash => BlockHash}),

    % broken block hash
    {ok, 400, #{<<"reason">> := <<"Invalid block hash">>}} =
              get_balance(EncodedPubKey, #{hash => BrokenHash}),
    % broken block hash and valid height
    {ok, 400, #{<<"reason">> := <<"Invalid block hash">>}} =
              get_balance(EncodedPubKey, #{hash => BrokenHash,
                                           height => Height}),
    % blocks mismatch
    {ok, 400, #{<<"reason">> := <<"Invalid height and hash combination">>}} =
              get_balance(EncodedPubKey, #{hash => BlockHash,
                                           height => Height + 1}),
    {ok, 200, #{<<"balance">> := _}} =
              get_balance(EncodedPubKey, #{hash => BlockHash,
                                           height => Height}),
    ok.

peers(_Config) ->
    rpc(application, set_env, [aehttp, enable_debug_endpoints, false]),
    {ok, 403, #{<<"reason">> := <<"Call not enabled">>}} = get_peers(),

    rpc(application, set_env, [aehttp, enable_debug_endpoints, true]),
    {ok, 200, #{<<"blocked">> := [], <<"peers">> := Peers}} = get_peers(),

    OkPeers = [ ok || P <- Peers, {ok, _} <- [aec_peers:parse_peer_address(P)] ],

    true = (length(OkPeers) == length(Peers)),

    %% ensure no peers
    lists:foreach(
        fun(Peer) -> rpc(aec_peers, remove, [Peer]) end,
        rpc(aec_peers, all, [])),

    {ok, 200, #{<<"blocked">> := [], <<"peers">> := []}} = get_peers(),

    ok.

%% ============================================================
%% WebSocket helpers
%% ============================================================

ws_do_request(ConnPid, Target, Action, Args) ->
    ok = ?WS:register_test_for_event(ConnPid, Target, Action),
    ?WS:send(ConnPid, Target, Action, Args),
    {ok, _, Res} = ?WS:wait_for_event(ConnPid, Target, Action),
    ok = ?WS:unregister_test_for_event(ConnPid, Target, Action),
    Res.

ws_subscribe(ConnPid, PayLoad) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, subscribe),
    ?WS:send(ConnPid, chain, subscribe, PayLoad),
    {ok, _, #{<<"result">> := <<"ok">>}} = ?WS:wait_for_event(ConnPid, chain, subscribe),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, subscribe).

ws_chain_get(ConnPid, PayLoad) ->
    ws_chain_get(ConnPid, undefined, PayLoad).

ws_chain_get(ConnPid, Tag, PayLoad) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, requested_data),
    case Tag of
        undefined -> ?WS:send(ConnPid, chain, get, PayLoad);
        _         -> ?WS:send(ConnPid, chain, get, Tag, PayLoad)
    end,
    {ok, Tag1, Res} = ?WS:wait_for_event(ConnPid, chain, requested_data),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, requested_data),
    {Tag1, Res}.

ws_mine_block(ConnPid, Node) ->
    ok = ?WS:register_test_for_event(ConnPid, chain, mined_block),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(Node), 1),
    {ok, #{<<"height">> := Height, <<"hash">> := Hash}} = ?WS:wait_for_event(ConnPid, chain, mined_block),
    ok = ?WS:unregister_test_for_event(ConnPid, chain, mined_block),
    {Height, Hash}.

%% ============================================================
%% HTTP Requests
%% ============================================================

get_top() ->
    Host = external_address(),
    http_request(Host, get, "top", []).

get_contract_create(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/create", Data).


get_contract_bytecode(SourceCode) ->
    Host = external_address(),
    http_request(Host, post, "contract/compile", #{ <<"code">> => SourceCode
                                                  , <<"options">> => <<>>}).

call_contract_directly(Data) ->
    Host = external_address(),
    http_request(Host, post, "contract/call", Data).

get_contract_call(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/call", Data).

get_contract_call_compute(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/contract/call/compute", Data).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "tx/"++binary_to_list(TxHash)++"/contract-call", []).

get_contract_decode_data(Request) ->
    Host = external_address(),
    http_request(Host, post, "contract/decode-data", Request).

get_spend(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/spend", Data).

get_oracle_register(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/oracle/register", Data).

get_oracle_extend(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/oracle/extend", Data).

get_oracle_query(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/oracle/query", Data).

get_oracle_response(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/oracle/response", Data).

get_name_preclaim(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/name/preclaim", Data).

get_name_claim(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/name/claim", Data).

get_name_update(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/name/update", Data).

get_name_transfer(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/name/transfer", Data).

get_name_revoke(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/name/revoke", Data).

get_channel_create(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/create", Data).

get_channel_deposit(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/deposit", Data).

get_channel_withdrawal(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/withdrawal", Data).

get_channel_close_mutual(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/close/mutual", Data).

get_channel_close_solo(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/close/solo", Data).

get_channel_slash(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/slash", Data).

get_channel_settle(Data) ->
    Host = external_address(),
    http_request(Host, post, "tx/channel/settle", Data).

get_block_by_height(Height, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = external_address(),
    http_request(Host, get, "block/height/" ++ integer_to_list(Height), Params).

get_block_by_height(Height) ->
    Host = external_address(),
    http_request(Host, get, "block/height/" ++ integer_to_list(Height), []).

get_block_by_hash(Hash, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = external_address(),
    http_request(Host, get, "block/hash/" ++ http_uri:encode(Hash), Params).

get_header_by_hash(Hash) ->
    Host = external_address(),
    http_request(Host, get, "header-by-hash", [{hash, Hash}]).

get_transactions() ->
    Host = external_address(),
    http_request(Host, get, "transactions", []).

get_transactions(EncodedPubKey) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/pending_transactions", []).


get_tx(TxHash, TxEncoding) ->
    Params = tx_encoding_param(TxEncoding),
    Host = external_address(),
    http_request(Host, get, "tx/" ++ binary_to_list(TxHash), Params).

get_tx_nonce(TxHash) ->
    {ok, 200, Tx} = get_tx(TxHash, json),
    maps:get(<<"nonce">>, maps:get(<<"tx">>, maps:get(<<"transaction">>, Tx))).

post_spend_tx(Recipient, Amount, Fee) ->
    post_spend_tx(Recipient, Amount, Fee, <<"foo">>).

post_spend_tx(Recipient, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "spend-tx",
                 #{recipient_pubkey => aec_base58c:encode(
                                         account_pubkey, Recipient),
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

post_name_preclaim_tx(Commitment, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-preclaim-tx",
                 #{commitment => aec_base58c:encode(commitment, Commitment),
                   fee        => Fee}).

post_name_claim_tx(Name, NameSalt, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-claim-tx",
                 #{name      => Name,
                   name_salt => NameSalt,
                   fee       => Fee}).

post_name_update_tx(NameHash, NameTTL, Pointers, ClientTTL, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-update-tx",
                 #{name_hash  => aec_base58c:encode(name, NameHash),
                   client_ttl => ClientTTL,
                   pointers   => Pointers,
                   name_ttl   => NameTTL,
                   fee        => Fee}).

post_name_transfer_tx(NameHash, RecipientPubKey, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-transfer-tx",
                 #{name_hash        => aec_base58c:encode(name, NameHash),
                   recipient_pubkey => aec_base58c:encode(account_pubkey, RecipientPubKey),
                   fee              => Fee}).

post_name_revoke_tx(NameHash, Fee) ->
    Host = internal_address(),
    http_request(Host, post, "name-revoke-tx",
                 #{name_hash => aec_base58c:encode(name, NameHash),
                   fee       => Fee}).

get_commitment_hash(Name, Salt) ->
    Host = external_address(),
    http_request(Host, get, "commitment-hash", [{name, Name}, {salt, Salt}]).

get_name(Name) ->
    Host = external_address(),
    http_request(Host, get, "name", [{name, Name}]).

get_balance_at_top() ->
    {ok, 200, #{<<"pub_key">> := EncodedPubKey}} = get_miner_pub_key(),
    get_balance_at_top(EncodedPubKey).

get_balance_at_top(EncodedPubKey) ->
    get_balance(EncodedPubKey, []).

get_balance(EncodedPubKey, Params) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/balance",
                 Params).

get_account_nonce(EncodedPubKey) ->
    Host = external_address(),
    http_request(Host, get, "account/" ++ binary_to_list(EncodedPubKey) ++ "/nonce", []).

post_block(Block) ->
    post_block_map(aehttp_api_parser:encode(block, Block)).

post_block_map(BlockMap) ->
    Host = external_address(),
    http_request(Host, post, "block", BlockMap).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "tx", #{tx => TxSerialized}).

get_all_accounts_balances() ->
    Host = external_address(),
    http_request(Host, get, "balances", []).

get_miner_pub_key() ->
    Host = internal_address(),
    http_request(Host, get, "account/pub-key", []).

get_peer_pub_key() ->
    Host = external_address(),
    http_request(Host, get, "peer/key", []).

get_version() ->
    Host = external_address(),
    http_request(Host, get, "version", []).

get_info() ->
    Host = external_address(),
    http_request(Host, get, "info", []).

get_block_number() ->
    Host = internal_address(),
    http_request(Host, get, "block/number", []).

get_internal_block_preset(Segment, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = external_address(),
    http_request(Host, get, "block/" ++ Segment, Params).

tx_encoding_param(default) -> #{};
tx_encoding_param(json) -> #{tx_encoding => <<"json">>};
tx_encoding_param(message_pack) -> #{tx_encoding => <<"message_pack">>}.

get_block_txs_count_by_height(Height) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/height/" ++ integer_to_list(Height),
                 []).

get_block_txs_count_by_hash(Hash) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/hash/" ++ http_uri:encode(Hash),
                 []).

get_block_txs_count_preset(Segment) ->
    Host = internal_address(),
    http_request(Host, get, "block/txs/count/" ++ Segment, []).

get_block_tx_by_index_height(Height, Index, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/height/" ++ integer_to_list(Height) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_hash(Hash, Index, TxObjects) when is_binary(Hash) ->
    get_block_tx_by_index_hash(binary_to_list(Hash), Index, TxObjects);
get_block_tx_by_index_hash(Hash, Index, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/hash/" ++ http_uri:encode(Hash) ++
                                       "/" ++ integer_to_list(Index), Params).

get_block_tx_by_index_latest(Index, TxObjects) ->
    Params = tx_encoding_param(TxObjects),
    Host = internal_address(),
    http_request(Host, get, "block/tx/latest/" ++ integer_to_list(Index), Params).

get_block_txs_list_by_height(From, To, TxObjects, TxTypes) ->
    Params0 = tx_encoding_param(TxObjects),
    Filter = make_tx_types_filter(TxTypes),
    Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/height", Params).

get_block_txs_list_by_hash(From, To, TxObjects, TxTypes) ->
    Params0 = tx_encoding_param(TxObjects),
    Filter = make_tx_types_filter(TxTypes),
    Params = maps:merge(Params0, maps:merge(Filter, #{from => From, to => To})),
    Host = internal_address(),
    http_request(Host, get, "block/txs/list/hash", Params).

make_tx_types_filter(Filter) ->
    Includes = maps:get(include, Filter, []),
    Excludes = maps:get(exclude, Filter, []),
    Encode =
        fun(_, [], Res) -> Res;
        (Key, TypesBin, Res) ->
            Types = lists:map(fun binary_to_list/1, TypesBin),
            T = list_to_binary(lists:join(",", Types)),
            maps:put(Key, T, Res)
        end,
    R0 = Encode(tx_types, Includes, #{}),
    R = Encode(exclude_tx_types, Excludes, R0),
    R.

get_list_oracles(Max) ->
    get_list_oracles(undefined, Max).

get_list_oracles(From, Max) ->
    Host = internal_address(),
    Params0 = #{ max => Max },
    Params = case From of undefined -> Params0; _ -> Params0#{ from => From } end,
    {ok, 200, Oracles} = http_request(Host, get, "oracles", Params),
    Oracles.

get_list_oracle_queries(Oracle, Max) ->
    get_list_oracle_queries(Oracle, undefined, Max).

get_list_oracle_queries(Oracle, From, Max) ->
    Host = internal_address(),
    Params0 = #{ max => Max, oracle_pub_key => aec_base58c:encode(oracle_pubkey, Oracle) },
    Params = case From of undefined -> Params0; _ -> Params0#{ from => From } end,
    {ok, 200, Queries} = http_request(Host, get, "oracle-questions", Params),
    Queries.

get_peers() ->
    Host = internal_address(),
    http_request(Host, get, "debug/peers", []).

get_contract_poi(ContractAddress) ->
    Host = external_address(),
    http_request(Host, get, "poi/contract/" ++ binary_to_list(ContractAddress), []).

%% ============================================================
%% Test swagger validation errors
%% ============================================================

swagger_validation_body(_Config) ->
    Host = internal_address(),
    URL = binary_to_list(iolist_to_binary([Host, "/v2/spend-tx"])),
    Type = "application/json",
    Body = <<"{broken_json">>,

    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"">>,
            <<"info">> := #{
                <<"data">> := <<"{broken_json">>,
                <<"error">> := <<"invalid_body">>
        }}} = process_http_return(R).

swagger_validation_enum(_Config) ->
    Host = external_address(),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"tx_encoding">>,
            <<"info">> := #{
                <<"data">> := <<"default">>,
                <<"error">> := <<"not_in_enum">>
        }}} = http_request(Host, get, "block/genesis", #{tx_encoding => <<"default">>}).

swagger_validation_required(_Config) ->
    Host = external_address(),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"name">>,
            <<"info">> := #{
                <<"error">> := <<"missing_required_property">>
            }
        }} = http_request(Host, get, "name", []),
    ok.

swagger_validation_schema(_Config) ->
    Host = internal_address(),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := <<"wrong_fee_data">>,
                        <<"error">> := <<"wrong_type">>,
                        <<"path">> := [<<"fee">>]
        }}} = http_request(Host, post, "spend-tx", #{
                   recipient_pubkey => <<"">>,
                   amount => 0,
                   fee => <<"wrong_fee_data">>,
                   ttl => 100,
                   payload => <<"">>}),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := <<"recipient_pubkey">>,
                        <<"error">> := <<"missing_required_property">>,
                        <<"path">> := []
        }}} = http_request(Host, post, "spend-tx", #{
                   amount => 0,
                   fee => <<"fee">>,
                   ttl => 100,
                   payload => <<"">>}),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"body">>,
            <<"info">> :=  #{
                        <<"data">> := -1,
                        <<"error">> := <<"not_in_range">>,
                        <<"path">> := [<<"amount">>]
        }}} = http_request(Host, post, "spend-tx", #{
                   recipient_pubkey => <<"">>,
                   amount => -1,
                   fee => <<"fee">>,
                   ttl => 100,
                   payload => <<"">>}).

swagger_validation_types(_Config) ->
    Host = internal_address(),
    {ok, 400, #{
            <<"reason">> := <<"validation_error">>,
            <<"parameter">> := <<"height">>,
            <<"info">> :=  #{
                        <<"data">> := <<"not_integer">>,
                        <<"error">> := <<"wrong_type">>
        }}} = http_request(Host, get, "block/txs/count/height/not_integer", []).

%% ============================================================
%% HTTP Requests with wrong method
%% ============================================================

wrong_http_method_top(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "top", []).

wrong_http_method_contract_create(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/contract/create", []).

wrong_http_method_contract_call(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/contract/call", []).

wrong_http_method_contract_call_compute(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/contract/call/compute", []).

wrong_http_method_spend(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/spend", []).

wrong_http_method_oracle_register(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/oracle/register", []).

wrong_http_method_oracle_extend(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/oracle/extend", []).

wrong_http_method_oracle_query(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/oracle/query", []).

wrong_http_method_oracle_response(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/oracle/response", []).

wrong_http_method_name_preclaim(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/name/preclaim", []).

wrong_http_method_name_claim(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/name/claim", []).

wrong_http_method_name_update(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/name/update", []).

wrong_http_method_name_transfer(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/name/transfer", []).

wrong_http_method_name_revoke(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx/name/revoke", []).

wrong_http_method_block_by_hash(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "block/hash/123", []).

wrong_http_method_header_by_hash(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "header-by-hash", []).

wrong_http_method_transactions(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "transactions", []).

wrong_http_method_tx_id(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "tx/123", []).

wrong_http_method_spend_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "spend-tx", []).

wrong_http_method_name_preclaim_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "name-preclaim-tx", []).

wrong_http_method_name_claim_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "name-claim-tx", []).

wrong_http_method_name_update_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "name-update-tx", []).

wrong_http_method_name_transfer_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "name-transfer-tx", []).

wrong_http_method_name_revoke_tx(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, get, "name-revoke-tx", []).

wrong_http_method_commitment_hash(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "commitment-hash", []).

wrong_http_method_name(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "name", []).

wrong_http_method_balance(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "account/123/balance", []).

wrong_http_method_block(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "block", []).

wrong_http_method_tx(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, get, "tx", []).

wrong_http_method_all_accounts_balances(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "balances", []).

wrong_http_method_miner_pub_key(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "account/pub-key", []).

wrong_http_method_version(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "version", []).

wrong_http_method_info(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "info", []).

wrong_http_method_block_number(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/number", []).

wrong_http_method_block_by_height(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "block/height/123", []).

wrong_http_method_block_latest(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "block/latest", []).

wrong_http_method_block_genesis(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "block/genesis", []).

wrong_http_method_block_pending(_Config) ->
    Host = external_address(),
    {ok, 405, _} = http_request(Host, post, "block/pending", []).

wrong_http_method_block_txs_count_by_height(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/count/height/123", []).

wrong_http_method_block_txs_count_by_hash(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/count/hash/123", []).

wrong_http_method_block_txs_count_latest(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/count/latest", []).

wrong_http_method_block_txs_count_genesis(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/count/genesis", []).

wrong_http_method_block_txs_count_pending(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/count/pending", []).

wrong_http_method_block_tx_by_index_height(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/tx/height/123/123", []).

wrong_http_method_block_tx_by_index_hash(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/tx/hash/123/123", []).

wrong_http_method_block_tx_by_index_latest(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/tx/latest/123", []).

wrong_http_method_block_txs_list_by_height(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/list/height", []).

wrong_http_method_block_txs_list_by_hash(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "block/txs/list/hash", []).

wrong_http_method_list_oracles(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "oracles", []).

wrong_http_method_list_oracle_queries(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "oracle-questions", []).

wrong_http_method_peers(_Config) ->
    Host = internal_address(),
    {ok, 405, _} = http_request(Host, post, "debug/peers", []).

%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc(?NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"external">>, <<"port">>],
                aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"internal">>, <<"port">>],
                aehttp, [internal, port], 8143]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"internal">>, <<"port">>],
                aehttp, [internal, websocket, port], 8144]),
    {"127.0.0.1", Port}.

channel_ws_host_and_port() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"websocket">>, <<"channel">>, <<"port">>],
                aehttp, [channel, websocket, port], 8045]),
    {"localhost", Port}.

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/v2/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K,V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result =
                    case iolist_to_binary(Body) of
                        <<>> ->
                            #{};
                        BodyB ->
                            jsx:decode(BodyB, [return_maps])
                    end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

header_to_endpoint_top(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    maps:put(<<"hash">>, aec_base58c:encode(block_hash, Hash),
             aehttp_api_parser:encode(header, Header)).

block_to_endpoint_gossip_map(Block) ->
    aehttp_api_parser:encode(block, Block).

block_to_endpoint_map(Block) ->
    block_to_endpoint_map(Block, #{tx_encoding => message_pack}).

block_to_endpoint_map(Block, Options) ->
    Encoding = maps:get(tx_encoding, Options, message_pack),
    BMap = aehttp_api_parser:encode_client_readable_block(Block, Encoding),
    Expected = aehttp_logic:cleanup_genesis(BMap),

    %% Validate that all transactions have the correct block height and hash
    ExpectedTxs = maps:get(<<"transactions">>, Expected, []),
    BlockHeight = aec_blocks:height(Block),
    {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
    lists:foreach(
        fun({EncodedTx, SignedTx}) ->
            #{block_hash := TxBlockHash,
              block_height := TxBlockHeight,
              hash := Hash} =
                  aetx_sign:meta_data_from_client_serialized(Encoding, EncodedTx),
            {BlockHeight, TxBlockHeight} = {TxBlockHeight, BlockHeight},
            {BlockHash, TxBlockHash} = {TxBlockHash, BlockHash},
            TxHash = aetx_sign:hash(SignedTx),
            {Hash, TxHash} = {TxHash, Hash}
        end,
        lists:zip(ExpectedTxs, aec_blocks:txs(Block))),
    Expected.

random_hash() ->
    HList =
        lists:map(
            fun(_) -> rand:uniform(255) end,
            lists:seq(1, 32)),
    list_to_binary(HList).

prepare_for_spending(BlocksToMine) ->
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   BlocksToMine),
    {ok, []} = rpc(aec_tx_pool, peek, [infinity]), % empty
    {ok, 200, _} = get_balance_at_top(), % account present
    {ok, PubKey} = rpc(aec_keys, pubkey, []),
    {ok, Nonce} = rpc(aec_next_nonce, pick_for_account, [PubKey]),
    {PubKey, Nonce}.

-spec block_hash_by_height(integer()) -> string().
block_hash_by_height(Height) ->
    {ok, B} = rpc(aec_chain, get_block_by_height, [Height]),
    {ok, HBin} = aec_blocks:hash_internal_representation(B),
    Hash = binary_to_list(aec_base58c:encode(block_hash, HBin)),
    {ok, Hash}.

-spec get_pending_block() -> {error, no_candidate}
                           | {error, not_mining}
                           | {ok, term()}.
get_pending_block() ->
    aec_test_utils:exec_with_timeout(
        fun TryGetting() ->
            case rpc(aec_conductor, get_block_candidate, []) of
                {ok, OK} -> OK;
                {error, not_mining} = Err->
                    Err;
                {error, miner_starting} ->
                    timer:sleep(10),
                    TryGetting()
            end
        end,
        10000).

add_spend_txs() ->
    MineReward = rpc(aec_governance, block_mine_reward, []),
    MinFee = rpc(aec_governance, minimum_tx_fee, []),
    %% For now. Mining is severly slowed down by having too many Tx:s in
    %% the tx pool
    MaxSpendTxsInBlock = 20,
    MinimalAmount = 1,
    MaxTxs = min(MineReward div (MinimalAmount + MinFee), % enough tokens
                 MaxSpendTxsInBlock), % so it can fit in one block
    true = MaxTxs > 0,
    TxsCnt =
        case MaxTxs of
            1 -> 1;
            _ -> rand:uniform(MaxTxs - 1) + 1
        end,
    ct:log("adding ~p spend txs", [TxsCnt]),
    Txs =
        lists:map(
            fun(_) ->
                #{recipient => random_hash(), amount => MinimalAmount, fee => MinFee}
            end,
            lists:seq(0, TxsCnt -1)),
    populate_block(#{spend_txs => Txs}).

populate_block(Txs) ->
    lists:map(
        fun(#{recipient := R, amount := A, fee := F}) ->
                {ok, 200, #{<<"tx_hash">> := TxHash}} = post_spend_tx(R, A, F),
                TxHash
        end,
        maps:get(spend_txs, Txs, [])).

give_tokens(RecipientPubkey, Amount) ->
    MineReward = rpc(aec_governance, block_mine_reward, []),
    MinFee = rpc(aec_governance, minimum_tx_fee, []),
    NeededBlocks = ((Amount + MinFee)  div MineReward) + 1,
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE),
                                   NeededBlocks),
    SpendData = #{recipient => RecipientPubkey,
                  amount => Amount,
                  fee => MinFee},
    populate_block(#{spend_txs => [SpendData]}),
    aecore_suite_utils:mine_blocks(aecore_suite_utils:node_name(?NODE), 2),
    ok.

%% we don't have any guarantee for the ordering of the txs in the block
equal_block_maps(MapL0, MapR0) ->
    Pop =
      fun(Key, Map0, Default) ->
          Val = maps:get(Key, Map0, Default),
          Map1 = maps:remove(Key, Map0),
          {Val, Map1}
      end,
    {TxsL, MapL1} = Pop(<<"transactions">>, MapL0, []),
    {TxsR, MapR1} = Pop(<<"transactions">>, MapR0, []),
    SortedTxsL = lists:sort(TxsL),
    SortedTxsR = lists:sort(TxsR),
    ct:log("Sorted txs left: ~p", [SortedTxsL]),
    ct:log("Sorted txs right: ~p", [SortedTxsR]),
    MapL1 =:= MapR1 andalso SortedTxsL =:= SortedTxsR.

minimal_fee_and_blocks_to_mine(Amount, ChecksCnt) ->
    Fee = rpc(aec_governance, minimum_tx_fee, []),
    MineReward = rpc(aec_governance, block_mine_reward, []),
    TokensRequired = (Amount + Fee) * ChecksCnt,
    BlocksToMine = trunc(math:ceil(TokensRequired / MineReward)),
    {BlocksToMine, Fee}.

ws_start_link() ->
    {Host, Port} = ws_host_and_port(),
    ?WS:start_link(Host, Port).

channel_ws_start(Role, Opts) ->
    {Host, Port} = channel_ws_host_and_port(),
    ?WS:start_channel(Host, Port, Role, Opts).

open_websockets_count() ->
    QueueName = ws_handlers_queue,
    % ensure queue exsits
    true = undefined =/= rpc(jobs, queue_info, [QueueName]),
    length([1 || {_, QName} <- rpc(jobs, info, [monitors]),
                 QName =:= QueueName]).

sign_and_post_tx(AccountPubKey, EncodedUnsignedTx) ->
    {ok, SerializedUnsignedTx} = aec_base58c:safe_decode(transaction, EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    {ok, SignedTx} = rpc(aec_keys, sign, [UnsignedTx]),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    %% Check that we get the correct hash
    TxHash = aec_base58c:encode(tx_hash, aetx_sign:hash(SignedTx)),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(aec_base58c:encode(transaction, SerializedTx)),
    %% Check tx is in mempool.
    Fun = fun() ->
                  tx_in_mempool(TxHash)
          end,
    {ok, true} = aec_test_utils:wait_for_it_or_timeout(Fun, true, 5000),
    TxHash.

tx_in_mempool(TxHash) ->
    case get_tx(TxHash, json) of
        {ok, 200, #{<<"transaction">> :=
                        #{<<"block_hash">> := <<"none">>}}} -> true;
        {ok, 200, #{<<"transaction">> :=
                        #{<<"block_hash">> := Other}}} ->
            ct:log("Tx not in mempool, but in chain: ~p", [Other]),
            false;
        {ok, 404, _} -> false
    end.

tx_in_chain(TxHash) ->
    case get_tx(TxHash, json) of
        {ok, 200, #{<<"transaction">> :=
                        #{<<"block_hash">> := <<"none">>}}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"transaction">> := #{<<"block_hash">> := _}}} -> true;
        {ok, 404, _} -> false
    end.

make_params(L) ->
    make_params(L, []).

make_params([], Accum) ->
    maps:from_list(Accum);
make_params([H | T], Accum) when is_map(H) ->
    make_params(T, maps:to_list(H) ++ Accum);
make_params([{K, V} | T], Accum) ->
    make_params(T, [{K, V} | Accum]).

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.
