-module(aehttp_validation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

-import(aecore_suite_utils, [http_request/4]).

%% test case exports
%% external endpoints
-export([ spend_tx/1,
          channel_create_tx/1,
          channel_deposit_tx/1,
          channel_withdraw_tx/1,
          channel_force_progress_tx/1,
          channel_close_mutual_tx/1,
          channel_close_solo_tx/1,
          channel_slash_tx/1,
          channel_settle_tx/1,
          channel_snapshot_solo/1,
          channel_set_delegates_tx/1
        ]).

-define(PUBKEY, <<40:32/unit:8>>).
-define(BLOCK_HASH, <<41:32/unit:8>>).
-define(BLOCK_HEIGHT, 42).
-define(STATE_HASH, <<42:32/unit:8>>).
-define(TX_HASH, <<43:32/unit:8>>).
-define(FEE, 20000 * aec_test_utils:min_gas_price()).

all() ->
    [{group, swagger2},
     {group, oas3}
    ].

groups() ->
    [{swagger2, [sequence], [{group, transactions}]},
     {oas3, [sequence], [{group, transactions}]},
     {transactions, [sequence],
      [ spend_tx,
        %% channel txs
        channel_create_tx,
        channel_deposit_tx,
        channel_withdraw_tx,
        channel_force_progress_tx,
        channel_close_mutual_tx,
        channel_close_solo_tx,
        channel_slash_tx,
        channel_settle_tx,
        channel_snapshot_solo,
        channel_set_delegates_tx
      ]}
    ].

%% TODO tx types:
%% OracleRegisterTx,
%% OracleExtendTx,
%% OracleQueryTx,
%% OracleRespondTx,
%% NamePreclaimTx,
%% NameClaimTx,
%% NameUpdateTx,
%% NameTransferTx,
%% NameRevokeTx,
%% ContractCreateTx,
%% ContractCallTx,
%% GAAttachTx,
%% PayingForTx

suite() ->
    [].

init_per_suite(Config0) ->
    Config0.

end_per_suite(_Config) ->
    ok.

init_per_group(SwaggerVsn, Config) when SwaggerVsn =:= swagger2;
                                        SwaggerVsn =:= oas3 ->
    Json = aehttp_spec:json(SwaggerVsn),
    R = jsx:decode(Json),
    Validator = jesse_state:new(R, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
    EndpointsMod =
        case SwaggerVsn of
            swagger2 -> endpoints;
            oas3     -> oas_endpoints
        end,
    [{swagger_version, SwaggerVsn}, {validator, Validator}, {endpoints_mod, EndpointsMod}
     | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_VMGroup, _Config) ->
    ok.

init_per_testcase(_Testcase, Config) ->
    Config.

end_per_testcase(_Testcase, _Config) ->
    ok.

spend_tx(Config) ->
    Opts =
        #{sender_id => aeser_id:create(account, ?PUBKEY),
          recipient_id => aeser_id:create(account, ?PUBKEY),
          amount => 1,
          fee => ?FEE,
          nonce => 1,
          payload => <<"">>},
    test_transaction_validation(Config, aec_spend_tx, Opts),
    test_transaction_validation(Config, aec_spend_tx, Opts#{payload => <<"something longer">>}),
    ok.

channel_create_tx(Config) ->
    Opts =
        #{initiator_id => aeser_id:create(account, ?PUBKEY),
          responder_id => aeser_id:create(account, ?PUBKEY),
          initiator_amount => 1,
          responder_amount => 1,
          channel_reserve => 1,
          lock_period => 1,
          fee => ?FEE,
          nonce => 1,
          state_hash => ?STATE_HASH,
          delegate_ids => {[],[]}},
    test_transaction_validation(Config, aesc_create_tx, Opts),
    %% old style delegates
    test_transaction_validation(Config, aesc_create_tx, Opts#{delegate_ids => []}),
    ok.

channel_deposit_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          amount => 1,
          fee => ?FEE,
          nonce => 1,
          state_hash => ?STATE_HASH,
          round => 2},
    test_transaction_validation(Config, aesc_deposit_tx, Opts),
    ok.

channel_withdraw_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          to_id => aeser_id:create(account, ?PUBKEY),
          amount => 1,
          fee => ?FEE,
          nonce => 1,
          state_hash => ?STATE_HASH,
          round => 2},
    test_transaction_validation(Config, aesc_withdraw_tx, Opts),
    ok.

channel_force_progress_tx(Config) ->
    OffChainCall =
        aesc_offchain_update:op_call_contract(aeser_id:create(account, ?PUBKEY),
                                              aeser_id:create(contract, ?PUBKEY),
                                              1,
                                              1,
                                              <<>>, [], 1, 1),
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          payload => <<>>, %% empty payload
          update => OffChainCall,
          offchain_trees => aec_trees:new_without_backend(),
          fee => ?FEE,
          nonce => 1,
          state_hash => ?STATE_HASH,
          round => 2},
    test_transaction_validation(Config, aesc_force_progress_tx, Opts),
    %% use a payload
    test_transaction_validation(Config, aesc_force_progress_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    %% GA authenticated payload
    test_transaction_validation(Config, aesc_force_progress_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_close_mutual_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          initiator_amount_final => 1,
          responder_amount_final => 1, 
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_close_mutual_tx, Opts),
    ok.

channel_close_solo_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          payload => <<>>, %% empty payload
          poi => aec_trees:new_poi(aec_trees:new_without_backend()),
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_close_solo_tx, Opts),
    test_transaction_validation(Config, aesc_close_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_close_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_slash_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          payload => <<>>, %% empty payload, not really possible
          poi => aec_trees:new_poi(aec_trees:new_without_backend()),
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_slash_tx, Opts),
    test_transaction_validation(Config, aesc_slash_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_slash_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_settle_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          initiator_amount_final => 1,
          responder_amount_final => 1,
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_settle_tx, Opts),
    ok.

channel_snapshot_solo(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          from_id => aeser_id:create(account, ?PUBKEY),
          payload => <<>>, %% empty payload, not really possible
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_snapshot_solo_tx, Opts),
    test_transaction_validation(Config, aesc_snapshot_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_snapshot_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_set_delegates_tx(Config) ->
    Opts =
        #{channel_id => aeser_id:create(channel, ?PUBKEY),
          initiator_delegate_ids => [], %% no delegates
          responder_delegate_ids => [], %% no delegates
          from_id => aeser_id:create(account, ?PUBKEY),
          payload => <<>>, %% empty payload, not really possible
          state_hash => ?STATE_HASH,
          round => 1,
          fee => ?FEE,
          nonce => 1},
    test_transaction_validation(Config, aesc_set_delegates_tx, Opts),
    test_transaction_validation(Config, aesc_set_delegates_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_set_delegates_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_set_delegates_tx,
        Opts#{initiator_delegate_ids => [aeser_id:create(channel, ?PUBKEY)]}),
    test_transaction_validation(Config, aesc_set_delegates_tx,
        Opts#{responder_delegate_ids => [aeser_id:create(channel, ?PUBKEY)]}),
    ok.

test_transaction_validation(Config, TxModule, Opts) ->
    validate_response(Config, get_transaction_by_hash_operation(), 200,
                      mk_transaction_response(TxModule, Opts, mempool, fun sign/1)),
    validate_response(Config, get_transaction_by_hash_operation(), 200,
                      mk_transaction_response(TxModule, Opts, mempool, fun meta/1)),
    validate_response(Config, get_transaction_by_hash_operation(), 200,
                      mk_transaction_response(TxModule, Opts, included_in_block, fun sign/1)),
    validate_response(Config, get_transaction_by_hash_operation(), 200,
                      mk_transaction_response(TxModule, Opts, included_in_block, fun meta/1)).
    
get_transaction_by_hash_operation() -> operation('GetTransactionByHash', "get").

operation(OperationId, Method) -> {OperationId, Method}.

mk_transaction_response(Mod, Opts, Location, Auth) ->
    {ok, Aetx} = Mod:new(Opts),
    case Location of
        mempool ->
            aetx_sign:serialize_for_client_pending(sign(Aetx));
        included_in_block ->
            aetx_sign:serialize_for_client(Auth(Aetx),
                                           ?BLOCK_HEIGHT, ?BLOCK_HASH,
                                           ?TX_HASH)
    end.

sign(Aetx) -> aetx_sign:new(Aetx, []).

meta(Aetx) ->
    {ok, Meta} = aega_meta_tx:new(
        #{ga_id => aeser_id:create(account, ?PUBKEY),
          auth_data => <<>>,
          abi_version => 1,
          gas => 1,
          gas_price => 1,
          fee => 1,
          tx => aetx_sign:new(Aetx, [])}),
    aetx_sign:new(Meta, []).

validate_response(Config, {OperationId, Method}, Code, Response) ->
    EndpointsMod = ?config(endpoints_mod, Config),
    Validator = ?config(validator, Config),
    aehttp_api_validate:response(OperationId, Method, Code, Response,
                                 Validator, EndpointsMod).

off_chain_tx() ->
    {ok, Aetx} =
        aesc_offchain_tx:new(
            #{channel_id => aeser_id:create(channel, ?PUBKEY),
              state_hash => ?STATE_HASH,
              round => 2}),
    Aetx.
