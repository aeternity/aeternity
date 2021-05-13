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
          channel_snapshot_solo_tx/1,
          channel_set_delegates_tx/1,
          oracle_register_tx/1,
          oracle_extend_tx/1,
          oracle_query_tx/1,
          oracle_response_tx/1,
          name_preclaim_tx/1,
          name_claim_tx/1,
          name_update_tx/1,
          name_transfer_tx/1,
          name_revoke_tx/1,
          contract_create_tx/1,
          contract_call_tx/1,
          generalized_accounts_attach_tx/1,
          paying_for_tx/1
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

        %% channels
        channel_create_tx,
        channel_deposit_tx,
        channel_withdraw_tx,
        channel_force_progress_tx,
        channel_close_mutual_tx,
        channel_close_solo_tx,
        channel_slash_tx,
        channel_settle_tx,
        channel_snapshot_solo_tx,
        channel_set_delegates_tx,

        %% oracles
        oracle_register_tx,
        oracle_extend_tx,
        oracle_query_tx,
        oracle_response_tx,

        %% names
        name_preclaim_tx,
        name_claim_tx,
        name_update_tx,
        name_transfer_tx,
        name_revoke_tx,

        %% contracts
        contract_create_tx,
        contract_call_tx,

        %% generalized_accounts
        generalized_accounts_attach_tx,
        %% all tx types are tested to be wrapped in a meta tx, so no need for
        %% an explicit test

        paying_for_tx
      ]}
    ].

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
    Opts = spend_tx_opts(),
    test_transaction_validation(Config, aec_spend_tx, Opts),
    test_transaction_validation(Config, aec_spend_tx, Opts#{payload => <<"something longer">>}),
    ok.

spend_tx_opts() ->
    #{sender_id => aeser_id:create(account, ?PUBKEY),
      recipient_id => aeser_id:create(account, ?PUBKEY),
      amount => 1,
      fee => ?FEE,
      nonce => 1,
      payload => <<"">>}.

channel_create_tx(Config) ->
    Opts = channel_create_tx_opts(),
    test_transaction_validation(Config, aesc_create_tx, Opts),
    %% old style delegates
    test_transaction_validation(Config, aesc_create_tx, Opts#{delegate_ids => []}),
    ok.

channel_create_tx_opts() ->
    #{initiator_id => aeser_id:create(account, ?PUBKEY),
      responder_id => aeser_id:create(account, ?PUBKEY),
      initiator_amount => 1,
      responder_amount => 1,
      channel_reserve => 1,
      lock_period => 1,
      fee => ?FEE,
      nonce => 1,
      state_hash => ?STATE_HASH,
      delegate_ids => {[],[]}}.

channel_deposit_tx(Config) ->
    Opts = channel_deposit_tx_opts(),
    test_transaction_validation(Config, aesc_deposit_tx, Opts),
    ok.

channel_deposit_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      amount => 1,
      fee => ?FEE,
      nonce => 1,
      state_hash => ?STATE_HASH,
      round => 2}.

channel_withdraw_tx(Config) ->
    Opts = channel_withdraw_tx_opts(),
    test_transaction_validation(Config, aesc_withdraw_tx, Opts),
    ok.

channel_withdraw_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      to_id => aeser_id:create(account, ?PUBKEY),
      amount => 1,
      fee => ?FEE,
      nonce => 1,
      state_hash => ?STATE_HASH,
      round => 2}.

channel_force_progress_tx(Config) ->
    Opts = channel_force_progress_tx_opts(),
    test_transaction_validation(Config, aesc_force_progress_tx, Opts),
    %% use a payload
    test_transaction_validation(Config, aesc_force_progress_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    %% GA authenticated payload
    test_transaction_validation(Config, aesc_force_progress_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_force_progress_tx_opts() ->
    OffChainCall =
        aesc_offchain_update:op_call_contract(aeser_id:create(account, ?PUBKEY),
                                              aeser_id:create(contract, ?PUBKEY),
                                              1,
                                              1,
                                              <<>>, [], 1, 1),
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      payload => <<>>, %% empty payload
      update => OffChainCall,
      offchain_trees => aec_trees:new_without_backend(),
      fee => ?FEE,
      nonce => 1,
      state_hash => ?STATE_HASH,
      round => 2}.

channel_close_mutual_tx(Config) ->
    Opts = channel_close_mutual_tx_opts(),
    test_transaction_validation(Config, aesc_close_mutual_tx, Opts),
    ok.

channel_close_mutual_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      initiator_amount_final => 1,
      responder_amount_final => 1, 
      fee => ?FEE,
      nonce => 1}.

channel_close_solo_tx(Config) ->
    Opts = channel_close_solo_tx_opts(),
    test_transaction_validation(Config, aesc_close_solo_tx, Opts),
    test_transaction_validation(Config, aesc_close_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_close_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_close_solo_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      payload => <<>>, %% empty payload
      poi => aec_trees:new_poi(aec_trees:new_without_backend()),
      fee => ?FEE,
      nonce => 1}.

channel_slash_tx(Config) ->
    Opts = channel_slash_tx_opts(),
    test_transaction_validation(Config, aesc_slash_tx, Opts),
    test_transaction_validation(Config, aesc_slash_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_slash_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.


channel_slash_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      payload => <<>>, %% empty payload, not really possible
      poi => aec_trees:new_poi(aec_trees:new_without_backend()),
      fee => ?FEE,
      nonce => 1}.

channel_settle_tx(Config) ->
    Opts = channel_settle_tx_opts(),
    test_transaction_validation(Config, aesc_settle_tx, Opts),
    ok.

channel_settle_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      initiator_amount_final => 1,
      responder_amount_final => 1,
      fee => ?FEE,
      nonce => 1}.

channel_snapshot_solo_tx(Config) ->
    Opts = channel_snapshot_solo_tx_opts(),
    test_transaction_validation(Config, aesc_snapshot_solo_tx, Opts),
    test_transaction_validation(Config, aesc_snapshot_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(sign(off_chain_tx()))}),
    test_transaction_validation(Config, aesc_snapshot_solo_tx,
        Opts#{payload => aetx_sign:serialize_to_binary(meta(off_chain_tx()))}),
    ok.

channel_snapshot_solo_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      from_id => aeser_id:create(account, ?PUBKEY),
      payload => <<>>, %% empty payload, not really possible
      fee => ?FEE,
      nonce => 1}.

channel_set_delegates_tx(Config) ->
    Opts = channel_set_delegates_tx_opts(),
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

channel_set_delegates_tx_opts() ->
    #{channel_id => aeser_id:create(channel, ?PUBKEY),
      initiator_delegate_ids => [], %% no delegates
      responder_delegate_ids => [], %% no delegates
      from_id => aeser_id:create(account, ?PUBKEY),
      payload => <<>>, %% empty payload, not really possible
      state_hash => ?STATE_HASH,
      round => 1,
      fee => ?FEE,
      nonce => 1}.

oracle_register_tx(Config) ->
    Opts = oracle_register_tx_opts(),
    test_transaction_validation(Config, aeo_register_tx, Opts),
    test_transaction_validation(Config, aeo_register_tx,
                                Opts#{oracle_ttl => {block, 1}}),
    ok.

oracle_register_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      query_format => <<>>,
      abi_version => 1,
      response_format => <<>>,
      query_fee => 1,
      oracle_ttl => {delta, 1},
      fee => ?FEE,
      nonce => 1}.

oracle_extend_tx(Config) ->
    Opts = oracle_extend_tx_opts(),
    test_transaction_validation(Config, aeo_extend_tx, Opts),
    ok.

oracle_extend_tx_opts() ->
    #{oracle_id => aeser_id:create(oracle, ?PUBKEY),
      oracle_ttl => {delta, 1},
      fee => ?FEE,
      nonce => 1}.

oracle_query_tx(Config) ->
    Opts = oracle_query_tx_opts(),
    test_transaction_validation(Config, aeo_query_tx, Opts),
    test_transaction_validation(Config, aeo_query_tx,
                                Opts#{query_ttl => {block, 1}}),
    test_transaction_validation(Config, aeo_query_tx,
                                Opts#{oracle_id => aeser_id:create(name, ?PUBKEY)}),
    ok.

oracle_query_tx_opts() ->
        #{sender_id => aeser_id:create(account, ?PUBKEY),
          oracle_id => aeser_id:create(oracle, ?PUBKEY),
          query => <<>>,
          query_fee => 1,
          query_ttl => {delta, 1},
          response_ttl => {delta, 1},
          fee => ?FEE,
          nonce => 1}.

oracle_response_tx(Config) ->
    Opts = oracle_response_tx_opts(),
    test_transaction_validation(Config, aeo_response_tx, Opts),
    ok.

oracle_response_tx_opts() ->
    #{oracle_id => aeser_id:create(oracle, ?PUBKEY),
      query_id => <<>>,
      response => <<>>,
      response_ttl => {delta, 1},
      fee => ?FEE,
      nonce => 1}.

name_preclaim_tx(Config) ->
    Opts = name_preclaim_tx_opts(),
    test_transaction_validation(Config, aens_preclaim_tx, Opts),
    ok.

name_preclaim_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      commitment_id => aeser_id:create(commitment, ?PUBKEY),
      fee => ?FEE,
      nonce => 1}.

name_claim_tx(Config) ->
    Opts = name_claim_tx_opts(),
    test_transaction_validation(Config, aens_claim_tx, Opts),
    test_transaction_validation(Config, aens_claim_tx,
                                Opts#{name => <<"asdf.ghj">>}),
    ok.

name_claim_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      name => <<>>,
      name_salt => 1,
      fee => ?FEE,
      nonce => 1}.

name_update_tx(Config) ->
    Opts = name_update_tx_opts(),
    test_transaction_validation(Config, aens_update_tx, Opts),
    AccountPointer = aens_pointer:new(<<"account_pubkey">>, aeser_id:create(account, ?PUBKEY)),
    ChannelPointer = aens_pointer:new(<<"channel_pubkey">>, aeser_id:create(channel, ?PUBKEY)),
    ContractPointer = aens_pointer:new(<<"contract_pubkey">>, aeser_id:create(contract, ?PUBKEY)),
    OraclePointer = aens_pointer:new(<<"oracle_pubkey">>, aeser_id:create(oracle, ?PUBKEY)),
    CustomPointer = aens_pointer:new(<<"my custom account">>, aeser_id:create(account, ?PUBKEY)),
    test_transaction_validation(Config, aens_update_tx,
                                Opts#{pointers => [AccountPointer,
                                                   ChannelPointer,
                                                   ContractPointer,
                                                   OraclePointer,
                                                   CustomPointer ]}),
    ok.

name_update_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      name_id => aeser_id:create(name, ?PUBKEY),
      name_ttl => 1,
      client_ttl => 1,
      pointers => [], %% no pointers
      fee => ?FEE,
      nonce => 1}.

name_transfer_tx(Config) ->
    Opts = name_transfer_tx_opts(),
    test_transaction_validation(Config, aens_transfer_tx, Opts),
    %% send to another name
    test_transaction_validation(Config, aens_transfer_tx,
                                Opts#{recipient_id => aeser_id:create(name, ?PUBKEY)}),
    ok.

name_transfer_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      name_id => aeser_id:create(name, ?PUBKEY),
      recipient_id => aeser_id:create(account, ?PUBKEY),
      fee => ?FEE,
      nonce => 1}.

name_revoke_tx(Config) ->
    Opts = name_revoke_tx_opts(),
    test_transaction_validation(Config, aens_revoke_tx, Opts),
    ok.

name_revoke_tx_opts() ->
    #{account_id => aeser_id:create(account, ?PUBKEY),
      name_id => aeser_id:create(name, ?PUBKEY),
      fee => ?FEE,
      nonce => 1}.

contract_create_tx(Config) ->
    Opts = contract_create_tx_opts(),
    test_transaction_validation(Config, aect_create_tx, Opts),
    ok.

contract_create_tx_opts() ->
        #{owner_id => aeser_id:create(account, ?PUBKEY),
          code => <<>>,
          vm_version => 1,
          abi_version => 1,
          deposit => 1,
          amount => 1,
          gas => 1,
          gas_price => 1,
          fee => ?FEE,
          call_data => <<>>,
          nonce => 1}.

contract_call_tx(Config) ->
    Opts = contract_call_tx_opts(),
    test_transaction_validation(Config, aect_call_tx, Opts),
    ok.

contract_call_tx_opts() ->
    #{caller_id => aeser_id:create(account, ?PUBKEY),
      contract_id => aeser_id:create(contract, ?PUBKEY),
      abi_version => 1,
      amount => 1,
      gas => 1,
      gas_price => 1,
      fee => ?FEE,
      call_data => <<>>,
      nonce => 1}.

generalized_accounts_attach_tx(Config) ->
    Opts = generalized_accounts_attach_tx_opts(),
    test_transaction_validation(Config, aega_attach_tx, Opts),
    ok.

generalized_accounts_attach_tx_opts() ->
    #{owner_id => aeser_id:create(account, ?PUBKEY),
      code => <<>>,
      auth_fun => <<"0xabcd">>,
      vm_version => 1,
      abi_version => 1,
      deposit => 1,
      gas => 1,
      gas_price => 1,
      fee => ?FEE,
      call_data => <<>>,
      nonce => 1}.

paying_for_tx(Config) ->
    Opts0 =
        #{payer_id => aeser_id:create(account, ?PUBKEY),
          %% must set tx
          fee => ?FEE,
          nonce => 1},
    MakeTx =
        fun(Mod, Opts) ->
            {ok, Aetx} = Mod:new(Opts),
            case lists:member(Mod, mutual_auth_modules()) of
                false ->
                    [ sign(Aetx),
                      meta(Aetx) ];
                true ->
                    [ sign(Aetx),
                      meta(Aetx),
                      meta(aetx_sign:tx(meta(Aetx))) ]
            end
        end,
        
    lists:foreach(
      fun(Tx) ->
          test_transaction_validation(Config, aec_paying_for_tx, Opts0#{tx => Tx})
      end,
      lists:flatten(
          [MakeTx(aec_spend_tx, spend_tx_opts()),
          MakeTx(aesc_create_tx, channel_create_tx_opts()),
          MakeTx(aesc_deposit_tx, channel_deposit_tx_opts()),
          MakeTx(aesc_withdraw_tx, channel_withdraw_tx_opts()),
          MakeTx(aesc_force_progress_tx, channel_force_progress_tx_opts()),
          MakeTx(aesc_close_mutual_tx, channel_close_mutual_tx_opts()),
          MakeTx(aesc_close_solo_tx, channel_close_solo_tx_opts()),
          MakeTx(aesc_slash_tx, channel_slash_tx_opts()),
          MakeTx(aesc_settle_tx, channel_settle_tx_opts()),
          MakeTx(aesc_snapshot_solo_tx, channel_snapshot_solo_tx_opts()),
          MakeTx(aesc_set_delegates_tx, channel_set_delegates_tx_opts()),
          MakeTx(aeo_register_tx, oracle_register_tx_opts()),
          MakeTx(aeo_extend_tx, oracle_extend_tx_opts()),
          MakeTx(aeo_query_tx, oracle_query_tx_opts()),
          MakeTx(aeo_response_tx, oracle_response_tx_opts()),
          MakeTx(aens_preclaim_tx, name_preclaim_tx_opts()),
          MakeTx(aens_claim_tx, name_claim_tx_opts()),
          MakeTx(aens_update_tx, name_update_tx_opts()),
          MakeTx(aens_transfer_tx, name_transfer_tx_opts()),
          MakeTx(aens_revoke_tx, name_revoke_tx_opts()),
          MakeTx(aect_create_tx, contract_create_tx_opts()),
          MakeTx(aect_call_tx, contract_call_tx_opts()),
          MakeTx(aega_attach_tx, generalized_accounts_attach_tx_opts())
          ])),
    ok.

test_transaction_validation(Config, TxModule, Opts) ->
    Test =
        fun(AuthFun) ->
            validate_response(Config, get_transaction_by_hash_operation(), 200,
                              mk_transaction_response(TxModule, Opts, mempool, AuthFun)),
            validate_response(Config, get_transaction_by_hash_operation(), 200,
                              mk_transaction_response(TxModule, Opts, included_in_block, AuthFun)),
            ok
        end,
    case lists:member(TxModule, mutual_auth_modules()) of
        false ->
            lists:foreach(Test, [fun sign/1, fun meta/1]);
        true ->
            lists:foreach(Test, [fun sign/1, fun meta/1,
                                 fun(Tx) -> meta(aetx_sign:tx(meta(Tx))) end])
    end.
    
mutual_auth_modules() ->
    [ aesc_create_tx, aesc_deposit_tx, aesc_withdraw_tx,
      aesc_close_mutual_tx, aesc_set_delegates_tx].

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
