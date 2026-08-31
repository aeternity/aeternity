%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Per-family gas table for aetx. Walks tx_types/0 and pins tx_min_gas/2,
%%%    fee_gas/3, gas_limit/3 and min_fee/3 per type on the last activated
%%%    protocol and on Arcus, so a new type arrives as a failing coverage test.
%%%    aetx_tests asserts that no type raises; this asserts the numbers.
%%% @end
%%%-------------------------------------------------------------------
-module(aetx_gas_table_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("aecontract/include/hard_forks.hrl").
-include_lib("aecontract/include/aecontract.hrl").

%% Fixed inputs. Every number below is a function of these plus the
%% serialized size they produce, so none of them may drift.
-define(HEIGHT, 100000).
-define(FEE, 20000 * 1000000).
-define(NONCE, 11).
-define(GAS, 1000).
-define(GAS_PRICE, 1000000).
-define(TTL_DELTA, 500).

-define(ACCOUNT_A, <<"aetx-gas-table-account-a_______x">>).
-define(ACCOUNT_B, <<"aetx-gas-table-account-b_______x">>).
-define(ORACLE,    <<"aetx-gas-table-oracle__________x">>).
-define(CONTRACT,  <<"aetx-gas-table-contract________x">>).
-define(CHANNEL,   <<"aetx-gas-table-channel_________x">>).
-define(COMMITMENT,<<"aetx-gas-table-commitment______x">>).
-define(NAME_HASH, <<"aetx-gas-table-name-hash_______x">>).
-define(QUERY_ID,  <<"aetx-gas-table-query-id________x">>).
-define(STATE_HASH,<<"aetx-gas-table-state-hash______x">>).

%%%===================================================================
%%% The table: {Type, {TxMinGas, FeeGas, GasLimit, MinFee}} per protocol
%%% column. A cell is a non-negative integer or `function_clause'.
%%%===================================================================

gas_table() ->
    [ {spend_tx,                     {16640, 16640, 16640, 16640000000}}
      %% Oracle rows: fee_gas/gas_limit exceed tx_min_gas by the TTL state
      %% gas, which tx_min_gas/2 does not carry.
    , {oracle_register_tx,           {16320, 16412, 16412, 16412000000}}
    , {oracle_extend_tx,             {15980, 16072, 16072, 16072000000}}
    , {oracle_query_tx,              {16940, 17032, 17032, 17032000000}}
    , {oracle_response_tx,           {16880, 16972, 16972, 16972000000}}
    , {name_preclaim_tx,             {16600, 16600, 16600, 16600000000}}
    , {name_claim_tx,                {16520, 16520, 16520, 16520000000}}
    , {name_transfer_tx,             {17280, 17280, 17280, 17280000000}}
    , {name_update_tx,               {16740, 16740, 16740, 16740000000}}
    , {name_revoke_tx,               {16600, 16600, 16600, 16600000000}}
      %% Contract rows: gas_limit adds the tx's own ?GAS on top.
    , {contract_create_tx,           {76900, 76900, 77900, 76900000000}}
    , {contract_call_tx,             {182100, 182100, 183100, 182100000000}}
    , {ga_attach_tx,                 {77520, 77520, 78520, 77520000000}}
      %% The wrappers are where the four functions genuinely disagree:
      %% tx_min_gas/2 charges the whole outer size, fee_gas/3 subtracts the
      %% inner size back out, gas_limit/3 adds the inner tx's own limit.
    , {ga_meta_tx,                   {78180, 76540, 94180, 76540000000}}
    , {paying_for_tx,                {5720, 4080, 20720, 4080000000}}
    , {channel_create_tx,            {17360, 17360, 17360, 17360000000}}
    , {channel_deposit_tx,           {17300, 17300, 17300, 17300000000}}
    , {channel_withdraw_tx,          {17300, 17300, 17300, 17300000000}}
      %% Priced as the most expensive contract operation since Fortuna.
    , {channel_force_progress_tx,    {455340, 455340, 455340, 455340000000}}
    , {channel_close_mutual_tx,      {16640, 16640, 16640, 16640000000}}
    , {channel_close_solo_tx,        {16820, 16820, 16820, 16820000000}}
    , {channel_slash_tx,             {16820, 16820, 16820, 16820000000}}
    , {channel_settle_tx,            {16640, 16640, 16640, 16640000000}}
    , {channel_snapshot_solo_tx,     {16620, 16620, 16620, 16620000000}}
    , {channel_set_delegates_tx,     {17380, 17380, 17380, 17380000000}}
      %% On no_base_gas_tx_types/0: all four arities read that list and answer 0.
    , {channel_offchain_tx,          {0, 0, 0, 0}}
    , {channel_client_reconnect_tx,  unconstructible}
      %% Also on no_base_gas_tx_types/0, so the same four zeros.
    , {hc_vote_tx,                   {0, 0, 0, 0}}
    ].

%% One table asserted at both protocols: Arcus reprices nothing in it today.
%% That is an assertion rather than a note - a repricing red-fails the
%% protocol-7 rows and has to be written out as its own table.
protocol_tables() ->
    [ {?CERES_PROTOCOL_VSN, gas_table()}
    , {?ARCUS_PROTOCOL_VSN, gas_table()}
    ].

%%%===================================================================
%%% Coverage: the table is exactly aetx:tx_types/0
%%%===================================================================

%% A new tx type fails here rather than reaching production unpriced.
table_covers_every_tx_type_test() ->
    ?assertEqual(lists:sort(aetx:tx_types()),
                 lists:sort([T || {T, _} <- gas_table()])).

table_has_no_duplicate_rows_test() ->
    Types = [T || {T, _} <- gas_table()],
    ?assertEqual(lists:sort(lists:usort(Types)), lists:sort(Types)).

%%%===================================================================
%%% The pins
%%%===================================================================

gas_table_test_() ->
    [ {label(P, Type, Fun),
       fun() -> ?assertEqual(Expected, measure(Fun, Type, P)) end}
      || {P, Table}    <- protocol_tables(),
         {Type, Row}   <- Table,
         Row =/= unconstructible,
         {Fun, Expected} <- lists:zip([tx_min_gas, fee_gas, gas_limit, min_fee],
                                      tuple_to_list(Row)) ].

%%%===================================================================
%%% channel_client_reconnect_tx is in tx_types/0 and wired to nothing:
%%% type_to_cb/1 has no clause, so no aetx() of that type exists to price -
%%% hence `unconstructible' above. type_to_cb/1 is not exported, so the
%%% observable twin, type_to_swagger_name/1, is asserted instead.
%%%===================================================================

channel_client_reconnect_tx_is_unwired_test() ->
    ?assert(lists:member(channel_client_reconnect_tx, aetx:tx_types())),
    ?assertError(function_clause, aetx:type_to_swagger_name(channel_client_reconnect_tx)),
    %% Every other type in tx_types/0 does have one.
    [ ?assert(is_binary(aetx:type_to_swagger_name(T)))
      || T <- aetx:tx_types(), T =/= channel_client_reconnect_tx ],
    ok.

%%%===================================================================
%%% Oracle TTL expiry, the documented zero-gas path: gas_limit/3 answers 0
%%% when ttl_delta/2 errors and fee_gas/3 delegates to it, so an ordinary
%%% oracle tx is a second way into the division in min_gas_price/3.
%%%===================================================================

expired_absolute_oracle_ttl_test_() ->
    {ok, Tx} = aeo_register_tx:new(oracle_register_opts(#{oracle_ttl => {block, ?HEIGHT - 1}})),
    [ {"gas_limit is 0 for an already-expired absolute oracle TTL",
       ?_assertEqual(0, aetx:gas_limit(Tx, ?HEIGHT, ?CERES_PROTOCOL_VSN))}
    , {"fee_gas follows it to 0",
       ?_assertEqual(0, aetx:fee_gas(Tx, ?HEIGHT, ?CERES_PROTOCOL_VSN))}
    , {"min_fee follows it to 0",
       ?_assertEqual(0, aetx:min_fee(Tx, ?HEIGHT, ?CERES_PROTOCOL_VSN))}
    ].

%%%===================================================================
%%% Measurement
%%%===================================================================

label(P, Type, Fun) ->
    lists:flatten(io_lib:format("protocol ~p: ~p ~p", [P, Type, Fun])).

measure(Fun, Type, Protocol) ->
    try apply_fun(Fun, tx(Type), Protocol)
    catch error:function_clause -> function_clause
    end.

apply_fun(tx_min_gas, Tx, Protocol) -> aetx:tx_min_gas(Tx, Protocol);
apply_fun(fee_gas,    Tx, Protocol) -> aetx:fee_gas(Tx, ?HEIGHT, Protocol);
apply_fun(gas_limit,  Tx, Protocol) -> aetx:gas_limit(Tx, ?HEIGHT, Protocol);
apply_fun(min_fee,    Tx, Protocol) -> aetx:min_fee(Tx, ?HEIGHT, Protocol).

%%%===================================================================
%%% One fixed transaction per type
%%%===================================================================

account_id(Pubkey)  -> aeser_id:create(account, Pubkey).
oracle_id(Pubkey)   -> aeser_id:create(oracle, Pubkey).
contract_id(Pubkey) -> aeser_id:create(contract, Pubkey).
channel_id(Pubkey)  -> aeser_id:create(channel, Pubkey).
name_id(Hash)       -> aeser_id:create(name, Hash).
commitment_id(H)    -> aeser_id:create(commitment, H).

oracle_register_opts(Extra) ->
    maps:merge(#{ account_id      => account_id(?ACCOUNT_A)
                , nonce           => ?NONCE
                , query_format    => <<"string">>
                , response_format => <<"string">>
                , query_fee       => 10
                , oracle_ttl      => {delta, ?TTL_DELTA}
                , abi_version     => ?ABI_NO_VM
                , fee             => ?FEE
                }, Extra).

tx(spend_tx) ->
    ok(aec_spend_tx:new(#{ sender_id    => account_id(?ACCOUNT_A)
                         , recipient_id => account_id(?ACCOUNT_B)
                         , amount       => 40
                         , fee          => ?FEE
                         , nonce        => ?NONCE
                         , payload      => <<>>
                         }));
tx(oracle_register_tx) ->
    ok(aeo_register_tx:new(oracle_register_opts(#{})));
tx(oracle_extend_tx) ->
    ok(aeo_extend_tx:new(#{ oracle_id  => oracle_id(?ORACLE)
                          , nonce      => ?NONCE
                          , oracle_ttl => {delta, ?TTL_DELTA}
                          , fee        => ?FEE
                          }));
tx(oracle_query_tx) ->
    ok(aeo_query_tx:new(#{ sender_id    => account_id(?ACCOUNT_A)
                         , nonce        => ?NONCE
                         , oracle_id    => oracle_id(?ORACLE)
                         , query        => <<"a-query">>
                         , query_fee    => 10
                         , query_ttl    => {delta, ?TTL_DELTA}
                         , response_ttl => {delta, ?TTL_DELTA}
                         , fee          => ?FEE
                         }));
tx(oracle_response_tx) ->
    ok(aeo_response_tx:new(#{ oracle_id    => oracle_id(?ORACLE)
                            , nonce        => ?NONCE
                            , query_id     => ?QUERY_ID
                            , response     => <<"a-response">>
                            , response_ttl => {delta, ?TTL_DELTA}
                            , fee          => ?FEE
                            }));
tx(name_preclaim_tx) ->
    ok(aens_preclaim_tx:new(#{ account_id    => account_id(?ACCOUNT_A)
                             , nonce         => ?NONCE
                             , commitment_id => commitment_id(?COMMITMENT)
                             , fee           => ?FEE
                             }));
tx(name_claim_tx) ->
    ok(aens_claim_tx:new(#{ account_id => account_id(?ACCOUNT_A)
                          , nonce      => ?NONCE
                          , name       => <<"aetxgastable.chain">>
                          , name_salt  => 1
                          , name_fee   => 100000000000000000000
                          , fee        => ?FEE
                          }));
tx(name_transfer_tx) ->
    ok(aens_transfer_tx:new(#{ account_id   => account_id(?ACCOUNT_A)
                             , nonce        => ?NONCE
                             , name_id      => name_id(?NAME_HASH)
                             , recipient_id => account_id(?ACCOUNT_B)
                             , fee          => ?FEE
                             }));
tx(name_update_tx) ->
    ok(aens_update_tx:new(#{ account_id => account_id(?ACCOUNT_A)
                           , nonce      => ?NONCE
                           , name_id    => name_id(?NAME_HASH)
                           , name_ttl   => 1000
                           , pointers   => []
                           , client_ttl => 1000
                           , fee        => ?FEE
                           }));
tx(name_revoke_tx) ->
    ok(aens_revoke_tx:new(#{ account_id => account_id(?ACCOUNT_A)
                           , nonce      => ?NONCE
                           , name_id    => name_id(?NAME_HASH)
                           , fee        => ?FEE
                           }));
tx(contract_create_tx) ->
    ok(aect_create_tx:new(#{ owner_id    => account_id(?ACCOUNT_A)
                           , nonce       => ?NONCE
                           , code        => <<"fixed-contract-code">>
                           , vm_version  => ?VM_FATE_SOPHIA_2
                           , abi_version => ?ABI_FATE_SOPHIA_1
                           , deposit     => 0
                           , amount      => 0
                           , gas         => ?GAS
                           , gas_price   => ?GAS_PRICE
                           , call_data   => <<"fixed-call-data">>
                           , fee         => ?FEE
                           }));
tx(contract_call_tx) ->
    ok(aect_call_tx:new(#{ caller_id   => account_id(?ACCOUNT_A)
                         , nonce       => ?NONCE
                         , contract_id => contract_id(?CONTRACT)
                         , abi_version => ?ABI_FATE_SOPHIA_1
                         , fee         => ?FEE
                         , amount      => 0
                         , gas         => ?GAS
                         , gas_price   => ?GAS_PRICE
                         , call_data   => <<"fixed-call-data">>
                         }));
tx(ga_attach_tx) ->
    ok(aega_attach_tx:new(#{ owner_id    => account_id(?ACCOUNT_A)
                           , nonce       => ?NONCE
                           , code        => <<"fixed-contract-code">>
                           , auth_fun    => <<"fixed-auth-fun-hash-32-bytes___x">>
                           , vm_version  => ?VM_FATE_SOPHIA_2
                           , abi_version => ?ABI_FATE_SOPHIA_1
                           , gas         => ?GAS
                           , gas_price   => ?GAS_PRICE
                           , call_data   => <<"fixed-call-data">>
                           , fee         => ?FEE
                           }));
tx(ga_meta_tx) ->
    ok(aega_meta_tx:new(#{ ga_id       => account_id(?ACCOUNT_A)
                         , auth_data   => <<"fixed-auth-data">>
                         , abi_version => ?ABI_FATE_SOPHIA_1
                         , gas         => ?GAS
                         , gas_price   => ?GAS_PRICE
                         , fee         => ?FEE
                         , tx          => signed(tx(spend_tx))
                         }));
tx(paying_for_tx) ->
    ok(aec_paying_for_tx:new(#{ payer_id => account_id(?ACCOUNT_B)
                              , nonce    => ?NONCE
                              , fee      => ?FEE
                              , tx       => signed(tx(spend_tx))
                              }));
tx(channel_create_tx) ->
    ok(aesc_create_tx:new(#{ initiator_id     => account_id(?ACCOUNT_A)
                           , initiator_amount => 10
                           , responder_id     => account_id(?ACCOUNT_B)
                           , responder_amount => 10
                           , channel_reserve  => 2
                           , lock_period      => 3
                           , fee              => ?FEE
                           , state_hash       => ?STATE_HASH
                           , nonce            => ?NONCE
                           }));
tx(channel_deposit_tx) ->
    ok(aesc_deposit_tx:new(#{ channel_id => channel_id(?CHANNEL)
                            , from_id    => account_id(?ACCOUNT_A)
                            , amount     => 10
                            , fee        => ?FEE
                            , state_hash => ?STATE_HASH
                            , round      => 2
                            , nonce      => ?NONCE
                            }));
tx(channel_withdraw_tx) ->
    ok(aesc_withdraw_tx:new(#{ channel_id => channel_id(?CHANNEL)
                             , to_id      => account_id(?ACCOUNT_A)
                             , amount     => 10
                             , fee        => ?FEE
                             , state_hash => ?STATE_HASH
                             , round      => 2
                             , nonce      => ?NONCE
                             }));
tx(channel_force_progress_tx) ->
    Update = aesc_offchain_update:op_transfer(account_id(?ACCOUNT_A),
                                              account_id(?ACCOUNT_B), 5),
    ok(aesc_force_progress_tx:new(#{ channel_id     => channel_id(?CHANNEL)
                                   , from_id        => account_id(?ACCOUNT_A)
                                   , payload        => <<>>
                                   , update         => Update
                                   , state_hash     => ?STATE_HASH
                                   , round          => 2
                                   , offchain_trees => aec_trees:new_without_backend()
                                   , fee            => ?FEE
                                   , nonce          => ?NONCE
                                   }));
tx(channel_close_mutual_tx) ->
    ok(aesc_close_mutual_tx:new(#{ channel_id             => channel_id(?CHANNEL)
                                 , from_id                => account_id(?ACCOUNT_A)
                                 , initiator_amount_final => 10
                                 , responder_amount_final => 10
                                 , fee                    => ?FEE
                                 , nonce                  => ?NONCE
                                 }));
tx(channel_close_solo_tx) ->
    ok(aesc_close_solo_tx:new(#{ channel_id => channel_id(?CHANNEL)
                               , from_id    => account_id(?ACCOUNT_A)
                               , payload    => <<>>
                               , poi        => empty_poi()
                               , fee        => ?FEE
                               , nonce      => ?NONCE
                               }));
tx(channel_slash_tx) ->
    ok(aesc_slash_tx:new(#{ channel_id => channel_id(?CHANNEL)
                          , from_id    => account_id(?ACCOUNT_A)
                          , payload    => <<>>
                          , poi        => empty_poi()
                          , fee        => ?FEE
                          , nonce      => ?NONCE
                          }));
tx(channel_settle_tx) ->
    ok(aesc_settle_tx:new(#{ channel_id             => channel_id(?CHANNEL)
                           , from_id                => account_id(?ACCOUNT_A)
                           , initiator_amount_final => 10
                           , responder_amount_final => 10
                           , fee                    => ?FEE
                           , nonce                  => ?NONCE
                           }));
tx(channel_snapshot_solo_tx) ->
    ok(aesc_snapshot_solo_tx:new(#{ channel_id => channel_id(?CHANNEL)
                                  , from_id    => account_id(?ACCOUNT_A)
                                  , payload    => <<>>
                                  , fee        => ?FEE
                                  , nonce      => ?NONCE
                                  }));
tx(channel_set_delegates_tx) ->
    ok(aesc_set_delegates_tx:new(#{ channel_id             => channel_id(?CHANNEL)
                                  , from_id                => account_id(?ACCOUNT_A)
                                  , initiator_delegate_ids => []
                                  , responder_delegate_ids => []
                                  , payload                => <<>>
                                  , state_hash             => ?STATE_HASH
                                  , round                  => 2
                                  , fee                    => ?FEE
                                  , nonce                  => ?NONCE
                                  }));
tx(channel_offchain_tx) ->
    ok(aesc_offchain_tx:new(#{ channel_id => channel_id(?CHANNEL)
                             , state_hash => ?STATE_HASH
                             , round      => 2
                             }));
tx(hc_vote_tx) ->
    ok(aec_hc_vote_tx:new(#{ voter_id => account_id(?ACCOUNT_A)
                           , epoch    => 1
                           , type     => 1
                           , data     => #{}
                           })).

ok({ok, Tx}) -> Tx.

signed(Tx) -> aetx_sign:new(Tx, []).

empty_poi() -> aec_trees:new_poi(aec_trees:new_without_backend()).
