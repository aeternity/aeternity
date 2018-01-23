-module(aec_tx_dispatcher).
-export([handler/1,
         handler_by_type/1]).

-include("core_txs.hrl").
-include_lib("apps/aeoracle/include/oracle_txs.hrl").
-include_lib("apps/aecontract/include/contract_txs.hrl").

handler(#coinbase_tx{}) ->
    aec_coinbase_tx;
handler(#spend_tx{}) ->
    aec_spend_tx;
handler(#contract_create_tx{}) ->
    aect_create_tx;
handler(#contract_call_tx{}) ->
    aect_call_tx;
handler(#oracle_register_tx{}) ->
    aeo_register_tx;
handler(#oracle_query_tx{}) ->
    aeo_query_tx;
handler(#oracle_response_tx{}) ->
    aeo_response_tx.

handler_by_type(<<"coinbase">>) ->
    aec_coinbase_tx;
handler_by_type(<<"spend">>) ->
    aec_spend_tx;
handler_by_type(<<"contract_create">>) ->
    aect_create_tx;
handler_by_type(<<"contract_call">>) ->
    aect_call_tx;
handler_by_type(<<"oracle_register">>) ->
    aeo_register_tx;
handler_by_type(<<"oracle_query">>) ->
    aeo_query_tx;
handler_by_type(<<"oracle_response">>) ->
    aeo_response_tx.

