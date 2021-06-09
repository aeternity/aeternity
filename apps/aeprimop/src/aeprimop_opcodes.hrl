%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%      Primitive operations list
%%% @end
%%%-------------------------------------------------------------------

-type opcode() :: inc_account_nonce
    | channel_create
    | channel_deposit
    | channel_withdraw
    | channel_close_mutual
    | channel_settle
    | contract_call
    | contract_create
    | ga_attach
    | ga_meta
    | ga_set_meta_res
    | name_claim
    | name_preclaim
    | name_revoke
    | name_transfer
    | name_update
    | oracle_earn_query_fee
    | oracle_extend
    | oracle_query
    | oracle_register
    | oracle_respond
    | resolve_account
    | spend
    | spend_fee
    | tx_event.
-type args() :: tuple().
-type generic_op() :: {opcode(), args()}.
