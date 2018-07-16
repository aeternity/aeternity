%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Functions for serializing chain objects to binary format.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_object_serialization).

-export([ serialize/4
        , deserialize/4
        , deserialize_type_and_vsn/1
        ]).

-type template() :: aec_serialization:template().
-type fields()   :: aec_serialization:fields().

%%%===================================================================
%%% API
%%%===================================================================

-spec serialize(atom(), non_neg_integer(), template(), fields()) -> binary().
serialize(Type, Vsn, Template, Fields) ->
    aec_serialization:serialize(tag(Type), Vsn, Template, Fields).

deserialize_type_and_vsn(Binary) ->
    {Tag, Vsn, Fields} = aec_serialization:deserialize_tag_and_vsn(Binary),
    {rev_tag(Tag), Vsn, Fields}.

-spec deserialize(atom(), non_neg_integer(), template(), binary()) -> fields().
deserialize(Type, Vsn, Template, Binary) ->
    aec_serialization:deserialize(Type, tag(Type), Vsn, Template, Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================

tag(account) -> 10;
tag(signed_tx) -> 11;
tag(spend_tx) -> 12;
tag(oracle) -> 20;
tag(oracle_query) -> 21;
tag(oracle_register_tx) -> 22;
tag(oracle_query_tx) -> 23;
tag(oracle_response_tx) -> 24;
tag(oracle_extend_tx) -> 25;
tag(name) -> 30;
tag(name_commitment) -> 31;
tag(name_claim_tx) -> 32;
tag(name_preclaim_tx) -> 33;
tag(name_update_tx) -> 34;
tag(name_revoke_tx) -> 35;
tag(name_transfer_tx) -> 36;
tag(contract) -> 40;
tag(contract_call) -> 41;
tag(contract_create_tx) -> 42;
tag(contract_call_tx) -> 43;
tag(channel_create_tx) -> 50;
tag(channel_deposit_tx) -> 51;
tag(channel_withdraw_tx) -> 52;
tag(channel_close_mutual_tx) -> 53;
tag(channel_close_solo_tx) -> 54;
tag(channel_slash_tx) -> 55;
tag(channel_settle_tx) -> 56;
tag(channel_offchain_tx) -> 57;
tag(channel) -> 58;
tag(channel_snapshot_solo_tx) -> 59;
tag(trees_poi) -> 60;
tag(trees_db) -> 61;
tag(key_block) -> 100;
tag(micro_block) -> 101.

rev_tag(10) -> account;
rev_tag(11) -> signed_tx;
rev_tag(12) -> spend_tx;
rev_tag(20) -> oracle;
rev_tag(21) -> oracle_query;
rev_tag(22) -> oracle_register_tx;
rev_tag(23) -> oracle_query_tx;
rev_tag(24) -> oracle_response_tx;
rev_tag(25) -> oracle_extend_tx;
rev_tag(30) -> name;
rev_tag(31) -> name_commitment;
rev_tag(32) -> name_claim_tx;
rev_tag(33) -> name_preclaim_tx;
rev_tag(34) -> name_update_tx;
rev_tag(35) -> name_revoke_tx;
rev_tag(36) -> name_transfer_tx;
rev_tag(40) -> contract;
rev_tag(41) -> contract_call;
rev_tag(42) -> contract_create_tx;
rev_tag(43) -> contract_call_tx;
rev_tag(50) -> channel_create_tx;
rev_tag(51) -> channel_deposit_tx;
rev_tag(52) -> channel_withdraw_tx;
rev_tag(53) -> channel_close_mutual_tx;
rev_tag(54) -> channel_close_solo_tx;
rev_tag(55) -> channel_slash_tx;
rev_tag(56) -> channel_settle_tx;
rev_tag(57) -> channel_offchain_tx;
rev_tag(58) -> channel;
rev_tag(59) -> channel_snapshot_solo_tx;
rev_tag(60) -> trees_poi;
rev_tag(61) -> trees_db;
rev_tag(100) -> key_block;
rev_tag(101) -> micro_block.
