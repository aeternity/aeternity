%% Test that transaction types have their corresponding definition in
%% the openAPI specification
%%
%% This is not done as a Eunit test, because the generated openAPI client
%% is needed.
%%
-module(aehttp_discriminator_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-define(API, oas_endpoints).

%% common_test exports
-export([ all/0, groups/0 ]).

%% test case exports
-export(
    [
     aetx_types_in_schema/1,
     aesc_types_in_schema/1
    ]).

all() ->
    [ {group, all} ].

groups() ->
    [
     {all, [],
      [
       aetx_types_in_schema,
       aesc_types_in_schema
      ]}
    ].

%% ============================================================
%% Test cases
%% ============================================================

%% Test that every atom mentioned in type aetx:tx_type()
%% has a translation into a OpenAPI name that is present in
%% the openAPI schema (by using oas_endpoints).
%% In addition, test that back translation is defined and equal.


aetx_types_in_schema(_Config) ->
    Defs = [ Def || {Def, _} <- ?API:definitions() ],
    lists:foreach(
        fun(Type) ->
            SwaggerType = aetx:type_to_swagger_name(Type),
            ?assertEqual(Type, aetx:swagger_name_to_type(SwaggerType)),
            %% For printing error message, assert with Type in it
            ?assertEqual({Type, true},
                         {Type,
                              Type == channel_offchain_tx orelse   %% not exposed in HTTP interface
                              string_member([?API:definitions_prefix(), SwaggerType], Defs)})
        end, aetx_types() -- [channel_client_reconnect_tx]).

aesc_types_in_schema(_Config) ->
    Defs = [ Def || {Def, _} <- ?API:definitions() ],
    lists:foreach(
        fun(Type) ->
            SwaggerType = aesc_offchain_update:type2swagger_name(Type),
            %% For printing error message, assert with Type in it
            ?assertEqual({Type, true},
                         {Type, string_member([?API:definitions_prefix(), SwaggerType], Defs)})
        end, aesc_offchain_update_types() -- [meta]).


%% We could use a parse transform to extract the type from aetx... but
%% we choose to not complicate it with that.
-spec aetx_types() -> [aetx:type()].
aetx_types() ->
    [ spend_tx
    , oracle_register_tx
    , oracle_extend_tx
    , oracle_query_tx
    , oracle_response_tx
    , name_preclaim_tx
    , name_claim_tx
    , name_transfer_tx
    , name_update_tx
    , name_revoke_tx
    , contract_create_tx
    , contract_call_tx
    , ga_attach_tx
    , ga_meta_tx
    , channel_create_tx
    , channel_deposit_tx
    , channel_withdraw_tx
    , channel_force_progress_tx
    , channel_close_mutual_tx
    , channel_close_solo_tx
    , channel_slash_tx
    , channel_settle_tx
    , channel_snapshot_solo_tx
    , channel_set_delegates_tx
    , channel_offchain_tx
    , channel_client_reconnect_tx
    , paying_for_tx
    ].


-spec aesc_offchain_update_types() -> [ aesc_offchain_update:update_type() ].
aesc_offchain_update_types() ->
    [ transfer, withdraw, deposit, create_contract, call_contract, meta ].

%% Check whether string occurs in a list disregarding string representation
string_member(_S, []) ->
    false;
string_member(S, [Str | Strs]) ->
    case string:equal(S, Str) of
        true -> true;
        false -> string_member(S, Strs)
    end.

