%% Test suite for Swagger discriminators.
%%
%% Ideally this shall be checked at runtime by Swagger validation.
-module(aehttp_discriminator_SUITE).

%% common_test exports
-export(
   [
    all/0, groups/0
   ]).

%% test case exports
-export(
   [
    'GenericTx'/1,
    'OffChainUpdate'/1
   ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [],
      [
       'GenericTx',
       'OffChainUpdate'
      ]}
    ].

%% ============================================================
%% Test cases
%% ============================================================

'GenericTx'(_Config) ->
    Types =
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
        , contract_call_tx
        , contract_create_tx
        %% TODO , ga_attach_tx
        %% TODO , ga_meta_tx
        , channel_create_tx
        , channel_deposit_tx
        , channel_withdraw_tx
        , channel_force_progress_tx
        , channel_close_solo_tx
        , channel_close_mutual_tx
        , channel_slash_tx
        , channel_settle_tx
        , channel_snapshot_solo_tx
        %% Not tested because not exposed in HTTP API: channel_offchain_tx
        ],
    lists:foreach(
      fun(T) -> {true, _} = {is_definition(binary_to_list(aetx:type_to_swagger_name(T))), T} end,
      Types).

'OffChainUpdate'(_Config) ->
    Types =
        [ transfer
        , deposit
        , withdraw
        , create_contract
        , call_contract
        ],
    lists:foreach(
      fun(T) -> {true, _} = {is_definition(binary_to_list(aesc_offchain_update:type2swagger_name(T))), T} end,
      Types).

%% Swagger discriminator must be definition. See https://github.com/OAI/OpenAPI-Specification/blob/04f659aeffd3082de70212189477def845c68aa6/versions/2.0.md#composition-and-inheritance-polymorphism
is_definition(X) when is_list(X) ->
    Prefix = endpoints:definitions_prefix(),
    Len = length(Prefix),
    case
        lists:filter(
          fun
              ({Def, _}) ->
                  Prefix1 = string:substr(Def, 1, Len),
                  D = string:substr(Def, Len + 1),
                  Prefix =:= Prefix1 andalso D =:= X
          end,
          endpoints:definitions())
    of
        [_] ->
            true;
        [] ->
            false
    end.
