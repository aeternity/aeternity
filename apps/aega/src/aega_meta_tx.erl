%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module defining the Meta transaction for Generalized Accounts
%%% @end
%%%=============================================================================
-module(aega_meta_tx).

-behavior(aetx).

-include("../../aecontract/include/aecontract.hrl").

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).
%% Additional getters
-export([abi_version/1,
         auth_data/1,
         auth_id/1,
         auth_id/2,
         ga_id/1,
         ga_pubkey/1,
         gas_limit/2,
         gas_price/1,
         tx/1
        ]).

-export([set_tx/2
        ]).

-define(GA_META_TX_VSN, 1).
-define(GA_META_TX_TYPE, ga_meta_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-type amount() :: aect_contracts:amount().

-record(ga_meta_tx, {
          ga_id       :: aeser_id:id(),
          auth_data   :: binary(),
          abi_version :: aect_contracts:abi_version(),
          gas         :: amount(),
          gas_price   :: amount(),
          fee         :: amount(),
          ttl         :: aetx:tx_ttl(),
          tx          :: aetx_sign:signed_tx()
        }).

-opaque tx() :: #ga_meta_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec ga_id(tx()) -> aeser_id:id().
ga_id(#ga_meta_tx{ga_id = GAId}) ->
    GAId.

-spec ga_pubkey(tx()) -> aec_keys:pubkey().
ga_pubkey(#ga_meta_tx{ga_id = GAId}) ->
    aeser_id:specialize(GAId, account).

-spec abi_version(tx()) -> aect_contracts:abi_version().
abi_version(#ga_meta_tx{abi_version = ABI}) ->
    ABI.

-spec gas(tx()) -> amount().
gas(#ga_meta_tx{gas = Gas}) ->
    Gas.

-spec gas_limit(tx(), non_neg_integer()) -> amount().
gas_limit(#ga_meta_tx{gas = Gas, tx = InnerTx}, Height) ->
    aetx:gas_limit(aetx_sign:tx(InnerTx), Height) + Gas.

-spec gas_price(tx()) -> amount().
gas_price(#ga_meta_tx{gas_price = GasPrice}) ->
    GasPrice.

-spec auth_data(tx()) -> binary().
auth_data(#ga_meta_tx{auth_data = AuthData}) ->
    AuthData.

-spec auth_id(tx()) -> aect_call:id().
auth_id(#ga_meta_tx{auth_data = AuthData} = Tx) ->
    auth_id(ga_pubkey(Tx), AuthData).

-spec auth_id(binary(), binary()) -> aect_call:id().
auth_id(GAPubkey, AuthData) ->
    aec_hash:hash(pubkey, <<GAPubkey/binary, AuthData/binary>>).

-spec tx(tx()) -> aetx_sign:signed_tx().
tx(#ga_meta_tx{tx = Tx}) ->
    Tx.

-spec set_tx(aetx_sign:signed_tx(), tx()) -> tx().
set_tx(NewTx, Tx) ->
    Tx#ga_meta_tx{tx = NewTx}.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#ga_meta_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ga_meta_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{ga_id       := GAId,
      auth_data   := AuthData,
      abi_version := ABIVersion,
      gas         := Gas,
      gas_price   := GasPrice,
      fee         := Fee,
      tx          := InnerTx} = Args) ->
    account = aeser_id:specialize_type(GAId),
    Tx = #ga_meta_tx{ga_id       = GAId,
                     auth_data   = AuthData,
                     abi_version = ABIVersion,
                     gas         = Gas,
                     gas_price   = GasPrice,
                     fee         = Fee,
                     ttl         = maps:get(ttl, Args, 0),
                     tx          = InnerTx},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?GA_META_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ga_meta_tx{}) ->
    0.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ga_meta_tx{} = Tx) ->
    ga_pubkey(Tx).

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ga_meta_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ga_meta_tx{}, _) ->
    {ok, []}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ga_meta_tx{} = Tx, Trees, Env0) ->
    AuthInstructions =
        aec_tx_processor:ga_meta_tx_instructions(
          ga_pubkey(Tx),
          auth_data(Tx),
          abi_version(Tx),
          gas(Tx),
          gas_price(Tx),
          fee(Tx)),
    Env = add_tx_hash(Env0, aetx_sign:tx(tx(Tx))),
    case aec_tx_processor:eval(AuthInstructions, Trees, Env) of
        {ok, Trees1, Env1} ->
            %% Successful authentication - we have a call object in Trees1
            Env11 = set_ga_context(Env1, Tx),
            {InnerRes, Trees2, Env2} =
                case aetx_sign:verify(tx(Tx), Trees1, Env11) of
                    ok ->
                        case aetx:process(aetx_sign:tx(tx(Tx)), Trees1, Env11) of
                            {ok, Trees21, Env21}    -> {ok, Trees21, Env21};
                            Err = {error, _Reason}  -> {Err, Trees1, Env1}
                        end;
                    Err = {error, _Reason} ->
                        {Err, Trees1, Env1}
                end,
            Env22   = reset_ga_context(Env2, Tx, Env1),
            Trees22 = set_meta_result(InnerRes, Tx, Trees2, Env22),
            {ok, Trees22, Env22};
        Err = {error, _} ->
            Err
    end.

set_meta_result(ok, _Tx, Trees, _Env) ->
    Trees;
set_meta_result(Err = {error, _}, Tx, Trees, Env) ->
    %% ct:pal("Setting error: ~p\n", [Err]),
    SetInstructions =
        aec_tx_processor:ga_set_meta_tx_res_instructions(
            ga_pubkey(Tx), auth_data(Tx), Err),
    {ok, Trees1, _Env} = aec_tx_processor:eval(SetInstructions, Trees, Env),
    Trees1.

serialize(#ga_meta_tx{ga_id       = GAId,
                      auth_data   = AuthData,
                      abi_version = ABIVersion,
                      fee         = Fee,
                      gas         = Gas,
                      gas_price   = GasPrice,
                      ttl         = TTL,
                      tx          = InnerTx}) ->
    SerTx = aetx_sign:serialize_to_binary(InnerTx),
    {version(),
     [ {ga_id, GAId}
     , {auth_data, AuthData}
     , {abi_version, ABIVersion}
     , {fee, Fee}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {ttl, TTL}
     , {tx, SerTx}
     ]}.

deserialize(?GA_META_TX_VSN,
            [ {ga_id, GAId}
            , {auth_data, AuthData}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {ttl, TTL}
            , {tx, SerTx}]) ->
    account = aeser_id:specialize_type(GAId),
    Tx = aetx_sign:deserialize_from_binary(SerTx),
    #ga_meta_tx{ga_id       = GAId,
                auth_data   = AuthData,
                abi_version = ABIVersion,
                fee         = Fee,
                gas         = Gas,
                gas_price   = GasPrice,
                ttl         = TTL,
                tx          = Tx}.

serialization_template(?GA_META_TX_VSN) ->
    [ {ga_id, id}
    , {auth_data, binary}
    , {abi_version, int}
    , {fee, int}
    , {gas, int}
    , {gas_price, int}
    , {ttl, int}
    , {tx, binary}
    ].

for_client(#ga_meta_tx{ ga_id       = GAId,
                        auth_data   = AuthData,
                        abi_version = ABIVersion,
                        fee         = Fee,
                        gas         = Gas,
                        gas_price   = GasPrice,
                        ttl         = TTL,
                        tx          = InnerTx}) ->
    #{<<"ga_id">>       => aeser_api_encoder:encode(id_hash, GAId),
      <<"auth_data">>   => aeser_api_encoder:encode(contract_bytearray, AuthData),
      <<"abi_version">> => aeu_hex:hexstring_encode(<<ABIVersion:16>>),
      <<"fee">>         => Fee,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"ttl">>         => TTL,
      <<"tx">>          => aetx_sign:serialize_for_client_pending(InnerTx)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?GA_META_TX_VSN.

add_tx_hash(Env0, Tx) ->
    BinForNetwork = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    aetx_env:set_ga_tx_hash(Env0, aec_hash:hash(tx, BinForNetwork)).

set_ga_context(Env0, Tx) ->
    Env1 = aetx_env:set_context(Env0, aetx_ga),
    Env2 = aetx_env:add_ga_auth_id(Env1, ga_pubkey(Tx)),
    Env3 = aetx_env:add_ga_nonce(Env2, ga_pubkey(Tx), auth_id(Tx)),
    aetx_env:set_ga_tx_hash(Env3, undefined).

reset_ga_context(Env0, Tx, OldEnv) ->
    Env1 = aetx_env:set_context(Env0, aetx_env:context(OldEnv)),
    Env2 = aetx_env:del_ga_auth_id(Env1, ga_pubkey(Tx)),
    aetx_env:del_ga_nonce(Env2, ga_pubkey(Tx)).

