%%%=============================================================================
%%% @copyright 2019, Aeternity Anstalt
%%% @doc
%%%    Module defining the PayingForTX
%%% @end
%%%=============================================================================
-module(aec_paying_for_tx).

-behavior(aetx).

-include("../../aecontract/include/aecontract.hrl").
-include("../../aecontract/include/hard_forks.hrl").

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         entities/1,
         check/3,
         process/3,
         signers/2,
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).
%% Additional getters
-export([payer_id/1,
         payer_pubkey/1,
         tx/1
        ]).

-export([set_tx/2
        ]).

-export([ record_fields/1 ]).

-define(PAYING_FOR_TX_VSN, 1).
-define(PAYING_FOR_TX_TYPE, paying_for_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-type amount() :: aect_contracts:amount().

-record(paying_for_tx, {
          payer_id    :: aeser_id:id(),
          nonce       :: non_neg_integer(),
          fee         :: non_neg_integer(),
          tx          :: aetx_sign:signed_tx()
        }).

-opaque tx() :: #paying_for_tx{}.

-export_type([tx/0]).

%% ==================================================================
%% Tracing support

record_fields(paying_for_tx) -> record_info(fields, paying_for_tx);
record_fields(_) ->
    no.

%% ==================================================================

%%%===================================================================
%%% Getters

-spec payer_id(tx()) -> aeser_id:id().
payer_id(#paying_for_tx{payer_id = PayerId}) ->
    PayerId.

-spec payer_pubkey(tx()) -> aec_keys:pubkey().
payer_pubkey(#paying_for_tx{payer_id = PayerId}) ->
    aeser_id:specialize(PayerId, account).

-spec gas(tx()) -> amount().
gas(#paying_for_tx{}) ->
    0.

-spec tx(tx()) -> aetx_sign:signed_tx().
tx(#paying_for_tx{tx = Tx}) ->
    Tx.

-spec set_tx(aetx_sign:signed_tx(), tx()) -> tx().
set_tx(NewTx, Tx) ->
    Tx#paying_for_tx{tx = NewTx}.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#paying_for_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#paying_for_tx{tx = STx}) ->
    aetx:ttl(aetx_sign:tx(STx)).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{payer_id := PayerId,
      nonce    := Nonce,
      fee      := Fee,
      tx       := InnerTx}) ->
    account = aeser_id:specialize_type(PayerId),
    Tx = #paying_for_tx{payer_id = PayerId,
                        nonce    = Nonce,
                        fee      = Fee,
                        tx       = InnerTx},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?PAYING_FOR_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#paying_for_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#paying_for_tx{} = Tx) ->
    payer_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#paying_for_tx{payer_id = PId, tx = STx}) ->
    Tx = aetx_sign:tx(STx),
    {CB, Tx} = aetx:specialize_callback(aetx_sign:tx(STx)),
    [PId | CB:entities(Tx)].

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#paying_for_tx{}, Trees, _Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#paying_for_tx{} = Tx, _) ->
    {ok, [payer_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#paying_for_tx{tx = STx} = Tx, Trees, Env0) ->
    case check_tx(STx) of
        ok ->
            PInstr = aeprimop:paying_for_tx_instructions(payer_pubkey(Tx), nonce(Tx), fee(Tx)),
            case aeprimop:eval(PInstr, Trees, Env0) of
                {ok, Trees1, Env1} ->
                    Env2 = aetx_env:set_payer(Env1, payer_pubkey(Tx)),
                    case aetx_sign:verify_w_env(STx, Trees1, Env2) of
                        ok ->
                            aetx:process(aetx_sign:tx(STx), Trees1, Env2);
                        Err = {error, _} ->
                            Err
                    end;
                Err = {error, _} ->
                    Err
            end;
        Err = {error, _} ->
            Err
    end.

serialize(#paying_for_tx{payer_id = PayerId,
                         nonce    = Nonce,
                         fee      = Fee,
                         tx       = InnerTx} = Tx) ->
    SerTx = aetx_sign:serialize_to_binary(InnerTx),
    {version(Tx),
     [ {payer_id, PayerId}
     , {nonce, Nonce}
     , {fee, Fee}
     , {tx, SerTx}
     ]}.

deserialize(?PAYING_FOR_TX_VSN,
            [ {payer_id, PayerId}
            , {nonce, Nonce}
            , {fee, Fee}
            , {tx, SerTx}]) ->
    account = aeser_id:specialize_type(PayerId),
    InnerTx = aetx_sign:deserialize_from_binary(SerTx),
    ok      = check_tx(InnerTx),
    #paying_for_tx{payer_id = PayerId,
                   nonce    = Nonce,
                   fee      = Fee,
                   tx       = InnerTx}.

serialization_template(?PAYING_FOR_TX_VSN) ->
    [ {payer_id, id}
    , {nonce, int}
    , {fee, int}
    , {tx, binary}
    ].

for_client(#paying_for_tx{ payer_id = PayerId,
                           nonce    = Nonce,
                           fee      = Fee,
                           tx       = InnerTx}) ->
    #{<<"payer_id">>  => aeser_api_encoder:encode(id_hash, PayerId),
      <<"nonce">>     => Nonce,
      <<"fee">>       => Fee,
      <<"tx">>        => aetx_sign:serialize_for_client_inner(InnerTx, #{})}.

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?PAYING_FOR_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(P, #paying_for_tx{ tx = STx }) ->
    P >= ?IRIS_PROTOCOL_VSN andalso
        aetx:valid_at_protocol(P, aetx_sign:tx(STx)).

%%%===================================================================
%%% Internal functions

check_tx(STx) ->
    Tx = aetx_sign:tx(STx),
    {Type, InnerTx} = aetx:specialize_type(Tx),
    case Type of
        paying_for_tx                   -> {error, not_allowed_to_pay_for_recursively};
        ga_meta_tx                      -> check_tx(aega_meta_tx:tx(InnerTx));
        X = channel_offchain_tx         -> {error, {not_allowed_to_pay_for, X}};
        X = channel_client_reconnect_tx -> {error, {not_allowed_to_pay_for, X}};
        _X                              -> ok
    end.

