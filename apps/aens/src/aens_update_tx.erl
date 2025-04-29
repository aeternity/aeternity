%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System prolong transaction
%%% @end
%%%=============================================================================

-module(aens_update_tx).

-behavior(aetx).

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
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).

%% Getters
-export([name_hash/1,
         name_ttl/1,
         pointers/1,
         client_ttl/1]).

-ifdef(TEST).

-export([serialize/2]).

-endif.

-include_lib("aecontract/include/hard_forks.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_UPDATE_TX_VSN_1, 1).
-define(NAME_UPDATE_TX_VSN_2, 2).
-define(NAME_UPDATE_TX_TYPE, name_update_tx).

-record(ns_update_tx, {
          account_id :: aeser_id:id(),
          nonce      :: integer(),
          name_id    :: aeser_id:id(),
          name_ttl   :: integer(),
          pointers   :: list(aens_pointer:pointer()),
          client_ttl :: integer(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_update_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id := AccountId,
      nonce      := Nonce,
      name_id    := NameId,
      name_ttl   := NameTTL,
      pointers   := Pointers,
      client_ttl := ClientTTL,
      fee        := Fee} = Args) ->
    account = aeser_id:specialize_type(AccountId),
    name    = aeser_id:specialize_type(NameId),
    %% TODO: check pointers: length, unique keys? unique ids?
    Tx = #ns_update_tx{account_id = AccountId,
                       nonce      = Nonce,
                       name_id    = NameId,
                       name_ttl   = NameTTL,
                       pointers   = Pointers,
                       client_ttl = ClientTTL,
                       fee        = Fee,
                       ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_UPDATE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_update_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_update_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_update_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_update_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_update_tx{} = Tx) ->
    account_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_update_tx{} = _Tx, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ns_update_tx{} = UTx, Trees, Env) ->
    Instructions =
        aeprimop:name_update_tx_instructions(
          account_pubkey(UTx),
          name_hash(UTx),
          name_ttl(UTx),
          client_ttl(UTx),
          pointers(UTx),
          fee(UTx),
          nonce(UTx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_update_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(Tx) ->
    serialize(Tx, version(Tx)).

serialize(#ns_update_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name_id    = NameId,
                        name_ttl   = NameTTL,
                        pointers   = Pointers,
                        client_ttl = ClientTTL,
                        fee        = Fee,
                        ttl        = TTL}, Vsn) ->
    SerializePtrFun =
        case Vsn of
            ?NAME_UPDATE_TX_VSN_1 -> fun aens_pointer:serialize_pointer_vsn1/1;
            ?NAME_UPDATE_TX_VSN_2 -> fun aens_pointer:serialize_pointer_vsn2/1
        end,
    {Vsn,
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name_id, NameId}
     , {name_ttl, NameTTL}
     , {pointers, [SerializePtrFun(P) || P <- Pointers]}
     , {client_ttl, ClientTTL}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.


-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(Vsn,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name_id, NameId}
            , {name_ttl, NameTTL}
            , {pointers, Pointers}
            , {client_ttl, ClientTTL}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aeser_id:specialize_type(AccountId),
    name = aeser_id:specialize_type(NameId),
    DeserializePtrFun =
        case Vsn of
            ?NAME_UPDATE_TX_VSN_1 -> fun aens_pointer:deserialize_pointer_vsn1/1;
            ?NAME_UPDATE_TX_VSN_2 -> fun aens_pointer:deserialize_pointer_vsn2/1
        end,

    Tx = #ns_update_tx{account_id = AccountId,
                       nonce      = Nonce,
                       name_id    = NameId,
                       name_ttl   = NameTTL,
                       pointers   = [DeserializePtrFun(P) || P <- Pointers],
                       client_ttl = ClientTTL,
                       fee        = Fee,
                       ttl        = TTL},

    %% Assert that the right version was used
    case version(Tx) of
        Vsn -> Tx;
        _   -> error(invalid_serialization_version)
    end.


serialization_template(?NAME_UPDATE_TX_VSN_1) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {name_ttl, int}
    , {pointers, [{binary, id}]}
    , {client_ttl, int}
    , {fee, int}
    , {ttl, int}
    ];
serialization_template(?NAME_UPDATE_TX_VSN_2) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {name_ttl, int}
    , {pointers, [{binary, binary}]}
    , {client_ttl, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_update_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name_id    = NameId,
                         name_ttl   = NameTTL,
                         pointers   = Pointers,
                         client_ttl = ClientTTL,
                         fee        = Fee,
                         ttl        = TTL}) ->
    #{<<"account_id">> => aeser_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name_id">>    => aeser_api_encoder:encode(id_hash, NameId),
      <<"name_ttl">>   => NameTTL,
      <<"pointers">>   => [aens_pointer:serialize_for_client(P) || P <- Pointers],
      <<"client_ttl">> => ClientTTL,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_ttl(tx()) -> integer().
name_ttl(#ns_update_tx{name_ttl = NameTTL}) ->
    NameTTL.

-spec pointers(tx()) -> list(aens_pointer:pointer()).
pointers(#ns_update_tx{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(tx()) -> non_neg_integer().
client_ttl(#ns_update_tx{client_ttl = ClientTTL}) ->
    ClientTTL.

-spec name_hash(tx()) -> binary().
name_hash(#ns_update_tx{name_id = NameId}) ->
    aeser_id:specialize(NameId, name).

-spec version(tx()) -> non_neg_integer().
version(#ns_update_tx{pointers = Pointers}) ->
    case aens_pointer:has_raw_data_pointer(Pointers) of
        false -> ?NAME_UPDATE_TX_VSN_1;
        true  -> ?NAME_UPDATE_TX_VSN_2
    end.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, #ns_update_tx{pointers = Pointers}) ->
    Protocol >= ?CERES_PROTOCOL_VSN
        orelse not aens_pointer:has_raw_data_pointer(Pointers).

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_update_tx{account_id = AccountId}) ->
    aeser_id:specialize(AccountId, account).
