%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System transfer transaction
%%% @end
%%%=============================================================================

-module(aens_transfer_tx).

-behavior(aetx).

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

-export([account_id/1,
         name_id/1,
         name_hash/1,
         recipient_pubkey/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_TRANSFER_TX_VSN, 1).
-define(NAME_TRANSFER_TX_TYPE, name_transfer_tx).

-record(ns_transfer_tx, {
          account_id   :: aeser_id:id(),
          nonce        :: integer(),
          name_id      :: aeser_id:id(),
          recipient_id :: aeser_id:id(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_transfer_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id   := AccountId,
      nonce        := Nonce,
      name_id      := NameId,
      recipient_id := RecipientId,
      fee          := Fee} = Args) ->
    account = aeser_id:specialize_type(AccountId),
    name    = aeser_id:specialize_type(NameId),
    case aeser_id:specialize_type(RecipientId) of
        account -> ok;
        name    -> ok
    end,
    Tx = #ns_transfer_tx{account_id   = AccountId,
                         nonce        = Nonce,
                         name_id      = NameId,
                         recipient_id = RecipientId,
                         fee          = Fee,
                         ttl          = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_TRANSFER_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_transfer_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_transfer_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_transfer_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_transfer_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_transfer_tx{} = Tx) ->
    account_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#ns_transfer_tx{account_id = AId, name_id = NId, recipient_id = RId}) ->
    [AId, NId, RId].

account_pubkey(#ns_transfer_tx{account_id = AccountId}) ->
    aeser_id:specialize(AccountId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#ns_transfer_tx{}, Trees,_Env) ->
    %% Checks are done in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#ns_transfer_tx{} = TTx, Trees, Env) ->
    Instructions =
        aeprimop:name_transfer_tx_instructions(
          account_pubkey(TTx),
          recipient_id(TTx),
          name_hash(TTx),
          fee(TTx),
          nonce(TTx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_transfer_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_transfer_tx{account_id   = AccountId,
                          nonce        = Nonce,
                          name_id      = NameId,
                          recipient_id = RecipientId,
                          fee          = Fee,
                          ttl          = TTL} = Tx) ->
    {version(Tx),
     [ {account_id, AccountId}
     , {nonce, Nonce}
     , {name_id, NameId}
     , {recipient_id, RecipientId}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_TRANSFER_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name_id, NameId}
            , {recipient_id, RecipientId}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aeser_id:specialize_type(AccountId),
    name    = aeser_id:specialize_type(NameId),
    case aeser_id:specialize_type(RecipientId) of
        account -> ok;
        name -> ok
    end,
    #ns_transfer_tx{account_id   = AccountId,
                    nonce        = Nonce,
                    name_id      = NameId,
                    recipient_id = RecipientId,
                    fee          = Fee,
                    ttl          = TTL}.

serialization_template(?NAME_TRANSFER_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name_id, id}
    , {recipient_id, id}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_transfer_tx{account_id   = AccountId,
                           nonce        = Nonce,
                           name_id      = NameId,
                           recipient_id = RecipientId,
                           fee          = Fee,
                           ttl          = TTL}) ->
    #{<<"account_id">>   => aeser_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>        => Nonce,
      <<"name_id">>      => aeser_api_encoder:encode(id_hash, NameId),
      <<"recipient_id">> => aeser_api_encoder:encode(id_hash, RecipientId),
      <<"fee">>          => Fee,
      <<"ttl">>          => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec recipient_pubkey(tx()) -> aec_keys:pubkey().
recipient_pubkey(#ns_transfer_tx{recipient_id = RecipientId}) ->
    aeser_id:specialize(RecipientId, account).

-spec account_id(tx()) -> aeser_id:id().
account_id(#ns_transfer_tx{account_id = AccountId}) ->
    AccountId.

-spec name_id(tx()) -> aeser_id:id().
name_id(#ns_transfer_tx{name_id = NameId}) ->
    NameId.

-spec name_hash(tx()) -> binary().
name_hash(#ns_transfer_tx{name_id = NameId}) ->
    aeser_id:specialize(NameId, name).

recipient_id(#ns_transfer_tx{recipient_id = RecipientId}) ->
    RecipientId.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?NAME_TRANSFER_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

