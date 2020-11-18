%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System claim transaction
%%% @end
%%%=============================================================================

-module(aens_claim_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         name_fee/1,
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
         valid_at_protocol/2,
         from_db_format/1
        ]).

%% Getters
-export([account_id/1,
         name/1
        ]).

-include("../../aecontract/include/hard_forks.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_CLAIM_TX_VSN_1, 1).
-define(NAME_CLAIM_TX_VSN_2, 2).
-define(NAME_CLAIM_TX_VSN, ?NAME_CLAIM_TX_VSN_2).

-define(NAME_CLAIM_TX_TYPE, name_claim_tx).

-record(ns_claim_tx, {
          account_id :: aeser_id:id(),
          nonce      :: integer(),
          name       :: binary(),
          name_salt  :: integer(),
          name_fee   :: integer() | prelima,
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

%% Pre Lima name claims may be different in DB
-record(db_ns_claim_tx, {
          account_id :: aeser_id:id(),
          nonce      :: integer(),
          name       :: binary(),
          name_salt  :: integer(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
         }).

-opaque tx() :: #ns_claim_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account_id := AccountId,
      nonce      := Nonce,
      name       := Name,
      name_salt  := NameSalt,
      fee        := Fee} = Args) ->
    account = aeser_id:specialize_type(AccountId),
    Tx = #ns_claim_tx{account_id = AccountId,
                      nonce      = Nonce,
                      name       = Name,
                      name_salt  = NameSalt,
                      name_fee   = maps:get(name_fee, Args, prelima),
                      fee        = Fee,
                      ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec from_db_format(tx() | tuple()) -> tx().
from_db_format(#ns_claim_tx{} = Tx) -> Tx;
from_db_format(Tuple) ->
    case setelement(1, Tuple, db_ns_claim_tx) of
        #db_ns_claim_tx{
           account_id = AccountId,
           nonce      = Nonce,
           name       = Name,
           name_salt  = NameSalt,
           fee        = Fee,
           ttl        = TTL} ->
            #ns_claim_tx{account_id = AccountId,
                         nonce      = Nonce,
                         name       = Name,
                         name_salt  = NameSalt,
                         name_fee   = prelima,
                         fee        = Fee,
                         ttl        = TTL};
        _ ->
            error(illegal_db_format)
    end.

-spec type() -> atom().
type() ->
    ?NAME_CLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_claim_tx{fee = Fee}) ->
    Fee.

-spec name_fee(tx()) -> integer() | prelima.
name_fee(#ns_claim_tx{name_fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#ns_claim_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_claim_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_claim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_claim_tx{} = Tx) ->
    account_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
entities(#ns_claim_tx{account_id = AId}) ->
    [AId].

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
check(#ns_claim_tx{}, Trees, _Env) ->
    %% Checks are done in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#ns_claim_tx{} = ClaimTx, Trees, Env) ->
    case {name_fee(ClaimTx), aetx_env:consensus_version(Env) >= ?LIMA_PROTOCOL_VSN} of
        {prelima, false} -> process_(ClaimTx, aec_governance:name_claim_locked_fee(), Trees, Env);
        {NameFee, true} when is_integer(NameFee) -> process_(ClaimTx, NameFee, Trees, Env);
        _ -> {error, bad_transaction}
    end.

process_(#ns_claim_tx{} = ClaimTx, NameOrLockFee, Trees, Env) ->
    Instructions =
        aeprimop:name_claim_tx_instructions(
          account_pubkey(ClaimTx),
          name(ClaimTx),
          name_salt(ClaimTx),
          NameOrLockFee,
          fee(ClaimTx),
          nonce(ClaimTx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_claim_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_claim_tx{account_id = AccountId,
                       nonce      = None,
                       name       = Name,
                       name_salt  = NameSalt,
                       name_fee   = NameFee,
                       fee        = Fee,
                       ttl        = TTL} = Tx) ->
    case NameFee of
        prelima ->
            {?NAME_CLAIM_TX_VSN_1,
             [ {account_id, AccountId}
             , {nonce, None}
             , {name, Name}
             , {name_salt, NameSalt}
             , {fee, Fee}
             , {ttl, TTL}
             ]};
        _ when is_integer(NameFee) ->
            {version(Tx),
             [ {account_id, AccountId}
             , {nonce, None}
             , {name, Name}
             , {name_salt, NameSalt}
             , {name_fee, NameFee}
             , {fee, Fee}
             , {ttl, TTL}
             ]}
    end.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_CLAIM_TX_VSN_1,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aeser_id:specialize_type(AccountId),
    #ns_claim_tx{account_id = AccountId,
                 nonce      = Nonce,
                 name       = Name,
                 name_salt  = NameSalt,
                 name_fee   = prelima,
                 fee        = Fee,
                 ttl        = TTL};
deserialize(?NAME_CLAIM_TX_VSN_2,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {name_fee, NameFee}
            , {fee, Fee}
            , {ttl, TTL}
            ]) ->
    account = aeser_id:specialize_type(AccountId),
    #ns_claim_tx{account_id = AccountId,
                 nonce      = Nonce,
                 name       = Name,
                 name_salt  = NameSalt,
                 name_fee   = NameFee,
                 fee        = Fee,
                 ttl        = TTL}.


serialization_template(?NAME_CLAIM_TX_VSN_1) ->
    [ {account_id, id}
    , {nonce, int}
    , {name, binary}
    , {name_salt, int}
    , {fee, int}
    , {ttl, int}
    ];
serialization_template(?NAME_CLAIM_TX_VSN_2) ->
    [ {account_id, id}
    , {nonce, int}
    , {name, binary}
    , {name_salt, int}
    , {name_fee, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec for_client(tx()) -> map().
for_client(#ns_claim_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name       = Name,
                        name_salt  = NameSalt,
                        name_fee   = NameFee,
                        fee        = Fee,
                        ttl        = TTL}) ->
    NameFeeMap = maps:from_list([{<<"name_fee">>, NameFee} || is_integer(NameFee)]),
    maps:merge(NameFeeMap,
               #{<<"account_id">> => aeser_api_encoder:encode(id_hash, AccountId),
                 <<"nonce">>      => Nonce,
                 <<"name">>       => Name,
                 <<"name_salt">>  => NameSalt,
                 <<"fee">>        => Fee,
                 <<"ttl">>        => TTL}).

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account_id(tx()) -> aeser_id:id().
account_id(#ns_claim_tx{account_id = AccountId}) ->
    AccountId.

-spec name(tx()) -> binary().
name(#ns_claim_tx{name = Name}) ->
    Name.

name_salt(#ns_claim_tx{name_salt = NameSalt}) ->
    NameSalt.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_claim_tx{account_id = AccountId}) ->
    aeser_id:specialize(AccountId, account).

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?NAME_CLAIM_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, #ns_claim_tx{name_fee = NameFee}) ->
    Protocol >= ?LIMA_PROTOCOL_VSN orelse NameFee == prelima;
valid_at_protocol(_, _) ->
    false.
