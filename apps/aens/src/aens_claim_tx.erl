%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System claim transaction
%%% @end
%%%=============================================================================

-module(aens_claim_tx).

-behavior(aetx).

-include_lib("aecontract/include/hard_forks.hrl").

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         name_fee/1,
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
         valid_at_protocol/2,
         assert_min_bid_fee/3
        ]).

%% Getters
-export([account_id/1,
         name/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================
-define(NAME_CLAIM_TX_VSN_1, 1).
-define(NAME_CLAIM_TX_VSN_2, 2).

-define(NAME_CLAIM_TX_TYPE, name_claim_tx).

-record(ns_claim_tx, {
          account_id :: aeser_id:id(),
          nonce      :: integer(),
          name       :: binary(),
          name_salt  :: integer(),
          name_fee   :: integer(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl(),
          version    :: ?NAME_CLAIM_TX_VSN_1 | ?NAME_CLAIM_TX_VSN_2
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
    {NameFee1, Ver} = case maps:get(name_fee, Args, undefined) of
                          undefined ->
                              {aec_governance:name_claim_locked_fee(),
                               ?NAME_CLAIM_TX_VSN_1};
                          NameFee0 ->
                              {NameFee0,
                               ?NAME_CLAIM_TX_VSN_2}
                      end,
    Tx = #ns_claim_tx{account_id = AccountId,
                      nonce      = Nonce,
                      name       = Name,
                      name_salt  = NameSalt,
                      name_fee   = NameFee1,
                      fee        = Fee,
                      version    = Ver,
                      ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?NAME_CLAIM_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#ns_claim_tx{fee = Fee}) ->
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

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
check(#ns_claim_tx{}, Trees, _Env) ->
    %% Checks are done in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#ns_claim_tx{} = ClaimTx, Trees, Env) ->
    Instructions =
        aeprimop:name_claim_tx_instructions(
          account_pubkey(ClaimTx),
          name(ClaimTx),
          name_salt(ClaimTx),
          name_fee(ClaimTx),
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
                       fee        = Fee,
                       version    = ?NAME_CLAIM_TX_VSN_1,
                       ttl        = TTL} = _Tx) ->
    %% Version = version(Tx), %% TODO: not really handy callback, consider abstracting up
    {?NAME_CLAIM_TX_VSN_1,
     [ {account_id, AccountId}
     , {nonce, None}
     , {name, Name}
     , {name_salt, NameSalt}
     , {fee, Fee}
     , {ttl, TTL}
     ]};
serialize(#ns_claim_tx{account_id = AccountId,
                       nonce      = None,
                       name       = Name,
                       name_salt  = NameSalt,
                       name_fee   = NameFee,
                       fee        = Fee,
                       version    = ?NAME_CLAIM_TX_VSN_2,
                       ttl        = TTL} = _Tx) ->
    {?NAME_CLAIM_TX_VSN_2,
     [ {account_id, AccountId}
     , {nonce, None}
     , {name, Name}
     , {name_salt, NameSalt}
     , {name_fee, NameFee}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

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
                 name_fee   = aec_governance:name_claim_locked_fee(), %% equals to pre-lima fee. Add at height API.
                 fee        = Fee,
                 version    = ?NAME_CLAIM_TX_VSN_1,
                 ttl        = TTL};
deserialize(?NAME_CLAIM_TX_VSN_2,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {name_fee, NameFee}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aeser_id:specialize_type(AccountId),
    #ns_claim_tx{account_id = AccountId,
                 nonce      = Nonce,
                 name       = Name,
                 name_salt  = NameSalt,
                 name_fee   = NameFee,
                 fee        = Fee,
                 version    = ?NAME_CLAIM_TX_VSN_2,
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
                        fee        = Fee,
                        version    = ?NAME_CLAIM_TX_VSN_1,
                        ttl        = TTL}) ->
    #{<<"account_id">> => aeser_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name">>       => Name,
      <<"name_salt">>  => NameSalt,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL};
for_client(#ns_claim_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name       = Name,
                        name_salt  = NameSalt,
                        name_fee   = NameFee,
                        fee        = Fee,
                        version    = ?NAME_CLAIM_TX_VSN_2,
                        ttl        = TTL}) ->
    #{<<"account_id">> => aeser_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name">>       => Name,
      <<"name_salt">>  => NameSalt,
      <<"name_fee">>   => NameFee,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

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

name_fee(#ns_claim_tx{name_fee = NameFee}) ->
    NameFee.

%%%===================================================================
%%% Assertions
%%%===================================================================
assert_min_bid_fee(NameFee, MinLockedFee, ProtocolVer)
  when ProtocolVer >= ?LIMA_PROTOCOL_VSN ->
    NameFee >= MinLockedFee;
assert_min_bid_fee(_NameFee, _MinLockedFee, _) ->
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================

account_pubkey(#ns_claim_tx{account_id = AccountId}) ->
    aeser_id:specialize(AccountId, account).

-spec version(tx()) -> non_neg_integer().
version(#ns_claim_tx{version = Ver}) ->
    Ver.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(ProtocolVersion, #ns_claim_tx{version = ?NAME_CLAIM_TX_VSN_2}) ->
    aec_hard_forks:is_valid_at_protocol(ProtocolVersion, ?LIMA_PROTOCOL_VSN);
valid_at_protocol(ProtocolVersion, #ns_claim_tx{version = ?NAME_CLAIM_TX_VSN_1}) ->
    aec_hard_forks:is_valid_at_protocol(ProtocolVersion, ?ROMA_PROTOCOL_VSN).

