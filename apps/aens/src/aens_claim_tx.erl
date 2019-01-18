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

%% Getters
-export([account_id/1,
         name/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_CLAIM_TX_VSN, 1).
-define(NAME_CLAIM_TX_TYPE, name_claim_tx).

-record(ns_claim_tx, {
          account_id :: aec_id:id(),
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
    account = aec_id:specialize_type(AccountId),
    Tx = #ns_claim_tx{account_id = AccountId,
                      nonce      = Nonce,
                      name       = Name,
                      name_salt  = NameSalt,
                      fee        = Fee,
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
check(#ns_claim_tx{}, Trees, Env) ->
    %% Checks are done in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#ns_claim_tx{} = ClaimTx, Trees, Env) ->
    Instructions =
        aec_tx_processor:name_claim_tx_instructions(
          account_pubkey(ClaimTx),
          name(ClaimTx),
          name_salt(ClaimTx),
          fee(ClaimTx),
          nonce(ClaimTx)),
    aec_tx_processor:eval(Instructions, Trees, aetx_env:height(Env)).

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_claim_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_claim_tx{account_id = AccountId,
                       nonce      = None,
                       name       = Name,
                       name_salt  = NameSalt,
                       fee        = Fee,
                       ttl        = TTL}) ->
    {version(),
     [ {account_id, AccountId}
     , {nonce, None}
     , {name, Name}
     , {name_salt, NameSalt}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_CLAIM_TX_VSN,
            [ {account_id, AccountId}
            , {nonce, Nonce}
            , {name, Name}
            , {name_salt, NameSalt}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    #ns_claim_tx{account_id = AccountId,
                 nonce      = Nonce,
                 name       = Name,
                 name_salt  = NameSalt,
                 fee        = Fee,
                 ttl        = TTL}.

serialization_template(?NAME_CLAIM_TX_VSN) ->
    [ {account_id, id}
    , {nonce, int}
    , {name, binary}
    , {name_salt, int}
    , {fee, int}
    , {ttl, int}
    ].


-spec for_client(tx()) -> map().
for_client(#ns_claim_tx{account_id = AccountId,
                        nonce      = Nonce,
                        name       = Name,
                        name_salt  = NameSalt,
                        fee        = Fee,
                        ttl        = TTL}) ->
    #{<<"account_id">> => aehttp_api_encoder:encode(id_hash, AccountId),
      <<"nonce">>      => Nonce,
      <<"name">>       => Name,
      <<"name_salt">>  => NameSalt,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account_id(tx()) -> aec_id:id().
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
    aec_id:specialize(AccountId, account).

check_commitment(NameAscii, NameSalt, AccountPubKey, Trees, Height) ->
    NSTree = aec_trees:ns(Trees),
    CommitmentHash = aens_hash:commitment_hash(NameAscii, NameSalt),
    case aens_state_tree:lookup_commitment(CommitmentHash, NSTree) of
        {value, Commitment} ->
            case aens_commitments:owner_pubkey(Commitment) =:= AccountPubKey of
                true  ->
                    Created = aens_commitments:created(Commitment),
                    Delta = aec_governance:name_claim_preclaim_delta(),
                    if Created + Delta =< Height -> ok;
                       true -> {error, commitment_delta_too_small}
                    end;
                false -> {error, commitment_not_owned}
            end;
        none ->
            {error, name_not_preclaimed}
    end.

check_name(NameAscii, Trees) ->
    NSTree = aec_trees:ns(Trees),
    NHash = aens_hash:name_hash(NameAscii),
    case aens_state_tree:lookup_name(NHash, NSTree) of
        {value, _} ->
            {error, name_already_taken};
        none ->
            ok
    end.

version() ->
    ?NAME_CLAIM_TX_VSN.

