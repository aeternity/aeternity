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
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([name_ttl/1,
         pointers/1,
         client_ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_UPDATE_TX_VSN, 1).
-define(NAME_UPDATE_TX_TYPE, name_update_tx).

-record(ns_update_tx, {
          account    :: aec_id:id(),
          nonce      :: integer(),
          name_hash  :: aec_id:id(),
          name_ttl   :: integer(),
          pointers   :: [{binary(),binary()}],
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
new(#{account    := Account,
      nonce      := Nonce,
      name_hash  := NameId,
      name_ttl   := NameTTL,
      pointers   := Pointers,
      client_ttl := ClientTTL,
      fee        := Fee} = Args) ->
    account = aec_id:specialize_type(Account),
    name    = aec_id:specialize_type(NameId),
    Tx = #ns_update_tx{account    = Account,
                       nonce      = Nonce,
                       name_hash  = NameId,
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

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#ns_update_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#ns_update_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#ns_update_tx{} = Tx) ->
    account_pubkey(Tx).

account(#ns_update_tx{account = Account}) ->
    Account.

account_pubkey(#ns_update_tx{account = Account}) ->
    aec_id:specialize(Account, account).

name_id(#ns_update_tx{name_hash = NameId}) ->
    NameId.

name_hash(#ns_update_tx{name_hash = NameId}) ->
    aec_id:specialize(NameId, name).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#ns_update_tx{nonce = Nonce, fee = Fee, name_ttl = NTTL} = Tx,
      _Context, Trees, _Height, _ConsensusVersion) ->
    AccountPubKey = account_pubkey(Tx),
    NameHash = name_hash(Tx),
    Checks =
        [fun() -> check_ttl(NTTL) end,
         fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> aens_utils:check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#ns_update_tx{nonce = Nonce, fee = Fee} = UpdateTx,
        _Context, Trees0, Height, _ConsensusVersion) ->
    AccountPubKey = account_pubkey(UpdateTx),
    NameHash = name_hash(UpdateTx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    NSTree0 = aec_trees:ns(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get_name(NameHash, NSTree0),
    Name1 = aens_names:update(UpdateTx, Name0, Height),
    NSTree1 = aens_state_tree:enter_name(Name1, NSTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_ns(Trees1, NSTree1),

    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#ns_update_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec serialize(tx()) -> {integer(), [{atom(), term()}]}.
serialize(#ns_update_tx{account    = AccountId,
                        nonce      = Nonce,
                        name_hash  = NameId,
                        name_ttl   = NameTTL,
                        pointers   = Pointers,
                        client_ttl = ClientTTL,
                        fee        = Fee,
                        ttl        = TTL}) ->
    {version(),
     [ {account, AccountId}
     , {nonce, Nonce}
     , {name_hash, NameId}
     , {name_ttl, NameTTL}
     , {pointers, jsx:encode(Pointers)} %% TODO: This might be ambigous
     , {client_ttl, ClientTTL}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

-spec deserialize(Vsn :: integer(), list({atom(), term()})) -> tx().
deserialize(?NAME_UPDATE_TX_VSN,
            [ {account, AccountId}
            , {nonce, Nonce}
            , {name_hash, NameId}
            , {name_ttl, NameTTL}
            , {pointers, Pointers}
            , {client_ttl, ClientTTL}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    account = aec_id:specialize_type(AccountId),
    name = aec_id:specialize_type(NameId),
    #ns_update_tx{account    = AccountId,
                  nonce      = Nonce,
                  name_hash  = NameId,
                  name_ttl   = NameTTL,
                  pointers   = jsx:decode(Pointers),
                  client_ttl = ClientTTL,
                  fee        = Fee,
                  ttl        = TTL}.

serialization_template(?NAME_UPDATE_TX_VSN) ->
    [ {account, id}
    , {nonce, int}
    , {name_hash, id}
    , {name_ttl, int}
    , {pointers, binary} %% TODO: This might be ambigous
    , {client_ttl, int}
    , {fee, int}
    , {ttl, int}
    ].


-spec for_client(tx()) -> map().
for_client(#ns_update_tx{nonce      = Nonce,
                         name_ttl   = NameTTL,
                         pointers   = Pointers,
                         client_ttl = ClientTTL,
                         fee        = Fee,
                         ttl        = TTL} = Tx) ->
    #{<<"vsn">>        => version(),
      <<"data_schema">> => <<"NameUpdateTxObject">>, % swagger schema name
      <<"account">>    => aec_base58c:encode(id_hash, account(Tx)),
      <<"nonce">>      => Nonce,
      <<"name_hash">>  => aec_base58c:encode(id_hash, name_id(Tx)),
      <<"name_ttl">>   => NameTTL,
      <<"pointers">>   => Pointers,
      <<"client_ttl">> => ClientTTL,
      <<"fee">>        => Fee,
      <<"ttl">>        => TTL}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec name_ttl(tx()) -> integer().
name_ttl(#ns_update_tx{name_ttl = NameTTL}) ->
    NameTTL.

-spec pointers(tx()) -> list().
pointers(#ns_update_tx{pointers = Pointers}) ->
    Pointers.

-spec client_ttl(tx()) -> non_neg_integer().
client_ttl(#ns_update_tx{client_ttl = ClientTTL}) ->
    ClientTTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_ttl(TTL) ->
    case TTL =< aec_governance:name_claim_max_expiration() of
        true ->
            ok;
        false ->
            {error, ttl_too_high}
    end.

version() ->
    ?NAME_UPDATE_TX_VSN.
