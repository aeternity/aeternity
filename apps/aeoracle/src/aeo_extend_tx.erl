%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_extend_tx).

-include_lib("apps/aecore/include/common.hrl").
-include("oracle_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         accounts/1,
         signers/2,
         serialize/1,
         serialization_template/1,
         deserialize/2,
         for_client/1
        ]).

%% Additional getters
-export([oracle/1,
         ttl/1]).

-define(ORACLE_EXTEND_TX_VSN, 1).
-define(ORACLE_EXTEND_TX_TYPE, oracle_extend_tx).
-define(ORACLE_EXTEND_TX_FEE, 1).

-opaque tx() :: #oracle_extend_tx{}.

-export_type([tx/0]).

-spec oracle(tx()) -> pubkey().
oracle(#oracle_extend_tx{oracle = OraclePK}) ->
    OraclePK.

-spec ttl(tx()) -> aeo_oracles:relative_ttl().
ttl(#oracle_extend_tx{ttl = TTL}) ->
    TTL.

-spec fee(tx()) -> integer().
fee(#oracle_extend_tx{fee = Fee}) ->
    Fee.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{oracle := OraclePK,
      nonce  := Nonce,
      ttl    := TTL,
      fee    := Fee}) ->
    Tx = #oracle_extend_tx{oracle = OraclePK,
                           nonce  = Nonce,
                           ttl    = TTL,
                           fee    = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_EXTEND_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_extend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#oracle_extend_tx{oracle = OraclePK}) ->
    OraclePK.

%% Account should exist, and have enough funds for the fee
%% Oracle should exist.
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_extend_tx{oracle = OraclePubKeyOrName, nonce = Nonce,
                        ttl = TTL, fee = Fee}, _Context, Trees, Height, _ConsensusVersion) ->

    NamesTree = aec_trees:ns(Trees),
    {ok, OraclePubKey} = aens:resolve_decoded(oracle_pubkey, OraclePubKeyOrName, NamesTree),

    Checks =
        [fun() -> aetx_utils:check_account(OraclePubKey, Trees, Nonce, Fee) end,
         fun() -> ensure_oracle(OraclePubKey, Trees) end,
         fun() -> aeo_utils:check_ttl_fee(Height, TTL, Fee - ?ORACLE_EXTEND_TX_FEE) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(#oracle_extend_tx{oracle = OraclePK}) ->
    [OraclePK].

-spec signers(tx(), aec_trees:trees()) -> {ok, [pubkey()]}.
signers(#oracle_extend_tx{oracle = OraclePK}, _) ->
    {ok, [OraclePK]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#oracle_extend_tx{oracle = OraclePKOrName, nonce = Nonce, fee = Fee, ttl = TTL},
        _Context, Trees0, _Height, _ConsensusVersion) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),
    NamesTree0 = aec_trees:ns(Trees0),

    {ok, OraclePK} = aens:resolve_decoded(oracle_pubkey, OraclePKOrName, NamesTree0),

    Account0 = aec_accounts_trees:get(OraclePK, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Oracle0 = aeo_state_tree:get_oracle(OraclePK, OraclesTree0),
    NewExpires = aeo_utils:ttl_expiry(aeo_oracles:expires(Oracle0), TTL),
    Oracle1 = aeo_oracles:set_expires(NewExpires, Oracle0),
    OraclesTree1 = aeo_state_tree:enter_oracle(Oracle1, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_extend_tx{oracle = OraclePK,
                            nonce  = Nonce,
                            ttl    = {?ttl_delta_atom, TTLValue},
                            fee    = Fee}) ->
    {version(),
    [ {oracle, OraclePK}
    , {nonce, Nonce}
    , {ttl_type, ?ttl_delta_int}
    , {ttl_value, TTLValue}
    , {fee, Fee}
    ]}.

deserialize(?ORACLE_EXTEND_TX_VSN,
           [ {oracle, OraclePK}
           , {nonce, Nonce}
           , {ttl_type, ?ttl_delta_int}
           , {ttl_value, TTLValue}
           , {fee, Fee}]) ->
    #oracle_extend_tx{oracle = OraclePK,
                      nonce  = Nonce,
                      ttl    = {?ttl_delta_atom, TTLValue},
                      fee    = Fee}.

serialization_template(?ORACLE_EXTEND_TX_VSN) ->
    [ {oracle, binary}
    , {nonce, int}
    , {ttl_type, int}
    , {ttl_value, int}
    , {fee, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_EXTEND_TX_VSN.

for_client(#oracle_extend_tx{ oracle = OraclePK,
                              nonce  = Nonce,
                              ttl    = {delta = TTLType, TTLValue},
                              fee    = Fee}) ->
    #{<<"data_schema">> => <<"OracleExtendTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"account">> => aec_base58c:encode(account_pubkey, OraclePK),
      <<"nonce">> => Nonce,
      <<"ttl">> => #{<<"type">> => TTLType, <<"value">> => TTLValue},
      <<"fee">> => Fee}.

%% -- Local functions  -------------------------------------------------------

ensure_oracle(PubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(PubKey, OraclesTree) of
        {value, _Oracle} -> ok;
        none             -> {error, account_is_not_an_active_oracle}
    end.
