%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_extend_tx).

-include("oracle_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1
        ]).

%% Additional getters
-export([oracle/1,
         ttl/1]).

-define(ORACLE_EXTEND_TX_VSN, 1).
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

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_extend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#oracle_extend_tx{oracle = OraclePK}) ->
    OraclePK.

%% Account should exist, and have enough funds for the fee
%% Oracle should exist.
-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_extend_tx{oracle = OraclePK, nonce = Nonce,
                        ttl = TTL, fee = Fee}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(OraclePK, Trees, Height, Nonce, Fee) end,
         fun() -> ensure_oracle(OraclePK, Trees) end,
         fun() -> aeo_utils:check_ttl_fee(Height, TTL, Fee - ?ORACLE_EXTEND_TX_FEE) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(#oracle_extend_tx{oracle = OraclePK}) ->
    [OraclePK].

-spec signers(tx()) -> [pubkey()].
signers(#oracle_extend_tx{oracle = OraclePK}) ->
    [OraclePK].

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#oracle_extend_tx{oracle = OraclePK, nonce = Nonce, fee = Fee, ttl = TTL},
        Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Account0 = aec_accounts_trees:get(OraclePK, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
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
                            ttl    = {delta, TTLValue},
                            fee    = Fee}) ->
    [#{<<"vsn">> => version()},
     #{<<"oracle">> => OraclePK},
     #{<<"nonce">> => Nonce},
     #{<<"ttl">> => #{<<"type">> => <<"delta">>, <<"value">> => TTLValue}},
     #{<<"fee">> => Fee}].

deserialize([#{<<"vsn">>    := ?ORACLE_EXTEND_TX_VSN},
             #{<<"oracle">> := OraclePK},
             #{<<"nonce">>  := Nonce},
             #{<<"ttl">>    := #{<<"type">>  := <<"delta">>, <<"value">> := TTLValue}},
             #{<<"fee">>    := Fee}]) ->
    #oracle_extend_tx{oracle = OraclePK,
                      nonce  = Nonce,
                      ttl    = {delta, TTLValue},
                      fee    = Fee}.

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
