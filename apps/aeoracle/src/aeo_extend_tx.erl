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
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialize/1,
         serialization_template/1,
         deserialize/2,
         for_client/1
        ]).

%% Additional getters
-export([oracle/1,
         oracle_ttl/1]).

-define(ORACLE_EXTEND_TX_VSN, 1).
-define(ORACLE_EXTEND_TX_TYPE, oracle_extend_tx).
-define(ORACLE_EXTEND_TX_FEE, 1).

-record(oracle_extend_tx, {
          oracle     :: aec_id:id(),
          nonce      :: integer(),
          oracle_ttl :: aeo_oracles:relative_ttl(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_extend_tx{}.

-export_type([tx/0]).

-spec oracle(tx()) -> aec_id:id().
oracle(#oracle_extend_tx{oracle = Oracle}) ->
    Oracle.

oracle_pubkey(#oracle_extend_tx{} = Tx) ->
    aec_id:specialize(oracle(Tx), oracle).

-spec oracle_ttl(tx()) -> aeo_oracles:relative_ttl().
oracle_ttl(#oracle_extend_tx{oracle_ttl = OTTL}) ->
    OTTL.

-spec fee(tx()) -> integer().
fee(#oracle_extend_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_extend_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{oracle     := Oracle,
      nonce      := Nonce,
      oracle_ttl := OracleTTL,
      fee        := Fee} = Args) ->
    oracle = aec_id:specialize_type(Oracle),
    Tx = #oracle_extend_tx{oracle     = Oracle,
                           nonce      = Nonce,
                           oracle_ttl = OracleTTL,
                           fee        = Fee,
                           ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_EXTEND_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_extend_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_extend_tx{} = Tx) ->
    aec_id:specialize(oracle(Tx), oracle).

%% Account should exist, and have enough funds for the fee
%% Oracle should exist.
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
    {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_extend_tx{nonce = Nonce, oracle_ttl = OTTL, fee = Fee} = Tx,
      _Context, Trees, Height, _ConsensusVersion) ->
    OraclePK = oracle_pubkey(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(OraclePK, Trees, Nonce, Fee) end,
         fun() -> ensure_oracle(OraclePK, Trees) end,
         fun() -> aeo_utils:check_ttl_fee(Height, OTTL, Fee - ?ORACLE_EXTEND_TX_FEE) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_extend_tx{} = Tx, _) ->
    {ok, [oracle_pubkey(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#oracle_extend_tx{nonce = Nonce, fee = Fee, oracle_ttl = OTTL} = Tx,
        _Context, Trees0, _Height, _ConsensusVersion) ->
    OraclePK      = aec_id:specialize(oracle(Tx), oracle),
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Account0 = aec_accounts_trees:get(OraclePK, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Oracle0 = aeo_state_tree:get_oracle(OraclePK, OraclesTree0),
    NewExpires = aeo_utils:ttl_expiry(aeo_oracles:expires(Oracle0), OTTL),
    Oracle1 = aeo_oracles:set_expires(NewExpires, Oracle0),
    OraclesTree1 = aeo_state_tree:enter_oracle(Oracle1, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_extend_tx{oracle     = OracleId,
                            nonce      = Nonce,
                            oracle_ttl = {?ttl_delta_atom, TTLValue},
                            fee        = Fee,
                            ttl        = TTL}) ->
    {version(),
    [ {oracle, OracleId}
    , {nonce, Nonce}
    , {oracle_ttl_type, ?ttl_delta_int}
    , {oracle_ttl_value, TTLValue}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_EXTEND_TX_VSN,
           [ {oracle, OracleId}
           , {nonce, Nonce}
           , {oracle_ttl_type, ?ttl_delta_int}
           , {oracle_ttl_value, TTLValue}
           , {fee, Fee}
           , {ttl, TTL}]) ->
    oracle = aec_id:specialize_type(OracleId),
    #oracle_extend_tx{oracle     = OracleId,
                      nonce      = Nonce,
                      oracle_ttl = {?ttl_delta_atom, TTLValue},
                      fee        = Fee,
                      ttl        = TTL}.

serialization_template(?ORACLE_EXTEND_TX_VSN) ->
    [ {oracle, id}
    , {nonce, int}
    , {oracle_ttl_type, int}
    , {oracle_ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_EXTEND_TX_VSN.

for_client(#oracle_extend_tx{ nonce      = Nonce,
                              oracle_ttl = {delta = TTLType, TTLValue},
                              fee        = Fee,
                              ttl        = TTL} = Tx) ->
    #{<<"data_schema">> => <<"OracleExtendTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"account">> => aec_base58c:encode(id_hash, oracle(Tx)),
      <<"nonce">> => Nonce,
      <<"oracle_ttl">> => #{<<"type">> => TTLType, <<"value">> => TTLValue},
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

ensure_oracle(PubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(PubKey, OraclesTree) of
        {value, _Oracle} -> ok;
        none             -> {error, account_is_not_an_active_oracle}
    end.
