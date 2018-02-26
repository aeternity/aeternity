%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_register_tx).

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
-export([account/1,
         query_spec/1,
         query_fee/1,
         response_spec/1,
         ttl/1]).

-define(ORACLE_REGISTER_TX_VSN, 1).
-define(ORACLE_REGISTER_TX_FEE, 4).

-opaque tx() :: #oracle_register_tx{}.

-export_type([tx/0]).

-spec account(tx()) -> pubkey().
account(#oracle_register_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec query_spec(tx()) -> aeo_oracles:type_spec().
query_spec(#oracle_register_tx{query_spec = QuerySpec}) ->
    QuerySpec.

-spec response_spec(tx()) -> aeo_oracles:type_spec().
response_spec(#oracle_register_tx{response_spec = ResponseSpec}) ->
    ResponseSpec.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_register_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec ttl(tx()) -> aeo_oracles:ttl().
ttl(#oracle_register_tx{ttl = TTL}) ->
    TTL.

-spec fee(tx()) -> integer().
fee(#oracle_register_tx{fee = Fee}) ->
    Fee.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account       := AccountPubKey,
      nonce         := Nonce,
      query_spec    := QuerySpec,
      response_spec := ResponseSpec,
      query_fee     := QueryFee,
      ttl           := TTL,
      fee           := Fee}) ->
    Tx = #oracle_register_tx{account       = AccountPubKey,
                             nonce         = Nonce,
                             query_spec    = QuerySpec,
                             response_spec = ResponseSpec,
                             query_fee     = QueryFee,
                             ttl           = TTL,
                             fee           = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_register_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#oracle_register_tx{account = AccountPubKey}) ->
    AccountPubKey.

%% Account should exist, and have enough funds for the fee.
-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_register_tx{account = AccountPubKey, nonce = Nonce,
                          ttl = TTL, fee = Fee}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> ensure_not_oracle(AccountPubKey, Trees) end,
         fun() -> aeo_utils:check_ttl_fee(Height, TTL, Fee - ?ORACLE_REGISTER_TX_FEE) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(#oracle_register_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#oracle_register_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#oracle_register_tx{account       = AccountPubKey,
                            nonce         = Nonce,
                            fee           = Fee} = RegisterTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Oracle = aeo_oracles:new(RegisterTx, Height),
    OraclesTree1 = aeo_state_tree:insert_oracle(Oracle, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_register_tx{account       = AccountPubKey,
                              nonce         = Nonce,
                              query_spec    = QuerySpec,
                              response_spec = ResponseSpec,
                              query_fee     = QueryFee,
                              ttl           = {TTLType, TTLValue},
                              fee           = Fee}) ->
    [#{<<"vsn">> => version()},
     #{<<"account">> => AccountPubKey},
     #{<<"nonce">> => Nonce},
     #{<<"query_spec">> => QuerySpec},
     #{<<"response_spec">> => ResponseSpec},
     #{<<"query_fee">> => QueryFee},
     #{<<"ttl">> =>
           #{<<"type">> => TTLType,
             <<"value">> => TTLValue}},
     #{<<"fee">> => Fee}].

deserialize([#{<<"vsn">>           := ?ORACLE_REGISTER_TX_VSN},
             #{<<"account">>       := AccountPubKey},
             #{<<"nonce">>         := Nonce},
             #{<<"query_spec">>    := QuerySpec},
             #{<<"response_spec">> := ResponseSpec},
             #{<<"query_fee">>     := QueryFee},
             #{<<"ttl">>           := #{<<"type">>  := TTLType,
                                        <<"value">> := TTLValue}},
             #{<<"fee">>           := Fee}]) ->
    #oracle_register_tx{account       = AccountPubKey,
                        nonce         = Nonce,
                        query_spec    = QuerySpec,
                        response_spec = ResponseSpec,
                        query_fee     = QueryFee,
                        ttl           = {binary_to_existing_atom(TTLType, utf8), TTLValue},
                        fee           = Fee}.

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_REGISTER_TX_VSN.

for_client(#oracle_register_tx{ account       = AccountPubKey,
                                nonce         = Nonce,
                                query_spec    = QuerySpec,
                                response_spec = ResponseSpec,
                                query_fee     = QueryFee,
                                ttl           = {TTLType, TTLValue},
                                fee           = Fee}) ->
    #{<<"data_schema">> => <<"OracleRegisterTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"account">> => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"nonce">> => Nonce,
      <<"query_spec">> => QuerySpec,
      <<"response_spec">> => ResponseSpec,
      <<"query_fee">> => QueryFee,
      <<"ttl">> => #{<<"type">> => TTLType,
                     <<"value">> => TTLValue},
      <<"fee">> => Fee}.

%% -- Local functions  -------------------------------------------------------

ensure_not_oracle(PubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(PubKey, OraclesTree) of
        {value, _Oracle} -> {error, account_is_already_an_oracle};
        none             -> ok
    end.
