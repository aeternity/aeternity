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

%% Additional getters
-export([account/1,
         query_spec/1,
         query_fee/1,
         response_spec/1,
         oracle_ttl/1]).

-define(ORACLE_REGISTER_TX_VSN, 1).
-define(ORACLE_REGISTER_TX_TYPE, oracle_register_tx).
-define(ORACLE_REGISTER_TX_FEE, 4).

-record(oracle_register_tx, {
          account                                     :: aec_id:id(),
          nonce                                       :: integer(),
          query_spec    = <<"string()">>              :: aeo_oracles:type_spec(),
          response_spec = <<"boolean() | integer()">> :: aeo_oracles:type_spec(),
          query_fee                                   :: integer(),
          oracle_ttl                                  :: aeo_oracles:ttl(),
          fee                                         :: integer(),
          ttl                                         :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_register_tx{}.

-export_type([tx/0]).

-spec account(tx()) -> aec_id:id().
account(#oracle_register_tx{account = Account}) ->
    Account.

account_pubkey(#oracle_register_tx{account = Account}) ->
    aec_id:specialize(Account, account).

-spec query_spec(tx()) -> aeo_oracles:type_spec().
query_spec(#oracle_register_tx{query_spec = QuerySpec}) ->
    QuerySpec.

-spec response_spec(tx()) -> aeo_oracles:type_spec().
response_spec(#oracle_register_tx{response_spec = ResponseSpec}) ->
    ResponseSpec.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_register_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec oracle_ttl(tx()) -> aeo_oracles:ttl().
oracle_ttl(#oracle_register_tx{oracle_ttl = TTL}) ->
    TTL.

-spec fee(tx()) -> integer().
fee(#oracle_register_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_register_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account       := Account,
      nonce         := Nonce,
      query_spec    := QuerySpec,
      response_spec := ResponseSpec,
      query_fee     := QueryFee,
      oracle_ttl    := OracleTTL,
      fee           := Fee} = Args) ->
    account = aec_id:specialize_type(Account),
    Tx = #oracle_register_tx{account       = Account,
                             nonce         = Nonce,
                             query_spec    = QuerySpec,
                             response_spec = ResponseSpec,
                             query_fee     = QueryFee,
                             oracle_ttl    = OracleTTL,
                             fee           = Fee,
                             ttl           = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_REGISTER_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_register_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_register_tx{account = AccountPubKey}) ->
    aec_id:specialize(AccountPubKey, account).

%% Account should exist, and have enough funds for the fee.
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_register_tx{nonce = Nonce, oracle_ttl = OTTL, fee = Fee} = Tx,
      Context, Trees, Height, _ConsensusVersion) ->
    AccountPubKey = aec_id:specialize(account(Tx), account),
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Nonce, Fee) end,
         fun() -> ensure_not_oracle(AccountPubKey, Trees) end
         | case Context of
               %% Contract is paying tx fee as gas.
               aetx_contract -> [];
               aetx_transaction ->
                   [fun() -> aeo_utils:check_ttl_fee(Height, OTTL, Fee - ?ORACLE_REGISTER_TX_FEE) end]
           end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_register_tx{} = Tx, _) ->
    {ok, [account_pubkey(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#oracle_register_tx{nonce = Nonce, fee = Fee} = RegisterTx,
        _Ctxt, Trees0, Height, _ConsensusVersion) ->
    AccountPubKey = aec_id:specialize(account(RegisterTx), account),
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Oracle = aeo_oracles:new(RegisterTx, Height),
    OraclesTree1 = aeo_state_tree:insert_oracle(Oracle, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_register_tx{account       = AccountId,
                              nonce         = Nonce,
                              query_spec    = QuerySpec,
                              response_spec = ResponseSpec,
                              query_fee     = QueryFee,
                              oracle_ttl    = {TTLType0, TTLValue},
                              fee           = Fee,
                              ttl           = TTL}) ->
    TTLType = case TTLType0 of
                  ?ttl_delta_atom -> ?ttl_delta_int;
                  ?ttl_block_atom -> ?ttl_block_int
              end,
    {version(),
    [ {account, AccountId}
    , {nonce, Nonce}
    , {query_spec, QuerySpec}
    , {response_spec, ResponseSpec}
    , {query_fee, QueryFee}
    , {ttl_type, TTLType}
    , {ttl_value, TTLValue}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_REGISTER_TX_VSN,
           [ {account, AccountId}
           , {nonce, Nonce}
           , {query_spec, QuerySpec}
           , {response_spec, ResponseSpec}
           , {query_fee, QueryFee}
           , {ttl_type, TTLType0}
           , {ttl_value, TTLValue}
           , {fee, Fee}
           , {ttl, TTL}]) ->
    TTLType = case TTLType0 of
                  ?ttl_delta_int -> ?ttl_delta_atom;
                  ?ttl_block_int -> ?ttl_block_atom
              end,
    account = aec_id:specialize_type(AccountId),
    #oracle_register_tx{account       = AccountId,
                        nonce         = Nonce,
                        query_spec    = QuerySpec,
                        response_spec = ResponseSpec,
                        query_fee     = QueryFee,
                        oracle_ttl    = {TTLType, TTLValue},
                        fee           = Fee,
                        ttl           = TTL}.

serialization_template(?ORACLE_REGISTER_TX_VSN) ->
    [ {account, id}
    , {nonce, int}
    , {query_spec, binary}
    , {response_spec, binary}
    , {query_fee, int}
    , {ttl_type, int}
    , {ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_REGISTER_TX_VSN.

for_client(#oracle_register_tx{ nonce         = Nonce,
                                query_spec    = QuerySpec,
                                response_spec = ResponseSpec,
                                query_fee     = QueryFee,
                                oracle_ttl    = {TTLType, TTLValue},
                                fee           = Fee,
                                ttl           = TTL} = Tx) ->
    #{<<"data_schema">> => <<"OracleRegisterTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"account">> => aec_base58c:encode(id_hash, account(Tx)),
      <<"nonce">> => Nonce,
      <<"query_spec">> => QuerySpec,
      <<"response_spec">> => ResponseSpec,
      <<"query_fee">> => QueryFee,
      <<"oracle_ttl">> => #{<<"type">> => TTLType,
                            <<"value">> => TTLValue},
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

ensure_not_oracle(PubKey, Trees) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(PubKey, OraclesTree) of
        {value, _Oracle} -> {error, account_is_already_an_oracle};
        none             -> ok
    end.
