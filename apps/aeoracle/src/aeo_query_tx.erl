%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_query_tx).

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
-export([oracle/1,
         query/1,
         query_fee/1,
         query_id/1,
         query_ttl/1,
         response_ttl/1,
         sender/1]).


-define(ORACLE_QUERY_TX_VSN, 1).
-define(ORACLE_QUERY_TX_TYPE, oracle_query_tx).
-define(ORACLE_QUERY_TX_FEE, 2).

-record(oracle_query_tx, {
          sender       :: aec_id:id(),
          nonce        :: integer(),
          oracle       :: aec_id:id(),
          query        :: aeo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: aeo_oracles:ttl(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_query_tx{}.

-export_type([tx/0]).

-spec sender(tx()) -> aec_id:id().
sender(#oracle_query_tx{sender = Sender}) ->
    Sender.

sender_pubkey(#oracle_query_tx{sender = Sender}) ->
    aec_id:specialize(Sender, account).

-spec oracle(tx()) -> aec_id:id().
oracle(#oracle_query_tx{oracle = Oracle}) ->
    Oracle.

oracle_pubkey(#oracle_query_tx{oracle = Oracle}) ->
    aec_id:specialize(Oracle, oracle).

-spec query(tx()) -> aeo_oracles:query().
query(#oracle_query_tx{query = Query}) ->
    Query.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_query_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec query_id(tx()) -> aeo_query:id().
query_id(#oracle_query_tx{} = Tx) ->
    aeo_query:id(sender_pubkey(Tx), nonce(Tx), oracle_pubkey(Tx)).

-spec query_ttl(tx()) -> aeo_oracles:ttl().
query_ttl(#oracle_query_tx{query_ttl = QueryTTL}) ->
    QueryTTL.

-spec response_ttl(tx()) -> aeo_oracles:relative_ttl().
response_ttl(#oracle_query_tx{response_ttl = ResponseTTL}) ->
    ResponseTTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{sender        := SenderAccount,
      nonce         := Nonce,
      oracle        := Oracle,
      query         := Query,
      query_fee     := QueryFee,
      query_ttl     := QueryTTL,
      response_ttl  := ResponseTTL,
      fee           := Fee} = Args) ->
    account = aec_id:specialize_type(SenderAccount),
    oracle  = aec_id:specialize_type(Oracle), %% TODO: Should also be 'name'
    Tx = #oracle_query_tx{sender        = SenderAccount,
                          nonce         = Nonce,
                          oracle        = Oracle,
                          query         = Query,
                          query_fee     = QueryFee,
                          query_ttl     = QueryTTL,
                          response_ttl  = ResponseTTL,
                          fee           = Fee,
                          ttl           = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_QUERY_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_query_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_query_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_query_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_query_tx{} = Tx) ->
    sender_pubkey(Tx).

%% SenderAccount should exist, and have enough funds for the fee + the query_fee.
%% Oracle should exist, and query_fee should be enough
%% Fee should cover TTL
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_query_tx{nonce = Nonce, query_fee = QFee, query_ttl = QTTL,
                       response_ttl = RTTL, fee = Fee} = QTx,
      Context, Trees, Height, _ConsensusVersion) ->
    OraclePubKey = oracle_pubkey(QTx),
    SenderPubKey = sender_pubkey(QTx),
    Checks =
        [fun() -> aetx_utils:check_account(SenderPubKey, Trees, Nonce, Fee + QFee) end,
         fun() -> check_oracle(OraclePubKey, Trees, QFee, Height, QTTL, RTTL) end,
         fun() -> check_query(QTx, SenderPubKey, OraclePubKey, Trees, Height) end
         | case Context of
               aetx_contract -> [];
               aetx_transaction ->
                   [fun() -> aeo_utils:check_ttl_fee(Height, QTTL, Fee - ?ORACLE_QUERY_TX_FEE) end]
           end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_query_tx{} = Tx, _) ->
    {ok, [sender_pubkey(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#oracle_query_tx{nonce = Nonce, fee = Fee,
                         query_fee = QueryFee} = QueryTx, _Context, Trees0, Height, _ConsensusVersion) ->
    SenderPubKey  = sender_pubkey(QueryTx),
    OraclePubKey  = oracle_pubkey(QueryTx),
    AccountsTree0 = aec_trees:accounts(Trees0),
    OraclesTree0  = aec_trees:oracles(Trees0),

    Sender0 = aec_accounts_trees:get(SenderPubKey, AccountsTree0),
    {ok, Sender1} = aec_accounts:spend(Sender0, QueryFee + Fee, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(Sender1, AccountsTree0),

    Query = aeo_query:new(QueryTx, SenderPubKey, OraclePubKey, Height),
    OraclesTree1 = aeo_state_tree:insert_query(Query, OraclesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_oracles(Trees1, OraclesTree1),

    {ok, Trees2}.

serialize(#oracle_query_tx{sender        = SenderId,
                           nonce         = Nonce,
                           oracle        = OracleId,
                           query         = Query,
                           query_fee     = QueryFee,
                           query_ttl     = {QueryTTLType0, QueryTTLValue},
                           response_ttl  = {?ttl_delta_atom, ResponseTTLValue},
                           fee           = Fee,
                           ttl           = TTL}) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_atom -> ?ttl_delta_int;
                       ?ttl_block_atom -> ?ttl_block_int
                   end,
    {version(),
     [ {sender, SenderId}
     , {nonce, Nonce}
     , {oracle, OracleId}
     , {query, Query}
     , {query_fee, QueryFee}
     , {query_ttl_type, QueryTTLType}
     , {query_ttl_value, QueryTTLValue}
     , {response_ttl_type, ?ttl_delta_int}
     , {response_ttl_value, ResponseTTLValue}
     , {fee, Fee}
     , {ttl, TTL}
     ]}.

deserialize(?ORACLE_QUERY_TX_VSN,
            [ {sender, SenderId}
            , {nonce, Nonce}
            , {oracle, OracleId}
            , {query, Query}
            , {query_fee, QueryFee}
            , {query_ttl_type, QueryTTLType0}
            , {query_ttl_value, QueryTTLValue}
            , {response_ttl_type, ?ttl_delta_int}
            , {response_ttl_value, ResponseTTLValue}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_int -> ?ttl_delta_atom;
                       ?ttl_block_int -> ?ttl_block_atom
                   end,
    account = aec_id:specialize_type(SenderId),
    oracle = aec_id:specialize_type(OracleId),
    #oracle_query_tx{sender        = SenderId,
                     nonce         = Nonce,
                     oracle        = OracleId,
                     query         = Query,
                     query_fee     = QueryFee,
                     query_ttl     = {QueryTTLType, QueryTTLValue},
                     response_ttl  = {?ttl_delta_atom, ResponseTTLValue},
                     fee           = Fee,
                     ttl           = TTL}.

serialization_template(?ORACLE_QUERY_TX_VSN) ->
    [ {sender, id}
    , {nonce, int}
    , {oracle, id}
    , {query, binary}
    , {query_fee, int}
    , {query_ttl_type, int}
    , {query_ttl_value, int}
    , {response_ttl_type, int}
    , {response_ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?ORACLE_QUERY_TX_VSN.

for_client(#oracle_query_tx{nonce         = Nonce,
                            query         = Query,
                            query_fee     = QueryFee,
                            query_ttl     = {QueryTLLType, QueryTTLValue},
                            response_ttl  = {delta=ResponseTTLType, ResponseTTLValue},
                            fee           = Fee,
                            ttl           = TTL} = Tx) ->
    #{<<"data_schema">> => <<"OracleQueryTxJSON">>, % swagger schema name
      <<"vsn">> => version(),
      <<"sender">> => aec_base58c:encode(id_hash, sender(Tx)),
      <<"nonce">> => Nonce,
      <<"oracle">> => aec_base58c:encode(id_hash, oracle(Tx)),
      <<"query">> => Query,
      <<"query_fee">> => QueryFee,
      <<"query_ttl">> => #{<<"type">> => QueryTLLType,
                           <<"value">> => QueryTTLValue},
      <<"response_ttl">> => #{<<"type">> => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">> => Fee,
      <<"ttl">> => TTL}.

%% -- Local functions  -------------------------------------------------------

check_query(Q, SenderPubKey, OraclePubKey, Trees, Height) ->
    Oracles  = aec_trees:oracles(Trees),
    I        = aeo_query:new(Q, SenderPubKey, OraclePubKey, Height),
    OracleId = aeo_query:oracle_address(I),
    Id       = aeo_query:id(I),
    case aeo_state_tree:lookup_query(OracleId, Id, Oracles) of
        none       -> ok;
        {value, _} -> {error, oracle_query_already_present}
    end.

check_oracle(OraclePubKey, Trees, QueryFee, Height, QTTL, RTTL) ->
    OraclesTree  = aec_trees:oracles(Trees),
    case aeo_state_tree:lookup_oracle(OraclePubKey, OraclesTree) of
        {value, Oracle} ->
            case QueryFee >= aeo_oracles:query_fee(Oracle) of
                true  -> check_oracle_ttl(Oracle, Height, QTTL, RTTL);
                false -> {error, query_fee_too_low}
            end;
        none -> {error, oracle_does_not_exist}
    end.

check_oracle_ttl(O, Height, QTTL, RTTL) ->
    try
        Delta  = aeo_utils:ttl_delta(Height, QTTL),
        MaxTTL = aeo_utils:ttl_expiry(Height + Delta, RTTL),
        case aeo_oracles:expires(O) < MaxTTL of
            false -> ok;
            true  -> {error, too_long_ttl}
        end
    catch _:_ ->
        {error, invalid_ttl}
    end.
