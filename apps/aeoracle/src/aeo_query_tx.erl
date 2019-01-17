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

%% Additional getters
-export([oracle_id/1,
         oracle_pubkey/1,
         query/1,
         query_fee/1,
         query_id/1,
         query_ttl/1,
         response_ttl/1,
         sender_id/1,
         sender_pubkey/1]).


-define(ORACLE_QUERY_TX_VSN, 1).
-define(ORACLE_QUERY_TX_TYPE, oracle_query_tx).

-record(oracle_query_tx, {
          sender_id    :: aec_id:id(),
          nonce        :: integer(),
          oracle_id    :: aec_id:id(),
          query        :: aeo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: aeo_oracles:ttl(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_query_tx{}.

-export_type([tx/0]).

-spec sender_id(tx()) -> aec_id:id().
sender_id(#oracle_query_tx{sender_id = SenderId}) ->
    SenderId.

-spec sender_pubkey(tx()) -> aec_keys:pubkey().
sender_pubkey(#oracle_query_tx{sender_id = SenderId}) ->
    aec_id:specialize(SenderId, account).

-spec oracle_id(tx()) -> aec_id:id().
oracle_id(#oracle_query_tx{oracle_id = OracleId}) ->
    OracleId.

-spec oracle_pubkey(tx()) -> aec_keys:pubkey().
oracle_pubkey(#oracle_query_tx{oracle_id = OracleId}) ->
    aec_id:specialize(OracleId, oracle).

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
new(#{sender_id    := SenderId,
      nonce        := Nonce,
      oracle_id    := OracleId,
      query        := Query,
      query_fee    := QueryFee,
      query_ttl    := QueryTTL,
      response_ttl := ResponseTTL,
      fee          := Fee} = Args) ->
    account = aec_id:specialize_type(SenderId),
    oracle  = aec_id:specialize_type(OracleId), %% TODO: Should also be 'name'
    Tx = #oracle_query_tx{sender_id     = SenderId,
                          nonce         = Nonce,
                          oracle_id     = OracleId,
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

-spec gas(tx()) -> non_neg_integer().
gas(#oracle_query_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_query_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_query_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_query_tx{} = Tx) ->
    sender_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_query_tx{}, Trees,_Env) ->
    %% Checks are in process/3.
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_query_tx{} = Tx, _) ->
    {ok, [sender_pubkey(Tx)]}.

query_op(QTx, Height) ->
    {delta, RTTL} = response_ttl(QTx),
    QTTL = aeo_utils:ttl_delta(Height, query_ttl(QTx)),
    { oracle_query
    , oracle_pubkey(QTx)
    , sender_pubkey(QTx)
    , nonce(QTx)
    , query(QTx)
    , query_fee(QTx)
    , QTTL
    , RTTL
    }.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#oracle_query_tx{} = QTx, Trees, Env) ->
    Sender = sender_pubkey(QTx),
    Height = aetx_env:height(Env),
    Instructions = [ {inc_account_nonce, Sender, nonce(QTx)}
                   , {spend_fee, Sender, fee(QTx) + query_fee(QTx)}
                   , query_op(QTx, Height)
                   ],
    aec_tx_processor:eval(Instructions, Trees, Height).

serialize(#oracle_query_tx{sender_id    = SenderId,
                           nonce        = Nonce,
                           oracle_id    = OracleId,
                           query        = Query,
                           query_fee    = QueryFee,
                           query_ttl    = {QueryTTLType0, QueryTTLValue},
                           response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                           fee          = Fee,
                           ttl          = TTL}) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_atom -> ?ttl_delta_int;
                       ?ttl_block_atom -> ?ttl_block_int
                   end,
    {version(),
     [ {sender_id, SenderId}
     , {nonce, Nonce}
     , {oracle_id, OracleId}
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
            [ {sender_id, SenderId}
            , {nonce, Nonce}
            , {oracle_id, OracleId}
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
    #oracle_query_tx{sender_id    = SenderId,
                     nonce        = Nonce,
                     oracle_id    = OracleId,
                     query        = Query,
                     query_fee    = QueryFee,
                     query_ttl    = {QueryTTLType, QueryTTLValue},
                     response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                     fee          = Fee,
                     ttl          = TTL}.

serialization_template(?ORACLE_QUERY_TX_VSN) ->
    [ {sender_id, id}
    , {nonce, int}
    , {oracle_id, id}
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

for_client(#oracle_query_tx{sender_id = SenderId,
                            nonce      = Nonce,
                            oracle_id  = OracleId,
                            query      = Query,
                            query_fee  = QueryFee,
                            fee        = Fee,
                            ttl = TTL} = Tx) ->
    {QueryTLLType, QueryTTLValue} = query_ttl(Tx),
    {ResponseTTLType = delta, ResponseTTLValue} = response_ttl(Tx),
    #{<<"sender_id">>    => aehttp_api_encoder:encode(id_hash, SenderId),
      <<"nonce">>        => Nonce,
      <<"oracle_id">>    => aehttp_api_encoder:encode(id_hash, OracleId),
      <<"query">>        => Query,
      <<"query_fee">>    => QueryFee,
      <<"query_ttl">>    => #{<<"type">>  => QueryTLLType,
                              <<"value">> => QueryTTLValue},
      <<"response_ttl">> => #{<<"type">>  => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">>          => Fee,
      <<"ttl">>          => TTL}.
