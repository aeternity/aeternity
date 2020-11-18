%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_query_tx).

-include("oracle_txs.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         entities/1,
         check/3,
         process/3,
         signers/2,
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).

%% Additional getters
-export([oracle_id/1,
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
          sender_id    :: aeser_id:id(),
          nonce        :: integer(),
          oracle_id    :: aeser_id:id(),
          query        :: aeo_oracles:query(),
          query_fee    :: integer(),
          query_ttl    :: aeo_oracles:ttl(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_query_tx{}.

-export_type([tx/0]).

-spec sender_id(tx()) -> aeser_id:id().
sender_id(#oracle_query_tx{sender_id = SenderId}) ->
    SenderId.

-spec sender_pubkey(tx()) -> aec_keys:pubkey().
sender_pubkey(#oracle_query_tx{sender_id = SenderId}) ->
    aeser_id:specialize(SenderId, account).

-spec oracle_id(tx()) -> aeser_id:id().
oracle_id(#oracle_query_tx{oracle_id = OracleId}) ->
    OracleId.

-spec query(tx()) -> aeo_oracles:query().
query(#oracle_query_tx{query = Query}) ->
    Query.

-spec query_fee(tx()) -> integer().
query_fee(#oracle_query_tx{query_fee = QueryFee}) ->
    QueryFee.

-spec query_id(tx()) -> aeo_query:id().
query_id(#oracle_query_tx{} = Tx) ->
    {_, OracleId} = aeser_id:specialize(oracle_id(Tx)),
    aeo_query:id(sender_pubkey(Tx), nonce(Tx), OracleId).

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
    account = aeser_id:specialize_type(SenderId),
    true = lists:member(aeser_id:specialize_type(OracleId), [oracle, name]),
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

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#oracle_query_tx{sender_id = SId, oracle_id = OId}) ->
    [SId, OId].

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_query_tx{}, Trees,_Env) ->
    %% Checks are in process/3.
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_query_tx{} = Tx, _) ->
    {ok, [sender_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#oracle_query_tx{} = QTx, Trees, Env) ->
    Height = aetx_env:height(Env),
    case aeo_utils:ttl_delta(Height, query_ttl(QTx)) of
        {error, _} = Err -> Err;
        QTTL ->
            {delta, RTTL} = response_ttl(QTx),
            Instructions =
                aeprimop:oracle_query_tx_instructions(
                  oracle_id(QTx),
                  sender_pubkey(QTx),
                  query(QTx),
                  query_fee(QTx),
                  QTTL,
                  RTTL,
                  fee(QTx),
                  nonce(QTx)),
            aeprimop:eval(Instructions, Trees, Env)
    end.

serialize(#oracle_query_tx{sender_id    = SenderId,
                           nonce        = Nonce,
                           oracle_id    = OracleId,
                           query        = Query,
                           query_fee    = QueryFee,
                           query_ttl    = {QueryTTLType0, QueryTTLValue},
                           response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                           fee          = Fee,
                           ttl          = TTL} = Tx) ->
    QueryTTLType = case QueryTTLType0 of
                       ?ttl_delta_atom -> ?ttl_delta_int;
                       ?ttl_block_atom -> ?ttl_block_int
                   end,
    {version(Tx),
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
    account = aeser_id:specialize_type(SenderId),
    true = lists:member(aeser_id:specialize_type(OracleId), [name, oracle]),
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

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?ORACLE_QUERY_TX_VSN.


-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Proto, Tx) ->
    case {Proto >= ?IRIS_PROTOCOL_VSN, aeser_id:specialize_type(oracle_id(Tx))} of
        {true, name} -> true;
        {_, oracle} -> true;
        _ -> false
    end.

for_client(#oracle_query_tx{sender_id = SenderId,
                            nonce      = Nonce,
                            oracle_id  = OracleId,
                            query      = Query,
                            query_fee  = QueryFee,
                            fee        = Fee,
                            ttl = TTL} = Tx) ->
    {QueryTLLType, QueryTTLValue} = query_ttl(Tx),
    {ResponseTTLType = delta, ResponseTTLValue} = response_ttl(Tx),
    #{<<"sender_id">>    => aeser_api_encoder:encode(id_hash, SenderId),
      <<"nonce">>        => Nonce,
      <<"oracle_id">>    => aeser_api_encoder:encode(id_hash, OracleId),
      <<"query">>        => Query,
      <<"query_fee">>    => QueryFee,
      <<"query_ttl">>    => #{<<"type">>  => QueryTLLType,
                              <<"value">> => QueryTTLValue},
      <<"response_ttl">> => #{<<"type">>  => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">>          => Fee,
      <<"ttl">>          => TTL}.
