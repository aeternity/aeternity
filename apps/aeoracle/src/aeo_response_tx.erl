%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Oracle register transaction
%%% @end
%%%=============================================================================
-module(aeo_response_tx).

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
         oracle_pubkey/1,
         query_id/1,
         response/1,
         response_ttl/1]).


-define(ORACLE_RESPONSE_TX_VSN, 1).
-define(ORACLE_RESPONSE_TX_TYPE, oracle_response_tx).

-record(oracle_response_tx, {
          oracle_id    :: aeser_id:id(),
          nonce        :: integer(),
          query_id     :: aeo_query:id(),
          response     :: aeo_oracles:response(),
          response_ttl :: aeo_oracles:relative_ttl(),
          fee          :: integer(),
          ttl          :: aetx:tx_ttl()
          }).

-opaque tx() :: #oracle_response_tx{}.

-export_type([tx/0]).

-spec oracle_id(tx()) -> aeser_id:id().
oracle_id(#oracle_response_tx{oracle_id = OracleId}) ->
    OracleId.

-spec oracle_pubkey(tx()) -> aec_keys:pubkey().
oracle_pubkey(#oracle_response_tx{oracle_id = OracleId}) ->
    aeser_id:specialize(OracleId, oracle).

-spec query_id(tx()) -> aeo_query:id().
query_id(#oracle_response_tx{query_id = QueryId}) ->
    QueryId.

-spec response(tx()) -> aeo_query:oracle_response().
response(#oracle_response_tx{response = Response}) ->
    Response.

-spec response_ttl(tx()) -> aeo_oracles:relative_ttl().
response_ttl(#oracle_response_tx{response_ttl = ResponseTTL}) ->
    ResponseTTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{oracle_id    := OracleId,
      nonce        := Nonce,
      query_id     := QueryId,
      response     := Response,
      response_ttl := ResponseTTL,
      fee          := Fee} = Args) ->
    oracle = aeser_id:specialize_type(OracleId),
    Tx = #oracle_response_tx{oracle_id    = OracleId,
                             nonce        = Nonce,
                             query_id     = QueryId,
                             response     = Response,
                             response_ttl = ResponseTTL,
                             fee          = Fee,
                             ttl          = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?ORACLE_RESPONSE_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#oracle_response_tx{fee = F}) ->
    F.

-spec gas(tx()) -> non_neg_integer().
gas(#oracle_response_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#oracle_response_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#oracle_response_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#oracle_response_tx{} = Tx) ->
    oracle_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#oracle_response_tx{oracle_id = OId, query_id = QId}) ->
    [OId, QId].

-spec check(tx(), aec_trees:trees(), aetx_env:env()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#oracle_response_tx{}, Trees,_Env) ->
    %% Checks are in process/3.
     {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#oracle_response_tx{} = Tx, _) ->
    {ok, [oracle_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
process(#oracle_response_tx{} = RTx, Trees, Env) ->
    {delta, RTTL} = response_ttl(RTx),
    {oracle, OraclePubkey} = aeser_id:specialize(oracle_id(RTx)),
    Instructions =
        aeprimop:oracle_response_tx_instructions(OraclePubkey,
                                                 query_id(RTx),
                                                 response(RTx),
                                                 RTTL,
                                                 fee(RTx),
                                                 nonce(RTx)),
    aeprimop:eval(Instructions, Trees, Env).

serialize(#oracle_response_tx{oracle_id    = OracleId,
                              nonce        = Nonce,
                              query_id     = QueryId,
                              response     = Response,
                              response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                              fee          = Fee,
                              ttl          = TTL} = Tx) ->
    {version(Tx),
    [ {oracle_id, OracleId}
    , {nonce, Nonce}
    , {query_id, QueryId}
    , {response, Response}
    , {response_ttl_type, ?ttl_delta_int}
    , {response_ttl_value, ResponseTTLValue}
    , {fee, Fee}
    , {ttl, TTL}
    ]}.

deserialize(?ORACLE_RESPONSE_TX_VSN,
            [ {oracle_id, OracleId}
            , {nonce, Nonce}
            , {query_id, QueryId}
            , {response, Response}
            , {response_ttl_type, ?ttl_delta_int}
            , {response_ttl_value, ResponseTTLValue}
            , {fee, Fee}
            , {ttl, TTL}]) ->
    oracle = aeser_id:specialize_type(OracleId),
    #oracle_response_tx{oracle_id    = OracleId,
                        nonce        = Nonce,
                        query_id     = QueryId,
                        response     = Response,
                        response_ttl = {?ttl_delta_atom, ResponseTTLValue},
                        fee          = Fee,
                        ttl          = TTL}.

serialization_template(?ORACLE_RESPONSE_TX_VSN) ->
    [ {oracle_id, id}
    , {nonce, int}
    , {query_id, binary}
    , {response, binary}
    , {response_ttl_type, int}
    , {response_ttl_value, int}
    , {fee, int}
    , {ttl, int}
    ].

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?ORACLE_RESPONSE_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

for_client(#oracle_response_tx{oracle_id = OracleId,
                               nonce     = Nonce,
                               query_id  = QueryId,
                               response  = Response,
                               fee       = Fee,
                               ttl       = TTL} = Tx) ->
    {ResponseTTLType = delta, ResponseTTLValue} = response_ttl(Tx),
    #{<<"oracle_id">>   => aeser_api_encoder:encode(id_hash, OracleId),
      <<"nonce">>       => Nonce,
      <<"query_id">>    => aeser_api_encoder:encode(oracle_query_id, QueryId),
      <<"response">>    => Response,
      <<"response_ttl">> => #{<<"type">>  => ResponseTTLType,
                              <<"value">> => ResponseTTLValue},
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL}.
