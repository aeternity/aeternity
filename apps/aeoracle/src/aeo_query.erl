%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for oracle query objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_query).

%% API
-export([ add_response/3
        , deserialize/1
        , deserialize_from_fields/3
        , serialization_type/0
        , serialization_template/1
        , ttl/1
        , fee/1
        , id/1
        , id/3
        , is_open/1
        , is_closed/1
        , new/7
        , oracle_id/1
        , oracle_pubkey/1
        , query/1
        , response/1
        , response_ttl/1
        , sender_id/1
        , sender_pubkey/1
        , sender_nonce/1
        , serialize/1
        , serialize_for_client/1
        , set_ttl/2
        , set_fee/2
        , set_oracle/2
        , set_query/2
        , set_response/2
        , set_response_ttl/2
        , set_sender/2
        , set_sender_nonce/2
        ]).

-ifdef(TEST).
-export([new/2]).
-endif.

-define(ORACLE_QUERY_TYPE, oracle_query).
-define(ORACLE_QUERY_VSN, 1).

%%%===================================================================
%%% Types
%%%===================================================================

-type oracle_query()    :: aeo_oracles:query().
-type oracle_response() :: 'undefined' | aeo_oracles:response().
-type relative_ttl()    :: aeo_oracles:relative_ttl().

-record(query, { sender_id    :: aec_id:id()
               , sender_nonce :: non_neg_integer()
               , oracle_id    :: aec_id:id()
               , query        :: oracle_query()
               , response     :: oracle_response()
               , ttl          :: aec_blocks:height()
               , response_ttl :: relative_ttl()
               , fee          :: integer()
               }).

-opaque query() :: #query{}.
-type id() :: binary().
-type serialized() :: binary().

-export_type([ id/0
             , oracle_response/0
             , query/0
             , serialized/0
             ]).

-define(PUB_SIZE, 32).
-define(NONCE_SIZE, 256).

-behavior(aeo_cache).
%%%===================================================================
%%% API
%%%===================================================================

-ifdef(TEST).
-spec new(aeo_query_tx:tx(), aec_blocks:height()) -> query().
new(QTx, BlockHeight) ->
    AbsoluteQTTL = aeo_utils:ttl_expiry(BlockHeight, aeo_query_tx:query_ttl(QTx)),
    new(aeo_query_tx:oracle_pubkey(QTx),
        aeo_query_tx:sender_pubkey(QTx),
        aeo_query_tx:nonce(QTx),
        aeo_query_tx:query(QTx),
        aeo_query_tx:query_fee(QTx),
        AbsoluteQTTL,
        aeo_query_tx:response_ttl(QTx)).
-endif.

-spec new(aec_keys:pubkey(), aec_keys:pubkey(), non_neg_integer(), oracle_query(),
          non_neg_integer(), non_neg_integer(), aeo_oracles:relative_ttl()) ->
             query().
new(OraclePubkey, SenderPubkey, SenderNonce, Query, Fee, AbsoluteQTTL, RTTL) ->
    I = #query{ sender_id    = aec_id:create(account, SenderPubkey)
              , sender_nonce = SenderNonce
              , oracle_id    = aec_id:create(oracle, OraclePubkey)
              , query        = Query
              , response     = undefined
              , ttl          = AbsoluteQTTL
              , response_ttl = RTTL
              , fee          = Fee
              },
    assert_fields(I).

-spec is_open(query()) -> boolean().
is_open(#query{response = undefined}) -> true;
is_open(#query{}) -> false.

-spec is_closed(query()) -> boolean().
%% @doc An query is closed if it is already answered.
is_closed(#query{response = undefined}) -> false;
is_closed(#query{}) -> true.

-spec add_response(aec_blocks:height(), oracle_response(), query()) -> query().
add_response(Height, Response, Q = #query{ response_ttl = RTTL }) ->
    NewTTL =  aeo_utils:ttl_expiry(Height, RTTL),
    Q#query{ response = assert_field(response, Response)
           , ttl  = NewTTL }.

-spec serialize(query()) -> binary().
serialize(#query{} = I) ->
    {delta, RespTTLValue} = response_ttl(I),
    {Response, HasRresponse}
        = case response(I) of
              undefined -> {<<>>, false};
              Bin when is_binary(Bin) -> {Bin, true}
          end,
    aec_object_serialization:serialize(
      ?ORACLE_QUERY_TYPE, ?ORACLE_QUERY_VSN,
      serialization_template(?ORACLE_QUERY_VSN),
      [ {sender_id, sender_id(I)}
      , {sender_nonce, sender_nonce(I)}
      , {oracle_id, oracle_id(I)}
      , {query, query(I)}
      , {has_response, HasRresponse}
      , {response, Response}
      , {ttl, ttl(I)}
      , {response_ttl, RespTTLValue}
      , {fee, fee(I)}
      ]).

-spec deserialize(binary()) -> query().
deserialize(B) ->
    Fields = aec_object_serialization:deserialize(
                ?ORACLE_QUERY_TYPE, ?ORACLE_QUERY_VSN,
                serialization_template(?ORACLE_QUERY_VSN), B),
    deserialize_from_fields(?ORACLE_QUERY_VSN, unused, Fields).

deserialize_from_fields(?ORACLE_QUERY_VSN, _Pubkey,
    [ {sender_id, SenderId}
    , {sender_nonce, SenderNonce}
    , {oracle_id, OracleId}
    , {query, Query}
    , {has_response, HasResponse}
    , {response, Response0}
    , {ttl, TTL}
    , {response_ttl, RespTTLValue}
    , {fee, Fee}]) ->
    Response = case HasResponse of
                   false -> undefined;
                   true -> Response0
               end,
    #query{ sender_id      = SenderId
          , sender_nonce   = SenderNonce
          , oracle_id      = OracleId
          , query          = Query
          , response       = Response
          , ttl            = TTL
          , response_ttl   = {delta, RespTTLValue}
          , fee            = Fee
          }.

serialization_template(?ORACLE_QUERY_VSN) ->
    [ {sender_id    , id}
    , {sender_nonce , int}
    , {oracle_id    , id}
    , {query        , binary}
    , {has_response , bool}
    , {response     , binary}
    , {ttl          , int}
    , {response_ttl , int}
    , {fee          , int}
    ].

serialization_type() -> ?ORACLE_QUERY_TYPE.

-spec serialize_for_client(query()) -> map().
serialize_for_client(#query{sender_id    = SenderId,
                            sender_nonce = SenderNonce,
                            oracle_id    = OracleId,
                            query        = Query,
                            ttl          = TTL,
                            fee          = Fee} = I) ->
    {delta, ResponseTtlValue} = response_ttl(I),
    Response =
        case response(I) of
            R when R =/= undefined -> R;
            undefined -> <<>>
        end,
    #{ <<"id">>           => aehttp_api_encoder:encode(oracle_query_id, id(I))
     , <<"sender_id">>    => aehttp_api_encoder:encode(id_hash, SenderId)
     , <<"sender_nonce">> => SenderNonce
     , <<"oracle_id">>    => aehttp_api_encoder:encode(id_hash, OracleId)
     , <<"query">>        => aehttp_api_encoder:encode(oracle_query, Query)
     , <<"response">>     => aehttp_api_encoder:encode(oracle_response, Response)
     , <<"ttl">>          => TTL
     , <<"response_ttl">> => #{ <<"type">>  => <<"delta">>
                              , <<"value">> => ResponseTtlValue
                              }
     , <<"fee">>          => Fee
     }.

%%%===================================================================
%%% Getters

-spec id(query()) -> id().
id(#query{} = Q) ->
    id(sender_pubkey(Q), sender_nonce(Q), oracle_pubkey(Q)).

-spec id(aec_keys:pubkey(), non_neg_integer(), aec_keys:pubkey()) -> id().
id(SenderPubkey, Nonce, OraclePubkey) ->
    Bin = <<SenderPubkey:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            OraclePubkey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec sender_id(query()) -> aec_id:id().
sender_id(#query{sender_id = SenderId}) ->
    SenderId.

-spec sender_pubkey(query()) -> aec_keys:pubkey().
sender_pubkey(#query{sender_id = SenderId}) ->
    aec_id:specialize(SenderId, account).

-spec sender_nonce(query()) -> integer().
sender_nonce(#query{sender_nonce = Nonce}) ->
    Nonce.

-spec oracle_id(query()) -> aec_id:id().
oracle_id(#query{oracle_id = OracleId}) ->
    OracleId.

-spec oracle_pubkey(query()) -> aec_keys:pubkey().
oracle_pubkey(#query{oracle_id = OracleId}) ->
    aec_id:specialize(OracleId, oracle).

-spec query(query()) -> oracle_query().
query(#query{query = Query}) ->
    Query.

-spec response(query()) -> oracle_response().
response(#query{response = Response}) ->
    Response.

-spec ttl(query()) -> aec_blocks:height().
ttl(#query{ttl = TTL}) ->
    TTL.

-spec response_ttl(query()) -> relative_ttl().
response_ttl(#query{response_ttl = ResponseTtl}) ->
    ResponseTtl.

-spec fee(query()) -> integer().
fee(#query{fee = Fee}) ->
    Fee.

%%%===================================================================
%%% Setters

-spec set_sender(aec_keys:pubkey(), query()) -> query().
set_sender(X, I) ->
    I#query{sender_id = aec_id:create(account, assert_field(sender, X))}.

-spec set_sender_nonce(integer(), query()) -> query().
set_sender_nonce(X, I) ->
    I#query{sender_nonce = assert_field(sender_nonce, X)}.

-spec set_oracle(aec_keys:pubkey(), query()) -> query().
set_oracle(X, I) ->
    I#query{oracle_id = aec_id:create(oracle, assert_field(oracle, X))}.

-spec set_query(oracle_query(), query()) -> query().
set_query(X, I) ->
    I#query{query = assert_field(query, X)}.

-spec set_response(oracle_response(), query()) -> query().
set_response(X, I) ->
    I#query{response = assert_field(response, X)}.

-spec set_ttl(aec_blocks:height(), query()) -> query().
set_ttl(X, I) ->
    I#query{ttl = assert_field(ttl, X)}.

-spec set_response_ttl(relative_ttl(), query()) -> query().
set_response_ttl(X, I) ->
    I#query{response_ttl = assert_field(response_ttl, X)}.

-spec set_fee(integer(), query()) -> query().
set_fee(X, I) ->
    I#query{fee = assert_field(fee, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {sender       , sender_pubkey(I)}
           , {sender_nonce , I#query.sender_nonce}
           , {oracle       , oracle_pubkey(I)}
           , {query        , I#query.query}
           , {response     , I#query.response}
           , {ttl          , I#query.ttl}
           , {response_ttl , I#query.response_ttl}
           , {fee          , I#query.fee}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(sender       , <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(sender_nonce , X) when is_integer(X), X >= 0 -> X;
assert_field(oracle       , <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(query        , X) when is_binary(X) -> X;
assert_field(response     , X) when X =:= 'undefined' -> X;
assert_field(response     , X) when is_binary(X) -> X;
assert_field(ttl          , X) when is_integer(X), X >= 0 -> X;
assert_field(response_ttl , {delta, I} = X) when is_integer(I), I > 0 -> X;
assert_field(fee          , X) when is_integer(X), X >= 0 -> X;
assert_field(Field,         X) -> error({illegal, Field, X}).
