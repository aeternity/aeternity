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
        , expires/1
        , fee/1
        , id/1
        , id/3
        , is_open/1
        , is_closed/1
        , new/4
        , oracle_address/1
        , query/1
        , response/1
        , response_ttl/1
        , sender_address/1
        , sender_nonce/1
        , serialize/1
        , serialize_for_client/1
        , set_expires/2
        , set_fee/2
        , set_oracle_address/2
        , set_query/2
        , set_response/2
        , set_response_ttl/2
        , set_sender_address/2
        , set_sender_nonce/2
        ]).

-define(ORACLE_QUERY_TYPE, oracle_query).
-define(ORACLE_QUERY_VSN, 1).

%%%===================================================================
%%% Types
%%%===================================================================

-type oracle_query()    :: aeo_oracles:response().
-type oracle_response() :: 'undefined' | aeo_oracles:response().
-type relative_ttl()    :: aeo_oracles:relative_ttl().

               %% TODO: consider renaming sender_address to sender
-record(query, { sender_address :: aec_keys:pubkey()
               , sender_nonce   :: integer()
               %% TODO: consider renaming oracle_address to id/oracle_id/oracle
               , oracle_address :: aec_keys:pubkey()
               , query          :: oracle_query()
               , response       :: oracle_response()
               , expires        :: aec_blocks:height()
               , response_ttl   :: relative_ttl()
               , fee            :: integer()
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

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeo_query_tx:tx(), aec_keys:pubkey(), aec_keys:pubkey(), aec_blocks:height()) -> query().
new(QTx, SenderPubkey, OraclePubkey, BlockHeight) ->
    Expires = aeo_utils:ttl_expiry(BlockHeight, aeo_query_tx:query_ttl(QTx)),
    I = #query{ sender_address = SenderPubkey
              , sender_nonce   = aeo_query_tx:nonce(QTx)
              , oracle_address = OraclePubkey
              , query          = aeo_query_tx:query(QTx)
              , response       = undefined
              , expires        = Expires
              , response_ttl   = aeo_query_tx:response_ttl(QTx)
              , fee            = aeo_query_tx:query_fee(QTx)
              },
    assert_fields(I).

-spec id(query()) -> id().
id(I) ->
    id(I#query.sender_address,
       I#query.sender_nonce,
       I#query.oracle_address).

-spec id(binary(), non_neg_integer(), binary()) -> binary().
id(SenderAddress, Nonce, OracleAddress) ->
    Bin = <<SenderAddress:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            OracleAddress:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec is_open(query()) -> boolean().
is_open(#query{response = undefined}) -> true;
is_open(#query{}) -> false.

-spec is_closed(query()) -> boolean().
%% @doc An query is closed if it is already answered.
is_closed(#query{response = undefined}) -> false;
is_closed(#query{}) -> true.

-spec add_response(aec_blocks:height(), oracle_response(), query()) -> query().
add_response(Height, Response, Q = #query{ response_ttl = RTTL }) ->
    NewExpires =  aeo_utils:ttl_expiry(Height, RTTL),
    Q#query{ response = assert_field(response, Response)
           , expires  = NewExpires }.

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
      [ {sender_address, sender_address(I)}
      , {sender_nonce, sender_nonce(I)}
      , {oracle_address, oracle_address(I)}
      , {query, query(I)}
      , {has_response, HasRresponse}
      , {response, Response}
      , {expires, expires(I)}
      , {response_ttl, RespTTLValue}
      , {fee, fee(I)}
      ]).

-spec deserialize(binary()) -> query().
deserialize(B) ->
    [ {sender_address, SenderAddress}
    , {sender_nonce, SenderNonce}
    , {oracle_address, OracleAddress}
    , {query, Query}
    , {has_response, HasResponse}
    , {response, Response0}
    , {expires, Expires}
    , {response_ttl, RespTTLValue}
    , {fee, Fee}
    ] = aec_object_serialization:deserialize(
          ?ORACLE_QUERY_TYPE, ?ORACLE_QUERY_VSN,
          serialization_template(?ORACLE_QUERY_VSN), B),

    Response = case HasResponse of
                   false -> undefined;
                   true -> Response0
               end,
    #query{ sender_address = SenderAddress
          , sender_nonce   = SenderNonce
          , oracle_address = OracleAddress
          , query          = Query
          , response       = Response
          , expires        = Expires
          , response_ttl   = {delta, RespTTLValue}
          , fee            = Fee
          }.

serialization_template(?ORACLE_QUERY_VSN) ->
    [ {sender_address , binary}
    , {sender_nonce   , int}
    , {oracle_address , binary}
    , {query          , binary}
    , {has_response   , bool}
    , {response       , binary}
    , {expires        , int}
    , {response_ttl   , int}
    , {fee            , int}
    ].

-spec serialize_for_client(query()) -> map().
serialize_for_client(#query{} = I) ->
    {delta, ResponseTtlValue} = response_ttl(I),
    Response = case response(I) of
                   undefined -> <<>>;
                   R -> R
               end,
    #{ <<"query_id">>     => aec_base58c:encode(oracle_query_id, id(I))
     , <<"sender">>       => aec_base58c:encode(account_pubkey, sender_address(I))
     , <<"sender_nonce">> => sender_nonce(I)
     , <<"oracle_id">>    => aec_base58c:encode(oracle_pubkey, oracle_address(I))
     , <<"query">>        => query(I)
     , <<"response">>     => Response
     , <<"expires">>      => expires(I)
     , <<"response_ttl">> => #{ <<"type">>  => <<"delta">>
                              , <<"value">> => ResponseTtlValue
                              }
     , <<"fee">>          => fee(I)
     }.

%%%===================================================================
%%% Getters

-spec sender_address(query()) -> aec_keys:pubkey().
sender_address(I) -> I#query.sender_address.

-spec sender_nonce(query()) -> integer().
sender_nonce(I) -> I#query.sender_nonce.

-spec oracle_address(query()) -> aec_keys:pubkey().
oracle_address(I) -> I#query.oracle_address.

-spec query(query()) -> oracle_query().
query(I) -> I#query.query.

-spec response(query()) -> oracle_response().
response(I) -> I#query.response.

-spec expires(query()) -> aec_blocks:height().
expires(I) -> I#query.expires.

-spec response_ttl(query()) -> relative_ttl().
response_ttl(I) -> I#query.response_ttl.

-spec fee(query()) -> integer().
fee(I) -> I#query.fee.

%%%===================================================================
%%% Setters

-spec set_sender_address(aec_keys:pubkey(), query()) -> query().
set_sender_address(X, I) ->
    I#query{sender_address = assert_field(sender_address, X)}.

-spec set_sender_nonce(integer(), query()) -> query().
set_sender_nonce(X, I) ->
    I#query{sender_nonce = assert_field(sender_nonce, X)}.

-spec set_oracle_address(aec_keys:pubkey(), query()) -> query().
set_oracle_address(X, I) ->
    I#query{oracle_address = assert_field(oracle_address, X)}.

-spec set_query(oracle_query(), query()) -> query().
set_query(X, I) ->
    I#query{query = assert_field(query, X)}.

-spec set_response(oracle_response(), query()) -> query().
set_response(X, I) ->
    I#query{response = assert_field(response, X)}.

-spec set_expires(aec_blocks:height(), query()) -> query().
set_expires(X, I) ->
    I#query{expires = assert_field(expires, X)}.

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
    List = [ {sender_address, I#query.sender_address}
           , {sender_nonce  , I#query.sender_nonce}
           , {oracle_address, I#query.oracle_address}
           , {query         , I#query.query}
           , {response      , I#query.response}
           , {expires       , I#query.expires}
           , {response_ttl  , I#query.response_ttl}
           , {fee           , I#query.fee}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(sender_address, <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(sender_nonce  , X) when is_integer(X), X >= 0 -> X;
assert_field(oracle_address, <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(query         , X) when is_binary(X) -> X;
assert_field(response      , X) when X =:= 'undefined' -> X;
assert_field(response      , X) when is_binary(X) -> X;
assert_field(expires       , X) when is_integer(X), X >= 0 -> X;
assert_field(response_ttl  , {delta, I} = X) when is_integer(I), I > 0 -> X;
assert_field(fee           , X) when is_integer(X), X >= 0 -> X;
assert_field(Field,          X) -> error({illegal, Field, X}).
