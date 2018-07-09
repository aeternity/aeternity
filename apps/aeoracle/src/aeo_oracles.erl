%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for oracle objects
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_oracles).

%% API
-export([ deserialize/2
        , expires/1
        , id/1
        , new/2
        , owner/1
        , query_fee/1
        , query_format/1
        , response_format/1
        , serialize/1
        , set_expires/2
        , set_owner/2
        , set_query_fee/2
        , set_query_format/2
        , set_response_format/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type amount()       :: integer().

-type relative_ttl() :: {delta, non_neg_integer()}.
-type fixed_ttl()    :: {block, aec_blocks:height()}.
-type ttl()          :: relative_ttl() | fixed_ttl().

-type type_spec()    :: binary(). %% Utf8 encoded string

-type query()    :: binary().             %% Don't use native types for queries
-type response() :: binary(). %% Don't use native types for responses

-record(oracle, { id              :: aec_id:id()
                , query_format    :: type_spec()
                , response_format :: type_spec()
                , query_fee       :: amount()
                , expires         :: aec_blocks:height()
                }).


-opaque oracle() :: #oracle{}.

-type id() :: aec_keys:pubkey().
-type serialized() :: binary().

-export_type([ fixed_ttl/0
             , id/0
             , oracle/0
             , query/0
             , response/0
             , relative_ttl/0
             , serialized/0
             , ttl/0
             , type_spec/0
             ]).

-define(PUB_SIZE, 32).
-define(HASH_SIZE, 32).
-define(ORACLE_TYPE, oracle).
-define(ORACLE_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(oracle()) -> aec_keys:pubkey().
id(O) ->
  owner(O).

-spec new(aeo_register_tx:tx(), aec_blocks:height()) -> oracle().
new(RTx, BlockHeight) ->
    Expires = aeo_utils:ttl_expiry(BlockHeight, aeo_register_tx:oracle_ttl(RTx)),
    Pubkey = aec_id:specialize(aeo_register_tx:account(RTx), account),
    O = #oracle{ id = aec_id:create(oracle, Pubkey)
               , query_format = aeo_register_tx:query_spec(RTx)
               , response_format = aeo_register_tx:response_spec(RTx)
               , query_fee = aeo_register_tx:query_fee(RTx)
               , expires = Expires
               },
    assert_fields(O).

-spec serialize(oracle()) -> binary().
serialize(#oracle{} = O) ->
    aec_object_serialization:serialize(
      ?ORACLE_TYPE,
      ?ORACLE_VSN,
      serialization_template(?ORACLE_VSN),
      [ {query_format, query_format(O)}
      , {response_format, response_format(O)}
      , {query_fee, query_fee(O)}
      , {expires, expires(O)}
      ]).

-spec deserialize(aec_keys:pubkey(), binary()) -> oracle().
deserialize(Owner, Bin) ->
      [ {query_format, QueryFormat}
      , {response_format, ResponseFormat}
      , {query_fee, QueryFee}
      , {expires, Expires}
      ] =
        aec_object_serialization:deserialize(
          ?ORACLE_TYPE,
          ?ORACLE_VSN,
          serialization_template(?ORACLE_VSN),
          Bin
         ),
    #oracle{ id              = aec_id:create(oracle, Owner)
           , query_format    = QueryFormat
           , response_format = ResponseFormat
           , query_fee       = QueryFee
           , expires         = Expires
           }.

serialization_template(?ORACLE_VSN) ->
    [ {query_format, binary}
    , {response_format, binary}
    , {query_fee, int}
    , {expires, int}
    ].


%%%===================================================================
%%% Getters

-spec owner(oracle()) -> aec_keys:pubkey().
owner(O) -> aec_id:specialize(O#oracle.id, oracle).

-spec query_format(oracle()) -> type_spec().
query_format(O) -> O#oracle.query_format.

-spec response_format(oracle()) -> type_spec().
response_format(O) -> O#oracle.response_format.

-spec query_fee(oracle()) -> amount().
query_fee(O) -> O#oracle.query_fee.

-spec expires(oracle()) -> aec_blocks:height().
expires(O) -> O#oracle.expires.

%%%===================================================================
%%% Setters

-spec set_owner(aec_keys:pubkey(), oracle()) -> oracle().
set_owner(X, O) ->
    O#oracle{id = aec_id:create(oracle, assert_field(owner, X))}.

-spec set_query_format(type_spec(), oracle()) -> oracle().
set_query_format(X, O) ->
    O#oracle{query_format = assert_field(query_format, X)}.

-spec set_response_format(type_spec(), oracle()) -> oracle().
set_response_format(X, O) ->
    O#oracle{response_format = assert_field(response_format, X)}.

-spec set_query_fee(amount(), oracle()) -> oracle().
set_query_fee(X, O) ->
    O#oracle{query_fee = assert_field(query_fee, X)}.

-spec set_expires(aec_blocks:height(), oracle()) -> oracle().
set_expires(X, O) ->
    O#oracle{expires = assert_field(expires, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(O) ->
    List = [ {owner          , owner(O)}
           , {query_format   , O#oracle.query_format}
           , {response_format, O#oracle.response_format}
           , {query_fee      , O#oracle.query_fee}
           , {expires        , O#oracle.expires}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> O;
        Other -> error({missing, Other})
    end.

assert_field(owner          , <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(query_format   , X) when is_binary(X) -> X;
assert_field(response_format, X) when is_binary(X) -> X;
assert_field(query_fee      , X) when is_integer(X), X >= 0 -> X;
assert_field(expires        , X) when is_integer(X), X >= 0 -> X;
assert_field(Field          , X) -> error({illegal, Field, X}).
