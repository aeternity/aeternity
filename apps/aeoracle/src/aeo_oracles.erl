%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for oracle objects
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_oracles).

%% API
-export([ deserialize/2
        , deserialize_from_fields/3
        , serialization_type/0
        , serialization_template/1
        , ttl/1
        , vm_version/1
        , id/1
        , pubkey/1
        , new/6
        , query_fee/1
        , query_format/1
        , response_format/1
        , serialize/1
        , serialize_for_client/1
        , set_ttl/2
        , set_pubkey/2
        , set_query_fee/2
        , set_query_format/2
        , set_response_format/2
        , set_vm_version/2
        ]).

-ifdef(TEST).
-export([new/2]).
-endif.


%%%===================================================================
%%% Types
%%%===================================================================

-type amount()       :: integer().

-type relative_ttl() :: {delta, non_neg_integer()}.
-type fixed_ttl()    :: {block, aec_blocks:height()}.
-type ttl()          :: relative_ttl() | fixed_ttl().

-type type_format()  :: binary().

-type query()    :: binary().
-type response() :: binary().

-record(oracle, { id              :: aec_id:id()
                , query_format    :: type_format()
                , response_format :: type_format()
                , query_fee       :: amount()
                , ttl             :: aec_blocks:height()
                , vm_version      :: non_neg_integer()
                }).


-opaque oracle() :: #oracle{}.

-type id() :: aec_id:id().
-type pubkey() :: aec_keys:pubkey().
-type serialized() :: binary().

-export_type([ fixed_ttl/0
             , id/0
             , pubkey/0
             , oracle/0
             , query/0
             , response/0
             , relative_ttl/0
             , serialized/0
             , ttl/0
             , type_format/0
             ]).

-define(PUB_SIZE, 32).
-define(HASH_SIZE, 32).
-define(ORACLE_TYPE, oracle).
-define(ORACLE_VSN, 1).

-behavior(aeo_cache).

%%%===================================================================
%%% API
%%%===================================================================


-ifdef(TEST).
-spec new(aeo_register_tx:tx(), aec_blocks:height()) -> oracle().
new(RTx, BlockHeight) ->
    TxTTL = aeo_register_tx:oracle_ttl(RTx),
    AbsoluteTTL = aeo_utils:ttl_expiry(BlockHeight, TxTTL),
    new(aeo_register_tx:account_pubkey(RTx),
        aeo_register_tx:query_format(RTx),
        aeo_register_tx:response_format(RTx),
        aeo_register_tx:query_fee(RTx),
        AbsoluteTTL,
        aeo_register_tx:vm_version(RTx)).
-endif.

-spec new(aec_keys:pubkey(), type_format(), type_format(),
          non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
             oracle().
new(AccountPubkey, QueryFormat, ResponseFormat,
    QueryFee, AbsoluteTTL, VMVersion) ->
    O = #oracle{ id              = aec_id:create(oracle, AccountPubkey)
               , query_format    = QueryFormat
               , response_format = ResponseFormat
               , query_fee       = QueryFee
               , ttl             = AbsoluteTTL
               , vm_version      = VMVersion
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
      , {ttl, ttl(O)}
      , {vm_version, vm_version(O)}
      ]).

-spec deserialize(aec_keys:pubkey(), binary()) -> oracle().
deserialize(Pubkey, Bin) ->
    Fields = aec_object_serialization:deserialize(
                  ?ORACLE_TYPE,
                  ?ORACLE_VSN,
                  serialization_template(?ORACLE_VSN),
                  Bin
                ),
    deserialize_from_fields(?ORACLE_VSN, Pubkey, Fields).

deserialize_from_fields(?ORACLE_VSN, Pubkey,
      [ {query_format, QueryFormat}
      , {response_format, ResponseFormat}
      , {query_fee, QueryFee}
      , {ttl, TTL}
      , {vm_version, VMVersion}
      ]) ->
    #oracle{ id              = aec_id:create(oracle, Pubkey)
           , query_format    = QueryFormat
           , response_format = ResponseFormat
           , query_fee       = QueryFee
           , ttl             = TTL
           , vm_version      = VMVersion
           }.

serialization_template(?ORACLE_VSN) ->
    [ {query_format, binary}
    , {response_format, binary}
    , {query_fee, int}
    , {ttl, int}
    , {vm_version, int}
    ].

serialization_type() -> ?ORACLE_TYPE.

-spec serialize_for_client(oracle()) -> map().
serialize_for_client(#oracle{id              = Id,
                             query_format    = QueryFormat,
                             response_format = ResponseFormat,
                             query_fee       = QueryFee,
                             ttl             = TTL,
                             vm_version      = VMVersion
                            }) ->
    #{ <<"id">>              => aehttp_api_encoder:encode(id_hash, Id)
     , <<"query_format">>    => QueryFormat
     , <<"response_format">> => ResponseFormat
     , <<"query_fee">>       => QueryFee
     , <<"ttl">>             => TTL
     , <<"vm_version">>      => VMVersion
     }.

%%%===================================================================
%%% Getters

-spec id(oracle()) -> aec_id:id().
id(#oracle{id = Id}) ->
    Id.

-spec pubkey(oracle()) -> aec_keys:pubkey().
pubkey(#oracle{id = Id}) ->
    aec_id:specialize(Id, oracle).

-spec query_format(oracle()) -> type_format().
query_format(#oracle{query_format = QueryFormat}) ->
   QueryFormat.

-spec response_format(oracle()) -> type_format().
response_format(#oracle{response_format = ResponseFormat}) ->
    ResponseFormat.

-spec query_fee(oracle()) -> amount().
query_fee(#oracle{query_fee = QueryFee}) ->
    QueryFee.

-spec ttl(oracle()) -> aec_blocks:height().
ttl(#oracle{ttl = TTL}) ->
    TTL.

-spec vm_version(oracle()) -> non_neg_integer().
vm_version(#oracle{vm_version = VMVersion}) ->
    VMVersion.

%%%===================================================================
%%% Setters

-spec set_pubkey(aec_keys:pubkey(), oracle()) -> oracle().
set_pubkey(X, O) ->
    O#oracle{id = aec_id:create(oracle, assert_field(pubkey, X))}.

-spec set_query_format(type_format(), oracle()) -> oracle().
set_query_format(X, O) ->
    O#oracle{query_format = assert_field(query_format, X)}.

-spec set_response_format(type_format(), oracle()) -> oracle().
set_response_format(X, O) ->
    O#oracle{response_format = assert_field(response_format, X)}.

-spec set_query_fee(amount(), oracle()) -> oracle().
set_query_fee(X, O) ->
    O#oracle{query_fee = assert_field(query_fee, X)}.

-spec set_ttl(aec_blocks:height(), oracle()) -> oracle().
set_ttl(X, O) ->
    O#oracle{ttl = assert_field(ttl, X)}.

-spec set_vm_version(non_neg_integer(), oracle()) -> oracle().
set_vm_version(X, O) ->
    O#oracle{vm_version = assert_field(vm_version, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(O) ->
    List = [ {pubkey         , pubkey(O)}
           , {query_format   , O#oracle.query_format}
           , {response_format, O#oracle.response_format}
           , {query_fee      , O#oracle.query_fee}
           , {ttl            , O#oracle.ttl}
           , {vm_version     , O#oracle.vm_version}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> O;
        Other -> error({missing, Other})
    end.

assert_field(pubkey         , <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(query_format   , X) when is_binary(X) -> X;
assert_field(response_format, X) when is_binary(X) -> X;
assert_field(query_fee      , X) when is_integer(X), X >= 0 -> X;
assert_field(ttl            , X) when is_integer(X), X >= 0 -> X;
assert_field(vm_version     , X) when is_integer(X), X >= 0 -> X;
assert_field(Field          , X) -> error({illegal, Field, X}).
