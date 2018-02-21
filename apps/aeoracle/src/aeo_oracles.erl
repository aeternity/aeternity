%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for oracle objects
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_oracles).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([ deserialize/1
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
-type fixed_ttl()    :: {block, height()}.
-type ttl()          :: relative_ttl() | fixed_ttl().

-type type_spec()    :: binary(). %% Utf8 encoded string

-type query()    :: binary().             %% Don't use native types for queries
-type response() :: binary(). %% Don't use native types for responses

-record(oracle, { owner           :: pubkey()
                , query_format    :: type_spec()
                , response_format :: type_spec()
                , query_fee       :: amount()
                , expires         :: height()
                }).


-opaque oracle() :: #oracle{}.

-type id() :: pubkey().
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

-define(PUB_SIZE, 65).
-define(HASH_SIZE, 32).
-define(ORACLE_TYPE, <<"oracle">>).
-define(ORACLE_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec id(oracle()) -> pubkey().
id(O) ->
  owner(O).

-spec new(aeo_register_tx:tx(), height()) -> oracle().
new(RTx, BlockHeight) ->
    Expires = aeo_utils:ttl_expiry(BlockHeight, aeo_register_tx:ttl(RTx)),
    O = #oracle{ owner = aeo_register_tx:account(RTx)
               , query_format = aeo_register_tx:query_spec(RTx)
               , response_format = aeo_register_tx:response_spec(RTx)
               , query_fee = aeo_register_tx:query_fee(RTx)
               , expires = Expires
               },
    assert_fields(O).

-spec serialize(oracle()) -> binary().
serialize(#oracle{} = O) ->
    msgpack:pack([ #{<<"type">>            => ?ORACLE_TYPE}
                 , #{<<"vsn">>             => ?ORACLE_VSN}
                 , #{<<"owner">>           => owner(O)}
                 , #{<<"query_format">>    => query_format(O)}
                 , #{<<"response_format">> => response_format(O)}
                 , #{<<"query_fee">>       => query_fee(O)}
                 , #{<<"expires">>         => expires(O)}
                 ]).

-spec deserialize(binary()) -> oracle().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [ #{<<"type">>            := ?ORACLE_TYPE}
    , #{<<"vsn">>             := ?ORACLE_VSN}
    , #{<<"owner">>           := Owner}
    , #{<<"query_format">>    := QueryFormat}
    , #{<<"response_format">> := ResponseFormat}
    , #{<<"query_fee">>       := QueryFee}
    , #{<<"expires">>         := Expires}
    ] = List,
    #oracle{ owner           = Owner
           , query_format    = QueryFormat
           , response_format = ResponseFormat
           , query_fee       = QueryFee
           , expires         = Expires
           }.

%%%===================================================================
%%% Getters

-spec owner(oracle()) -> pubkey().
owner(O) -> O#oracle.owner.

-spec query_format(oracle()) -> type_spec().
query_format(O) -> O#oracle.query_format.

-spec response_format(oracle()) -> type_spec().
response_format(O) -> O#oracle.response_format.

-spec query_fee(oracle()) -> amount().
query_fee(O) -> O#oracle.query_fee.

-spec expires(oracle()) -> height().
expires(O) -> O#oracle.expires.

%%%===================================================================
%%% Setters

-spec set_owner(pubkey(), oracle()) -> oracle().
set_owner(X, O) ->
    O#oracle{owner = assert_field(owner, X)}.

-spec set_query_format(type_spec(), oracle()) -> oracle().
set_query_format(X, O) ->
    O#oracle{query_format = assert_field(query_format, X)}.

-spec set_response_format(type_spec(), oracle()) -> oracle().
set_response_format(X, O) ->
    O#oracle{response_format = assert_field(response_format, X)}.

-spec set_query_fee(amount(), oracle()) -> oracle().
set_query_fee(X, O) ->
    O#oracle{query_fee = assert_field(query_fee, X)}.

-spec set_expires(height(), oracle()) -> oracle().
set_expires(X, O) ->
    O#oracle{expires = assert_field(expires, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(O) ->
    List = [ {owner          , O#oracle.owner}
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
