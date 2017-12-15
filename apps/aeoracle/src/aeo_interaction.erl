%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for oracle interaction objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aeo_interaction).

%% API
-export([ deserialize/1
        , expires/1
        , fee/1
        , id/1
        , new/2
        , oracle_address/1
        , response/1
        , response_ttl/1
        , sender_address/1
        , sender_nonce/1
        , serialize/1
        , set_expires/2
        , set_fee/2
        , set_oracle_address/2
        , set_response/2
        , set_response_ttl/2
        , set_sender_address/2
        , set_sender_nonce/2
        ]).

-include_lib("apps/aecore/include/common.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type block_height()    :: integer().
-type oracle_response() :: aeo_oracles:response().
-type relative_ttl()    :: aeo_oracles:relative_ttl().

-record(interaction, { sender_address :: pubkey()
                     , sender_nonce   :: integer()
                     , oracle_address :: pubkey()
                     , response       :: oracle_response() | 'undefined'
                     , expires        :: block_height()
                     , response_ttl   :: relative_ttl()
                     , fee            :: integer()
                     }).

-opaque interaction() :: #interaction{}.

-export_type([ interaction/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeo_query_tx:query_tx(), height()) -> interaction().
new(QTx, BlockHeight) ->
    Expires = aeo_utils:ttl_expiry(BlockHeight, aeo_query_tx:query_ttl(QTx)),
    I = #interaction{ sender_address = aeo_query_tx:sender(QTx)
                    , sender_nonce   = aeo_query_tx:nonce(QTx)
                    , oracle_address = aeo_query_tx:oracle(QTx)
                    , response       = undefined
                    , expires        = Expires
                    , response_ttl   = aeo_query_tx:response_ttl(QTx)
                    , fee            = aeo_query_tx:query_fee(QTx)
                    },
    assert_fields(I).

-spec id(interaction()) -> binary().
id(I) ->
    Bin = <<(I#interaction.sender_address):32/binary,
            (I#interaction.sender_nonce):256,
            (I#interaction.oracle_address):32/binary>>,
    aec_sha256:hash(Bin).

-spec serialize(interaction()) -> binary().
serialize(#interaction{} = I) ->
    term_to_binary(I).

-spec deserialize(binary()) -> interaction().
deserialize(B) ->
    try binary_to_term(B) of
        #interaction{} = I -> I;
        Other -> error({not_interaction, Other})
    catch _:_ -> error(invalid_binary)
    end.

%%%===================================================================
%%% Getters

-spec sender_address(interaction()) -> pubkey().
sender_address(I) -> I#interaction.sender_address.

-spec sender_nonce(interaction()) -> integer().
sender_nonce(I) -> I#interaction.sender_nonce.

-spec oracle_address(interaction()) -> pubkey().
oracle_address(I) -> I#interaction.oracle_address.

-spec response(interaction()) -> oracle_response().
response(I) -> I#interaction.response.

-spec expires(interaction()) -> block_height().
expires(I) -> I#interaction.expires.

-spec response_ttl(interaction()) -> relative_ttl().
response_ttl(I) -> I#interaction.response_ttl.

-spec fee(interaction()) -> integer().
fee(I) -> I#interaction.fee.

%%%===================================================================
%%% Setters

-spec set_sender_address(pubkey(), interaction()) -> interaction().
set_sender_address(X, I) ->
    I#interaction{sender_address = assert_field(sender_address, X)}.

-spec set_sender_nonce(integer(), interaction()) -> interaction().
set_sender_nonce(X, I) ->
    I#interaction{sender_nonce = assert_field(sender_nonce, X)}.

-spec set_oracle_address(pubkey(), interaction()) -> interaction().
set_oracle_address(X, I) ->
    I#interaction{oracle_address = assert_field(oracle_address, X)}.

-spec set_response(oracle_response(), interaction()) -> interaction().
set_response(X, I) ->
    I#interaction{response = assert_field(response, X)}.

-spec set_expires(block_height(), interaction()) -> interaction().
set_expires(X, I) ->
    I#interaction{expires = assert_field(expires, X)}.

-spec set_response_ttl(relative_ttl(), interaction()) -> interaction().
set_response_ttl(X, I) ->
    I#interaction{response_ttl = assert_field(response_ttl, X)}.

-spec set_fee(integer(), interaction()) -> interaction().
set_fee(X, I) ->
    I#interaction{fee = assert_field(fee, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {sender_address, I#interaction.sender_address}
           , {sender_nonce  , I#interaction.sender_nonce}
           , {oracle_address, I#interaction.oracle_address}
           , {response      , I#interaction.response}
           , {expires       , I#interaction.expires}
           , {response_ttl  , I#interaction.response_ttl}
           , {fee           , I#interaction.fee}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(sender_address, X) when is_binary(X), byte_size(X) =:= 32 -> X;
assert_field(sender_nonce  , X) when is_integer(X), X >= 0 -> X;
assert_field(oracle_address, X) when is_binary(X), byte_size(X) =:= 32 -> X;
assert_field(response      , X) when is_list(X); 'undefined' -> X;
assert_field(expires       , X) when is_integer(X), X >= 0 -> X;
assert_field(response_ttl  , {delta, I} = X) when is_integer(I), I > 0 -> X;
assert_field(fee           , X) when is_integer(X), X >= 0 -> X;
assert_field(Field,          X) -> error({illegal, Field, X}).
