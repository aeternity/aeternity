%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract call objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call).

%% API
-export([ deserialize/1
        , fee/1
        , id/1
        , new/2
        , contract_address/1
        , sender_address/1
        , sender_nonce/1
        , serialize/1
        , set_fee/2
        , set_contract_address/2
        , set_sender_address/2
        , set_sender_nonce/2
        ]).

-include_lib("apps/aecore/include/common.hrl").

-define(CONTRACT_INTERACTION_TYPE, <<"contract_i">>).
-define(CONTRACT_INTERACTION_VSN, 1).

%%%===================================================================
%%% Types
%%%===================================================================

-record(call, { sender_address   :: pubkey()
              , sender_nonce     :: integer()
              , contract_address :: pubkey()
              , fee              :: integer()
              }).

-opaque call() :: #call{}.
-type id() :: binary().
-type serialized() :: binary().

-export_type([ id/0
             , call/0
             , serialized/0
             ]).

-define(PUB_SIZE, 65).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aect_call_tx:call_tx(), height()) -> call().
new(CallTx, _BlockHeight) ->
    C = #call{ sender_address   = aect_call_tx:sender(CallTx)
             , sender_nonce     = aect_call_tx:nonce(CallTx)
             , contract_address = aect_call_tx:contract(CallTx)
             , fee              = aect_call_tx:fee(CallTx)
             },
    assert_fields(C).

-spec id(call()) -> id().
id(I) ->
    Bin = <<(I#call.sender_address):?PUB_SIZE/binary,
            (I#call.sender_nonce):?NONCE_SIZE,
            (I#call.contract_address):?PUB_SIZE/binary>>,
    aec_sha256:hash(Bin).

-spec serialize(call()) -> binary().
serialize(#call{} = I) ->
    msgpack:pack([ #{<<"type">>             => ?CONTRACT_INTERACTION_TYPE}
                 , #{<<"vsn">>              => ?CONTRACT_INTERACTION_VSN}
                 , #{<<"sender_address">>   => sender_address(I)}
                 , #{<<"sender_nonce">>     => sender_nonce(I)}
                 , #{<<"contract_address">> => contract_address(I)}
                 , #{<<"fee">>              => fee(I)}
                 ]).

-spec deserialize(binary()) -> call().
deserialize(B) ->
    {ok, List} = msgpack:unpack(B),
    [ #{<<"type">>             := ?CONTRACT_INTERACTION_TYPE}
    , #{<<"vsn">>              := ?CONTRACT_INTERACTION_VSN}
    , #{<<"sender_address">>   := SenderAddress}
    , #{<<"sender_nonce">>     := SenderNonce}
    , #{<<"contract_address">> := ContractAddress}
    , #{<<"fee">>              := Fee}
    ] = List,
    #call{ sender_address   = SenderAddress
         , sender_nonce     = SenderNonce
         , contract_address = ContractAddress
         , fee              = Fee
         }.


%%%===================================================================
%%% Getters

-spec sender_address(call()) -> pubkey().
sender_address(I) -> I#call.sender_address.

-spec sender_nonce(call()) -> integer().
sender_nonce(I) -> I#call.sender_nonce.

-spec contract_address(call()) -> pubkey().
contract_address(I) -> I#call.contract_address.

-spec fee(call()) -> integer().
fee(I) -> I#call.fee.

%%%===================================================================
%%% Setters

-spec set_sender_address(pubkey(), call()) -> call().
set_sender_address(X, I) ->
    I#call{sender_address = assert_field(sender_address, X)}.

-spec set_sender_nonce(integer(), call()) -> call().
set_sender_nonce(X, I) ->
    I#call{sender_nonce = assert_field(sender_nonce, X)}.

-spec set_contract_address(pubkey(), call()) -> call().
set_contract_address(X, I) ->
    I#call{contract_address = assert_field(contract_address, X)}.

-spec set_fee(integer(), call()) -> call().
set_fee(X, I) ->
    I#call{fee = assert_field(fee, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {sender_address,   I#call.sender_address}
           , {sender_nonce  ,   I#call.sender_nonce}
           , {contract_address, I#call.contract_address}
           , {fee           ,   I#call.fee}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(sender_address,   <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(sender_nonce,     X) when is_integer(X), X >= 0 -> X;
assert_field(contract_address, <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(fee,              X) when is_integer(X), X >= 0 -> X;
assert_field(Field,            X) -> error({illegal, Field, X}).
