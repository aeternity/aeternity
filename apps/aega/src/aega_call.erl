%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%% ADT for generalized account call objects.
%%      These need no binary serialization, since they do not occur on chain
%%% @end
%%%-------------------------------------------------------------------

-module(aega_call).

%% API
-export([ new/7
        , contract_id/1
        , contract_pubkey/1
        , caller_id/1
        , caller_pubkey/1
        , height/1
        , return_type/1
        , return_value/1
        , gas_price/1
        , gas_used/1
        , inner/1
        , serialize_for_client/1
        , set_inner/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================


-type call_object() :: atom() |
                       {ga_meta_tx, aega_call:call()} |
                       {contract_create_tx, aect_call:call()} |
                       {contract_call_tx, aect_call:call()}.

-record(call, { id           :: aeser_id:id()    %% not used.... shall we use call id?
              , caller_id    :: aeser_id:id()    %% generalized account
              , height       :: aec_blocks:height()
              , contract_id  :: aeser_id:id()
              , gas_price    :: amount()
              , gas_used     :: amount()
              , return_value :: binary()
              , return_type  :: ok | error
              , inner_call   :: call_object()
              }).

-opaque call() :: #call{}.
-type amount() :: aect_contracts:amount().

-export_type([ call/0]).

-define(PUB_SIZE, 32).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeser_id:id(), aec_keys:pubkey(), aec_blocks:height(), amount(), amount(), ok | error, binary()) -> call().
new(GAId, Id, BlockHeight, GasPrice, GasUsed, ReturnType, ReturnValue) ->
    C = #call{ id           = aeser_id:create(account, Id)
             , caller_id    = GAId
             , height       = BlockHeight
             , contract_id  = aeser_id:create(contract, Id)  %% possibly not interesting
             , gas_price    = GasPrice
             , gas_used     = GasUsed
             , return_value = ReturnValue
             , return_type  = ReturnType
             , inner_call   = undefined
             },
    assert_fields(C).

-spec serialize_for_client(call()) -> map().
serialize_for_client(#call{caller_id    = CallerId,
                           height       = Height,
                           contract_id  = _ContractId,
                           gas_price    = GasPrice,
                           gas_used     = GasUsed,
                           return_value = ReturnValue,
                           return_type  = ReturnType,
                           inner_call   = Inner
                          }) ->
    #{ <<"caller_id">>    => aeser_api_encoder:encode(id_hash, CallerId)
     , <<"height">>       => Height
     , <<"gas_price">>    => GasPrice
     , <<"gas_used">>     => GasUsed
     , <<"return_value">> => aeser_api_encoder:encode(contract_bytearray, ReturnValue)
     , <<"return_type">>  => atom_to_binary(ReturnType, utf8)
     , <<"inner_object">>  =>
           case Inner of
               Atom when is_atom(Atom) -> #{<<"tx_info">> => atom_to_binary(Atom, utf8)};
               {contract_call_tx, Call} -> #{<<"call_info">> => aect_call:serialize_for_client(Call)};
               {contract_create_tx, Call} -> #{<<"call_info">> => aect_call:serialize_for_client(Call)};
               {ga_meta_tx, Call} -> #{<<"ga_info">> => serialize_for_client(Call)}
           end
     }.


%%%===================================================================
%%% Getters

-spec caller_id(call()) -> aeser_id:id().
caller_id(#call{caller_id = CallerId}) ->
    CallerId.

-spec caller_pubkey(call()) -> aec_keys:pubkey().
caller_pubkey(#call{caller_id = CallerId}) ->
    {_, CallerPubkey} = aeser_id:specialize(CallerId),
    CallerPubkey.

-spec height(call()) -> aec_blocks:height().
height(#call{height = Height}) ->
    Height.

-spec contract_id(call()) -> aeser_id:id().
contract_id(#call{contract_id = ContractId}) ->
    ContractId.

-spec contract_pubkey(call()) -> aec_keys:pubkey().
contract_pubkey(#call{contract_id = ContractId}) ->
  aeser_id:specialize(ContractId, contract).

-spec return_type(call()) -> ok | error | revert.
return_type(#call{return_type = ReturnType}) ->
    ReturnType.

-spec return_value(call()) -> binary().
return_value(#call{return_value = ReturnValue}) ->
    ReturnValue.

-spec gas_price(call()) -> amount().
gas_price(#call{gas_price = GasPrice}) ->
    GasPrice.

-spec gas_used(call()) -> amount().
gas_used(#call{gas_used = GasUsed}) ->
    GasUsed.

-spec inner(call()) -> call_object().
inner(I) ->
    I#call.inner_call.

%%%===================================================================
%%% Setters

-spec set_inner(call_object(), call()) -> call().
set_inner(InnerCall, Call) ->
    Call#call{inner_call = assert_field(inner_call, InnerCall)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {caller,       caller_pubkey(I)}
           , {height,       I#call.height}
           , {contract,     contract_pubkey(I)}
           , {return_value, I#call.return_value}
           , {gas_price,    I#call.gas_price}
           , {gas_used,     I#call.gas_used}
           , {inner_call,   I#call.inner_call}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(caller,           <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(height,           X) when is_integer(X), X > 0 -> X;
assert_field(contract,         <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(return_value,     X) when is_binary(X) -> X;
assert_field(gas_price,        X) when is_integer(X), X >= 0 -> X;
assert_field(gas_used,         X) when is_integer(X), X >= 0 -> X;
assert_field(inner_call,       X) when is_atom(X) -> X;
assert_field(inner_call,       {contract_call_tx, X})-> {contract_call_tx, aect_call:assert_fields(X)};
assert_field(inner_call,       {contract_create_tx, X})-> {contract_create_tx, aect_call:assert_fields(X)};
assert_field(inner_call,       {ga_meta_tx, X})-> {ga_meta_tx, assert_fields(X)};
assert_field(Field,            X) -> error({illegal, Field, X}).
