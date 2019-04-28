%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract call objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call).

%% API
-export([ deserialize/2
        , id/1
        , ga_id/2
        , id/3
        , new/5
        , contract_id/1
        , contract_pubkey/1
        , caller_id/1
        , caller_pubkey/1
        , caller_nonce/1
        , height/1
        , return_type/1
        , return_value/1
        , gas_price/1
        , gas_used/1
        , log/1
        , serialize/1
        , serialize_for_client/1
        , set_contract/2
        , set_caller/3
        , set_caller_nonce/2
        , set_id/2
        , set_height/2
        , set_return_type/2
        , set_return_value/2
        , set_gas_used/2
        , set_log/2
        ]).

-export([assert_fields/1]).  %% for aega_call asserting an inner tx

-define(CONTRACT_INTERACTION_TYPE, contract_call).
-define(CONTRACT_INTERACTION_VSN, 2).

%%%===================================================================
%%% Types
%%%===================================================================
-type log_entry() ::  { binary()    %% 256 bit account address
                      , [binary()]  %% topics
                      , binary()}.  %% data

-record(call, { id           :: id()
              , caller_id    :: aeser_id:id()
              , caller_nonce :: integer()
              , height       :: aec_blocks:height()
              , contract_id  :: aeser_id:id()
              , gas_price    :: amount()
              , gas_used     :: amount()
              , return_value :: binary()
              , return_type  :: ok | error | revert
              , log          :: [log_entry()]
              }).

-opaque call() :: #call{}.
-type id() :: binary().
-type serialized() :: binary().
-type amount() :: aect_contracts:amount().

-export_type([ id/0
             , call/0
             , serialized/0
             ]).

-define(PUB_SIZE, 32).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(aeser_id:id(), non_neg_integer(), aeser_id:id(), aec_blocks:height(), amount()) -> call().
new(CallerId, Nonce, ContractId, BlockHeight, GasPrice) ->
    {_, CallerPubkey} = aeser_id:specialize(CallerId),
    {_, ContractPubkey} = aeser_id:specialize(ContractId),
    C = #call{ id           = id(CallerPubkey, Nonce, ContractPubkey)
             , caller_id    = CallerId
             , caller_nonce = Nonce
             , height       = BlockHeight
             , contract_id  = ContractId
             , gas_price    = GasPrice
             , gas_used     = 0     %% These are filled later
             , return_value = <<>>  %% in aect_call_tx:process()
             , return_type  = ok
             , log          = []
             },
    assert_fields(C).

-spec id(call()) -> id().
id(#call{id = Id}) ->
    Id.

-spec ga_id(id(), aec_keys:pubkey()) -> id().
ga_id(<<_:?PUB_SIZE/unit:8>> = GANonce, <<_:?PUB_SIZE/unit:8>> = CtPubkey) ->
    aec_hash:hash(pubkey, <<GANonce/binary, CtPubkey/binary>>).


-spec id(aec_keys:pubkey(), non_neg_integer(), aec_keys:pubkey()) -> id().
id(CallerPubkey, CallerNonce, ContractPubkey) ->
    Bin = <<CallerPubkey:?PUB_SIZE/binary,
            CallerNonce:?NONCE_SIZE,
            ContractPubkey:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec serialize(call()) -> binary().
serialize(#call{caller_id    = CallerId,
                caller_nonce = CallerNonce,
                height       = Height,
                contract_id  = ContractId,
                gas_price    = GasPrice,
                gas_used     = GasUsed,
                return_value = ReturnValue,
                return_type  = ReturnType,
                log          = Log
               }) ->
    aeser_chain_objects:serialize(
      ?CONTRACT_INTERACTION_TYPE,
      ?CONTRACT_INTERACTION_VSN,
      serialization_template(?CONTRACT_INTERACTION_VSN),
      [ {caller_id, CallerId}
      , {caller_nonce, CallerNonce}
      , {height, Height}
      , {contract_id, ContractId}
      , {gas_price, GasPrice}
      , {gas_used, GasUsed}
      , {return_value, ReturnValue}
      , {return_type, serialize_return_type(ReturnType)}
      , {log, Log}
     ]).

-spec deserialize(id(), binary()) -> call().
deserialize(CallId, B) ->
    {?CONTRACT_INTERACTION_TYPE, Vsn, Fields} =
        aeser_chain_objects:deserialize_type_and_vsn(B),
    case Vsn of
        ?CONTRACT_INTERACTION_VSN ->
            [ {caller_id, CallerId}
            , {caller_nonce, CallerNonce}
            , {height, Height}
            , {contract_id, ContractId}
            , {gas_price, GasPrice}
            , {gas_used, GasUsed}
            , {return_value, ReturnValue}
            , {return_type, ReturnType}
            , {log, Log}
            ] =  aeserialization:decode_fields(
                   serialization_template(?CONTRACT_INTERACTION_VSN),
                   Fields)
    end,
    %% TODO: check caller_id type
    contract = aeser_id:specialize_type(ContractId),
    #call{ id           = CallId
         , caller_id    = CallerId
         , caller_nonce = CallerNonce
         , height       = Height
         , contract_id  = ContractId
         , gas_price    = GasPrice
         , gas_used     = GasUsed
         , return_value = ReturnValue
         , return_type  = deserialize_return_type(ReturnType)
         , log          = Log
         }.

serialization_template(?CONTRACT_INTERACTION_VSN) ->
    [ {caller_id, id}
    , {caller_nonce, int}
    , {height, int}
    , {contract_id, id}
    , {gas_price, int}
    , {gas_used, int}
    , {return_value, binary}
    , {return_type, int}
    , {log, [{binary, [binary], binary}]}
    ].

serialize_return_type(ok) -> 0;
serialize_return_type(error) -> 1;
serialize_return_type(revert) ->  2.

deserialize_return_type(0) -> ok;
deserialize_return_type(1) -> error;
deserialize_return_type(2) -> revert.

-spec serialize_for_client(call()) -> map().
serialize_for_client(#call{caller_id    = CallerId,
                           caller_nonce = CallerNonce,
                           height       = Height,
                           contract_id  = ContractId,
                           gas_price    = GasPrice,
                           gas_used     = GasUsed,
                           return_value = ReturnValue,
                           return_type  = ReturnType,
                           log          = Log
                          }) ->
    #{ <<"caller_id">>    => aeser_api_encoder:encode(id_hash, CallerId)
     , <<"caller_nonce">> => CallerNonce
     , <<"height">>       => Height
     , <<"contract_id">>  => aeser_api_encoder:encode(id_hash, ContractId)
     , <<"gas_price">>    => GasPrice
     , <<"gas_used">>     => GasUsed
     , <<"return_value">> => aeser_api_encoder:encode(contract_bytearray, ReturnValue)
     , <<"return_type">>  => atom_to_binary(ReturnType, utf8)
     , <<"log">>          => [serialize_log_entry_for_client(E) || E <- Log]
     }.

serialize_log_entry_for_client({Address, Topics, Data}) ->
    #{ <<"address">> => aeser_api_encoder:encode(contract_pubkey, Address)
     , <<"topics">>  => [ Topic || <<Topic:256>> <- Topics ]
     , <<"data">>    => aeser_api_encoder:encode(contract_bytearray, Data)
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

-spec caller_nonce(call()) -> integer().
caller_nonce(#call{caller_nonce = CallerNonce}) ->
    CallerNonce.

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

-spec log(call()) -> [log_entry()].
log(I) -> I#call.log.

%%%===================================================================
%%% Setters

-spec set_id(id(), call()) -> call().
set_id(<<_:?PUB_SIZE/unit:8>> = X, I) ->
    I#call{id = X}.

-spec set_caller(aeser_id:tag(), aec_keys:pubkey(), call()) -> call().
set_caller(T, X, I) ->
    I#call{caller_id = aeser_id:create(T, assert_field(caller, X))}.

-spec set_caller_nonce(integer(), call()) -> call().
set_caller_nonce(X, I) ->
    I#call{caller_nonce = assert_field(caller_nonce, X)}.

-spec set_height(integer(), call()) -> call().
set_height(X, I) ->
    I#call{height = assert_field(height, X)}.

-spec set_contract(aec_keys:pubkey(), call()) -> call().
set_contract(X, I) ->
    I#call{contract_id = aeser_id:create(contract, assert_field(contract, X))}.

-spec set_return_value(binary(), call()) -> call().
set_return_value(X, I) ->
    I#call{return_value = assert_field(return_value, X)}.

-spec set_return_type(ok | error | revert, call()) -> call().
set_return_type(ok, I) -> I#call{return_type = ok };
set_return_type(error, I) -> I#call{return_type = error };
set_return_type(revert, I) -> I#call{return_type = revert }.

-spec set_gas_used(integer(), call()) -> call().
set_gas_used(X, I) ->
    I#call{gas_used = assert_field(gas_used, X)}.

-spec set_log(list(), call()) -> call().
set_log(Log, I) ->
    I#call{log = assert_field(log, Log)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {caller,       caller_pubkey(I)}
           , {caller_nonce, I#call.caller_nonce}
           , {height,       I#call.height}
           , {contract,     contract_pubkey(I)}
           , {return_value, I#call.return_value}
           , {gas_price,    I#call.gas_price}
           , {gas_used,     I#call.gas_used}
           , {log,          I#call.log}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(caller,           <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(caller_nonce,     X) when is_integer(X), X >= 0 -> X;
assert_field(height,           X) when is_integer(X), X > 0 -> X;
assert_field(contract,         <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(return_value,     X) when is_binary(X) -> X;
assert_field(gas_price,        X) when is_integer(X), X >= 0 -> X;
assert_field(gas_used,         X) when is_integer(X), X >= 0 -> X;
assert_field(log,              []) -> [];
assert_field(log,              [{_A,_T,_D}=E|Log])-> [E|assert_field(log, Log)];
assert_field(Field,            X) -> error({illegal, Field, X}).
