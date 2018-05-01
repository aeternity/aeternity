%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract call objects.
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call).

%% API
-export([ deserialize/1
        , id/1
        , id/3
        , new/4
        , contract_address/1
        , caller_address/1
        , caller_nonce/1
        , height/1
        , return_type/1
        , return_value/1
        , gas_used/1
        , serialize/1
        , set_contract_address/2
        , set_caller_address/2
        , set_caller_nonce/2
        , set_height/2
        , set_return_type/2
        , set_return_value/2
        , set_gas_used/2
        ]).

-include_lib("apps/aecore/include/common.hrl").

-define(CONTRACT_INTERACTION_TYPE, contract_call).
-define(CONTRACT_INTERACTION_VSN, 1).

%%%===================================================================
%%% Types
%%%===================================================================

-record(call, { caller_address   :: pubkey()
              , caller_nonce     :: integer()
              , height           :: height()
              , contract_address :: pubkey()
              , gas_used         :: amount()
              , return_value     :: binary()
	      , return_type      :: ok | error | revert			    
              }).

-opaque call() :: #call{}.
-type id() :: binary().
-type serialized() :: binary().
-type amount() :: aect_contracts:amount().

-export_type([ id/0
             , call/0
             , serialized/0
             ]).

-define(PUB_SIZE, 65).
-define(NONCE_SIZE, 256).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Caller::pubkey(), Nonce::non_neg_integer(), Address::pubkey(),
	  height()) -> call().
new(Caller, Nonce, Address, BlockHeight) ->
    C = #call{ caller_address   = Caller
             , caller_nonce     = Nonce
             , height           = BlockHeight
             , contract_address = Address
             , gas_used         = 0     %% These are filled later
             , return_value     = <<>>  %% in aect_call_tx:process()
             , return_type      = ok
             },
    assert_fields(C).

-spec id(call()) -> id().
id(I) ->
    id(I#call.caller_address,
       I#call.caller_nonce,
       I#call.contract_address).

-spec id(pubkey(), non_neg_integer(), pubkey()) -> id().
id(Caller, Nonce, Contract) ->
    Bin = <<Caller:?PUB_SIZE/binary,
            Nonce:?NONCE_SIZE,
            Contract:?PUB_SIZE/binary>>,
    aec_hash:hash(pubkey, Bin).

-spec serialize(call()) -> binary().
serialize(#call{} = I) ->
    aec_object_serialization:serialize(
      ?CONTRACT_INTERACTION_TYPE,
      ?CONTRACT_INTERACTION_VSN,
      serialization_template(?CONTRACT_INTERACTION_VSN),
      [ {caller_address, caller_address(I)}
      , {caller_nonce, caller_nonce(I)}
      , {height, height(I)}
      , {contract_address, contract_address(I)}
      , {gas_used, gas_used(I)}
      , {return_value, return_value(I)}
     ]).

-spec deserialize(binary()) -> call().
deserialize(B) ->
    [ {caller_address, CallerAddress}
    , {caller_nonce, CallerNonce}
    , {height, Height}
    , {contract_address, ContractAddress}
    , {gas_used, GasUsed}
    , {return_value, ReturnValue}
    ] = aec_object_serialization:deserialize(
          ?CONTRACT_INTERACTION_TYPE,
          ?CONTRACT_INTERACTION_VSN,
          serialization_template(?CONTRACT_INTERACTION_VSN),
          B),
    #call{ caller_address   = CallerAddress
         , caller_nonce     = CallerNonce
         , height           = Height
         , contract_address = ContractAddress
         , gas_used         = GasUsed
         , return_value     = ReturnValue
         }.

serialization_template(?CONTRACT_INTERACTION_VSN) ->
    [ {caller_address, binary}
    , {caller_nonce, int}
    , {height, int}
    , {contract_address, binary}
    , {gas_used, int}
    , {return_value, binary}
    ].


%%%===================================================================
%%% Getters

-spec caller_address(call()) -> pubkey().
caller_address(I) -> I#call.caller_address.

-spec caller_nonce(call()) -> integer().
caller_nonce(I) -> I#call.caller_nonce.

-spec height(call()) -> height().
height(I) -> I#call.height.

-spec contract_address(call()) -> pubkey().
contract_address(I) -> I#call.contract_address.

-spec return_type(call()) -> ok | error | revert.
return_type(I) -> I#call.return_type.

-spec return_value(call()) -> binary().
return_value(I) -> I#call.return_value.

-spec gas_used(call()) -> amount().
gas_used(I) -> I#call.gas_used.

%%%===================================================================
%%% Setters

-spec set_caller_address(pubkey(), call()) -> call().
set_caller_address(X, I) ->
    I#call{caller_address = assert_field(caller_address, X)}.

-spec set_caller_nonce(integer(), call()) -> call().
set_caller_nonce(X, I) ->
    I#call{caller_nonce = assert_field(caller_nonce, X)}.

-spec set_height(integer(), call()) -> call().
set_height(X, I) ->
    I#call{height = assert_field(height, X)}.

-spec set_contract_address(pubkey(), call()) -> call().
set_contract_address(X, I) ->
    I#call{contract_address = assert_field(contract_address, X)}.

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


%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(I) ->
    List = [ {caller_address,   I#call.caller_address}
           , {caller_nonce,     I#call.caller_nonce}
           , {height,           I#call.height}
           , {contract_address, I#call.contract_address}
           , {return_value,     I#call.return_value}
           , {gas_used,         I#call.gas_used}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> I;
        Other -> error({missing, Other})
    end.

assert_field(caller_address,   <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(caller_nonce,     X) when is_integer(X), X >= 0 -> X;
assert_field(height,           X) when is_integer(X), X > 0 -> X;
assert_field(contract_address, <<_:?PUB_SIZE/binary>> = X) -> X;
assert_field(return_value,     X) when is_binary(X) -> X;
assert_field(gas_used,         X) when is_integer(X), X >= 0 -> X;
assert_field(Field,            X) -> error({illegal, Field, X}).
