%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract call transaction
%%% @end
%%%=============================================================================
-module(aect_call_tx).

-include("aecontract.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         entities/1,
         check/3,
         process/3,
         signers/2,
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
        ]).

-export([process_call_from_contract/3]).

%% Additional getters
-export([call_id/1,
         caller_id/1,
         caller_pubkey/1,
         contract_id/1,
         contract_pubkey/1,
         abi_version/1,
         amount/1,
         gas_limit/1,
         gas_price/1,
         call_data/1,
         call_stack/1]).

%% For converting old db format
-export([from_db_format/1
        ]).

-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_TYPE, contract_call_tx).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_call_tx, {
          caller_id        :: aeser_id:id(),
          nonce            :: integer(),
          contract_id      :: aeser_id:id(),
          abi_version      :: aect_contracts:abi_version(),
          fee              :: integer(),
          ttl              :: aetx:tx_ttl(),
          amount           :: aect_contracts:amount(),
          gas              :: aect_contracts:amount(),
          gas_price        :: aect_contracts:amount(),
          call_data        :: binary(),
          call_stack  = [] :: [non_neg_integer()],
            %% addresses (the pubkey as an integer) of contracts on the call
            %% stack, not serialized
          call_origin      :: aec_keys:pubkey()
            %% Only different from caller if this is called from a contract,
            %% not serialized
          }).

-record(db_contract_call_tx, {
          caller_id        :: aeser_id:id(),
          nonce            :: integer(),
          contract_id      :: aeser_id:id(),
          abi_version      :: aect_contracts:abi_version(),
          fee              :: integer(),
          ttl              :: aetx:tx_ttl(),
          amount           :: aect_contracts:amount(),
          gas              :: aect_contracts:amount(),
          gas_price        :: aect_contracts:amount(),
          call_data        :: binary(),
          call_stack  = [] :: [non_neg_integer()]
          }).


-opaque tx() :: #contract_call_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{caller_id   := CallerId,
      nonce       := Nonce,
      contract_id := ContractId,
      abi_version := ABIVersion,
      fee         := Fee,
      amount      := Amount,
      gas         := Gas,
      gas_price   := GasPrice,
      call_data   := CallData} = Args) ->
    CallStack = maps:get(call_stack, Args, []),
    TTL = maps:get(ttl, Args, 0),
    CallOrigin =
        case aeser_id:specialize(CallerId) of
            {contract, Pubkey} -> maps:get(origin, Args, Pubkey);
            {account,  Pubkey} -> maps:get(origin, Args, Pubkey)
        end,
    contract = aeser_id:specialize_type(ContractId),
    Tx = #contract_call_tx{caller_id   = CallerId,
                           nonce       = Nonce,
                           contract_id = ContractId,
                           abi_version = ABIVersion,
                           fee         = Fee,
                           ttl         = TTL,
                           amount      = Amount,
                           gas         = Gas,
                           gas_price   = GasPrice,
                           call_data   = CallData,
                           call_stack  = CallStack,
                           call_origin = CallOrigin
                          },
    {ok, aetx:new(?MODULE, Tx)}.

-spec from_db_format(tx() | tuple()) -> tx().
from_db_format(#contract_call_tx{} = Tx) -> Tx;
from_db_format(Tuple) ->
    case setelement(1, Tuple, db_contract_call_tx) of
        #db_contract_call_tx{
           caller_id        = CallerId,
           nonce            = Nonce,
           contract_id      = ContractId,
           abi_version      = ABI,
           fee              = Fee,
           ttl              = TTL,
           amount           = Amount,
           gas              = Gas,
           gas_price        = GasPrice,
           call_data        = CallData,
           call_stack       = CallStack
          } ->
            CallOrigin =
                case aeser_id:specialize(CallerId) of
                    {contract, Pubkey} -> Pubkey;
                    {account,  Pubkey} -> Pubkey
                end,
            #contract_call_tx{
               caller_id        = CallerId,
               nonce            = Nonce,
               contract_id      = ContractId,
               abi_version      = ABI,
               fee              = Fee,
               ttl              = TTL,
               amount           = Amount,
               gas              = Gas,
               gas_price        = GasPrice,
               call_data        = CallData,
               call_stack       = CallStack,
               call_origin      = CallOrigin
              };
        _ ->
            error(illegal_db_format)
    end.

-spec type() -> atom().
type() ->
    ?CONTRACT_CALL_TX_TYPE.

-spec fee(tx()) -> integer().
fee(#contract_call_tx{fee = F}) ->
    F.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#contract_call_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#contract_call_tx{} = Tx) ->
    caller_pubkey(Tx).

-spec entities(tx()) -> [aeser_id:id()].
%% origin id first
entities(#contract_call_tx{caller_id = CId, contract_id = CoId}) ->
    [CId, CoId].

call_origin(#contract_call_tx{call_origin = CallOrigin}) ->
    CallOrigin.

-spec call_id(tx()) -> aect_call:id().
call_id(#contract_call_tx{} = Tx) ->
    aect_call:id(caller_pubkey(Tx), nonce(Tx), contract_pubkey(Tx)).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_call_tx{}, Trees,_Env) ->
    %% Checks are in process/3
    {ok, Trees}.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(Tx, _) ->
    {ok, [caller_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#contract_call_tx{} = Tx, Trees, Env) ->
    %% Assert
    case aetx_env:context(Env) of
      aetx_transaction -> ok;
      aetx_ga          -> ok
    end,
    Instructions =
        aeprimop:contract_call_tx_instructions(
          caller_pubkey(Tx),
          contract_pubkey(Tx),
          call_data(Tx),
          gas_limit(Tx),
          gas_price(Tx),
          amount(Tx),
          call_stack(Tx),
          abi_version(Tx),
          call_origin(Tx),
          fee(Tx),
          nonce(Tx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec process_call_from_contract(tx(), aec_trees:trees(), aetx_env:env()) ->
                                        {ok, aect_call:call(), aec_trees:trees()}
                                            | {error, term()}.
process_call_from_contract(#contract_call_tx{} = Tx, Trees, Env) ->
    %% Assert
    aetx_contract = aetx_env:context(Env),
    Instructions =
        aeprimop:contract_call_from_contract_instructions(
          caller_pubkey(Tx),
          contract_pubkey(Tx),
          call_data(Tx),
          gas_limit(Tx),
          gas_price(Tx),
          amount(Tx),
          call_stack(Tx),
          abi_version(Tx),
          call_origin(Tx),
          fee(Tx),
          nonce(Tx)),
    aeprimop:eval_with_return(Instructions, Trees, Env).

serialize(#contract_call_tx{caller_id   = CallerId,
                            nonce       = Nonce,
                            contract_id = ContractId,
                            abi_version = ABIVersion,
                            fee         = Fee,
                            ttl         = TTL,
                            amount      = Amount,
                            gas         = Gas,
                            gas_price   = GasPrice,
                            call_data   = CallData} = Tx) ->
    %% Note that the call_stack is not serialized. This is ok since we don't
    %% serialize transactions originating from contract execution, and for
    %% top-level transactions the call_stack is always empty.
    {version(Tx),
     [ {caller_id, CallerId}
     , {nonce, Nonce}
     , {contract_id, ContractId}
     , {abi_version, ABIVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CALL_TX_VSN,
            [ {caller_id, CallerId}
            , {nonce, Nonce}
            , {contract_id, ContractId}
            , {abi_version, ABIVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    {account, Origin} = aeser_id:specialize(CallerId),
    contract = aeser_id:specialize_type(ContractId),
    #contract_call_tx{caller_id   = CallerId,
                      nonce       = Nonce,
                      contract_id = ContractId,
                      abi_version = ABIVersion,
                      fee         = Fee,
                      ttl         = TTL,
                      amount      = Amount,
                      gas         = Gas,
                      gas_price   = GasPrice,
                      call_data   = CallData,
                      call_origin = Origin
                     }.

serialization_template(?CONTRACT_CALL_TX_VSN) ->
    [ {caller_id, id}
    , {nonce, int}
    , {contract_id, id}
    , {abi_version, int}
    , {fee, int}
    , {ttl, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?CONTRACT_CALL_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

for_client(#contract_call_tx{caller_id   = CallerId,
                             nonce       = Nonce,
                             contract_id = ContractId,
                             abi_version = ABIVersion,
                             fee         = Fee,
                             ttl         = TTL,
                             amount      = Amount,
                             gas         = Gas,
                             gas_price   = GasPrice,
                             call_data   = CallData}) ->
    #{<<"caller_id">>   => aeser_api_encoder:encode(id_hash, CallerId),
      <<"nonce">>       => Nonce,
      <<"contract_id">> => aeser_api_encoder:encode(id_hash, ContractId),
      <<"abi_version">> => ABIVersion,
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aeser_api_encoder:encode(contract_bytearray, CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller_id(tx()) -> aeser_id:id().
caller_id(#contract_call_tx{caller_id = CallerId}) ->
    CallerId.

-spec caller_pubkey(tx()) -> aec_keys:pubkey().
caller_pubkey(#contract_call_tx{caller_id = CallerId}) ->
    {_, CallerPubkey} = aeser_id:specialize(CallerId),
    CallerPubkey.

-spec contract_id(tx()) -> aeser_id:id().
contract_id(#contract_call_tx{contract_id = ContractId}) ->
    ContractId.

-spec contract_pubkey(tx()) -> aec_keys:pubkey().
contract_pubkey(#contract_call_tx{contract_id = ContractId}) ->
  aeser_id:specialize(ContractId, contract).

-spec abi_version(tx()) -> aect_contracts:abi_version().
abi_version(#contract_call_tx{abi_version = ABIVersion}) ->
    ABIVersion.

-spec amount(tx()) -> aect_contracts:amount().
amount(#contract_call_tx{amount = Amount}) ->
    Amount.

-spec gas(tx()) -> aect_contracts:amount().
gas(#contract_call_tx{} = Tx) ->
    gas_limit(Tx).

-spec gas_limit(tx()) -> aect_contracts:amount().
gas_limit(#contract_call_tx{gas = Gas}) ->
    Gas.

-spec gas_price(tx()) -> aect_contracts:amount().
gas_price(#contract_call_tx{gas_price = GasPrice}) ->
    GasPrice.

-spec call_data(tx()) -> binary().
call_data(#contract_call_tx{call_data = CallData}) ->
    CallData.

-spec call_stack(tx()) -> [non_neg_integer()].
call_stack(#contract_call_tx{call_stack = CallStack}) ->
    CallStack.
