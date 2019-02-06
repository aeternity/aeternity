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
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

-export([process_call_from_contract/3]).

%% Additional getters
-export([call_id/1,
         caller_id/1,
         caller_pubkey/1,
         contract_id/1,
         contract_pubkey/1,
         vm_version/1,
         amount/1,
         gas_limit/1,
         gas_price/1,
         call_data/1,
         call_stack/1]).

-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_TYPE, contract_call_tx).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_call_tx, {
          caller_id        :: aec_id:id(),
          nonce            :: integer(),
          contract_id      :: aec_id:id(),
          vm_version       :: aect_contracts:vm_version(),
          fee              :: integer(),
          ttl              :: aetx:tx_ttl(),
          amount           :: aect_contracts:amount(),
          gas              :: aect_contracts:amount(),
          gas_price        :: aect_contracts:amount(),
          call_data        :: binary(),
          call_stack  = [] :: [non_neg_integer()]
            %% addresses (the pubkey as an integer) of contracts on the call
            %% stack
          }).

-opaque tx() :: #contract_call_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{caller_id   := CallerId,
      nonce       := Nonce,
      contract_id := ContractId,
      vm_version  := VmVersion,
      fee         := Fee,
      amount      := Amount,
      gas         := Gas,
      gas_price   := GasPrice,
      call_data   := CallData} = Args) ->
    CallStack = maps:get(call_stack, Args, []),
    TTL = maps:get(ttl, Args, 0),
    case aec_id:specialize_type(CallerId) of
        contract -> ok;
        account  -> ok
    end,
    contract = aec_id:specialize_type(ContractId),
    Tx = #contract_call_tx{caller_id   = CallerId,
                           nonce       = Nonce,
                           contract_id = ContractId,
                           vm_version  = VmVersion,
                           fee         = Fee,
                           ttl         = TTL,
                           amount      = Amount,
                           gas         = Gas,
                           gas_price   = GasPrice,
                           call_data   = CallData,
                           call_stack  = CallStack},
    {ok, aetx:new(?MODULE, Tx)}.

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

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#contract_call_tx{} = Tx, Trees, Env) ->
    %% Assert
    aetx_transaction = aetx_env:context(Env),
    Instructions =
        aec_tx_processor:contract_call_tx_instructions(
          caller_pubkey(Tx),
          contract_pubkey(Tx),
          call_data(Tx),
          gas_limit(Tx),
          gas_price(Tx),
          amount(Tx),
          call_stack(Tx),
          vm_version(Tx),
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

-spec process_call_from_contract(tx(), aec_trees:trees(), aetx_env:env()) ->
                                        {ok, aect_call:call(), aec_trees:trees()}
                                            | {error, term()}.
process_call_from_contract(#contract_call_tx{} = Tx, Trees, Env) ->
    %% Assert
    aetx_contract = aetx_env:context(Env),
    Instructions =
        aec_tx_processor:contract_call_from_contract_instructions(
          caller_pubkey(Tx),
          contract_pubkey(Tx),
          call_data(Tx),
          gas_limit(Tx),
          gas_price(Tx),
          amount(Tx),
          call_stack(Tx),
          vm_version(Tx),
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval_with_return(Instructions, Trees, Env).

serialize(#contract_call_tx{caller_id   = CallerId,
                            nonce       = Nonce,
                            contract_id = ContractId,
                            vm_version  = VmVersion,
                            fee         = Fee,
                            ttl         = TTL,
                            amount      = Amount,
                            gas         = Gas,
                            gas_price   = GasPrice,
                            call_data   = CallData}) ->
    %% Note that the call_stack is not serialized. This is ok since we don't
    %% serialize transactions originating from contract execution, and for
    %% top-level transactions the call_stack is always empty.
    {version(),
     [ {caller_id, CallerId}
     , {nonce, Nonce}
     , {contract_id, ContractId}
     , {vm_version, VmVersion}
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
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    account = aec_id:specialize_type(CallerId),
    contract = aec_id:specialize_type(ContractId),
    #contract_call_tx{caller_id   = CallerId,
                      nonce       = Nonce,
                      contract_id = ContractId,
                      vm_version  = VmVersion,
                      fee         = Fee,
                      ttl         = TTL,
                      amount      = Amount,
                      gas         = Gas,
                      gas_price   = GasPrice,
                      call_data   = CallData}.

serialization_template(?CONTRACT_CALL_TX_VSN) ->
    [ {caller_id, id}
    , {nonce, int}
    , {contract_id, id}
    , {vm_version, int}
    , {fee, int}
    , {ttl, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CALL_TX_VSN.

for_client(#contract_call_tx{caller_id   = CallerId,
                             nonce       = Nonce,
                             contract_id = ContractId,
                             vm_version  = VmVersion,
                             fee         = Fee,
                             ttl         = TTL,
                             amount      = Amount,
                             gas         = Gas,
                             gas_price   = GasPrice,
                             call_data   = CallData}) ->
    #{<<"caller_id">>   => aehttp_api_encoder:encode(id_hash, CallerId),
      <<"nonce">>       => Nonce,
      <<"contract_id">> => aehttp_api_encoder:encode(id_hash, ContractId),
      <<"vm_version">>  => aeu_hex:hexstring_encode(<<VmVersion:8>>),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aehttp_api_encoder:encode(contract_bytearray, CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller_id(tx()) -> aec_id:id().
caller_id(#contract_call_tx{caller_id = CallerId}) ->
    CallerId.

-spec caller_pubkey(tx()) -> aec_keys:pubkey().
caller_pubkey(#contract_call_tx{caller_id = CallerId}) ->
    {_, CallerPubkey} = aec_id:specialize(CallerId),
    CallerPubkey.

-spec contract_id(tx()) -> aec_id:id().
contract_id(#contract_call_tx{contract_id = ContractId}) ->
    ContractId.

-spec contract_pubkey(tx()) -> aec_keys:pubkey().
contract_pubkey(#contract_call_tx{contract_id = ContractId}) ->
  aec_id:specialize(ContractId, contract).

-spec vm_version(tx()) -> aect_contracts:vm_version().
vm_version(#contract_call_tx{vm_version = VmVersion}) ->
    VmVersion.

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
