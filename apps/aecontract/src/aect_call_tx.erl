%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract call transaction
%%% @end
%%%=============================================================================
-module(aect_call_tx).

-include("contract_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0,
         for_client/1
        ]).

%% Additional getters
-export([caller/1,
         contract/1,
         vm_version/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1]).


-define(CONTRACT_CALL_TX_TYPE, <<"contract_call">>).
-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_FEE, 2).

-opaque call_tx() :: #contract_call_tx{}.

-export_type([call_tx/0]).

-spec new(map()) -> {ok, call_tx()}.
new(#{caller     := CallerPubKey,
      nonce      := Nonce,
      contract   := Contract,
      vm_version := VmVersion,
      fee        := Fee,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData}) ->
    {ok, #contract_call_tx{caller     = CallerPubKey,
                           nonce      = Nonce,
                           contract   = Contract,
                           vm_version = VmVersion,
                           fee        = Fee,
                           amount     = Amount,
                           gas        = Gas,
                           gas_price  = GasPrice,
                           call_data  = CallData}}.

-spec fee(call_tx()) -> integer().
fee(#contract_call_tx{fee = F}) ->
    F.

-spec nonce(call_tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(call_tx()) -> pubkey().
origin(#contract_call_tx{caller = CallerPubKey}) ->
    CallerPubKey.

%% CallerAccount should exist, and have enough funds for the fee + the call_fee.
%% Contract should exist, and call_fee should be enough
%% Fee should cover TTL
-spec check(call_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#contract_call_tx{caller = CallerPubKey, nonce = Nonce,
                        contract = ContractPubKey,
                        fee = Fee} = CallTx, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(CallerPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_contract(ContractPubKey, Trees, Height) end,
         fun() -> check_call(CallTx, Trees, Height) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(call_tx()) -> [pubkey()].
signers(Tx) ->
    [caller(Tx)].

-spec process(call_tx(), trees(), height()) -> {ok, trees()}.
process(#contract_call_tx{caller = _CallerPubKey, nonce = _Nonce, fee = _Fee
                         } = _Query, Trees, _Height) ->
    %% PLACEHOLDER
    {ok, Trees}.

serialize(#contract_call_tx{caller     = CallerPubKey,
                            nonce      = Nonce,
                            contract   = ContractPubKey,
                            vm_version = VmVersion,
                            fee        = Fee,
                            amount     = Amount,
                            gas        = Gas,
                            gas_price  = GasPrice,
                            call_data  = CallData}) ->
    [#{<<"type">>       => type()},
     #{<<"vsn">>        => version()},
     #{<<"caller">>     => CallerPubKey},
     #{<<"nonce">>      => Nonce},
     #{<<"contract">>   => ContractPubKey},
     #{<<"vm_version">> => VmVersion},
     #{<<"fee">>        => Fee},
     #{<<"amount">>     => Amount},
     #{<<"gas">>        => Gas},
     #{<<"gas_price">>  => GasPrice},
     #{<<"call_data">>  => CallData}].

deserialize([#{<<"type">>       := ?CONTRACT_CALL_TX_TYPE},
             #{<<"vsn">>        := ?CONTRACT_CALL_TX_VSN},
             #{<<"caller">>     := CallerPubKey},
             #{<<"nonce">>      := Nonce},
             #{<<"contract">>   := ContractPubKey},
             #{<<"vm_version">> := VmVersion},
             #{<<"fee">>        := Fee},
             #{<<"amount">>     := Amount},
             #{<<"gas">>        := Gas},
             #{<<"gas_price">>  := GasPrice},
             #{<<"call_data">>  := CallData}]) ->
    #contract_call_tx{caller     = CallerPubKey,
                      nonce      = Nonce,
                      contract   = ContractPubKey,
                      vm_version = VmVersion,
                      fee        = Fee,
                      amount     = Amount,
                      gas        = Gas,
                      gas_price  = GasPrice,
                      call_data  = CallData}.

-spec type() -> binary().
type() ->
    ?CONTRACT_CALL_TX_TYPE.

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CALL_TX_VSN.

for_client(#contract_call_tx{caller     = CallerPubKey,
                             nonce      = Nonce,
                             contract   = ContractPubKey,
                             vm_version = VmVersion,
                             fee        = Fee,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData}) ->
    #{<<"type">>       => <<"ContractQueryTxObject">>, % swagger schema name
      <<"vsn">>        => version(),
      <<"caller">>     => aec_base58c:encode(account_pubkey, CallerPubKey),
      <<"nonce">>      => Nonce,
      <<"contract">>   => aec_base58c:encode(account_pubkey, ContractPubKey), %% TODO: different tag?
      <<"vm_version">> => aect_utils:hex_byte(VmVersion),
      <<"fee">>        => Fee,
      <<"amount">>     => Amount,
      <<"gas">>        => Gas,
      <<"gas_price">>  => GasPrice,
      <<"call_data">>  => aect_utils:hex_bytes(CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller(call_tx()) -> pubkey().
caller(C) -> C#contract_call_tx.caller.

-spec contract(call_tx()) -> pubkey().
contract(C) -> C#contract_call_tx.contract.

-spec vm_version(call_tx()) -> aect_contracts:vm_version().
vm_version(C) -> C#contract_call_tx.vm_version.

-spec amount(call_tx()) -> aect_contracts:amount().
amount(C) -> C#contract_call_tx.amount.

-spec gas(call_tx()) -> aect_contracts:amount().
gas(C) -> C#contract_call_tx.gas.

-spec gas_price(call_tx()) -> aect_contracts:amount().
gas_price(C) -> C#contract_call_tx.gas_price.

-spec call_data(call_tx()) -> binary().
call_data(C) -> C#contract_call_tx.call_data.

%% -- Local functions  -------------------------------------------------------

check_call(_CallTx, _Trees, _Height) ->
    %% PLACEHOLDER
    ok.

check_contract(_ContractPubKey, _Trees, _Height) ->
    %% PLACEHOLDER
    ok.

