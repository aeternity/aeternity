%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract register transaction
%%% @end
%%%=============================================================================
-module(aect_create_tx).

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
-export([owner/1,
         code/1,
         vm_version/1,
         deposit/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1]).

-define(CONTRACT_CREATE_TX_TYPE, <<"contract_create">>).
-define(CONTRACT_CREATE_TX_VSN, 1).
-define(CONTRACT_CREATE_TX_FEE, 4).

-type amount() :: aect_contracts:amount().

-opaque create_tx() :: #contract_create_tx{}.

-export_type([create_tx/0]).

%%%===================================================================
%%% Getters

-spec owner(create_tx()) -> pubkey().
owner(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

-spec code(create_tx()) -> binary().
code(#contract_create_tx{code = X}) ->
    X.

-spec vm_version(create_tx()) -> non_neg_integer().
vm_version(#contract_create_tx{vm_version = X}) ->
    X.

-spec deposit(create_tx()) -> amount().
deposit(#contract_create_tx{deposit = X}) ->
    X.

-spec amount(create_tx()) -> amount().
amount(#contract_create_tx{amount = X}) ->
    X.

-spec gas(create_tx()) -> amount().
gas(#contract_create_tx{gas = X}) ->
    X.

-spec gas_price(create_tx()) -> amount().
gas_price(#contract_create_tx{gas_price = X}) ->
    X.

-spec call_data(create_tx()) -> binary().
call_data(#contract_create_tx{call_data = X}) ->
    X.

%%%===================================================================
%%% Behavior API

-spec fee(create_tx()) -> integer().
fee(#contract_create_tx{fee = Fee}) ->
    Fee.

-spec new(map()) -> {ok, create_tx()}.
new(#{owner      := OwnerPubKey,
      nonce      := Nonce,
      code       := Code,
      vm_version := VmVersion,
      deposit    := Deposit,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData,
      fee        := Fee}) ->
    {ok, #contract_create_tx{owner      = OwnerPubKey,
                             nonce      = Nonce,
                             code       = Code,
                             vm_version = VmVersion,
                             deposit    = Deposit,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData,
                             fee        = Fee}}.

-spec nonce(create_tx()) -> non_neg_integer().
nonce(#contract_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(create_tx()) -> pubkey().
origin(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

%% Owner should exist, and have enough funds for the fee
-spec check(create_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#contract_create_tx{owner = OwnerPubKey, nonce = Nonce,
                          fee = Fee}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(OwnerPubKey, Trees, Height, Nonce, Fee) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(create_tx()) -> [pubkey()].
signers(#contract_create_tx{owner = OwnerPubKey}) ->
    [OwnerPubKey].

-spec process(create_tx(), trees(), height()) -> {ok, trees()}.
process(#contract_create_tx{owner = _OwnerPubKey,
                            nonce = _Nonce,
                            fee   = _Fee} = _CreateTx, Trees, _Height) ->
    %% PLACEHOLDER
    {ok, Trees}.

serialize(#contract_create_tx{owner      = OwnerPubKey,
                              nonce      = Nonce,
                              code       = Code,
                              vm_version = VmVersion,
                              fee        = Fee,
                              deposit    = Deposit,
                              amount     = Amount,
                              gas        = Gas,
                              gas_price  = GasPrice,
                              call_data  = CallData}) ->
    [#{<<"type">>       => type()},
     #{<<"vsn">>        => version()},
     #{<<"owner">>      => OwnerPubKey},
     #{<<"nonce">>      => Nonce},
     #{<<"code">>       => Code},
     #{<<"vm_version">> => VmVersion},
     #{<<"fee">>        => Fee},
     #{<<"deposit">>    => Deposit},
     #{<<"amount">>     => Amount},
     #{<<"gas">>        => Gas},
     #{<<"gas_price">>  => GasPrice},
     #{<<"call_data">>  => CallData}].

deserialize([#{<<"type">>       := ?CONTRACT_CREATE_TX_TYPE},
             #{<<"vsn">>        := ?CONTRACT_CREATE_TX_VSN},
             #{<<"owner">>      := OwnerPubKey},
             #{<<"nonce">>      := Nonce},
             #{<<"code">>       := Code},
             #{<<"vm_version">> := VmVersion},
             #{<<"fee">>        := Fee},
             #{<<"deposit">>    := Deposit},
             #{<<"amount">>     := Amount},
             #{<<"gas">>        := Gas},
             #{<<"gas_price">>  := GasPrice},
             #{<<"call_data">>  := CallData}]) ->
    #contract_create_tx{owner      = OwnerPubKey,
                        nonce      = Nonce,
                        code       = Code,
                        vm_version = VmVersion,
                        fee        = Fee,
                        deposit    = Deposit,
                        amount     = Amount,
                        gas        = Gas,
                        gas_price  = GasPrice,
                        call_data  = CallData}.

for_client(#contract_create_tx{ owner      = OwnerPubKey,
                                nonce      = Nonce,
                                code       = Code,
                                vm_version = VmVersion,
                                fee        = Fee,
                                deposit    = Deposit,
                                amount     = Amount,
                                gas        = Gas,
                                gas_price  = GasPrice,
                                call_data  = CallData}) ->
    #{<<"type">>       => <<"ContractCreateTxObject">>, % swagger schema name
      <<"vsn">>        => version(),
      <<"owner">>      => aec_base58c:encode(account_pubkey, OwnerPubKey),
      <<"nonce">>      => Nonce,
      <<"code">>       => hex_bytes(Code),
      <<"vm_version">> => hex_byte(VmVersion),
      <<"fee">>        => Fee,
      <<"deposit">>    => Deposit,
      <<"amount">>     => Amount,
      <<"gas">>        => Gas,
      <<"gas_price">>  => GasPrice,
      <<"call_data">>  => hex_bytes(CallData)}.

%%%===================================================================
%%% Internal functions

-spec type() -> binary().
type() ->
    ?CONTRACT_CREATE_TX_TYPE.

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CREATE_TX_VSN.

-spec hex_byte(byte()) -> string().
hex_byte(N) ->
    hex_bytes(<<N:8>>).

-spec hex_bytes(binary()) -> string().
hex_bytes(Bin) ->
    lists:flatten("0x" ++ [io_lib:format("~2.16.0B", [B]) || <<B:8>> <= Bin]).

