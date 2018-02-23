%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract call transaction
%%% @end
%%%=============================================================================
-module(aect_call_tx).

-include("contract_txs.hrl").
-include_lib("apps/aecore/include/common.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
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


-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_FEE, 2).

-opaque tx() :: #contract_call_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{caller     := CallerPubKey,
      nonce      := Nonce,
      contract   := Contract,
      vm_version := VmVersion,
      fee        := Fee,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData}) ->
    Tx = #contract_call_tx{caller     = CallerPubKey,
                           nonce      = Nonce,
                           contract   = Contract,
                           vm_version = VmVersion,
                           fee        = Fee,
                           amount     = Amount,
                           gas        = Gas,
                           gas_price  = GasPrice,
                           call_data  = CallData},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> integer().
fee(#contract_call_tx{fee = F}) ->
    F.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#contract_call_tx{caller = CallerPubKey}) ->
    CallerPubKey.

%% CallerAccount should exist, and have enough funds for the fee + gas cost
%% Contract should exist and its vm_version should match the one in the call.
-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_call_tx{caller = CallerPubKey, nonce = Nonce,
                        fee = Fee,
                        gas = GasLimit, gas_price = GasPrice
                       } = CallTx, Trees, Height) ->
    RequiredAmount = Fee + GasLimit * GasPrice,
    Checks =
        [fun() -> aetx_utils:check_account(CallerPubKey, Trees, Height, Nonce, RequiredAmount) end,
         fun() -> check_call(CallTx, Trees, Height) end
        ],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(Tx) ->
    [caller(Tx)].

-spec signers(tx()) -> [pubkey()].
signers(Tx) ->
    [caller(Tx)].

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#contract_call_tx{caller = CallerPubKey, nonce = Nonce, fee = Fee,
                          gas_price = GasPrice
                         } = CallTx, Trees0, Height) ->
    AccountsTree0  = aec_trees:accounts(Trees0),
    ContractsTree0 = aec_trees:contracts(Trees0),

    %% Create the call.
    %% gas used.
    Call0 = aect_call:new(CallTx, Height),

    %% Run the contract code. Also computes the amount of gas left and updates
    %% the call object.
    %% TODO: handle transactions performed by the contract code
    Call = run_contract(CallTx, Call0, Height, Trees0),

    %% Charge the fee and the used gas to the caller
    Amount         = Fee + aect_call:gas_used(Call) * GasPrice,
    Caller0        = aec_accounts_trees:get(CallerPubKey, AccountsTree0),
    {ok, Caller1}  = aec_accounts:spend(Caller0, Amount, Nonce, Height),
    AccountsTree1  = aec_accounts_trees:enter(Caller1, AccountsTree0),

    %% Insert the call into the state tree. This is mainly to remember what the
    %% return value was so that the caller can access it easily.
    ContractsTree1 = aect_state_tree:insert_call(Call, ContractsTree0),

    %% Update the state tree
    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_contracts(Trees1, ContractsTree1),

    {ok, Trees2}.

serialize(#contract_call_tx{caller     = CallerPubKey,
                            nonce      = Nonce,
                            contract   = ContractPubKey,
                            vm_version = VmVersion,
                            fee        = Fee,
                            amount     = Amount,
                            gas        = Gas,
                            gas_price  = GasPrice,
                            call_data  = CallData}) ->
    [#{<<"vsn">>        => version()},
     #{<<"caller">>     => CallerPubKey},
     #{<<"nonce">>      => Nonce},
     #{<<"contract">>   => ContractPubKey},
     #{<<"vm_version">> => VmVersion},
     #{<<"fee">>        => Fee},
     #{<<"amount">>     => Amount},
     #{<<"gas">>        => Gas},
     #{<<"gas_price">>  => GasPrice},
     #{<<"call_data">>  => CallData}].

deserialize([#{<<"vsn">>        := ?CONTRACT_CALL_TX_VSN},
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
    #{<<"data_schema">> => <<"ContractQueryTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"caller">>      => aec_base58c:encode(account_pubkey, CallerPubKey),
      <<"nonce">>       => Nonce,
      <<"contract">>    => aec_base58c:encode(account_pubkey, ContractPubKey), %% TODO: different tag?
      <<"vm_version">>  => aect_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aect_utils:hex_bytes(CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller(tx()) -> pubkey().
caller(C) -> C#contract_call_tx.caller.

-spec contract(tx()) -> pubkey().
contract(C) -> C#contract_call_tx.contract.

-spec vm_version(tx()) -> aect_contracts:vm_version().
vm_version(C) -> C#contract_call_tx.vm_version.

-spec amount(tx()) -> aect_contracts:amount().
amount(C) -> C#contract_call_tx.amount.

-spec gas(tx()) -> aect_contracts:amount().
gas(C) -> C#contract_call_tx.gas.

-spec gas_price(tx()) -> aect_contracts:amount().
gas_price(C) -> C#contract_call_tx.gas_price.

-spec call_data(tx()) -> binary().
call_data(C) -> C#contract_call_tx.call_data.

%% -- Local functions  -------------------------------------------------------

%% Check that the contract exists and has the right VM version.
check_call(#contract_call_tx{ contract   = ContractPubKey,
                              vm_version = VmVersion },
               Trees, _Height) ->
    ContractsTree = aec_trees:contracts(Trees),
    case aect_state_tree:lookup_contract(ContractPubKey, ContractsTree) of
        {value, C} ->
            case aect_contracts:vm_version(C) == VmVersion of
                true  -> ok;
                false -> {error, wrong_vm_version}
            end;
        none -> {error, contract_does_not_exist}
    end.

%% -- Running contract code --------------------------------------------------

%% Call the contract and update the call object with the return value and gas
%% used.
-spec run_contract(tx(), aect_call:call(), height(), aec_trees:trees()) -> aect_call:call().
run_contract(#contract_call_tx
             { caller    = Caller
             , contract  = ContractPubKey
             , gas       = Gas
             , gas_price = GasPrice
             , call_data = CallData
             , amount    = Value
             } = _Tx, Call, Height, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = aect_contracts:code(Contract),
    try aevm_eeevm_state:init(
	  #{ exec => #{ code     => Code,
			address  => 0,        %% We start executing at address 0
			caller   => Caller,
			data     => CallData,
			gas      => Gas,
			gasPrice => GasPrice,
			origin   => Caller,
			value    => Value },
             %% TODO: set up the env properly
             env => #{currentCoinbase   => 0,
                      currentDifficulty => 0,
                      currentGasLimit   => Gas,
                      currentNumber     => Height,
                      currentTimestamp  => 0},
             pre => #{}},
          #{trace => false})
    of
	InitState ->
	    try aevm_eeevm:eval(InitState) of
		#{ gas := GasLeft, out := ReturnValue } ->
		    aect_call:set_gas_used(Gas - GasLeft,
					   aect_call:set_return_value(ReturnValue, Call))
		    %% TODO: Nicer error handling - do more in check.
		    %% Update gas_used depending on exit type.x
	    catch _:_ -> Call
	    end
    catch _:_ ->
	    Call
    end.

