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
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         accounts/1,
         signers/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Additional getters
-export([caller/1,
         contract/1,
         vm_version/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1,
         call_stack/1]).

-define(PUB_SIZE, 65).

-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_TYPE, contract_call_tx).
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
      call_data  := CallData} = Args) ->
    CallStack = maps:get(call_stack, Args, []),
    Tx = #contract_call_tx{caller     = CallerPubKey,
                           nonce      = Nonce,
                           contract   = Contract,
                           vm_version = VmVersion,
                           fee        = Fee,
                           amount     = Amount,
                           gas        = Gas,
                           gas_price  = GasPrice,
                           call_data  = CallData,
                           call_stack = CallStack},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CALL_TX_TYPE.

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
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_call_tx{caller = CallerPubKey, nonce = Nonce,
                        fee = Fee, amount = Value,
                        gas = GasLimit, gas_price = GasPrice,
                        call_stack = CallStack
                       } = CallTx, Context, Trees, Height, _ConsensusVersion) ->
    Checks =
        case Context of
            aetx_transaction ->
                RequiredAmount = Fee + GasLimit * GasPrice + Value,
                [fun() -> aetx_utils:check_account(CallerPubKey, Trees, Height, Nonce, RequiredAmount) end,
                 fun() -> check_call(CallTx, Trees, Height) end,
                 fun() -> aect_utils:check(CallStack == [], nonempty_call_stack) end];
            aetx_contract ->
                [fun() -> aect_utils:check_balance(CallerPubKey, Trees, Value) end,
                 fun() -> check_call(CallTx, Trees, Height) end]
        end,

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

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#contract_call_tx{caller = CallerPubKey, contract = CalleePubKey, nonce = Nonce,
                          fee = Fee, gas_price = GasPrice, amount = Value
                         } = CallTx, Context, Trees0, Height, _ConsensusVersion) ->
    AccountsTree0  = aec_trees:accounts(Trees0),
    ContractsTree0 = aec_trees:contracts(Trees0),

    %% Create the call.
    Call0 = aect_call:new(CallTx, Height),

    %% Run the contract code. Also computes the amount of gas left and updates
    %% the call object.
    %% TODO: handle transactions performed by the contract code
    Call = run_contract(CallTx, Call0, Height, Trees0),

    %% Charge the fee and the used gas to the caller (not if called from another contract!)
    {AccountsTree1, ContractsTree1} =
        case Context of
            aetx_contract    ->
                %% When calling from another contract we only charge the 'amount'
                Caller0 = aect_state_tree:get_contract(CallerPubKey, ContractsTree0),
                Caller1 = aect_contracts:spend(Value, Caller0),
                {AccountsTree0, aect_state_tree:enter_contract(Caller1, ContractsTree0)};
            aetx_transaction ->
                %% When calling from the top-level we charge Fee and Gas as well.
                Amount        = Fee + aect_call:gas_used(Call) * GasPrice + Value,
                Caller0       = aec_accounts_trees:get(CallerPubKey, AccountsTree0),
                {ok, Caller1} = aec_accounts:spend(Caller0, Amount, Nonce, Height),
                {aec_accounts_trees:enter(Caller1, AccountsTree0), ContractsTree0}
        end,

    %% Credit the attached funds to the callee
    Callee0 = aect_state_tree:get_contract(CalleePubKey, ContractsTree1),
    Callee1 = aect_contracts:earn(Value, Callee0),
    ContractsTree2 = aect_state_tree:enter_contract(Callee1, ContractsTree1),

    %% Insert the call into the state tree. This is mainly to remember what the
    %% return value was so that the caller can access it easily.
    ContractsTree3 = aect_state_tree:insert_call(Call, ContractsTree2),

    %% Update the state tree
    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_contracts(Trees1, ContractsTree3),

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
    %% Note that the call_stack is not serialized. This is ok since we don't
    %% serialize transactions originating from contract execution, and for
    %% top-level transactions the call_stack is always empty.
    {version(),
     [ {caller, CallerPubKey}
     , {nonce, Nonce}
     , {contract, ContractPubKey}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CALL_TX_VSN,
            [ {caller, CallerPubKey}
            , {nonce, Nonce}
            , {contract, ContractPubKey}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    #contract_call_tx{caller     = CallerPubKey,
                      nonce      = Nonce,
                      contract   = ContractPubKey,
                      vm_version = VmVersion,
                      fee        = Fee,
                      amount     = Amount,
                      gas        = Gas,
                      gas_price  = GasPrice,
                      call_data  = CallData}.

serialization_template(?CONTRACT_CALL_TX_VSN) ->
    [ {caller, binary}
    , {nonce, int}
    , {contract, binary}
    , {vm_version, int}
    , {fee, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

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
    #{<<"data_schema">> => <<"ContractCallTxObject">>, % swagger schema name
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

-spec call_stack(tx()) -> [non_neg_integer()].
call_stack(C) -> C#contract_call_tx.call_stack.

%% -- Local functions  -------------------------------------------------------

%% Check that the contract exists and has the right VM version.
check_call(#contract_call_tx{ contract   = ContractPubKey,
                              vm_version = VmVersion,
                              amount     = Value},
               Trees, _Height) ->
    ContractsTree = aec_trees:contracts(Trees),
    %% Dialyzer, in its infinite wisdom, complains if it thinks we're checking
    %% that something of type non_neg_integer() is negative. Since Dialyzer
    %% doesn't _actually_ guarantee that this isn't the case, we do need the
    %% check and this is the least convoluted way of writing it that Dialyzer's
    %% static analysis cannot see through.
    NegativeAmount = -Value > 0,
    case aect_state_tree:lookup_contract(ContractPubKey, ContractsTree) of
        _ when NegativeAmount -> {error, negative_amount};
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
             { caller     = Caller
             , contract   = ContractPubKey
             , gas        = Gas
             , gas_price  = GasPrice
             , call_data  = CallData
             , amount     = Value
             , call_stack = CallStack
             } = _Tx, Call, Height, Trees) ->
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = aect_contracts:code(Contract),

    %% TODO: Handle different VMs and ABIs.
    %% TODO: Move init and execution to a separate moidule to be re used by
    %% both on chain and off chain calls.
    ChainState = aec_vm_chain:new_state(Trees, Height, ContractPubKey),
    <<Address:?PUB_SIZE/unit:8>> = ContractPubKey,
    try aevm_eeevm_state:init(
	  #{ exec => #{ code       => Code,
			address    => Address,
			caller     => Caller,
			data       => CallData,
			gas        => Gas,
			gasPrice   => GasPrice,
			origin     => Caller,
			value      => Value,
                        call_stack => CallStack },
             %% TODO: set up the env properly
             env => #{currentCoinbase   => 0,
                      currentDifficulty => 0,
                      currentGasLimit   => Gas,
                      currentNumber     => Height,
                      currentTimestamp  => 0,
                      chainState        => ChainState,
                      chainAPI          => aec_vm_chain},
             pre => #{}},
          #{trace => false})
    of
	InitState ->
	    %% TODO: Nicer error handling - do more in check.
	    %% Update gas_used depending on exit type.x
	    try aevm_eeevm:eval(InitState) of
		%% Succesful execution
		{ok, #{ gas := GasLeft, out := ReturnValue }} ->
		    aect_call:set_gas_used(Gas - GasLeft,
					   aect_call:set_return_value(ReturnValue, Call));
		%% Executinon reulting in VM exeception.
		%% Gas used, but other state not affected.
		%% TODO: Use up the right amount of gas depending on error
		%% TODO: Store errorcode in state tree
		{error,_Error, #{ gas :=_GasLeft}} ->
		    aect_call:set_gas_used(Gas,
					   aect_call:set_return_value(<<>>, Call))

	    catch _:_ -> Call
	    end
    catch _:_ ->
	    Call
    end.

