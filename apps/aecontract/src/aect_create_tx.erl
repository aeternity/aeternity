%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract register transaction
%%% @end
%%%=============================================================================
-module(aect_create_tx).

-include("aecontract.hrl").
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
-export([owner/1,
         code/1,
         vm_version/1,
         deposit/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1]).

-define(CONTRACT_CREATE_TX_VSN, 1).
-define(CONTRACT_CREATE_TX_TYPE, contract_create_tx).
-define(CONTRACT_CREATE_TX_FEE, 4).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-type amount() :: aect_contracts:amount().

-opaque tx() :: #contract_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec owner(tx()) -> pubkey().
owner(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

-spec code(tx()) -> binary().
code(#contract_create_tx{code = X}) ->
    X.

-spec vm_version(tx()) -> aect_contracts:vm_version().
vm_version(#contract_create_tx{vm_version = X}) ->
    X.

-spec deposit(tx()) -> amount().
deposit(#contract_create_tx{deposit = X}) ->
    X.

-spec amount(tx()) -> amount().
amount(#contract_create_tx{amount = X}) ->
    X.

-spec gas(tx()) -> amount().
gas(#contract_create_tx{gas = X}) ->
    X.

-spec gas_price(tx()) -> amount().
gas_price(#contract_create_tx{gas_price = X}) ->
    X.

-spec call_data(tx()) -> binary().
call_data(#contract_create_tx{call_data = X}) ->
    X.

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#contract_create_tx{fee = Fee}) ->
    Fee.

-spec new(map()) -> {ok, aetx:tx()}.
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
    Tx = #contract_create_tx{owner      = OwnerPubKey,
                             nonce      = Nonce,
                             code       = Code,
                             vm_version = VmVersion,
                             deposit    = Deposit,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData,
                             fee        = Fee},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CREATE_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#contract_create_tx{owner = OwnerPubKey}) ->
    OwnerPubKey.

%% Owner should exist, and have enough funds for the fee
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_create_tx{owner = OwnerPubKey, nonce = Nonce,
                          fee = Fee}, _Context, Trees, Height, _ConsensusVersion) ->
    Checks =
        [fun() -> aetx_utils:check_account(OwnerPubKey, Trees, Height, Nonce, Fee) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec accounts(tx()) -> [pubkey()].
accounts(#contract_create_tx{owner = OwnerPubKey}) ->
    [OwnerPubKey].

-spec signers(tx()) -> [pubkey()].
signers(#contract_create_tx{owner = OwnerPubKey}) ->
    [OwnerPubKey].

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#contract_create_tx{owner = OwnerPubKey,
                            nonce = Nonce,
			    vm_version = VmVersion,
                            fee   = Fee} = CreateTx, _Context, Trees0, Height, _ConsensusVersion) ->

    AccountsTree0  = aec_trees:accounts(Trees0),

    %% Charge the fee to the contract owner (caller)
    Owner0        = aec_accounts_trees:get(OwnerPubKey, AccountsTree0),
    {ok, Owner1}  = aec_accounts:spend(Owner0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Owner1, AccountsTree0),
    Trees1        = aec_trees:set_accounts(Trees0, AccountsTree1),

    %% Create the contract and add to state tree
    ContractPubKey = aect_contracts:compute_contract_pubkey(OwnerPubKey, Nonce),
    Contract       = aect_contracts:new(ContractPubKey, CreateTx, Height),

    %% Create the init call.
    Call0 = aect_call:new(OwnerPubKey, Nonce, ContractPubKey, Height),

    %% Create the contract and insert it into the contract state tree
    %%   The public key for the contract is generated from the owners pubkey
    %%   and the nonce, so that no one has the private key. Though, even if
    %%   someone did have the private key, we should not accept spend
    %%   transactions on a contract account.
    Trees5 =
	case VmVersion of
	    ?AEVM_01_Sophia_01 ->
		%% Execute init call to get the contract state and return value
		ContractsTree0a = aec_trees:contracts(Trees1),
		ContractsTree1a = aect_state_tree:insert_contract(Contract, ContractsTree0a),
		Trees2 = aec_trees:set_contracts(Trees1, ContractsTree1a),
		{CallRes, Trees3} = run_contract(CreateTx, Call0, Height, Trees2, Contract, ContractPubKey),
		case aect_call:return_type(CallRes) of
		    ok ->
			%% Insert the call into the state tree for one block.
			%% This is mainly to make the return value accessible.
			%% Each block starts with an empty calls tree.
			CallsTree0 = aec_trees:calls(Trees3),
			CallsTree1 = aect_call_state_tree:insert_call(CallRes, CallsTree0),
			aec_trees:set_calls(Trees3, CallsTree1);
		    E ->
			lager:debug("Init call error ~w ~w~n",[E, CallRes]),
			Trees2
		end;
	    ?AEVM_01_Solidity_01 ->

		%% Execute init call to get the contract bytecode
		%% as a result. to be used for insertion
		ContractsTree0a = aec_trees:contracts(Trees1),
		ContractsTree1a = aect_state_tree:insert_contract(Contract, ContractsTree0a),
		Trees2 = aec_trees:set_contracts(Trees1, ContractsTree1a),
		{CallRes, Trees3} = run_contract(CreateTx, Call0, Height, Trees2, Contract, ContractPubKey),
		case aect_call:return_type(CallRes) of
		    ok ->
			%% Insert the call into the state tree.
			%% This is mainly to remember what the
			%% return value was so that the caller
			%% can access it easily.
			%% Each block starts with an empty calls tree.
			CallsTree0 = aec_trees:calls(Trees3),
			CallsTree1 = aect_call_state_tree:insert_call(CallRes, CallsTree0),
			Trees4 = aec_trees:set_calls(Trees3, CallsTree1),

			%% Update contract
			ContractsTree0 = aec_trees:contracts(Trees4),
			NewCode = aect_call:return_value(CallRes),
			Contract1 = aect_contracts:set_code(NewCode, Contract),
			ContractsTree1 = aect_state_tree:enter_contract(Contract1, ContractsTree0),
			aec_trees:set_contracts(Trees4, ContractsTree1);
		    E ->
			lager:debug("Init call error ~w ~w~n",[E, CallRes]),
			Trees3
		end;
	    _ ->
		Trees1
	end,


    {ok, Trees5}.

run_contract(#contract_create_tx{ owner      = Caller
				, nonce      =_Nonce
				, code       = Code
				, vm_version = VmVersion
				, amount     =_Amount
				, gas        = Gas
				, gas_price  = GasPrice
				, call_data  = CallData
				} =_Tx,
	     Call, Height, Trees,_Contract, ContractPubKey)->
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.

    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => 0 %% Initial call takes no amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },

    aect_dispatch:run(VmVersion, CallDef).


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
    {version(),
     [ {owner, OwnerPubKey}
     , {nonce, Nonce}
     , {code, Code}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {deposit, Deposit}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CREATE_TX_VSN,
            [ {owner, OwnerPubKey}
            , {nonce, Nonce}
            , {code, Code}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {deposit, Deposit}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
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

serialization_template(?CONTRACT_CREATE_TX_VSN) ->
    [ {owner, binary}
    , {nonce, int}
    , {code, binary}
    , {vm_version, int}
    , {fee, int}
    , {deposit, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

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
    #{<<"data_schema">> => <<"ContractCreateTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"owner">>       => aec_base58c:encode(account_pubkey, OwnerPubKey),
      <<"nonce">>       => Nonce,
      <<"code">>        => aect_utils:hex_bytes(Code),
      <<"vm_version">>  => aect_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"deposit">>     => Deposit,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aect_utils:hex_bytes(CallData)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CREATE_TX_VSN.

