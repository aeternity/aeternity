%%%=============================================================================
%%% @copyright 2017, Aeternity Anstalt
%%% @doc
%%%    Module defining the Contract register transaction
%%% @end
%%%=============================================================================
-module(aect_create_tx).

-include("aecontract.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
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

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_create_tx, {
          owner      :: aec_id:id(),
          nonce      :: non_neg_integer(),
          code       :: binary(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: aect_contracts:amount(),
          deposit    :: aect_contracts:amount(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary(),
          ttl        :: aetx:tx_ttl()
        }).

-type amount() :: aect_contracts:amount().

-opaque tx() :: #contract_create_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Getters

-spec owner(tx()) -> aec_keys:pubkey().
owner(#contract_create_tx{owner = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

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

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#contract_create_tx{ttl = TTL}) ->
    TTL.

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
      fee        := Fee} = Args) ->
    Tx = #contract_create_tx{owner      = aec_id:create(account, OwnerPubKey),
                             nonce      = Nonce,
                             code       = Code,
                             vm_version = VmVersion,
                             deposit    = Deposit,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData,
                             fee        = Fee,
                             ttl        = maps:get(ttl, Args, 0)},
    {ok, aetx:new(?MODULE, Tx)}.

-spec type() -> atom().
type() ->
    ?CONTRACT_CREATE_TX_TYPE.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#contract_create_tx{} = Tx) ->
    owner(Tx).

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
                   {ok, aec_trees:trees()} | {error, term()}.
check(#contract_create_tx{nonce = Nonce,
                          vm_version = VmVersion,
                          call_data  = CallData,
                          amount     = Amount,
                          gas        = Gas,
                          gas_price  = GasPrice,
                          deposit    = Deposit,
                          fee = Fee} = Tx, _Context, Trees, _Height, _ConsensusVersion
     ) when ?is_non_neg_integer(GasPrice) ->
    OwnerPubKey = owner(Tx),
    TotalAmount = Fee + Amount + Deposit + Gas * GasPrice,
    Checks =
        [fun() ->
                 aetx_utils:check_account(OwnerPubKey, Trees, Nonce, TotalAmount)
         end |
         case VmVersion of
            ?AEVM_01_Sophia_01 ->
                 [fun() ->
                          case aeso_data:get_function_from_calldata(CallData) of
                              {ok, <<"init">>} -> ok;
                              _Other -> {error, bad_init_function}
                          end
                  end
                 ];
            ?AEVM_01_Solidity_01 -> []
         end
         %% TODO: Check minum gas price.
        ],
    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#contract_create_tx{} = Tx, _) ->
    {ok, [owner(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#contract_create_tx{nonce      = Nonce,
			    vm_version = _VmVersion,
                            amount     = Amount,
                            gas        = _Gas,
                            gas_price  = GasPrice,
                            deposit    = _Deposit,
                            fee        = Fee} = CreateTx,
        Context, Trees0, Height, ConsensusVersion) ->
    OwnerPubKey = owner(CreateTx),

    {ContractPubKey, Contract, Trees1} = create_contract(CreateTx, Trees0),

    %% Charge the fee to the contract owner (caller)
    %% and transfer the funds (amount) to the contract account.
    Trees2 =
        spend(OwnerPubKey, ContractPubKey, Amount, Fee,
              Nonce, Context, Height, Trees1, ConsensusVersion),

    %% Create the init call.
    Call0 = aect_call:new(OwnerPubKey, Nonce, ContractPubKey, Height, GasPrice),
    %% Execute init calQl to get the contract state and return value
    {CallRes, Trees3} =
        run_contract(CreateTx, Call0, Height, Trees2, Contract, ContractPubKey),

    case aect_call:return_type(CallRes) of
        ok ->
            initialize_contract(CreateTx, ContractPubKey, Contract,
                                CallRes, Context, Trees3, Height,
                                ConsensusVersion);
        E ->
            lager:debug("Init call error ~w ~w~n",[E, CallRes]),
            %% Don't create the contract if 'init' fails!
            %% Go back to state trees without contract or any account changes
            %% Spend gas + fee
            %% (The VM will decide how much gas is used: 0, some, all.)
            Trees5 = aect_utils:insert_call_in_trees(CallRes, Trees0),
            GasCost = aect_call:gas_used(CallRes) * GasPrice,
            Trees6 =
                spend(OwnerPubKey, ContractPubKey, 0, Fee+GasCost, Nonce,
                      Context, Height, Trees5, ConsensusVersion),
            {ok, Trees6}
    end.


create_contract(CreateTx, Trees0) ->
    %% Create the contract and insert it into the contract state tree
    %%   The public key for the contract is generated from the owners pubkey
    %%   and the nonce, so that no one has the private key.
    Contract        = aect_contracts:new(CreateTx),
    ContractPubKey  = aect_contracts:pubkey(Contract),
    ContractsTree0  = aec_trees:contracts(Trees0),
    ContractsTree1  = aect_state_tree:insert_contract(Contract, ContractsTree0),
    Trees1          = aec_trees:set_contracts(Trees0, ContractsTree1),
    {ContractPubKey, Contract, Trees1}.


spend(SenderPubKey, ReceiverPubKey, Value, Fee, Nonce,
      Context, Height, Trees, ConsensusVersion) ->
    {ok, SpendTx} = aec_spend_tx:new(
                      #{ sender => aec_id:create(account, SenderPubKey)
                       , recipient => aec_id:create(account, ReceiverPubKey)
                       , amount => Value
                       , fee => Fee
                       , ttl => Height
                       , nonce => Nonce
                       , payload => <<>>}),
    Trees1 = aec_trees:ensure_account(ReceiverPubKey, Trees),
    case Context of
        aetx_contract ->
            {ok, Trees2} =
                aetx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion),
            Trees2;
        aetx_transaction ->
            {ok, Trees2} =
                aetx:process(SpendTx, Trees1, Height, ConsensusVersion),
            Trees2
    end.


run_contract(#contract_create_tx{ nonce      =_Nonce
				, code       = Code
				, vm_version = VmVersion
				, amount     =_Amount
				, gas        = Gas
				, gas_price  = GasPrice
				, call_data  = CallData
				} = Tx,
	     Call, Height, Trees,_Contract, ContractPubKey)->
    Caller = owner(Tx),
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

initialize_contract(#contract_create_tx{nonce      = Nonce,
                                        vm_version = VmVersion,
                                        amount     =_Amount,
                                        gas        =_Gas,
                                        gas_price  = GasPrice,
                                        deposit    = Deposit,
                                        fee   =_Fee} = Tx,
                    ContractPubKey, Contract,
                    CallRes,  Context, Trees, Height, ConsensusVersion) ->
    OwnerPubKey = owner(Tx),

    %% Insert the call into the state tree for one block.
    %% This is mainly to make the return value accessible.
    %% Each block starts with an empty calls tree.
    Trees1 = aect_utils:insert_call_in_trees(CallRes, Trees),

    %% Spend Gas and burn
    %% Deposit (the deposit is stored in the contract.)
    GasCost = aect_call:gas_used(CallRes) * GasPrice,
    Trees2 =
        spend(OwnerPubKey, ContractPubKey, 0, Deposit+GasCost, Nonce,
              Context, Height, Trees1,
              ConsensusVersion),

    %% TODO: Move ABI specific code to abi module(s).
    Contract1 =
        case VmVersion of
            ?AEVM_01_Sophia_01 ->
                %% Save the initial state (returned by `init`) in the store.
                InitState  = aect_call:return_value(CallRes),
                %% TODO: move to/from_sophia_state to make nicer dependencies?
                aect_contracts:set_state(
                  aevm_eeevm_store:from_sophia_state(InitState), Contract);
            ?AEVM_01_Solidity_01 ->
                %% Solidity inital call returns the code to store in the contract.
                NewCode = aect_call:return_value(CallRes),
                aect_contracts:set_code(NewCode, Contract)
        end,
    ContractsTree0 = aec_trees:contracts(Trees2),
    ContractsTree1 = aect_state_tree:enter_contract(Contract1, ContractsTree0),
    {ok, aec_trees:set_contracts(Trees2, ContractsTree1)}.




serialize(#contract_create_tx{owner      = OwnerId,
                              nonce      = Nonce,
                              code       = Code,
                              vm_version = VmVersion,
                              fee        = Fee,
                              ttl        = TTL,
                              deposit    = Deposit,
                              amount     = Amount,
                              gas        = Gas,
                              gas_price  = GasPrice,
                              call_data  = CallData}) ->
    {version(),
     [ {owner, OwnerId}
     , {nonce, Nonce}
     , {code, Code}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {deposit, Deposit}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CREATE_TX_VSN,
            [ {owner, OwnerId}
            , {nonce, Nonce}
            , {code, Code}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {deposit, Deposit}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    account = aec_id:specialize_type(OwnerId),
    #contract_create_tx{owner      = OwnerId,
                        nonce      = Nonce,
                        code       = Code,
                        vm_version = VmVersion,
                        fee        = Fee,
                        ttl        = TTL,
                        deposit    = Deposit,
                        amount     = Amount,
                        gas        = Gas,
                        gas_price  = GasPrice,
                        call_data  = CallData}.

serialization_template(?CONTRACT_CREATE_TX_VSN) ->
    [ {owner, id}
    , {nonce, int}
    , {code, binary}
    , {vm_version, int}
    , {fee, int}
    , {ttl, int}
    , {deposit, int}
    , {amount, int}
    , {gas, int}
    , {gas_price, int}
    , {call_data, binary}
    ].

for_client(#contract_create_tx{ nonce      = Nonce,
                                code       = Code,
                                vm_version = VmVersion,
                                fee        = Fee,
                                ttl        = TTL,
                                deposit    = Deposit,
                                amount     = Amount,
                                gas        = Gas,
                                gas_price  = GasPrice,
                                call_data  = CallData} = Tx) ->
    #{<<"data_schema">> => <<"ContractCreateTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"owner">>       => aec_base58c:encode(account_pubkey, owner(Tx)),
      <<"nonce">>       => Nonce,
      <<"code">>        => aect_utils:hex_bytes(Code),
      <<"vm_version">>  => aect_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
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

