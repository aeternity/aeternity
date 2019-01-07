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

%% Additional getters
-export([owner_id/1,
         owner_pubkey/1,
         code/1,
         contract_pubkey/1,
         vm_version/1,
         deposit/1,
         amount/1,
         gas_limit/1,
         gas_price/1,
         call_data/1,
         call_id/1
        ]).

-define(CONTRACT_CREATE_TX_VSN, 1).
-define(CONTRACT_CREATE_TX_TYPE, contract_create_tx).

%% Should this be in a header file somewhere?
-define(PUB_SIZE, 32).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_create_tx, {
          owner_id   :: aec_id:id(),
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

-spec owner_id(tx()) -> aec_id:id().
owner_id(#contract_create_tx{owner_id = OwnerId}) ->
    OwnerId.

-spec owner_pubkey(tx()) -> aec_keys:pubkey().
owner_pubkey(#contract_create_tx{owner_id = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

-spec code(tx()) -> binary().
code(#contract_create_tx{code = X}) ->
    X.

-spec contract_pubkey(tx()) -> aec_keys:pubkey().
contract_pubkey(#contract_create_tx{} = Tx) ->
    aect_contracts:compute_contract_pubkey(owner_pubkey(Tx), nonce(Tx)).

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
gas(#contract_create_tx{} = Tx) ->
    gas_limit(Tx).

-spec gas_limit(tx()) -> amount().
gas_limit(#contract_create_tx{gas = X}) ->
    X.

-spec gas_price(tx()) -> amount().
gas_price(#contract_create_tx{gas_price = X}) ->
    X.

-spec call_data(tx()) -> binary().
call_data(#contract_create_tx{call_data = X}) ->
    X.

-spec call_id(tx()) -> aect_call:id().
call_id(#contract_create_tx{} = Tx) ->
    aect_call:id(owner_pubkey(Tx), nonce(Tx), contract_pubkey(Tx)).

%%%===================================================================
%%% Behavior API

-spec fee(tx()) -> integer().
fee(#contract_create_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#contract_create_tx{ttl = TTL}) ->
    TTL.

-spec new(map()) -> {ok, aetx:tx()}.
new(#{owner_id   := OwnerId,
      nonce      := Nonce,
      code       := Code,
      vm_version := VmVersion,
      deposit    := Deposit,
      amount     := Amount,
      gas        := Gas,
      gas_price  := GasPrice,
      call_data  := CallData,
      fee        := Fee} = Args) ->
    account = aec_id:specialize_type(OwnerId),
    Tx = #contract_create_tx{owner_id   = OwnerId,
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
    owner_pubkey(Tx).

%% Owner should exist, and have enough funds for the fee, the amount
%% the deposit and the gas
-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_create_tx{nonce      = Nonce,
                          vm_version = VmVersion,
                          amount     = Amount,
                          gas        = Gas,
                          gas_price  = GasPrice,
                          deposit    = Deposit,
                          fee = Fee} = Tx,
      Trees, Env) when ?is_non_neg_integer(GasPrice) ->
    OwnerPubKey = owner_pubkey(Tx),
    Height = aetx_env:height(Env),
    RequiredAmount =
        case aetx_env:context(Env) of
            aetx_transaction -> Amount + Deposit + Fee + Gas * GasPrice;
            aetx_contract    -> Amount + Deposit
        end,
    Checks =
        [fun() ->
                 aetx_utils:check_account(OwnerPubKey, Trees, Nonce, RequiredAmount)
         end,
         fun() ->
                 case aect_contracts:is_legal_vm_version_at_height(create, VmVersion, Height) of
                     true  -> ok;
                     false -> {error, illegal_vm_version}
                 end
         end,
         fun() ->
                 if
                     ?IS_AEVM_SOPHIA(VmVersion) ->
                         case get_sophia_serialization(Tx) of
                             {ok, #{type_info := TypeInfo}} ->
                                 check_init_function(Tx, TypeInfo);
                             {error, _} = Err -> Err
                         end;
                     VmVersion =:= ?AEVM_01_Solidity_01 ->
                         ok
                 end
         end
        ],
    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

get_sophia_serialization(#contract_create_tx{code = SerializedCode}) ->
    try aect_sophia:deserialize(SerializedCode) of
        Map -> {ok, Map}
    catch
        _:_ -> {error, bad_sophia_code}
    end.

check_init_function(#contract_create_tx{call_data = CallData}, TypeInfo) ->
    case aeso_abi:get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
            case aeso_abi:function_name_from_type_hash(Hash, TypeInfo) of
                {ok, <<"init">>} -> ok;
                _ -> {error, bad_init_function}
            end;
        _Other -> {error, bad_init_function}
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(#contract_create_tx{} = Tx, _) ->
    {ok, [owner_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#contract_create_tx{owner_id   = OwnerId,
                            nonce      = Nonce,
                            amount     = Amount,
                            gas        = Gas,
                            gas_price  = GasPrice,
                            deposit    = Deposit,
                            fee        = Fee} = CreateTx,
        Trees0, Env) ->
    Height = aetx_env:height(Env),
    OwnerPubKey = owner_pubkey(CreateTx),

    {Contract, Trees1} = create_contract(CreateTx, Trees0),
    ContractId = aect_contracts:id(Contract),
    ContractPubKey  = aect_contracts:pubkey(Contract),

    %% Charge the fee, the gas (the unused portion will be refunded)
    %% and the deposit (stored in the contract) to the contract owner (caller),
    %% and transfer the funds (amount) to the contract account.
    %% Also bump nonce.
    Charges =
        case aetx_env:context(Env) of
            aetx_transaction -> Deposit + Fee + Gas * GasPrice;
            aetx_contract    -> Deposit
        end,
    Trees2 = spend(OwnerPubKey, ContractPubKey, Amount, Charges, Nonce,
                   Trees1, Env),

    %% Create the init call.
    Call0 = aect_call:new(OwnerId, Nonce, ContractId, Height, GasPrice),
    %% Execute init calQl to get the contract state and return value
    {CallRes, Trees3} =
        run_contract(CreateTx, Call0, Env, Trees2, Contract, ContractPubKey),

    case aect_call:return_type(CallRes) of
        ok ->
            initialize_contract(CreateTx, ContractPubKey, Contract,
                                CallRes, Trees3, Env);
        E ->
            lager:debug("Init call error ~w ~w~n",[E, CallRes]),
            %% Don't create the contract if 'init' fails!
            %% Go back to state trees without contract or any account changes
            %% Spend gas + fee
            %% (The VM will decide how much gas is used: 0, some, all.)
            Trees5 = aect_utils:insert_call_in_trees(CallRes, Trees0),
            UsedAmount = aect_call:gas_used(CallRes) * GasPrice,
            Trees6 = spend(OwnerPubKey, ContractPubKey, 0, Fee + UsedAmount, Nonce,
                           Trees5, Env),
            {ok, Trees6}
    end.


create_contract(CreateTx, Trees0) ->
    %% Create the contract and insert it into the contract state tree
    %%   The public key for the contract is generated from the owners pubkey
    %%   and the nonce, so that no one has the private key.
    Contract        = aect_contracts:new(CreateTx),
    ContractsTree0  = aec_trees:contracts(Trees0),
    ContractsTree1  = aect_state_tree:insert_contract(Contract, ContractsTree0),
    Trees1          = aec_trees:set_contracts(Trees0, ContractsTree1),
    {Contract, Trees1}.


spend(SenderPubKey, ReceiverPubKey, Value, Fee, Nonce,
      Trees, Env) ->
    Height = aetx_env:height(Env),
    {ok, SpendTx} = aec_spend_tx:new(
                      #{ sender_id    => aec_id:create(account, SenderPubKey)
                       , recipient_id => aec_id:create(account, ReceiverPubKey)
                       , amount       => Value
                       , fee          => Fee
                       , ttl          => Height
                       , nonce        => Nonce
                       , payload      => <<>>}),
    Trees1 = aec_trees:ensure_account(ReceiverPubKey, Trees),
    {ok, Trees2} = aetx:process(SpendTx, Trees1, Env),
    Trees2.

run_contract(#contract_create_tx{ nonce      =_Nonce
                                , code       = Code
                                , vm_version = VmVersion
                                , amount     =_Amount
                                , gas        = Gas
                                , gas_price  = GasPrice
                                , call_data  = CallData
                                } = Tx,
             Call, Env, Trees, Contract, ContractPubKey)->
    Caller = owner_pubkey(Tx),
    CallStack = [], %% TODO: should we have a call stack for create_tx also
                    %% when creating a contract in a contract.

    Store     = aect_contracts:state(Contract),
    CallDef = #{ caller      => Caller
               , contract    => ContractPubKey
               , gas         => Gas
               , gas_price   => GasPrice
               , call_data   => CallData
               , amount      => 0 %% Initial call takes no amount
               , call_stack  => CallStack
               , code        => Code
               , store       => Store
               , call        => Call
               , trees       => Trees
               , tx_env      => Env
               , off_chain   => false
               },
    aect_dispatch:run(VmVersion, CallDef).


initialize_contract(#contract_create_tx{vm_version = VmVersion,
                                        amount     =_Amount,
                                        gas        = Gas,
                                        gas_price  = GasPrice} = Tx,
                    _ContractPubKey, Contract,
                    CallRes, Trees, Env) ->
    OwnerPubKey = owner_pubkey(Tx),

    %% Insert the call into the state tree for one block.
    %% This is mainly to make the return type (ok | error | revert) accessible.
    %% The return value is cleared (empty binary) for init calls.
    %% Each block starts with an empty calls tree.
    ClearedCallRes = aect_call:set_return_value(<<>>, CallRes),
    Trees1 = aect_utils:insert_call_in_trees(ClearedCallRes, Trees),

    %% Refund unused gas.
    Trees2 =
        case aetx_env:context(Env) of
            aetx_transaction ->
                aect_utils:refund_unused_gas(OwnerPubKey, GasPrice, Gas, CallRes, Trees1);
            aetx_contract ->
                Trees1
        end,

    %% TODO: Move ABI specific code to abi module(s).
    Contract1 =
        case VmVersion of
            _ when ?IS_AEVM_SOPHIA(VmVersion) ->
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



serialize(#contract_create_tx{owner_id   = OwnerId,
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
     [ {owner_id, OwnerId}
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
            [ {owner_id, OwnerId}
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
    #contract_create_tx{owner_id   = OwnerId,
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
    [ {owner_id, id}
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

for_client(#contract_create_tx{ owner_id   = OwnerId,
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
    #{<<"owner_id">>    => aehttp_api_encoder:encode(id_hash, OwnerId),
      <<"nonce">>       => Nonce,
      <<"code">>        => aehttp_api_encoder:encode(contract_bytearray, Code),
      <<"vm_version">>  => aeu_hex:hexstring_encode(<<VmVersion:8>>),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"deposit">>     => Deposit,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aehttp_api_encoder:encode(contract_bytearray, CallData)}.

%%%===================================================================
%%% Internal functions

-spec version() -> non_neg_integer().
version() ->
    ?CONTRACT_CREATE_TX_VSN.

