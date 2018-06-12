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
-export([caller/1,
         contract/1,
         vm_version/1,
         amount/1,
         gas/1,
         gas_price/1,
         call_data/1,
         call_stack/1]).

-define(CONTRACT_CALL_TX_VSN, 1).
-define(CONTRACT_CALL_TX_TYPE, contract_call_tx).
-define(CONTRACT_CALL_TX_FEE, 2).

-define(is_non_neg_integer(X), (is_integer(X) andalso (X >= 0))).

-record(contract_call_tx, {
          caller     :: aec_id:id(),
          nonce      :: integer(),
          contract   :: aec_id:id(),
          vm_version :: aect_contracts:vm_version(),
          fee        :: integer(),
          ttl        :: aetx:tx_ttl(),
          amount     :: aect_contracts:amount(),
          gas        :: aect_contracts:amount(),
          gas_price  :: aect_contracts:amount(),
          call_data  :: binary(),
          call_stack = [] :: [non_neg_integer()]
            %% addresses (the pubkey as an integer) of contracts on the call
            %% stack
          }).

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
    TTL = maps:get(ttl, Args, 0),
    %% TODO: There should be a difference between a contract as caller
    %% and an account as caller, but when this is created from
    %% contract the transaction is never serialized.
    Tx = #contract_call_tx{caller     = aec_id:create(account, CallerPubKey),
                           nonce      = Nonce,
                           contract   = aec_id:create(contract, Contract),
                           vm_version = VmVersion,
                           fee        = Fee,
                           ttl        = TTL,
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

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#contract_call_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#contract_call_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#contract_call_tx{} = Tx) ->
    caller(Tx).

%% CallerAccount should exist, and have enough funds for the fee + gas cost
%% Contract should exist and its vm_version should match the one in the call.
-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#contract_call_tx{nonce = Nonce,
                        fee = Fee, amount = Value,
                        gas = GasLimit, gas_price = GasPrice,
                        call_stack = CallStack
                       } = CallTx, Context, Trees, Height, _ConsensusVersion
     ) when ?is_non_neg_integer(GasPrice) ->
    CallerPubKey = caller(CallTx),
    Checks =
        case Context of
            aetx_transaction ->
                RequiredAmount = Fee + GasLimit * GasPrice + Value,
                [fun() -> aetx_utils:check_account(CallerPubKey, Trees, Nonce, RequiredAmount) end,
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

-spec signers(tx(), aec_trees:trees()) -> {ok, [aec_keys:pubkey()]}.
signers(Tx, _) ->
    {ok, [caller(Tx)]}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#contract_call_tx{nonce = Nonce,
                          fee = Fee, gas =_Gas, gas_price = GasPrice, amount = Value
                         } = CallTx, Context, Trees1, Height, ConsensusVersion) ->

    %% Transfer the attached funds to the callee (before calling the contract!)
    CallerPubKey = caller(CallTx),
    CalleePubKey = contract(CallTx),
    Trees2 = spend(CallerPubKey, CalleePubKey, Value,
                   Nonce, Context, Height, Trees1, ConsensusVersion),

    %% Create the call.
    Call0 = aect_call:new(caller(CallTx),
			  nonce(CallTx),
			  contract(CallTx),
			  Height,
                          aect_call_tx:gas_price(CallTx)),

    %% Run the contract code. Also computes the amount of gas left and updates
    %% the call object.
    {Call, Trees3} = run_contract(CallTx, Call0, Height, Trees2),

    %% Charge the fee and the used gas to the caller (not if called from another contract!)
    AccountsTree1 = aec_trees:accounts(Trees3),
    AccountsTree2 =
        case Context of
            aetx_contract    ->
                AccountsTree1;
            aetx_transaction ->
                %% When calling from the top-level we charge Fee and Gas as well.
                GasCost       = aect_call:gas_used(Call) * GasPrice,
                Amount        = Fee + GasCost,
                Caller2       = aec_accounts_trees:get(CallerPubKey, AccountsTree1),
                {ok, Caller3} = aec_accounts:spend(Caller2, Amount, Nonce),
                aec_accounts_trees:enter(Caller3, AccountsTree1)
        end,
    Trees4 = aec_trees:set_accounts(Trees3, AccountsTree2),

    %% Insert the call into the state tree. This is mainly to remember what the
    %% return value was so that the caller can access it easily.
    %% Each block starts with an empty calls tree.
    {ok, aect_utils:insert_call_in_trees(Call, Trees4)}.

spend(CallerPubKey, CalleePubKey, Value, Nonce,_Context, Height, Trees,
      ConsensusVersion) ->
    {ok, SpendTx} = aec_spend_tx:new(#{ sender => CallerPubKey
                                , recipient => CalleePubKey
                                , amount => Value
                                , fee => 0
                                , nonce => Nonce
                                , payload => <<>>}),
    {ok, Trees1} =
        aetx:check_from_contract(SpendTx, Trees, Height, ConsensusVersion),
    {ok, Trees2} =
        aetx:process_from_contract(SpendTx, Trees1, Height, ConsensusVersion),
    Trees2.

run_contract(#contract_call_tx{ nonce  = _Nonce
			      , vm_version = VmVersion
			      , amount     = Amount
			      , gas        = Gas
			      , gas_price  = GasPrice
			      , call_data  = CallData
			      , call_stack = CallStack
			      } = Tx, Call, Height, Trees) ->
    Caller        = caller(Tx),
    ContractPubKey= contract(Tx),
    ContractsTree = aec_trees:contracts(Trees),
    Contract      = aect_state_tree:get_contract(ContractPubKey, ContractsTree),
    Code          = aect_contracts:code(Contract),
    CallDef = #{ caller     => Caller
	       , contract   => ContractPubKey
	       , gas        => Gas
	       , gas_price  => GasPrice
	       , call_data  => CallData
	       , amount     => Amount
	       , call_stack => CallStack
	       , code       => Code
	       , call       => Call
	       , height     => Height
	       , trees      => Trees
	       },
    aect_dispatch:run(VmVersion, CallDef).

serialize(#contract_call_tx{caller     = CallerId,
                            nonce      = Nonce,
                            contract   = ContractId,
                            vm_version = VmVersion,
                            fee        = Fee,
                            ttl        = TTL,
                            amount     = Amount,
                            gas        = Gas,
                            gas_price  = GasPrice,
                            call_data  = CallData}) ->
    %% Note that the call_stack is not serialized. This is ok since we don't
    %% serialize transactions originating from contract execution, and for
    %% top-level transactions the call_stack is always empty.
    {version(),
     [ {caller, CallerId}
     , {nonce, Nonce}
     , {contract, ContractId}
     , {vm_version, VmVersion}
     , {fee, Fee}
     , {ttl, TTL}
     , {amount, Amount}
     , {gas, Gas}
     , {gas_price, GasPrice}
     , {call_data, CallData}
     ]}.

deserialize(?CONTRACT_CALL_TX_VSN,
            [ {caller, CallerId}
            , {nonce, Nonce}
            , {contract, ContractId}
            , {vm_version, VmVersion}
            , {fee, Fee}
            , {ttl, TTL}
            , {amount, Amount}
            , {gas, Gas}
            , {gas_price, GasPrice}
            , {call_data, CallData}]) ->
    account = aec_id:specialize_type(CallerId),
    contract = aec_id:specialize_type(ContractId),
    #contract_call_tx{caller     = CallerId,
                      nonce      = Nonce,
                      contract   = ContractId,
                      vm_version = VmVersion,
                      fee        = Fee,
                      ttl        = TTL,
                      amount     = Amount,
                      gas        = Gas,
                      gas_price  = GasPrice,
                      call_data  = CallData}.

serialization_template(?CONTRACT_CALL_TX_VSN) ->
    [ {caller, id}
    , {nonce, int}
    , {contract, id}
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

for_client(#contract_call_tx{nonce      = Nonce,
                             vm_version = VmVersion,
                             fee        = Fee,
                             ttl        = TTL,
                             amount     = Amount,
                             gas        = Gas,
                             gas_price  = GasPrice,
                             call_data  = CallData} = Tx) ->
    #{<<"data_schema">> => <<"ContractCallTxObject">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"caller">>      => aec_base58c:encode(account_pubkey, caller(Tx)),
      <<"nonce">>       => Nonce,
      <<"contract">>    => aec_base58c:encode(contract_pubkey, contract(Tx)),
      <<"vm_version">>  => aect_utils:hex_byte(VmVersion),
      <<"fee">>         => Fee,
      <<"ttl">>         => TTL,
      <<"amount">>      => Amount,
      <<"gas">>         => Gas,
      <<"gas_price">>   => GasPrice,
      <<"call_data">>   => aect_utils:hex_bytes(CallData)}.

%% -- Getters ----------------------------------------------------------------

-spec caller(tx()) -> aec_keys:pubkey().
caller(C) -> aec_id:specialize(C#contract_call_tx.caller, account).

-spec contract(tx()) -> aec_keys:pubkey().
contract(C) -> aec_id:specialize(C#contract_call_tx.contract, contract).

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
check_call(#contract_call_tx{ vm_version = VmVersion,
                              amount     = Value} = Tx,
               Trees, _Height) ->
    ContractPubKey = contract(Tx),
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
