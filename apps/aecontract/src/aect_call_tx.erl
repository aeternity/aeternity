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

-export([check_and_process_call/3]).

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

%% CallerAccount should exist, and have enough funds for the fee + gas
%% Contract should exist and its vm_version should match the one in the call.
-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#contract_call_tx{nonce = Nonce,
                        fee = Fee, amount = Value,
                        gas = GasLimit, gas_price = GasPrice,
                        call_stack = CallStack
                       } = CallTx,
      Trees, Env) when ?is_non_neg_integer(GasPrice) ->
    Height = aetx_env:height(Env),
    CallerPubKey = caller_pubkey(CallTx),
    Checks =
        case aetx_env:context(Env) of
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
    {ok, [caller_pubkey(Tx)]}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(Tx, Trees, Env) ->
    {ok, Call, Trees1} = process_call(Tx, Trees, Env),

    %% Insert the call into the state tree. This is mainly to remember what the
    %% return value was so that the caller can access it easily.
    %% Each block starts with an empty calls tree. Only for the top-level transaction.
    case aetx_env:context(Env) of
        aetx_transaction -> {ok, aect_utils:insert_call_in_trees(Call, Trees1)};
        aetx_contract    -> {ok, Trees1}
    end.

-spec check_and_process_call(tx(), aec_trees:trees(), aetx_env:env()) ->
                                {ok, aect_call:call(), aec_trees:trees()}
                              | {error, term()}.
check_and_process_call(Tx, Trees, Env) ->
    case check(Tx, Trees, Env) of
        {ok, Trees1} ->
            process_call(Tx, Trees1, Env);
        {error, _} = Err ->
            Err
    end.

%% Process a call transaction and return the call object, without writing it to the state trees.
process_call(#contract_call_tx{caller_id   = CallerId,
                               nonce       = Nonce,
                               contract_id = ContractId,
                               fee         = Fee,
                               gas         = Gas,
                               gas_price   = GasPrice,
                               amount      = Value} = CallTx,
        Trees1, Env) ->
    Height = aetx_env:height(Env),

    %% Transfer the attached funds to the callee (before calling the contract!).
    %% When calling from the top-level we charge Fee and Gas as well.
    %% Also bump nonce.
    Charges =
        case aetx_env:context(Env) of
            aetx_transaction -> Fee + Gas * GasPrice;
            aetx_contract    -> 0
        end,
    CallerPubkey = caller_pubkey(CallTx),
    ContractPubkey = contract_pubkey(CallTx),
    Trees2 = spend(CallerPubkey, ContractPubkey, Value, Charges, Nonce, Trees1, Env),

    %% Create the call.
    Call0 = aect_call:new(CallerId, Nonce, ContractId, Height, GasPrice),

    %% Run the contract code. Also computes the amount of gas left and updates
    %% the call object.
    {Call, Trees3} = run_contract(CallTx, Call0, Env, Trees2),

    %% Roll back state changes if call failed
    Trees4 =
        case aect_call:return_type(Call) of
            ok -> Trees3;
            E  ->
                lager:debug("Contract call error ~w ~w~n",[E, Call]),
                %% We still need to charge gas and fee for the top-level call
                case aetx_env:context(Env) of
                    aetx_transaction -> spend(CallerPubkey, ContractPubkey, 0, Charges, Nonce, Trees1, Env);
                    aetx_contract    -> Trees1
                end
        end,

    %% Refund unused gas.
    Trees5 =
        case aetx_env:context(Env) of
            aetx_transaction ->
                aect_utils:refund_unused_gas(CallerPubkey, GasPrice, Gas, Call, Trees4);
            aetx_contract ->
                Trees4
        end,

    {ok, Call, Trees5}.

spend(CallerPubkey, ContractPubkey, Value, Fee, Nonce, Trees, Env) ->
    {ok, SpendTx} =
        aec_spend_tx:new(#{ sender_id    => aec_id:create(account, CallerPubkey)
                          , recipient_id => aec_id:create(account, ContractPubkey)
                          , amount       => Value
                          , fee          => Fee
                          , nonce        => Nonce
                          , payload      => <<>>}),
    %% These spends should always be evaluated in contract context
    %% (i.e., no checks for minimum fee etc)
    Env1 = aetx_env:set_context(Env, aetx_contract),
    {ok, Trees1} = aetx:process(SpendTx, Trees, Env1),
    Trees1.

run_contract(#contract_call_tx{ nonce  = _Nonce
                              , vm_version = VmVersion
                              , amount     = Amount
                              , gas        = Gas
                              , gas_price  = GasPrice
                              , call_data  = CallData
                              , call_stack = CallStack
                              } = Tx, Call, Env, Trees) ->
    CallerPubkey   = caller_pubkey(Tx),
    ContractPubkey = contract_pubkey(Tx),
    ContractsTree  = aec_trees:contracts(Trees),
    Contract       = aect_state_tree:get_contract(ContractPubkey, ContractsTree),
    Code           = aect_contracts:code(Contract),
    Store          = aect_contracts:state(Contract),
    CallDef = #{ caller     => CallerPubkey
               , contract   => ContractPubkey
               , gas        => Gas
               , gas_price  => GasPrice
               , call_data  => CallData
               , amount     => Amount
               , call_stack => CallStack
               , code       => Code
               , store      => Store
               , call       => Call
               , trees      => Trees
               , tx_env     => Env
               , off_chain  => false
               },
    aect_dispatch:run(VmVersion, CallDef).

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

%% -- Local functions  -------------------------------------------------------

%% Check that the contract exists and has the right VM version.
check_call(#contract_call_tx{ vm_version = VmVersion,
                              amount     = Value} = Tx,
               Trees, _Height) ->
    ContractPubKey = contract_pubkey(Tx),
    ContractsTree = aec_trees:contracts(Trees),
    %% Dialyzer, in its infinite wisdom, complains if it thinks we're checking
    %% that something of type non_neg_integer() is negative. Since Dialyzer
    %% doesn't _actually_ guarantee that this isn't the case, we do need the
    %% check and this is the least convoluted way of writing it that Dialyzer's
    %% static analysis cannot see through.
    NegativeAmount = -Value > 0,
    case aect_state_tree:lookup_contract(ContractPubKey, ContractsTree, [no_store]) of
        _ when NegativeAmount -> {error, negative_amount};
        {value, C} ->
            case aect_contracts:vm_version(C) == VmVersion of
                true  -> ok;
                false -> {error, wrong_vm_version}
            end;
        none -> {error, contract_does_not_exist}
    end.
