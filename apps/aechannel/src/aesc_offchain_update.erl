-module(aesc_offchain_update).

-define(UPDATE_VSN, 1).

-define(OP_TRANSFER,        0).
-define(OP_WITHDRAW,        1).
-define(OP_DEPOSIT ,        2).
-define(OP_CREATE_CONTRACT, 3).
-define(OP_CALL_CONTRACT,   4).

-opaque update() :: {?OP_TRANSFER, aec_id:id(), aec_id:id(), non_neg_integer()}
        | {?OP_WITHDRAW | ?OP_DEPOSIT, aec_id:id(), non_neg_integer()}
        | {?OP_CREATE_CONTRACT, aec_id:id(), aect_contracts:vm_version(), binary(),
           non_neg_integer(), binary()}
        | {?OP_CALL_CONTRACT, aec_id:id(), aec_id:id(), aect_contracts:vm_version(),
           non_neg_integer(), aect_call:call(), [non_neg_integer()]}.

-export_type([update/0]).

-export([ op_transfer/3
        , op_deposit/2
        , op_withdraw/2
        , op_new_contract/5
        , op_call_contract/6
        ]).

-export([serialize/1,
         deserialize/1,
         for_client/1,
         apply_on_trees/4]).

-export([is_call/1,
         is_contract_create/1,
         extract_call/1,
         extract_caller/1]).

-spec op_transfer(aec_id:id(), aec_id:id(), non_neg_integer()) -> update().
op_transfer(From, To, Amount) ->
    account = aec_id:specialize_type(From),
    account = aec_id:specialize_type(To),
    {?OP_TRANSFER, From, To, Amount}.

-spec op_deposit(aec_id:id(), non_neg_integer()) -> update().
op_deposit(Acct, Amount) ->
    account = aec_id:specialize_type(Acct),
    {?OP_DEPOSIT, Acct, Amount}.

-spec op_withdraw(aec_id:id(), non_neg_integer()) -> update().
op_withdraw(Acct, Amount) ->
    account = aec_id:specialize_type(Acct),
    {?OP_WITHDRAW, Acct, Amount}.

-spec op_new_contract(aec_id:id(), aect_contracts:vm_version(), binary(),
           non_neg_integer(), binary()) -> update().
op_new_contract(OwnerId, VmVersion, Code, Deposit, CallData) ->
    account = aec_id:specialize_type(OwnerId),
    {?OP_CREATE_CONTRACT, OwnerId, VmVersion, Code, Deposit, CallData}.


-spec op_call_contract(aec_id:id(), aec_id:id(), aect_contracts:vm_version(),
                       non_neg_integer(), aect_call:call(), [non_neg_integer()]) -> update().
op_call_contract(CallerId, ContractId, VmVersion, Amount, CallData, CallStack) ->
    account = aec_id:specialize_type(CallerId),
    contract = aec_id:specialize_type(ContractId),
    {?OP_CALL_CONTRACT, CallerId, ContractId, VmVersion, Amount, CallData, CallStack}.

-spec apply_on_trees(update(), aec_trees:trees(), non_neg_integer(), map()) -> aec_trees:trees().
apply_on_trees({?OP_TRANSFER, FromId, ToId, Amount}, Trees0, _, Opts) ->
    From = account_pubkey(FromId),
    To = account_pubkey(ToId),
    Trees1 = remove_tokens(From, Amount, Trees0, Opts),
    add_tokens(To, Amount, Trees1);
apply_on_trees({?OP_DEPOSIT, ToId, Amount}, Trees, _, _Opts) ->
    To = account_pubkey(ToId),
    add_tokens(To, Amount, Trees);
apply_on_trees({?OP_WITHDRAW, FromId, Amount}, Trees, _, Opts) ->
    From = account_pubkey(FromId),
    remove_tokens(From, Amount, Trees, Opts);
apply_on_trees({?OP_CREATE_CONTRACT, OwnerId, VmVersion, Code, Deposit, CallData}, Trees, Round, Opts) ->
    Owner = account_pubkey(OwnerId),
    {ContractPubKey, _Contract, Trees1} =
        aect_channel_contract:new(Owner, Round, VmVersion, Code, Deposit, Trees),
    Trees2 = remove_tokens(Owner, Deposit, Trees1, Opts),
    Trees3 = create_account(ContractPubKey, Trees2),
    Trees4 = add_tokens(ContractPubKey, Deposit, Trees3),
    Call = aect_call:new(Owner, Round, ContractPubKey, Round, 0),
    _Trees = aect_channel_contract:run_new(ContractPubKey, Call, CallData,
                                           Round, Trees4);
apply_on_trees({?OP_CALL_CONTRACT, CallerId, ContractId, VmVersion, Amount, CallData, CallStack},
             Trees, Round, Opts) ->
    Caller = account_pubkey(CallerId),
    ContractPubKey = contract_pubkey(ContractId),
    Trees1 = remove_tokens(Caller, Amount, Trees, Opts),
    Trees2 = add_tokens(ContractPubKey, Amount, Trees1),
    Call = aect_call:new(Caller, Round, ContractPubKey, Round, 0),
    _Trees = aect_channel_contract:run(ContractPubKey, VmVersion, Call,
                                       CallData, CallStack, Round, Trees2).

-spec for_client(update()) -> map().
for_client({?OP_TRANSFER, From, To, Amount}) ->
    #{<<"op">> => <<"transfer">>,
      <<"from">> => aec_base58c:encode(id_hash, From),
      <<"to">> => aec_base58c:encode(id_hash, To),
      <<"am">>   => Amount};
for_client({?OP_WITHDRAW, To, Amount}) ->
    #{<<"op">> => <<"withdraw">>,
      <<"to">> => aec_base58c:encode(id_hash, To),
      <<"am">>   => Amount};
for_client({?OP_DEPOSIT, From, Amount}) ->
    #{<<"op">> => <<"deposit">>,
      <<"from">> => aec_base58c:encode(id_hash, From),
      <<"am">>   => Amount};
for_client({?OP_CREATE_CONTRACT, OwnerId, VmVersion, Code, Deposit, CallData}) ->
    #{<<"op">>          => <<"new_contract">>,
      <<"owner">>       => aec_base58c:encode(id_hash, OwnerId),
      <<"vm_version">>  => VmVersion,
      <<"code">>        => Code,
      <<"deposit">>     => Deposit,
      <<"call_data">>   => CallData};
for_client({?OP_CALL_CONTRACT, CallerId, ContractId, VmVersion, Amount, CallData, CallStack}) ->
    #{<<"op">>          => <<"contract_call">>,
      <<"caller">>      => aec_base58c:encode(id_hash, CallerId),
      <<"contract">>    => aec_base58c:encode(id_hash, ContractId),
      <<"vm_version">>  => VmVersion,
      <<"amount">>      => Amount,
      <<"call_data">>   => CallData,
      <<"call_stack">>  => CallStack}.

-spec serialize(update()) -> binary().
serialize(Update) ->
    Fields = update2fields(Update),
    Vsn = ?UPDATE_VSN,
    UpdateType = element(1, Update),
    aec_object_serialization:serialize(
      ut2type(UpdateType),
      Vsn,
      update_serialization_template(Vsn, UpdateType),
      Fields).

-spec deserialize(binary()) -> update().
deserialize(Bin) ->
    {Type, Vsn, RawFields} =
        aec_object_serialization:deserialize_type_and_vsn(Bin),
    UpdateType = type2ut(Type),
    Template = update_serialization_template(Vsn, UpdateType),
    Fields = aec_serialization:decode_fields(Template, RawFields),
    fields2update(UpdateType, Fields).

update2fields({?OP_TRANSFER, From, To, Amount}) ->
    [ {from,    From},
      {to,      To},
      {amount,  Amount}];
update2fields({?OP_DEPOSIT, From, Amount}) ->
    [ {from,    From},
      {amount,  Amount}];
update2fields({?OP_WITHDRAW, To, Amount}) ->
    [ {to,      To},
      {amount,  Amount}];
update2fields({?OP_CREATE_CONTRACT, OwnerId, VmVersion, Code, Deposit, CallData}) ->
    [ {owner, OwnerId},
      {vm_version, VmVersion},
      {code, Code},
      {deposit, Deposit},
      {call_data, CallData}];
update2fields({?OP_CALL_CONTRACT, CallerId, ContractId, VmVersion, Amount, CallData, CallStack}) ->
    [ {caller, CallerId},
      {contract, ContractId},
      {vm_version, VmVersion},
      {amount, Amount},
      {call_data, CallData},
      {call_stack, CallStack}].

fields2update(?OP_TRANSFER, [{from,   From},
                             {to,     To},
                             {amount, Amount}]) ->
    op_transfer(From, To, Amount);
fields2update(?OP_DEPOSIT, [{from,   From},
                            {amount, Amount}]) ->
    op_deposit(From, Amount);
fields2update(?OP_DEPOSIT, [{to,     To},
                            {amount, Amount}]) ->
    op_withdraw(To, Amount);
fields2update(?OP_CREATE_CONTRACT, [{owner, OwnerId},
                                    {vm_version, VmVersion},
                                    {code, Code},
                                    {deposit, Deposit},
                                    {call_data, CallData}]) ->
    op_new_contract(OwnerId, VmVersion, Code, Deposit, CallData);
fields2update(?OP_CALL_CONTRACT, [ {caller, CallerId},
                                    {contract, ContractId},
                                    {vm_version, VmVersion},
                                    {amount, Amount},
                                    {call_data, CallData},
                                    {call_stack, CallStack}]) ->
    op_call_contract(CallerId, ContractId, VmVersion, Amount, CallData, CallStack).


ut2type(?OP_TRANSFER)         -> channel_offchain_update_transfer;
ut2type(?OP_DEPOSIT)          -> channel_offchain_update_deposit;
ut2type(?OP_WITHDRAW)         -> channel_offchain_update_withdraw;
ut2type(?OP_CREATE_CONTRACT)  -> channel_offchain_update_create_contract;
ut2type(?OP_CALL_CONTRACT)    -> channel_offchain_update_call_contract.

type2ut(channel_offchain_update_transfer)         -> ?OP_TRANSFER;
type2ut(channel_offchain_update_deposit)          -> ?OP_DEPOSIT;
type2ut(channel_offchain_update_withdraw)         -> ?OP_WITHDRAW;
type2ut(channel_offchain_update_create_contract)  -> ?OP_CREATE_CONTRACT;
type2ut(channel_offchain_update_call_contract)    -> ?OP_CALL_CONTRACT.

update_serialization_template(?UPDATE_VSN, ?OP_TRANSFER) ->
    [ {from,    id},
      {to,      id},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_DEPOSIT) ->
    [ {from,    id},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_WITHDRAW) ->
    [ {to,      id},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_CREATE_CONTRACT) ->
    [ {owner,       id},
      {vm_version,  int},
      {code,        binary},
      {deposit,     int},
      {call_data,   binary}];
update_serialization_template(?UPDATE_VSN, ?OP_CALL_CONTRACT) ->
    [ {caller,      id},
      {contract,    id},
      {vm_version,  int},
      {amount,      int},
      {call_data,   binary},
      {call_stack,  [int]}].

check_min_amt(Amt, Opts) ->
    Reserve = maps:get(channel_reserve, Opts, 0),
    if Amt < Reserve ->
            erlang:error(insufficient_balance);
       true ->
            Amt
    end.

create_account(Pubkey, Trees) ->
    AccountTrees = aec_trees:accounts(Trees),
    %TODO none = aec_accounts_trees:lookup(Pubkey, Trees),
    Acc = aec_accounts:new(Pubkey, 0),
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).


add_tokens(Pubkey, Amount, Trees) ->
    AccountTrees = aec_trees:accounts(Trees),
    Acc0 = aec_accounts_trees:get(Pubkey, AccountTrees), %% enforce account is present
    {ok, Acc} = aec_accounts:earn(Acc0, Amount),
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).

remove_tokens(Pubkey, Amount, Trees, Opts) ->
    AccountTrees = aec_trees:accounts(Trees),
    Acc0 = aec_accounts_trees:get(Pubkey, AccountTrees),
    Balance = aec_accounts:balance(Acc0),
    check_min_amt(Balance - Amount, Opts),
    Nonce = aec_accounts:nonce(Acc0),
    {ok, Acc} = aec_accounts:spend(Acc0, Amount, Nonce), %no nonce bump
    AccountTrees1 = aec_accounts_trees:enter(Acc, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).

account_pubkey(Id) ->
    aec_id:specialize(Id, account).

contract_pubkey(Id) ->
    aec_id:specialize(Id, contract).

-spec extract_caller(update()) -> aec_keys:pubkey().
extract_caller({?OP_CALL_CONTRACT, CallerId, _, _, _, _, _}) ->
    account_pubkey(CallerId);
extract_caller({?OP_CREATE_CONTRACT, OwnerId, _, _, _, _}) ->
    account_pubkey(OwnerId).

-spec is_call(update()) -> boolean().
is_call({?OP_CALL_CONTRACT, _, _, _, _, _, _}) ->
      true;
is_call(_) ->
      false.

-spec is_contract_create(update()) -> boolean().
is_contract_create({?OP_CREATE_CONTRACT, _, _, _, _, _}) ->
      true;
is_contract_create(_) ->
      false.

-spec extract_call(aesc_offchain_update:update()) -> {aect_contracts:id(), aec_keys:pubkey()}
                                                     | not_call.
extract_call(Update) ->
    case Update of
        {?OP_CALL_CONTRACT, CallerId, ContractId, _, _, _, _} ->
            {contract_pubkey(ContractId), account_pubkey(CallerId)};
        _ -> not_call
    end.

