-module(aesc_offchain_state).

-include_lib("apps/aecore/include/blocks.hrl").
-include("apps/aecontract/src/aecontract.hrl").

-record(state, { trees                  :: aec_trees:trees()
               , signed_txs = []        :: [aetx_sign:signed_tx()]
               , half_signed_txs = []   :: [aetx_sign:signed_tx()]
              }).

-opaque state() :: #state{}.

-define(UPDATE_VSN, 1).

-define(OP_TRANSFER,        0).
-define(OP_WITHDRAW,        1).
-define(OP_DEPOSIT ,        2).
-define(OP_CREATE_CONTRACT, 3).
-define(OP_CALL_CONTRACT,   4).

-type transfer_operation() :: ?OP_TRANSFER | ?OP_WITHDRAW | ?OP_DEPOSIT.

-opaque update() :: {transfer_operation(), aec_keys:pubkey(), aec_keys:pubkey(), non_neg_integer()}
        | {?OP_CREATE_CONTRACT, aec_keys:pubkey(), aect_contracts:vm_version(), binary(),
           non_neg_integer(), binary()}
        | {?OP_CALL_CONTRACT, aec_keys:pubkey(), aect_contracts:id(), aect_contracts:vm_version(),
           non_neg_integer(), aect_call:call(), [non_neg_integer()]}.

-export_type([state/0, update/0]).

-export([ new/1                       %%  (Opts) -> {ok, Tx, State}
        , check_initial_update_tx/3   %%  (SignedTx, State, Opts)
        , check_update_tx/3           %%  (SignedTx, State, Opts)
        , check_reestablish_tx/2      %%  (SignedTx, State) -> {ok,NewSt} | error()
        , is_latest_signed_tx/2       %%  (SignedTx, State) -> boolean()
        , verify_signatures/2         %%  (SignedTx, State)
        , make_update_tx/3            %%  (Updates, State, Opts) -> Tx
        , add_signed_tx/3             %%  (SignedTx, State0, Opts) -> State
        , add_half_signed_tx/2        %%  (SignedTx, State0) -> State
        , get_latest_half_signed_tx/1 %%  (State) -> SignedTx
        , get_latest_signed_tx/1      %%  (State) -> {Round, SignedTx}
        , get_fallback_state/1        %%  (State) -> {Round, State'}
        , fallback_to_stable_state/1  %%  (State) -> State'
        , hash/1                      %%  (State) -> hash()
        , update_for_client/1
        , balance/2                   %%  (Pubkey, State) -> Balance
        ]).

-export([get_contract_call/4
        ]).
-export([ op_transfer/3
        , op_deposit/2
        , op_withdraw/2
        , op_new_contract/5
        , op_call_contract/6
        ]).

-export([serialize_update/1,
         deserialize_update/1]).

-spec new(map()) -> {ok, state()}.
new(Opts) ->
    lager:debug("offchain_tx:new(~p)", [Opts]),
    case Opts of
        #{existing_channel_id := _
         , offchain_tx        := _ } ->
            recover_from_offchain_tx(Opts);
        #{initiator          := _,
          responder          := _,
          initiator_amount   := _,
          responder_amount   := _} ->
            new_(Opts)
    end.

new_(#{ initiator          := InitiatorPubKey
      , responder          := ResponderPubKey
      , initiator_amount   := InitiatorAmount
      , responder_amount   := ResponderAmount }) ->
    Trees0 = aec_trees:new_without_backend(),
    Accounts =
        lists:foldl(
            fun({Pubkey, Amount}, AccTree) ->
            Account = aec_accounts:new(Pubkey, Amount),
            aec_accounts_trees:enter(Account, AccTree)
        end,
        aec_trees:accounts(Trees0),
        [{InitiatorPubKey, InitiatorAmount},
         {ResponderPubKey, ResponderAmount}]),
    Trees = aec_trees:set_accounts(Trees0, Accounts),
    {ok, #state{trees=Trees}}.

recover_from_offchain_tx(#{ existing_channel_id := ChId
                          , offchain_tx         := SignedTx } = Opts) ->
    case aesc_state_cache:reestablish(ChId, my_pubkey(Opts)) of
        {ok, #state{} = State} ->
            case is_latest_signed_tx(SignedTx, State) of
                true ->
                    {ok, State};
                false ->
                    {error, latest_state_mismatch}
            end;
        error ->
            {error, state_tree_missing}
    end.

-spec hash(state()) -> binary().
hash(#state{trees=Trees}) ->
    aec_trees:hash(Trees).

my_pubkey(#{role := responder, responder := R}) -> R;
my_pubkey(#{role := initiator, initiator := I}) -> I.

is_latest_signed_tx(SignedTx, #state{signed_txs = [LatestSignedTx|_]}) ->
    aetx_sign:serialize_to_binary(SignedTx)
        == aetx_sign:serialize_to_binary(LatestSignedTx);
is_latest_signed_tx(_, _) ->
    false.

%% update_tx checks
check_initial_state({previous_round, N}, _) ->
    assert(N =:= 0, {error, not_initial_round});
check_initial_state({round, N}, _) ->
    assert(N =:= 1, {error, invalid_round});
check_initial_state({updates, Ds}, _) ->
    assert(Ds == [], {error, updates_in_initial_round});
check_initial_state(_, _) -> ok.

assert(true ,  _   ) -> ok;
assert(false, Error) -> Error.

-spec check_initial_update_tx(aetx_sign:signed_tx(), state(), map()) -> ok | {error, atom()}.
check_initial_update_tx(SignedTx, State, Opts) ->
    check_update_tx(fun check_initial_state/2, SignedTx, State, Opts).

-spec check_reestablish_tx(aetx_sign:signed_tx(), state()) -> {ok, state()} | {error, atom()}.
check_reestablish_tx(SignedTx, State) ->
    lager:debug("check_reestablish_tx()", []),
    case mutually_signed(SignedTx) of
        true ->
            case is_latest_signed_tx(SignedTx, State) of
                true ->
                    {ok, State};
                false ->
                    {error, not_latest_state}
            end;
        false ->
            {error, not_mutually_signed}
    end.

-spec check_update_tx(aetx_sign:signed_tx(), state(), map()) -> ok | {error, atom()}.
check_update_tx(SignedTx, State, Opts) ->
    check_update_tx(none, SignedTx, State, Opts).

check_update_tx(F, SignedTx, #state{signed_txs = Txs}=State, Opts) ->
    lager:debug("check_update_tx(State = ~p)", [State]),
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    lager:debug("Tx = ~p", [Tx]),
    case Mod:round(TxI) - 1 of
        0 when Txs == [] ->
            lager:debug("previous round = 0", []),
            check_update_tx_(F, Mod, TxI, State, Opts);
        PrevRound ->
            lager:debug("PrevRound = ~p", [PrevRound]),
            {LastRound, _LastSignedTx} = get_latest_signed_tx(State),
            lager:debug("LastRound = ~p", [LastRound]),
            case PrevRound == LastRound of
                true ->
                    lager:debug("PrevRound == LastRound", []),
                    check_update_tx_(F, Mod, TxI, State, Opts);
                false -> {error, invalid_previous_round}
            end
    end.

check_update_tx_(F, Mod, RefTx, #state{} = State, Opts) ->
    Updates = Mod:updates(RefTx),
    try Tx1 = make_update_tx(Updates, State, Opts),
         {Mod1, Tx1I} = aetx:specialize_callback(Tx1),
         case Mod1:state_hash(Tx1I) =:= Mod:state_hash(RefTx) of
             true ->
                 run_extra_checks(F, Mod, RefTx);
             false ->
                 {error, state_hash_mismatch}
         end
    catch
        error:Reason ->
            {error, Reason}
    end.

-spec verify_signatures(aetx_sign:signed_tx(), state()) -> ok | error.
verify_signatures(SignedTx, #state{trees = Trees}) ->
    aetx_sign:verify(SignedTx, Trees).

get_contract_call(Contract, Caller, Round, #state{trees=Trees}) ->
    aect_channel_contract:get_call(Contract, Caller, Round, Trees).

-spec make_update_tx(list(update()), state(), map()) -> aetx:tx().
make_update_tx(Updates, #state{signed_txs=[SignedTx|_], trees=Trees}, Opts) ->
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    ChannelId = Mod:channel_id(TxI),

    NextRound = Mod:round(TxI) + 1,

    Trees1 = apply_updates(Updates, NextRound, Trees, Opts),
    StateHash = aec_trees:hash(Trees1),
    {ok, OffchainTx} =
        aesc_offchain_tx:new(#{channel_id    => aec_id:create(channel, ChannelId),
                              state_hash     => StateHash,
                              updates        => Updates,
                              round          => NextRound}),
    OffchainTx.

apply_updates(Updates, Round, Trees, Opts) ->
    lists:foldl(
        fun(U, AccumTrees) -> modify_trees(U, AccumTrees, Round, Opts) end,
        Trees,
        Updates).

-spec modify_trees(update(), aec_trees:trees(), non_neg_integer(), map()) -> aec_trees:trees().
modify_trees({?OP_TRANSFER, From, To, Amount}, Trees0, _, Opts) ->
    Trees1 = remove_tokens(From, Amount, Trees0, Opts),
    add_tokens(To, Amount, Trees1);
modify_trees({?OP_DEPOSIT, To, To, Amount}, Trees, _, _Opts) ->
    add_tokens(To, Amount, Trees);
modify_trees({?OP_WITHDRAW, From, From, Amount}, Trees, _, Opts) ->
    remove_tokens(From, Amount, Trees, Opts);
modify_trees({?OP_CREATE_CONTRACT, Owner, VmVersion, Code, Deposit, CallData}, Trees, Round, Opts) ->
    {ContractPubKey, _Contract, Trees1} =
        aect_channel_contract:new(Owner, Round, VmVersion, Code, Deposit, Trees),
    Trees2 = remove_tokens(Owner, Deposit, Trees1, Opts),
    Trees3 = create_account(ContractPubKey, Trees2),
    Trees4 = add_tokens(ContractPubKey, Deposit, Trees3),
    Call = aect_call:new(Owner, Round, ContractPubKey, Round, 0),
    _Trees = aect_channel_contract:run_new(ContractPubKey, Call, CallData,
                                           Round, Trees4);
modify_trees({?OP_CALL_CONTRACT, Caller, ContractPubKey, VmVersion, Amount, CallData, CallStack},
             Trees, Round, Opts) ->
    Trees1 = remove_tokens(Caller, Amount, Trees, Opts),
    Trees2 = add_tokens(ContractPubKey, Amount, Trees1),
    Call = aect_call:new(Caller, Round, ContractPubKey, Round, 0),
    _Trees = aect_channel_contract:run(ContractPubKey, VmVersion, Call,
                                       CallData, CallStack, Round, Trees2).

check_min_amt(Amt, Opts) ->
    Reserve = maps:get(channel_reserve, Opts, 0),
    if Amt < Reserve ->
            erlang:error(insufficient_balance);
       true ->
            Amt
    end.

run_extra_checks(none, _, _) -> ok;
run_extra_checks(F, Mod, Tx) when is_function(F, 2) ->
    {_Vsn, Vals} = Mod:serialize(Tx),
    case [Err ||
             {error,_} = Err <- [F(E, Tx) || E <- Vals]] of
        [] ->
            ok;
        [_|_] = Errors ->
            {error, Errors}
    end.

-spec add_signed_tx(aetx_sign:signed_tx(), state(), map()) -> state().
add_signed_tx(SignedTx, #state{signed_txs=Txs0}=State, Opts) ->
    true = mutually_signed(SignedTx), % ensure it is mutually signed
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_callback(Tx) of
        {aesc_create_tx, _} ->
            State#state{signed_txs=[SignedTx | Txs0], half_signed_txs=[]};
        {Mod, TxI} ->
            Trees =
                lists:foldl(
                    fun(Update, TrAccum) ->
                        TrAccum1 = modify_trees(Update, TrAccum, Mod:round(TxI), Opts),
                        TrAccum1
                    end,
                    State#state.trees,
                    Mod:updates(TxI)),
            State#state{signed_txs=[SignedTx | Txs0], half_signed_txs=[], trees=Trees}
    end.

-spec add_half_signed_tx(aetx_sign:signed_tx(), state()) -> state().
add_half_signed_tx(SignedTx, #state{half_signed_txs=Txs0}=State) ->
    State#state{half_signed_txs=[SignedTx | Txs0]}.

-spec get_latest_half_signed_tx(state()) -> aetx_sign:signed_tx().
get_latest_half_signed_tx(#state{half_signed_txs=[Tx| _]}) ->
    Tx.

-spec get_latest_signed_tx(state()) -> {non_neg_integer(), aetx_sign:signed_tx()}.
get_latest_signed_tx(#state{signed_txs=[SignedTx|_]}) ->
    {tx_round(aetx_sign:tx(SignedTx)), SignedTx}.

-spec get_fallback_state(state()) -> {non_neg_integer(), state()}.
get_fallback_state(#state{signed_txs=[SignedTx|_]}=State) -> %% half_signed_txs= []?
    {tx_round(aetx_sign:tx(SignedTx)), State#state{half_signed_txs=[]}}.

-spec op_transfer(aec_keys:pubkey(), aec_keys:pubkey(), non_neg_integer()) -> update().
op_transfer(From, To, Amount) ->
    {?OP_TRANSFER, From, To, Amount}.

-spec op_deposit(aec_keys:pubkey(), non_neg_integer()) -> update().
op_deposit(Acct, Amount) ->
    {?OP_DEPOSIT, Acct, Acct, Amount}.

-spec op_withdraw(aec_keys:pubkey(), non_neg_integer()) -> update().
op_withdraw(Acct, Amount) ->
    {?OP_WITHDRAW, Acct, Acct, Amount}.

-spec op_new_contract(aec_keys:pubkey(), aect_contracts:vm_version(), binary(),
           non_neg_integer(), binary()) -> update().
op_new_contract(Owner, VmVersion, Code, Deposit, CallData) ->
    {?OP_CREATE_CONTRACT, Owner, VmVersion, Code, Deposit, CallData}.


-spec op_call_contract(aec_keys:pubkey(), aect_contracts:id(), aect_contracts:vm_version(),
                       non_neg_integer(), aect_call:call(), [non_neg_integer()]) -> update().
op_call_contract(Caller, ContractPubKey, VmVersion, Amount, CallData, CallStack) ->
    {?OP_CALL_CONTRACT, Caller, ContractPubKey, VmVersion, Amount, CallData, CallStack}.

tx_round(Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Mod:round(TxI).

-spec fallback_to_stable_state(state()) -> state().
fallback_to_stable_state(#state{signed_txs=[_|_]}=State) ->
    State#state{half_signed_txs=[]}.

-spec mutually_signed(aetx_sign:signed_tx()) -> boolean().
mutually_signed(SignedTx) ->
    case aetx_sign:signatures(SignedTx) of
        [_, _] ->
            %% mutually signed
            true;
        _ ->
            false
    end.

-spec update_for_client(update()) -> map().
update_for_client({?OP_TRANSFER, From, To, Amount}) ->
    #{<<"op">> => <<"transfer">>,
      <<"from">> => From,
      <<"to">>   => To,
      <<"am">>   => Amount};
update_for_client({?OP_WITHDRAW, To, To, Amount}) ->
    #{<<"op">> => <<"withdraw">>,
      <<"to">>   => To,
      <<"am">>   => Amount};
update_for_client({?OP_DEPOSIT, From, From, Amount}) ->
    #{<<"op">> => <<"deposit">>,
      <<"from">>   => From,
      <<"am">>   => Amount}.

-spec balance(aec_keys:pubkey(), state()) -> {ok, non_neg_integer()}
                                           | {error, not_found}.
balance(Pubkey, #state{trees=Trees}) ->
    AccTrees = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(Pubkey, AccTrees) of
        none -> {error, not_found};
        {value, Account} -> {ok, aec_accounts:balance(Account)}
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


serialize_update(Update) ->
    Fields = update2fields(Update),
    Vsn = ?UPDATE_VSN,
    UpdateType = element(1, Update),
    aec_object_serialization:serialize(
      ut2type(UpdateType),
      Vsn,
      update_serialization_template(Vsn, UpdateType),
      Fields).

deserialize_update(Bin) ->
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
update2fields({?OP_DEPOSIT, From, From, Amount}) ->
    [ {from,    From},
      {to,      From},
      {amount,  Amount}];
update2fields({?OP_WITHDRAW, To, To, Amount}) ->
    [ {from,    To},
      {to,      To},
      {amount,  Amount}];
update2fields({?OP_CREATE_CONTRACT, Owner, VmVersion, Code, Deposit, CallData}) ->
    [ {owner, Owner},
      {vm_version, VmVersion},
      {code, Code},
      {deposit, Deposit},
      {call_data, CallData}];
update2fields({?OP_CALL_CONTRACT, Caller, ContractPubKey, VmVersion, Amount, CallData, CallStack}) ->
    [ {caller, Caller},
      {contract, ContractPubKey},
      {vm_version, VmVersion},
      {amount, Amount},
      {call_data, CallData},
      {call_stack, CallStack}].

fields2update(?OP_TRANSFER, [{from,   From},
                             {to,     To},
                             {amount, Amount}]) ->
    op_transfer(From, To, Amount);
fields2update(?OP_DEPOSIT, [{from,   From},
                            {to,     From},
                            {amount, Amount}]) ->
    op_deposit(From, Amount);
fields2update(?OP_DEPOSIT, [{from,   To},
                            {to,     To},
                            {amount, Amount}]) ->
    op_withdraw(To, Amount);
fields2update(?OP_CREATE_CONTRACT, [{owner, Owner},
                                    {vm_version, VmVersion},
                                    {code, Code},
                                    {deposit, Deposit},
                                    {call_data, CallData}]) ->
    op_new_contract(Owner, VmVersion, Code, Deposit, CallData);
fields2update(?OP_CALL_CONTRACT, [ {caller, Caller},
                                    {contract, ContractPubKey},
                                    {vm_version, VmVersion},
                                    {amount, Amount},
                                    {call_data, CallData},
                                    {call_stack, CallStack}]) ->
    op_call_contract(Caller, ContractPubKey, VmVersion, Amount, CallData, CallStack).


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
    [ {from,    binary},
      {to,      binary},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_DEPOSIT) ->
    [ {from,    binary},
      {to,      binary},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_WITHDRAW) ->
    [ {from,    binary},
      {to,      binary},
      {amount,  int}];
update_serialization_template(?UPDATE_VSN, ?OP_CREATE_CONTRACT) ->
    [ {owner,       binary},
      {vm_version,  int},
      {code,        binary},
      {deposit,     int},
      {call_data,   binary}];
update_serialization_template(?UPDATE_VSN, ?OP_CALL_CONTRACT) ->
    [ {caller,      binary},
      {contract,    binary},
      {vm_version,  int},
      {amount,      int},
      {call_data,   binary},
      {call_stack,  [int]}].
