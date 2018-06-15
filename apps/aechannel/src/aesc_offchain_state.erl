-module(aesc_offchain_state).

-include_lib("apps/aecore/include/blocks.hrl").

-record(state, { trees                  :: aec_trees:trees()
               , signed_txs = []        :: [aetx_sign:signed_tx()]
               , half_signed_txs = []   :: [aetx_sign:signed_tx()]
              }).

-opaque state() :: #state{}.

-define(OP_TRANSFER, 0).
-define(OP_WITHDRAW, 1).
-define(OP_DEPOSIT , 2).

-type operation() :: ?OP_TRANSFER | ?OP_WITHDRAW | ?OP_DEPOSIT.

-opaque update() :: {operation(), aec_keys:pubkey(), aec_keys:pubkey(), non_neg_integer()}.

-export_type([state/0, update/0]).

-export([ new/1                       %%  (Opts) -> {ok, Tx, State}
        , check_initial_update_tx/3   %%  (SignedTx, State, Opts)
        , check_update_tx/3           %%  (SignedTx, State, Opts)
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
-export([ op_transfer/3
        , op_deposit/2
        , op_withdraw/2
        ]).

-spec new(map()) -> {ok, state()}.
new(Opts) ->
    lager:debug("offchain_tx:new(~p)", [Opts]),
    #{initiator          := InitiatorPubKey,
      responder          := ResponderPubKey,
      initiator_amount   := InitiatorAmount,
      responder_amount   := ResponderAmount} = Opts,
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

-spec hash(state()) -> binary().
hash(#state{trees=Trees}) ->
    aec_trees:hash(Trees).

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

-spec make_update_tx(list(update()), state(), map()) -> aetx:tx().
make_update_tx(Updates, #state{signed_txs=[SignedTx|_], trees=Trees}, Opts) ->
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Round = Mod:round(TxI),
    ChannelId = Mod:channel_id(TxI),
    #{initiator          := InitiatorPubKey,
      responder          := ResponderPubKey} = Opts,
    Trees1 = apply_updates(Updates, Trees, Opts),
    StateHash = aec_trees:hash(Trees1),
    {ok, OffchainTx} =
        aesc_offchain_tx:new(#{channel_id     => ChannelId,
                              initiator      => InitiatorPubKey,
                              responder      => ResponderPubKey,
                              state_hash     => StateHash,
                              updates        => Updates,
                              previous_round => Round,
                              round          => Round + 1}),
    OffchainTx.

apply_updates(Updates, Trees, Opts) ->
    lists:foldl(
        fun(U, AccumTrees) -> modify_trees(U, AccumTrees, Opts) end,
        Trees,
        Updates).

-spec modify_trees(update(), aec_trees:trees(), map()) -> aec_trees:trees().
modify_trees({?OP_TRANSFER, From, To, Amount}, Trees0, Opts) ->
    AccountTrees = aec_trees:accounts(Trees0),
    AccFrom0 = aec_accounts_trees:get(From, AccountTrees),
    AccTo0 = aec_accounts_trees:get(To, AccountTrees),
    FromBalance = aec_accounts:balance(AccFrom0),
    check_min_amt(FromBalance - Amount, Opts),
    Nonce = aec_accounts:nonce(AccFrom0),
    {ok, AccFrom} = aec_accounts:spend(AccFrom0, Amount, Nonce + 1),
    {ok, AccTo} = aec_accounts:earn(AccTo0, Amount),
    AccountTrees1 =
        lists:foldl(
            fun(Acc, Accum) -> aec_accounts_trees:enter(Acc, Accum) end,
            AccountTrees,
            [AccFrom, AccTo]),
    aec_trees:set_accounts(Trees0, AccountTrees1);
modify_trees({?OP_DEPOSIT, To, To, Amount}, Trees, _Opts) ->
    AccountTrees = aec_trees:accounts(Trees),
    AccTo0 = aec_accounts_trees:get(To, AccountTrees),
    {ok, AccTo} = aec_accounts:earn(AccTo0, Amount),
    AccountTrees1 = aec_accounts_trees:enter(AccTo, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1);
modify_trees({?OP_WITHDRAW, From, From, Amount}, Trees, Opts) ->
    AccountTrees = aec_trees:accounts(Trees),
    AccFrom0 = aec_accounts_trees:get(From, AccountTrees),
    FromBalance = aec_accounts:balance(AccFrom0),
    check_min_amt(FromBalance - Amount, Opts),
    Nonce = aec_accounts:nonce(AccFrom0),
    {ok, AccFrom} = aec_accounts:spend(AccFrom0, Amount, Nonce), %no nonce bump
    AccountTrees1 = aec_accounts_trees:enter(AccFrom, AccountTrees),
    aec_trees:set_accounts(Trees, AccountTrees1).

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
                        TrAccum1 = modify_trees(Update, TrAccum, Opts),
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
