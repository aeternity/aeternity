-module(aesc_offchain_state).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/blocks.hrl").

-record(state, { trees                  :: aesc_trees:trees()
               , signed_txs = []        :: [aetx_sign:signed_tx()]
               , half_signed_txs = []   :: [aetx_sign:signed_tx()]
              }).

-opaque state() :: #state{}.

-define(OP_TRANSFER, 0).
-define(OP_WITHDRAW, 1).
-define(OP_DEPOSIT , 2).

-type operation() :: ?OP_TRANSFER | ?OP_WITHDRAW | ?OP_DEPOSIT.

-opaque update() :: {operation(), pubkey(), pubkey(), non_neg_integer()}. 

-export_type([state/0, update/0]).

-export([ new/1                       %%  (Opts) -> {ok, Tx, State}
        , check_initial_update_tx/3   %%  (SignedTx, State, Opts)
        , check_update_tx/3           %%  (SignedTx, State, Opts)
        , make_update_tx/3            %%  (Updates, State, Opts) -> Tx
        , add_signed_tx/2             %%  (SignedTx, State0) -> State
        , add_half_signed_tx/2        %%  (SignedTx, State0) -> State
        , get_latest_half_signed_tx/1 %%  (State) -> SignedTx
        , get_latest_signed_tx/1      %%  (State) -> {Round, SignedTx}
        , get_fallback_state/1        %%  (State) -> {Round, State'}
        , fallback_to_stable_state/1  %%  (State) -> State'
        , hash/1                      %%  (State) -> hash()
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
    Trees = aesc_trees:new([{InitiatorPubKey, InitiatorAmount},
                            {ResponderPubKey, ResponderAmount}]),
    {ok, #state{trees=Trees}}.

-spec hash(state()) -> binary().
hash(#state{trees=Trees}) ->
    aesc_trees:hash(Trees).

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
    case Mod:previous_round(TxI) of
        0 when Txs == [] ->
            lager:debug("previous round = 0", []),
            check_update_tx_(F, Mod, TxI, TxI, State, Opts);
        PrevRound ->
            lager:debug("PrevRound = ~p", [PrevRound]),
            {LastRound, LastSignedTx} = get_latest_signed_tx(State),
            LastTx = aetx_sign:tx(LastSignedTx),
            {Mod, LastTxI} = aetx:specialize_callback(LastTx),  %% Mod bound!
            lager:debug("LastRound = ~p", [LastRound]),
            case PrevRound == LastRound of
                true ->
                    lager:debug("PrevRound == LastRound", []),
                    check_update_tx_(F, Mod, TxI, LastTxI, State, Opts);
                false -> {error, invalid_previous_round}
            end
    end.

check_update_tx_(F, Mod, RefTx, LastTx, #state{trees=Trees}, Opts) ->
    Updates = Mod:updates(RefTx),
    try  {Tx1, Trees1} = apply_updates(Updates, Mod, LastTx, Trees, Opts),
         case {{Mod:initiator_amount(RefTx), Mod:responder_amount(RefTx)},
               {Mod:initiator_amount(Tx1)  , Mod:responder_amount(Tx1)},
               Mod:state_hash(Tx1) =:= aesc_trees:hash(Trees1)} of
             {X, X, true} ->
                 run_extra_checks(F, Mod, RefTx);
             {_, _, false} ->
                 {error, state_hash_mismatch};
             {X, Y, _} ->
                 {error, {amount_mismatch, {X, Y}}}
         end
    catch
        error:Reason ->
            {error, Reason}
    end.

-spec make_update_tx(list(update()), state(), map()) -> aetx:tx().
make_update_tx(Updates, #state{signed_txs=[SignedTx|_], trees=Trees}, Opts) ->
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    {TxI1, _Trees1} = apply_updates(Updates, Mod, TxI, Trees, Opts),
    aetx:update_tx(Tx, TxI1).

apply_updates(Updates, Mod, Tx, Trees, Opts) ->
    Round = Mod:round(Tx),
    {Tx1, NewTrees} = apply_updates_(Updates, Mod, Tx, Trees, Opts),
    Tx2 = set_tx_values([{round, Round+1},
                         {previous_round, Round},
                         {updates, Updates}], Mod, Tx1),
    {Tx2, NewTrees}.

apply_updates_([], _Mod, Tx, Trees, _Opts) ->
    {Tx, Trees};
apply_updates_([{?OP_DEPOSIT, Acct, Acct, Amount} = U | Ds], Mod, Tx, Trees, Opts) ->
    Initiator = Mod:initiator(Tx),
    Responder = Mod:responder(Tx),
    IAmt = Mod:initiator_amount(Tx),
    RAmt = Mod:responder_amount(Tx),
    Vals =
        case Acct of
            Initiator -> [{initiator_amount, IAmt + Amount}];
            Responder -> [{responder_amount, RAmt + Amount}]
        end,
    {Trees1, _OldBalance, NewBalance} = modify_trees(U, Trees),
    [{_, NewBalance}] = Vals, %% TODO: remove assert
    Tx1 = set_tx_values(Vals, Mod, Tx),
    apply_updates_(Ds, Mod, Tx1, Trees1, Opts);
apply_updates_([{?OP_WITHDRAW, Acct, Acct, Amount} = U | Ds], Mod, Tx, Trees, Opts) ->
    Initiator = Mod:initiator(Tx),
    Responder = Mod:responder(Tx),
    IAmt = Mod:initiator_amount(Tx),
    RAmt = Mod:responder_amount(Tx),
    Vals =
        case Acct of
            Initiator -> [{initiator_amount, check_min_amt(IAmt - Amount, Opts)}];
            Responder -> [{responder_amount, check_min_amt(RAmt - Amount, Opts)}]
        end,
    {Trees1, _OldBalance, NewBalance} = modify_trees(U, Trees),
    [{_, NewBalance}] = Vals, %% TODO: remove assert
    Tx1 = set_tx_values(Vals, Mod, Tx),
    apply_updates_(Ds, Mod, Tx1, Trees1, Opts);
apply_updates_([{?OP_TRANSFER, From, To, Amount} = U | Ds], Mod, Tx, Trees, Opts)
  when is_binary(From), is_binary(To), is_integer(Amount) ->
    Initiator = Mod:initiator(Tx),
    Responder = Mod:responder(Tx),
    IAmt = Mod:initiator_amount(Tx),
    RAmt = Mod:responder_amount(Tx),
    {FA, FB, A, B} =
        case {From, To} of
            {Initiator, Responder} ->
                {initiator_amount, responder_amount, IAmt, RAmt};
            {Responder, Initiator} ->
                {responder_amount, initiator_amount, RAmt, IAmt};
            _Other ->
                %% TODO: If multi-party channel, this could be valid
                error(unknown_pubkeys)
        end,
    {A1, B1} = {check_min_amt(A - Amount, Opts), B + Amount},
    {Trees1, FromBalance, ToBalance} = modify_trees(U, Trees),
    {A, _} = {FromBalance, A}, %% TODO: remove assert
    {B, _} = {ToBalance, B}, %% TODO: remove assert
    {A1, B1} = {FromBalance - Amount, ToBalance + Amount},
    StateHash = aesc_trees:hash(Trees1),
    Tx1 = set_tx_values([{FA, A1},
                          {FB, B1},
                          {state_hash, StateHash}], Mod, Tx),
    apply_updates_(Ds, Mod, Tx1, Trees1, Opts).

-spec modify_trees(update(), aesc_trees:trees()) ->
    {aesc_trees:trees(), non_neg_integer(), non_neg_integer()}.
modify_trees({?OP_TRANSFER, From, To, Amount}, Trees) ->
    {ok, AccFrom0} = aesc_trees:get_account(Trees, From),
    {ok, AccTo0} = aesc_trees:get_account(Trees, To),
    FromBalance = aec_accounts:balance(AccFrom0),
    ToBalance = aec_accounts:balance(AccTo0),
    {ok, AccFrom} = aec_accounts:spend(AccFrom0, Amount, 0),
    {ok, AccTo} = aec_accounts:earn(AccTo0, Amount),
    {lists:foldl(
        fun(Acc, Accum) -> aesc_trees:set_account(Accum, Acc) end,
        Trees,
        [AccFrom, AccTo]), FromBalance, ToBalance};
modify_trees({?OP_DEPOSIT, To, To, Amount}, Trees) ->
    {ok, AccTo0} = aesc_trees:get_account(Trees, To),
    ToBalance = aec_accounts:balance(AccTo0),
    {ok, AccTo} = aec_accounts:earn(AccTo0, Amount),
    NewBalance = ToBalance + Amount,
    {aesc_trees:set_account(Trees, AccTo), ToBalance, NewBalance};
modify_trees({?OP_WITHDRAW, From, From, Amount}, Trees) ->
    {ok, AccFrom0} = aesc_trees:get_account(Trees, From),
    FromBalance = aec_accounts:balance(AccFrom0),
    {ok, AccFrom} = aec_accounts:spend(AccFrom0, Amount, 0),
    NewBalance = FromBalance - Amount,
    {aesc_trees:set_account(Trees, AccFrom), FromBalance, NewBalance}.

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

-spec add_signed_tx(aetx_sign:signed_tx(), state()) -> state().
add_signed_tx(SignedTx, #state{signed_txs=Txs0}=State) ->
    true = mutually_signed(SignedTx), % ensure it is mutually signed
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Trees =
        lists:foldl(
            fun(Update, TrAccum) ->
                {TAccum1, _, _} = modify_trees(Update, TrAccum),
                TAccum1
            end,
            State#state.trees,
            Mod:updates(TxI)),
    State#state{signed_txs=[SignedTx | Txs0], half_signed_txs=[], trees=Trees}.

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

-spec op_transfer(pubkey(), pubkey(), non_neg_integer()) -> update().
op_transfer(From, To, Amount) ->
    {?OP_TRANSFER, From, To, Amount}.

-spec op_deposit(pubkey(), non_neg_integer()) -> update().
op_deposit(Acct, Amount) ->
    {?OP_DEPOSIT, Acct, Acct, Amount}.

-spec op_withdraw(pubkey(), non_neg_integer()) -> update().
op_withdraw(Acct, Amount) ->
    {?OP_WITHDRAW, Acct, Acct, Amount}.


tx_round(Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Mod:round(TxI).

-spec fallback_to_stable_state(state()) -> state().
%% TODO: handle empty state?
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

set_tx_values([{K, V}|T], Mod, Tx) ->
    set_tx_values(T, Mod, Mod:set_value(Tx, K, V));
set_tx_values([], _, Tx) ->
    Tx.
