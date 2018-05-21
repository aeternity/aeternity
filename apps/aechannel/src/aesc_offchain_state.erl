-module(aesc_offchain_state).

-export([ new/1                     %%  (Opts) -> Tx
        , check_initial_state/2     %%  (SerializedTxElem, Tx) -> ok | {error,_}
        , check_initial_update_tx/3 %%  (SignedTx, State, Opts)
        , check_update_tx/3         %%  (SignedTx, State, Opts)
        , apply_updates/3           %%  (Updates, Tx, Opts) -> Tx'
        , get_latest_state_tx/1     %%  (State) -> {Round, SignedTx}
        , get_fallback_state/1      %%  (State) -> {Round, State'}
        , fallback_to_stable_state/1 %% (State) -> State'
        ]).
-export([ op_transfer/3
        , op_deposit/2
        , op_withdrawal/2
        ]).

-define(OP_TRANSFER, 0).
-define(OP_WITHDRAW, 1).
-define(OP_DEPOSIT , 2).


new(Opts) ->
    lager:debug("offchain_tx:new(~p)", [Opts]),
    aesc_offchain_tx:new(Opts).

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

check_initial_update_tx(SignedTx, State, Opts) ->
    check_update_tx(fun check_initial_state/2, SignedTx, State, Opts).

check_update_tx(SignedTx, State, Opts) ->
    check_update_tx(none, SignedTx, State, Opts).

check_update_tx(F, SignedTx, State, Opts) ->
    lager:debug("check_update_tx(State = ~p)", [State]),
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    lager:debug("Tx = ~p", [Tx]),
    case Mod:previous_round(TxI) of
        0 when State == [] ->
            lager:debug("previous round = 0", []),
            check_update_tx_(F, Mod, TxI, TxI, Opts);
        PrevRound ->
            lager:debug("PrevRound = ~p", [PrevRound]),
            {LastRound, LastSignedTx} = get_latest_state_tx(State),
            LastTx = aetx_sign:tx(LastSignedTx),
            {Mod, LastTxI} = aetx:specialize_callback(LastTx),  %% Mod bound!
            lager:debug("LastRound = ~p", [LastRound]),
            case PrevRound == LastRound of
                true ->
                    lager:debug("PrevRound == LastRound", []),
                    check_update_tx_(F, Mod, TxI, LastTxI, Opts);
                false -> {error, invalid_previous_round}
            end
    end.

check_update_tx_(F, Mod, RefTx, LastTx, Opts) ->
    Updates = Mod:updates(RefTx),
    try  Tx1 = apply_updates(Updates, Mod, LastTx, Opts),
         case {{Mod:initiator_amount(RefTx), Mod:responder_amount(RefTx)},
               {Mod:initiator_amount(Tx1)  , Mod:responder_amount(Tx1)}} of
             {X, X} ->
                 run_extra_checks(F, Mod, RefTx);
             Other ->
                 {error, {amount_mismatch, Other}}
         end
    catch
        error:Reason ->
            {error, Reason}
    end.

apply_updates(Updates, Tx, Opts) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    TxI1 = apply_updates(Updates, Mod, TxI, Opts),
    aetx:update_tx(Tx, TxI1).

apply_updates(Updates, Mod, Tx, Opts) ->
    Round = Mod:round(Tx),
    Tx1 = apply_updates_(Updates, Mod, Tx, Opts),
    set_tx_values([{round, Round+1},
                   {previous_round, Round},
                   {updates, Updates}], Mod, Tx1).

apply_updates_([], _Mod, Tx, _Opts) ->
    Tx;
apply_updates_([{?OP_DEPOSIT, Acct, Acct, Amount}|Ds], Mod, Tx, Opts) ->
    Initiator = Mod:initiator(Tx),
    Responder = Mod:responder(Tx),
    IAmt = Mod:initiator_amount(Tx),
    RAmt = Mod:responder_amount(Tx),
    Vals =
        case Acct of
            Initiator -> [{initiator_amount, IAmt + Amount}];
            Responder -> [{responder_amount, RAmt + Amount}]
        end,
    Tx1 = set_tx_values(Vals, Mod, Tx),
    apply_updates_(Ds, Mod, Tx1, Opts);
apply_updates_([{?OP_TRANSFER, From, To, Amount}|Ds], Mod, Tx, Opts)
  when is_binary(From), is_binary(To), is_integer(Amount) ->
    Initiator = Mod:initiator(Tx),
    Responder = Mod:responder(Tx),
    IAmt = Mod:initiator_amount(Tx),
    RAmt = Mod:responder_amount(Tx),
    Reserve = maps:get(channel_reserve, Opts, 0),
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
    {A1, B1} = {A - Amount, B + Amount},
    Tx1 = if A1 < Reserve ->
                  %% TODO: consider minimum balance
                  error(insufficient_balance);
             true ->
                  set_tx_values([{FA, A1},
                                 {FB, B1}], Mod, Tx)
          end,
    apply_updates_(Ds, Mod, Tx1, Opts).

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

get_latest_state_tx(State) ->
    [SignedTx|_] = drop_until_mutually_signed(State),
    {tx_round(aetx_sign:tx(SignedTx)), SignedTx}.

get_fallback_state(State) ->
    [SignedTx|_] = L = drop_until_mutually_signed(State),
    {tx_round(aetx_sign:tx(SignedTx)), L}.

op_transfer(From, To, Amount) ->
    {?OP_TRANSFER, From, To, Amount}.

op_deposit(Acct, Amount) ->
    {?OP_DEPOSIT, Acct, Acct, Amount}.

op_withdrawal(Acct, Amount) ->
    {?OP_WITHDRAW, Acct, Acct, Amount}.


tx_round(Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Mod:round(TxI).

fallback_to_stable_state(State) ->
    [_|_] = drop_until_mutually_signed(State).

drop_until_mutually_signed([SignedTx|T] = State) ->
    case aetx_sign:signatures(SignedTx) of
        [_, _] ->
            %% mutually signed
            State;
        _ ->
            drop_until_mutually_signed(T)
    end;
drop_until_mutually_signed([]) ->
    error(no_latest_state).

set_tx_values([{K, V}|T], Mod, Tx) ->
    set_tx_values(T, Mod, Mod:set_value(Tx, K, V));
set_tx_values([], _, Tx) ->
    Tx.
