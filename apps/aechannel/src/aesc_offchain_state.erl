-module(aesc_offchain_state).

-include_lib("apps/aecore/include/blocks.hrl").
-include("apps/aecontract/src/aecontract.hrl").

-record(state, { trees                  :: aec_trees:trees()
               , calls                  :: aect_call_state_tree:tree()
               , signed_txs = []        :: [aetx_sign:signed_tx()]
               , half_signed_txs = []   :: [aetx_sign:signed_tx()]
              }).

-opaque state() :: #state{}.

-export_type([state/0]).

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
        , balance/2                   %%  (Pubkey, State) -> Balance
        , poi/2                       %%  (Filter, State) -> {ok, PoI} | {error, not_found}
        ]).

-export([get_contract_call/4,
         prune_calls/1
        ]).

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
    {ok, #state{trees=Trees, calls = aect_call_state_tree:empty()}}.

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

-spec get_contract_call(aect_contracts:id(), aec_keys:pubkey(),
                        non_neg_integer(), state()) -> {error, call_not_found}
                                                    |  {ok, aect_call:call()}.

get_contract_call(Contract, Caller, Round, #state{calls=CallsTree}) ->
    aect_channel_contract:get_call(Contract, Caller, Round, CallsTree).

-spec prune_calls(state()) -> state().
prune_calls(State) ->
    Calls = aect_call_state_tree:empty(),
    State#state{calls = Calls}.

-spec make_update_tx(list(aesc_offchain_update:update()), state(), map()) -> aetx:tx().
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
        fun(U, AccumTrees) -> aesc_offchain_update:apply_on_trees(U, AccumTrees, Round, Opts) end,
        Trees,
        Updates).

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
            {Trees, Calls} =
                lists:foldl(
                    fun(Update, {TrAccum, CallsAccum}) ->
                        TrAccum1 = aesc_offchain_update:apply_on_trees(Update, TrAccum, Mod:round(TxI), Opts),
                        IsCall = aesc_offchain_update:is_call(Update),
                        IsNewContract = aesc_offchain_update:is_contract_create(Update),
                        case IsNewContract orelse IsCall of
                            false -> {TrAccum1, CallsAccum};
                            true ->
                                CallsAccum1 =  move_call(Update,
                                                         Mod:round(TxI),
                                                         CallsAccum,
                                                         TrAccum1),
                                {TrAccum1, CallsAccum1}
                        end
                    end,
                    {aect_call_state_tree:prune_without_backend(State#state.trees),
                     State#state.calls},
                    Mod:updates(TxI)),
            State#state{signed_txs=[SignedTx | Txs0], half_signed_txs=[],
                        trees=Trees, calls=Calls}
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

-spec balance(aec_keys:pubkey(), state()) -> {ok, non_neg_integer()}
                                           | {error, not_found}.
balance(Pubkey, #state{trees=Trees}) ->
    AccTrees = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(Pubkey, AccTrees) of
        none -> {error, not_found};
        {value, Account} -> {ok, aec_accounts:balance(Account)}
    end.

-spec poi(list(), state()) -> {ok, aec_trees:poi()}
                            | {error, not_found}.
poi(Filter, #state{trees=Trees}) ->
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
           ({account, Pubkey}, {ok, PoI}) -> aec_trees:add_poi(accounts,
                                                               Pubkey, Trees,
                                                               PoI);
           ({contract, Id}, {ok, PoI}) -> aec_trees:add_poi(contracts,
                                                            Id, Trees,
                                                            PoI)
        end,
        {ok, aec_trees:new_poi(Trees)},
        Filter).

-spec move_call(aesc_offchain_update:update(), non_neg_integer(), aect_call_state_tree:tree(),
                aec_trees:trees()) -> aect_call_state_tree:tree().
move_call(Update, Round, Calls, Trees) ->
    IsCall = aesc_offchain_update:is_call(Update),
    IsNewContract = aesc_offchain_update:is_contract_create(Update),
    {ContractPubkey, Caller} =
        case IsNewContract of
            true ->
                Owner = aesc_offchain_update:extract_caller(Update),
                ContractPk = aect_contracts:compute_contract_pubkey(Owner, Round),
                {ContractPk, Owner};
            false when IsCall ->
                aesc_offchain_update:extract_call(Update)
        end,
    {ok, Call} = aect_channel_contract:get_call(ContractPubkey,
                                                Caller,
                                                Round,
                                                aec_trees:calls(Trees)),
    aect_call_state_tree:insert_call(Call, Calls).

