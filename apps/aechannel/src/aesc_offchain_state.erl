-module(aesc_offchain_state).

-include_lib("apps/aecore/include/blocks.hrl").
-include("apps/aecontract/src/aecontract.hrl").

-define(NO_TX, no_tx).

-record(state, { trees                  :: aec_trees:trees()
               , calls                  :: aect_call_state_tree:tree()
               , signed_tx = ?NO_TX     :: aetx_sign:signed_tx() | ?NO_TX
               , half_signed_tx = ?NO_TX:: aetx_sign:signed_tx() | ?NO_TX
              }).

-opaque state() :: #state{}.

-export_type([state/0]).

-export([ new/1                       %%  (Opts) -> {ok, Tx, State}
        , check_initial_update_tx/5   %%  (SignedTx, State, OnChainTrees, OnChainEnv, Opts)
        , check_update_tx/5           %%  (SignedTx, State, OnChainTrees, OnChainEnv, Opts)
        , check_reestablish_tx/2      %%  (SignedTx, State) -> {ok,NewSt} | error()
        , is_latest_signed_tx/2       %%  (SignedTx, State) -> boolean()
        , verify_signatures/2         %%  (SignedTx, State)
        , make_update_tx/5            %%  (Updates, State, OnChainTrees, OnChainEnv, Opts) -> Tx
        , set_signed_tx/5             %%  (SignedTx, State0, OnChainTrees, OnCHainEnv, Opts) -> State
        , set_half_signed_tx/2        %%  (SignedTx, State0) -> State
        , get_latest_half_signed_tx/1 %%  (State) -> SignedTx
        , get_latest_signed_tx/1      %%  (State) -> {Round, SignedTx}
        , get_fallback_state/1        %%  (State) -> {Round, State'}
        , fallback_to_stable_state/1  %%  (State) -> State'
        , hash/1                      %%  (State) -> hash()
        , balance/2                   %%  (Pubkey, State) -> Balance
        , poi/2                       %%  (Filter, State) -> {ok, PoI} | {error, not_found}
        , serialize_for_client/1
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
      , responder_amount   := ResponderAmount
      , push_amount        := PushAmount}) ->
    Trees0 = aec_trees:new_without_backend(),
    Accounts =
        lists:foldl(
            fun({Pubkey, Amount}, AccTree) ->
            Account = aec_accounts:new(Pubkey, Amount),
            aec_accounts_trees:enter(Account, AccTree)
        end,
        aec_trees:accounts(Trees0),
        [{InitiatorPubKey, InitiatorAmount - PushAmount},
         {ResponderPubKey, ResponderAmount + PushAmount}]),
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

is_latest_signed_tx(_SignedTx, #state{signed_tx = ?NO_TX}) ->
    false;
is_latest_signed_tx(SignedTx, #state{signed_tx = LatestSignedTx}) ->
    aetx_sign:serialize_to_binary(SignedTx)
        == aetx_sign:serialize_to_binary(LatestSignedTx).

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

-spec check_initial_update_tx(aetx_sign:signed_tx(), state(),
                              aec_trees:trees(), aetx_env:env(), map()) -> ok | {error, atom()}.
check_initial_update_tx(SignedTx, State, OnChainTrees, OnChainEnv, Opts) ->
    check_update_tx_(fun check_initial_state/2, SignedTx, State, OnChainTrees,
                    OnChainEnv, Opts).

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

-spec check_update_tx(aetx_sign:signed_tx(), state(), aec_trees:trees(),
                      aetx_env:env(), map()) -> ok | {error, atom()}.
check_update_tx(SignedTx, State, OnChainTrees, OnChainEnv, Opts) ->
    check_update_tx_(none, SignedTx, State, OnChainTrees, OnChainEnv, Opts).

check_update_tx_(F, SignedTx, #state{signed_tx = OldSignedTx}=State, OnChainTrees,
                 OnChainEnv, Opts) when OldSignedTx =/= ?NO_TX ->
    lager:debug("check_update_tx(State = ~p)", [State]),
    Tx = aetx_sign:tx(SignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    lager:debug("Tx = ~p", [Tx]),
    case Mod:round(TxI) - 1 of
        0 when OldSignedTx == ?NO_TX ->
            lager:debug("previous round = 0", []),
            check_update_tx_(F, Mod, TxI, State, OnChainTrees, OnChainEnv, Opts);
        PrevRound ->
            lager:debug("PrevRound = ~p", [PrevRound]),
            {LastRound, _LastSignedTx} = get_latest_signed_tx(State),
            lager:debug("LastRound = ~p", [LastRound]),
            case PrevRound == LastRound of
                true ->
                    lager:debug("PrevRound == LastRound", []),
                    check_update_tx_(F, Mod, TxI, State, OnChainTrees,
                                     OnChainEnv, Opts);
                false -> {error, invalid_previous_round}
            end
    end.

check_update_tx_(F, Mod, RefTx, #state{} = State, OnChainTrees, OnChainEnv, Opts) ->
    Updates = Mod:updates(RefTx),
    try Tx1 = make_update_tx(Updates, State, OnChainTrees, OnChainEnv, Opts),
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

-spec get_contract_call(aect_contracts:pubkey(), aec_keys:pubkey(),
                        non_neg_integer(), state()) -> {error, call_not_found}
                                                    |  {ok, aect_call:call()}.

get_contract_call(ContractPubkey, CallerPubkey, Round, #state{calls=CallsTree}) ->
    aect_channel_contract:get_call(ContractPubkey, CallerPubkey, Round, CallsTree).

-spec prune_calls(state()) -> state().
prune_calls(State) ->
    Calls = aect_call_state_tree:empty(),
    State#state{calls = Calls}.

-spec make_update_tx(list(aesc_offchain_update:update()), state(),
                     aec_trees:trees(), aetx_env:env(), map()) -> aetx:tx().
make_update_tx(Updates, #state{signed_tx = LastSignedTx, trees=Trees},
               OnChainTrees, OnChainEnv, Opts)
    when LastSignedTx =/= ?NO_TX ->
    Tx = aetx_sign:tx(LastSignedTx),
    {Mod, TxI} = aetx:specialize_callback(Tx),
    ChannelPubKey = Mod:channel_pubkey(TxI),

    NextRound     = Mod:round(TxI) + 1,

    Reserve = maps:get(channel_reserve, Opts, 0),
    Trees1 = apply_updates(Updates, NextRound, Trees, OnChainTrees,
                           OnChainEnv, Reserve),
    StateHash = aec_trees:hash(Trees1),
    {ok, OffchainTx} =
        aesc_offchain_tx:new(#{channel_id => aec_id:create(channel, ChannelPubKey),
                               state_hash => StateHash,
                               updates    => Updates,
                               round      => NextRound}),
    OffchainTx.

apply_updates(Updates, Round, Trees, OnChainTrees, OnChainEnv, Reserve) ->
    lists:foldl(
        fun(U, AccumTrees) ->
            aesc_offchain_update:apply_on_trees(U, AccumTrees, OnChainTrees,
                                                OnChainEnv, Round, Reserve)
        end,
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

-spec set_signed_tx(aetx_sign:signed_tx(), state(), aec_trees:trees(),
                    aetx_env:env(), map()) -> state().
set_signed_tx(SignedTx, #state{}=State, OnChainTrees,
              OnChainEnv, Opts) ->
    true = mutually_signed(SignedTx), % ensure it is mutually signed
    Tx = aetx_sign:tx(SignedTx),
    case aetx:specialize_callback(Tx) of
        {aesc_create_tx, _} ->
            State#state{signed_tx = SignedTx, half_signed_tx = ?NO_TX};
        {Mod, TxI} ->
            Reserve = maps:get(channel_reserve, Opts, 0),
            {Trees, Calls} =
                lists:foldl(
                    fun(Update, {TrAccum, CallsAccum}) ->
                        TrAccum1 = aesc_offchain_update:apply_on_trees(Update,
                                                                       TrAccum,
                                                                       OnChainTrees,
                                                                       OnChainEnv,
                                                                       Mod:round(TxI),
                                                                       Reserve),
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
            State#state{signed_tx = SignedTx, half_signed_tx = ?NO_TX,
                        trees=Trees, calls=Calls}
    end.

-spec set_half_signed_tx(aetx_sign:signed_tx(), state()) -> state().
set_half_signed_tx(SignedTx, #state{}=State) ->
    State#state{half_signed_tx = SignedTx}.

-spec get_latest_half_signed_tx(state()) -> aetx_sign:signed_tx().
get_latest_half_signed_tx(#state{half_signed_tx = Tx}) when Tx =/= ?NO_TX ->
    Tx.

-spec get_latest_signed_tx(state()) -> {non_neg_integer(), aetx_sign:signed_tx()}.
get_latest_signed_tx(#state{signed_tx = LastSignedTx})
    when LastSignedTx =/= ?NO_TX ->
    {tx_round(aetx_sign:tx(LastSignedTx)), LastSignedTx}.

-spec get_fallback_state(state()) -> {non_neg_integer(), state()}.
get_fallback_state(#state{signed_tx = LastSignedTx}=State)
    when LastSignedTx =/= ?NO_TX ->
    {tx_round(aetx_sign:tx(LastSignedTx)), State#state{half_signed_tx = ?NO_TX}}.

tx_round(Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    Mod:round(TxI).

-spec fallback_to_stable_state(state()) -> state().
fallback_to_stable_state(#state{signed_tx = LastSignedTx}=State)
    when LastSignedTx =/= ?NO_TX ->
    State#state{half_signed_tx = ?NO_TX}.

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

-spec serialize_for_client(state()) -> map().
serialize_for_client(#state{trees = Trees,
                            calls = Calls,
                            signed_tx = SignedTx,
                            half_signed_tx = HalfSignedTx
                           }) ->
    #{ <<"trees">> => aec_trees:serialize_to_client(Trees)
     , <<"calls">> => aect_call_state_tree:serialize_to_client(Calls)
     , <<"signed_tx">>       => serialize_for_client_tx_or_notx(SignedTx)
     , <<"half_signed_tx">>  => serialize_for_client_tx_or_notx(HalfSignedTx)
     }.

-spec serialize_for_client_tx_or_notx(aetx_sign:signed_tx() | ?NO_TX) -> binary() | map().
serialize_for_client_tx_or_notx(?NO_TX) ->
    <<"">>;
serialize_for_client_tx_or_notx(Tx) ->
    STx = aetx_sign:serialize_to_binary(Tx),
    aehttp_api_encoder:encode(transaction, STx).

