-module(aesc_offchain_state).

-include("../../aecore/include/blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(NO_TX, no_tx).

-record(state, { trees                  :: aec_trees:trees()
               , calls                  :: aect_call_state_tree:tree()
               , signed_tx = ?NO_TX     :: aetx_sign:signed_tx() | ?NO_TX
               , half_signed_tx = ?NO_TX:: aetx_sign:signed_tx() | ?NO_TX
              }).

-opaque state() :: #state{}.

-export_type([state/0]).

-export([ new/1                       %%  (Opts) -> {ok, Tx, State}
        , check_initial_update_tx/3   %%  (SignedTx, Updates, State)
        , check_update_tx/8           %%  (SignedTx, Updates, State, ChannelPubkey, Protocol, OnChainTrees, OnChainEnv, Opts)
        , check_reestablish_tx/2      %%  (SignedTx, State) -> {ok,NewSt} | error()
        , is_latest_signed_tx/2       %%  (SignedTx, State) -> boolean()
        , make_update_tx/7            %%  (Updates, State, ChannelPubkey, Protocol, OnChainTrees, OnChainEnv, Opts) -> Tx
        , set_signed_tx/6             %%  (SignedTx, Updates, State0, OnChainTrees, OnCHainEnv, Opts) -> State
        , set_signed_tx/7             %%  (SignedTx, Updates, State0, OnChainTrees, OnCHainEnv, Opts, CheckSigs) -> State
        , set_half_signed_tx/2        %%  (SignedTx, State0) -> State
        , get_latest_half_signed_tx/1 %%  (State) -> SignedTx
        , get_latest_signed_tx/1      %%  (State) -> {Round, SignedTx}
        , get_latest_trees/1          %%  (State) -> Trees
        , get_fallback_state/1        %%  (State) -> {Round, State'}
        , fallback_to_stable_state/1  %%  (State) -> State'
        , hash/1                      %%  (State) -> hash()
        , balance/2                   %%  (Pubkey, State) -> Balance
        , poi/2                       %%  (Filter, State) -> {ok, PoI} | {error, not_found}
        , serialize_for_client/1      %%  (State) -> map()
        , serialize_to_binary/1       %%  (State) -> binary()
        , deserialize_from_binary/1   %%  (binary()) -> State
        ]).

-export([get_contract_call/4,
         prune_calls/1
        ]).

-export([record_fields/1]).

%% ==================================================================
%% Tracing support
record_fields(state) -> record_info(fields, state);
record_fields(Other) -> aec_trees:record_fields(Other).
%% ==================================================================


-spec new(map()) -> {ok, map()} | {error, atom()}.
new(Opts) ->
    lager:debug("offchain_tx:new(~p)", [Opts]),
    case Opts of
        #{ existing_channel_id     := _
         , offchain_tx             := _
         , existing_fsm_id_wrapper := _} ->
            recover_from_offchain_tx(Opts);
        #{ initiator          := _
         , responder          := _
         , initiator_amount   := _
         , responder_amount   := _ } ->
            new_(Opts)
    end.

-spec new_(map()) -> {ok, map()}.
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
    {ok, #{ mode => new
          , state => #state{trees=Trees, calls = aect_call_state_tree:empty() }}}.

-spec recover_from_offchain_tx(map()) -> {ok, map()} | {error, atom()}.
recover_from_offchain_tx(#{ existing_channel_id     := ChId
                          , offchain_tx             := SignedTx
                          , existing_fsm_id_wrapper := FsmIdWrapper} = Opts) ->
    MyPubkey = my_pubkey(Opts),
    case aesc_state_cache:reestablish(ChId, MyPubkey, FsmIdWrapper) of
        {ok, #state{} = State, CachedOpts} ->
            {ok, #{ mode => reestablish
                  , is_latest_signed_tx => is_latest_signed_tx(SignedTx, State)
                  , state => State
                  , cached_opts => CachedOpts }};
        {error, _} = Error ->
            Error
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

-spec check_initial_update_tx(aetx_sign:signed_tx(), [aesc_offchain_update:update()], state())
    -> ok | {error, atom()}.
check_initial_update_tx(SignedTx, _Updates, State) ->
    {Mod, Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    Checks =
        [ fun() ->
              case Mod of
                  aesc_create_tx -> ok; % this checks the round in an implicit manner
                  _ -> {error, not_create_tx}
              end
            end,
          fun() ->
              case aesc_create_tx:state_hash(Tx) =:= hash(State) of
                  true -> ok;
                  false -> {error, bad_state_hash}
              end
          end],
    lists:foldl(
        fun(_, {error, _} = Err) -> Err;
           (F, ok) -> F()
        end,
        ok,
        Checks).

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

-spec check_update_tx(aetx_sign:signed_tx(), [aesc_offchain_update:update()],
                      state(), binary(), aec_hard_forks:protocol_vsn(), aec_trees:trees(),
                      aetx_env:env(), non_neg_integer()) -> ok | {error, atom()}.
check_update_tx(SignedTx, Updates, #state{signed_tx = OldSignedTx} = State,
                ChannelPubkey,
                Protocol, OnChainTrees,
                OnChainEnv, Reserve) when OldSignedTx =/= ?NO_TX ->
    {Mod, TxI} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    lager:debug("Tx = ~p", [TxI]),
    case Mod:valid_at_protocol(Protocol, TxI) of
        false -> {error, invalid_at_protocol};
        true ->
            case Mod of
                aesc_close_mutual_tx -> ok; % no particular check for a close mutual
                _ ->
                    PrevRound = Mod:round(TxI) - 1,
                    true = PrevRound > 0, % assert not channel_create
                    lager:debug("PrevRound = ~p", [PrevRound]),
                    {LastRound, _LastSignedTx} = get_latest_signed_tx(State),
                    lager:debug("LastRound = ~p", [LastRound]),
                    case PrevRound == LastRound of
                        true ->
                            lager:debug("PrevRound == LastRound", []),
                            check_update_tx_(Mod, TxI, Updates, State,
                                             ChannelPubkey,
                                             Protocol, OnChainTrees,
                                             OnChainEnv, Reserve);
                        false -> {error, invalid_previous_round}
                    end
            end
    end.

check_update_tx_(Mod, RefTx, Updates, #state{} = State,
                 ChannelPubkey,
                 Protocol, OnChainTrees, OnChainEnv, Reserve) ->
    try Tx1 = make_update_tx(Updates, State, ChannelPubkey,
                             Protocol, OnChainTrees, OnChainEnv, Reserve),
         {Mod1, Tx1I} = aetx:specialize_callback(Tx1),
         case Mod1:state_hash(Tx1I) =:= Mod:state_hash(RefTx) of
             true ->
                ok;
             false ->
                 {error, bad_state_hash}
         end
    catch
        error:Reason ->
            {error, Reason}
    end.

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
                     binary(),
                     aec_hard_forks:protocol_vsn(),
                     aec_trees:trees(), aetx_env:env(),
                     non_neg_integer()) -> aetx:tx().
make_update_tx(Updates, #state{signed_tx = LastSignedTx, trees=Trees},
               ChannelPubkey, Protocol,
               OnChainTrees, OnChainEnv, Reserve)
    when LastSignedTx =/= ?NO_TX ->
    {Mod, TxI} =
        aetx:specialize_callback(aetx_sign:innermost_tx(LastSignedTx)),

    NextRound = Mod:round(TxI) + 1,

    Trees1 = apply_updates(Updates, NextRound, Trees, OnChainTrees,
                           OnChainEnv, Reserve),
    StateHash = aec_trees:hash(Trees1),
    Props0 =
        #{channel_id => aeser_id:create(channel, ChannelPubkey),
          state_hash => StateHash,
          round      => NextRound},
    Props =
        case Protocol of
            ?ROMA_PROTOCOL_VSN    -> Props0#{updates => Updates};
            ?MINERVA_PROTOCOL_VSN -> Props0#{updates => Updates};
            _                     -> Props0
        end,
    {ok, OffchainTx} = aesc_offchain_tx:new(Props),
    OffchainTx.

apply_updates(Updates, Round, Trees0, OnChainTrees, OnChainEnv, Reserve) ->
    Trees = aect_call_state_tree:prune_without_backend(Trees0),
    lists:foldl(
        fun(U, AccumTrees) ->
            aesc_offchain_update:apply_on_trees(U, AccumTrees, OnChainTrees,
                                                OnChainEnv, Round, Reserve)
        end,
        Trees,
        Updates).

-spec set_signed_tx(aetx_sign:signed_tx(), [aesc_offchain_update:update()],
                    state(), aec_trees:trees(),
                    aetx_env:env(), map()) -> state().
set_signed_tx(SignedTx, Updates, #state{}=State, OnChainTrees,
              OnChainEnv, Opts) ->
    set_signed_tx(SignedTx, Updates, State, OnChainTrees, OnChainEnv, Opts,
                  false).


-spec set_signed_tx(aetx_sign:signed_tx(), [aesc_offchain_update:update()],
                    state(), aec_trees:trees(),
                    aetx_env:env(), map(), boolean()) -> state().
set_signed_tx(SignedTx, Updates, #state{}=State, OnChainTrees,
              OnChainEnv, Opts, CheckSigs) ->
    case CheckSigs of
        true ->
            true = mutually_signed(SignedTx); % ensure it is mutually signed
        false -> pass
    end,
    case aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)) of
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
                    Updates),
            State#state{signed_tx = SignedTx, half_signed_tx = ?NO_TX,
                        trees = Trees, calls = Calls}
    end.

-spec set_half_signed_tx(aetx_sign:signed_tx(), state()) -> state().
set_half_signed_tx(SignedTx, #state{}=State) ->
    State#state{half_signed_tx = SignedTx}.

-spec get_latest_half_signed_tx(state()) -> aetx_sign:signed_tx().
get_latest_half_signed_tx(#state{half_signed_tx = Tx}) when Tx =/= ?NO_TX ->
    Tx;
get_latest_half_signed_tx(#state{half_signed_tx = ?NO_TX}) ->
    error(no_tx).

-spec get_latest_signed_tx(state()) -> {non_neg_integer(), aetx_sign:signed_tx()}.
get_latest_signed_tx(#state{signed_tx = LastSignedTx})
    when LastSignedTx =/= ?NO_TX ->
    {tx_round(LastSignedTx), LastSignedTx};
get_latest_signed_tx(#state{signed_tx = ?NO_TX}) ->
    error(no_tx).

-spec get_latest_trees(state()) -> aec_trees:trees().
get_latest_trees(#state{trees = Trees}) ->
    Trees.

-spec get_fallback_state(state()) -> {non_neg_integer(), state()}.
get_fallback_state(#state{signed_tx = LastSignedTx}=State)
    when LastSignedTx =/= ?NO_TX ->
    {tx_round(LastSignedTx), State#state{half_signed_tx = ?NO_TX}}.

tx_round(SignedTx) ->
    {Mod, TxI} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    Mod:round(TxI).

-spec fallback_to_stable_state(state()) -> state().
fallback_to_stable_state(#state{signed_tx = LastSignedTx}=State)
    when LastSignedTx =/= ?NO_TX ->
    State#state{half_signed_tx = ?NO_TX}.

-spec mutually_signed(aetx_sign:signed_tx()) -> boolean().
mutually_signed(SignedTx) ->
    {Mod, _Tx} = aetx:specialize_callback(aetx_sign:innermost_tx(SignedTx)),
    %% here we expect only state changing transactions.
    %% Examples for state changing transactions would be off-chain
    %% transactions, channel create, deposit, withdrawal and force progress
    %% Examples for non-state changing transactions: close solo, slash and
    %% snapshot
    %% Close mutual and settle do change the state but we don't expect them
    %% here
    RequiredAuths =
        case Mod of
            aesc_force_progress_tx -> 1;
            _ -> 2
        end,
    aesc_utils:count_authentications(SignedTx) =:= RequiredAuths.


-spec balance(aec_keys:pubkey(), state()) -> {ok, non_neg_integer()}
                                           | {error, not_found}.
balance(Pubkey, #state{trees = Trees}) ->
    AccTrees = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(Pubkey, AccTrees) of
        none -> {error, not_found};
        {value, Account} -> {ok, aec_accounts:balance(Account)}
    end.

-spec poi(list(), state()) -> {ok, aec_trees:poi()}
                            | {error, not_found}.
poi(Filter, #state{trees = Trees}) ->
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
    aeser_api_encoder:encode(transaction, STx).

-spec serialize_to_binary(state()) -> binary().
serialize_to_binary(#state{ trees = Trees
                          , calls = Calls
                          , signed_tx = SignedTx
                          , half_signed_tx = HalfSignedTx
                          }) ->
        aeser_rlp:encode([
            aec_trees:serialize_to_binary(Trees),
            aect_call_state_tree:to_binary_without_backend(Calls),
            serialize_to_binary_tx_or_notx(SignedTx),
            serialize_to_binary_tx_or_notx(HalfSignedTx)
        ]).

-spec serialize_to_binary_tx_or_notx(aetx_sign:signed_tx() | ?NO_TX) -> binary().
serialize_to_binary_tx_or_notx(?NO_TX) ->
    <<"">>;
serialize_to_binary_tx_or_notx(Tx) ->
    aetx_sign:serialize_to_binary(Tx).

-spec deserialize_from_binary(binary()) -> state().
deserialize_from_binary(Bin) ->
    case aeser_rlp:decode(Bin) of
        [ BinTrees
        , BinCalls
        , BinSignedTx
        , BinHalfSignedTx
        ] ->
            #state{ trees = aec_trees:deserialize_from_binary_without_backend(BinTrees)
                  , calls = aect_call_state_tree:from_binary_without_backend(BinCalls)
                  , signed_tx = deserialize_tx_or_notx_from_binary(BinSignedTx)
                  , half_signed_tx = deserialize_tx_or_notx_from_binary(BinHalfSignedTx)
                  };
        _ ->
            erlang:error(deserialization_failed)
    end.

-spec deserialize_tx_or_notx_from_binary(binary()) -> aetx_sign:signed_tx() | ?NO_TX.
deserialize_tx_or_notx_from_binary(<<"">>) ->
    ?NO_TX;
deserialize_tx_or_notx_from_binary(Bin) ->
    aetx_sign:deserialize_from_binary(Bin).
