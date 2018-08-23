%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    State Channel utility functions
%%% @end
%%%=============================================================================
-module(aesc_utils).

%% API
-export([get_channel/2,
         accounts_in_poi/2,
         check_is_active/1,
         check_is_peer/2,
         check_are_funds_in_channel/3,
         check_round_greater_than_last/2,
         check_state_hash_size/1,
         deserialize_payload/1,
         check_solo_close_payload/8,
         check_slash_payload/8,
         check_solo_snapshot_payload/7,
         process_solo_close/8,
         process_slash/8,
         process_solo_snapshot/6
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_channel(aesc_channels:pubkey(), aec_trees:trees()) ->
                         {error, term()} | ok.
get_channel(ChannelPubKey, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelPubKey, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Ch} ->
            {ok, Ch}
    end.

accounts_in_poi(Peers, PoI) ->
    Lookups = [aec_trees:lookup_poi(accounts, Pubkey, PoI) || Pubkey <- Peers],
    Accounts = [Acc || {ok, Acc} <- Lookups], % filter successful ones
    case length(Accounts) =:= length(Peers) of
        false -> {error, wrong_channel_peers};
        true ->
            {ok, Accounts}
    end.

-spec check_is_active(aesc_channels:channel()) -> ok | {error, channel_not_active}.
check_is_active(Channel) ->
    case aesc_channels:is_active(Channel) of
        true  -> ok;
        false -> {error, channel_not_active}
    end.

check_is_closing(Channel, Height) ->
    case aesc_channels:is_solo_closing(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

-spec check_round_greater_than_last(aesc_channels:channel(), non_neg_integer())
    -> ok | {error, old_round}.
check_round_greater_than_last(Channel, Round) ->
    case aesc_channels:round(Channel) < Round of
        true  -> ok;
        false -> {error, old_round}
    end.

-spec check_is_peer(aec_keys:pubkey(), list(aec_keys:pubkey())) -> ok | {error, account_not_peer}.
check_is_peer(PubKey, Peers) ->
    case lists:member(PubKey, Peers) of
        true  -> ok;
        false -> {error, account_not_peer}
    end.

-spec check_are_funds_in_channel(aesc_channels:pubkey(), non_neg_integer(), aec_trees:trees()) ->
                                        ok | {error, insufficient_channel_funds}.
check_are_funds_in_channel(ChannelPubKey, Amount, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    Channel      = aesc_state_tree:get(ChannelPubKey, ChannelsTree),
    case aesc_channels:total_amount(Channel) >= Amount of
        true  -> ok;
        false -> {error, insufficient_channel_funds}
    end.

-spec check_state_hash_size(binary()) -> boolean().
check_state_hash_size(Hash) ->
    byte_size(Hash) =:= aec_base58c:byte_size_for_type(state).

-spec deserialize_payload(binary()) -> {ok, aetx_sign:signed_tx(), aesc_offchain_tx:tx()}
                                         | {ok, last_onchain}
                                         | {error, bad_offchain_state_type}.
deserialize_payload(<<>>) ->
    {ok, last_onchain};
deserialize_payload(Payload) ->
    try
        SignedTx = aetx_sign:deserialize_from_binary(Payload),
        Tx       = aetx_sign:tx(SignedTx),
        case aetx:specialize_type(Tx) of
            {channel_offchain_tx, PayloadTx} ->
                {ok, SignedTx, PayloadTx};
            _ ->
                {error, bad_offchain_state_type}
        end
    catch _:_ ->
            {error, payload_deserialization_failed}
    end.


%%%===================================================================
%%% Check payload for slash, solo close and snapshot
%%%===================================================================

check_solo_close_payload(ChannelPubKey, FromPubKey, Nonce, Fee, Payload,
                         PoI, Height, Trees) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Fee) end,
         fun() ->
                 check_payload(ChannelPubKey, FromPubKey, Payload, PoI,
                               Height, Trees, solo_close)
         end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

check_slash_payload(ChannelPubKey, FromPubKey, Nonce, Fee, Payload,
                    PoI, Height, Trees) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Fee) end,
         fun() ->
                 check_payload(ChannelPubKey, FromPubKey, Payload, PoI,
                               Height, Trees, slash)
         end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

check_solo_snapshot_payload(ChannelPubKey, FromPubKey, Nonce, Fee, Payload,
                            Height, Trees) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Fee) end,
         fun() ->
                 check_payload(ChannelPubKey, FromPubKey, Payload, no_poi,
                               Height, Trees, solo_snapshot)
         end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

check_payload(ChannelPubKey, FromPubKey, Payload, PoI, Height, Trees, Type) ->
    case aesc_utils:get_channel(ChannelPubKey, Trees) of
        {error, _} = E -> E;
        {ok, Channel} ->
            case aesc_utils:deserialize_payload(Payload) of
                {ok, last_onchain} when Type =:= slash ->
                    {error, slash_must_have_payload};
                {ok, last_onchain} when Type =:= solo_close ->
                    Checks =
                        [fun() -> check_is_active(Channel) end,
                         fun() -> check_root_hash_in_channel(Channel, PoI) end,
                         fun() -> check_peers_and_amounts_in_poi(Channel, PoI) end
                        ],
                    aeu_validation:run(Checks);
                {ok, SignedState, PayloadTx} ->
                    ActiveChecks =
                        case Type of
                            solo_close    -> [fun() -> check_is_active(Channel) end];
                            slash         -> [fun() -> check_is_closing(Channel, Height) end];
                            solo_snapshot -> [fun() -> check_is_active(Channel) end]
                        end,
                    PayloadChecks =
                        [fun() -> check_channel_id_in_payload(Channel, PayloadTx) end,
                         fun() -> check_round_in_payload(Channel, PayloadTx) end,
                         fun() -> is_peer_or_delegate(ChannelPubKey, FromPubKey, SignedState, Trees, Type) end,
                         fun() -> aetx_sign:verify(SignedState, Trees) end
                        ],
                    PoIChecks =
                        [fun() -> check_root_hash_in_payload(PayloadTx, PoI) end,
                         fun() -> check_peers_and_amounts_in_poi(Channel, PoI) end
                        ],
                    Checks =
                        case Type of
                            T when T =:= solo_close orelse T =:= slash ->
                                ActiveChecks ++ PayloadChecks ++ PoIChecks;
                            solo_snapshot -> ActiveChecks ++ PayloadChecks %no poi
                        end,
                    aeu_validation:run(Checks);
                {error, _Reason} = Error ->
                    Error
            end
    end.

check_peers_and_amounts_in_poi(Channel, PoI) ->
    InitiatorPubKey   = aesc_channels:initiator_pubkey(Channel),
    ResponderPubKey   = aesc_channels:responder_pubkey(Channel),
    ChannelAmount     = aesc_channels:total_amount(Channel),
    case aesc_utils:accounts_in_poi([InitiatorPubKey, ResponderPubKey], PoI) of
        {error, _} = Err -> Err;
        {ok, [PoIInitiatorAcc, PoIResponderAcc]} ->
            PoIInitiatorAmt = aec_accounts:balance(PoIInitiatorAcc),
            PoIResponderAmt = aec_accounts:balance(PoIResponderAcc),
            PoIAmount       = PoIInitiatorAmt + PoIResponderAmt,
            case ChannelAmount =:= PoIAmount of
                true  -> ok;
                false -> {error, poi_amounts_change_channel_funds}
            end
    end.

is_peer_or_delegate(ChannelPubKey, FromPubKey, SignedState, Trees, Type) ->
    case is_peer(FromPubKey, SignedState, Trees) of
        ok -> ok;
        {error, account_not_peer} = E0 ->
            case is_delegatable_tx_type(Type) of
                true ->
                    case is_delegate(ChannelPubKey, FromPubKey, Trees) of
                        ok -> ok;
                        {error, account_not_delegate} ->
                            {error, account_not_peer_or_delegate};
                        {error,_} = E ->
                            E
                    end;
                false ->
                    E0
            end
    end.

is_peer(FromPubKey, SignedState, Trees) ->
    Tx = aetx_sign:tx(SignedState),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            case lists:member(FromPubKey, Signers) of
                true  -> ok;
                false -> {error, account_not_peer}
            end;
        {error, _Reason}=Err -> Err
    end.

is_delegatable_tx_type(Type) ->
    lists:member(Type, delegatable_tx_types()).

delegatable_tx_types() ->
    [slash].

is_delegate(ChannelPubKey, FromPubKey, Trees) ->
    with_channel(fun(Channel) ->
                         is_delegate_(Channel, FromPubKey)
                 end, ChannelPubKey, Trees).

is_delegate_(Channel, FromPubKey) ->
    case lists:member(FromPubKey, aesc_channels:delegate_pubkeys(Channel)) of
        true ->
            ok;
        false ->
            {error, account_not_delegate}
    end.

with_channel(F, ChannelPubKey, Trees) ->
    case get_channel(ChannelPubKey, Trees) of
        {ok, Channel}  -> F(Channel);
        {error, _} = E -> E
    end.

check_channel_id_in_payload(Channel, PayloadTx) ->
    case aesc_channels:pubkey(Channel) =:= aesc_offchain_tx:channel_pubkey(PayloadTx) of
        false -> {error, bad_state_channel_pubkey};
        true -> ok
    end.

check_round_in_payload(Channel, PayloadTx) ->
    check_round_greater_than_last(Channel, aesc_offchain_tx:round(PayloadTx)).

check_root_hash_in_payload(PayloadTx, PoI) ->
    ChannelStateHash = aesc_offchain_tx:state_hash(PayloadTx),
    PoIHash = aec_trees:poi_hash(PoI),
    case ChannelStateHash =:= PoIHash of
        true -> ok;
        false -> {error, invalid_poi_hash}
    end.

check_root_hash_in_channel(Channel, PoI) ->
    ChannelStateHash = aesc_channels:state_hash(Channel),
    PoIHash = aec_trees:poi_hash(PoI),
    case ChannelStateHash =:= PoIHash of
        true -> ok;
        false -> {error, invalid_poi_hash}
    end.

%%%===================================================================
%%% Process payload for slash and solo close
%%%===================================================================

process_solo_close(ChannelPubKey, FromPubKey, Nonce, Fee,
                   Payload, PoI, Height, Trees) ->
    process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                             Payload, PoI, Height, Trees).


process_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
              Payload, PoI, Height, Trees) ->
    process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                             Payload, PoI, Height, Trees).

process_solo_snapshot(ChannelPubKey, FromPubKey, Nonce, Fee, Payload, Trees) ->
    ChannelsTree0      = aec_trees:channels(Trees),
    Channel0 = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    {ok, _SignedTx, PayloadTx} = deserialize_payload(Payload),
    Channel1 = aesc_channels:snapshot_solo(Channel0, PayloadTx),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),
    Trees1 = aec_trees:set_channels(Trees, ChannelsTree1),
    AccountsTree0      = aec_trees:accounts(Trees),
    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Fee, Nonce),
    AccountsTree1      = aec_accounts_trees:enter(FromAccount1, AccountsTree0),
    Trees2 = aec_trees:set_accounts(Trees1, AccountsTree1),
    {ok, Trees2}.

process_solo_close_slash(ChannelPubKey, FromPubKey, Nonce, Fee,
                         Payload, PoI, Height, Trees) ->
    AccountsTree0      = aec_trees:accounts(Trees),
    ChannelsTree0      = aec_trees:channels(Trees),
    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Fee, Nonce),
    AccountsTree1      = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    Channel0 = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    Channel1 =
        case aesc_utils:deserialize_payload(Payload) of
            {ok, _SignedTx, PayloadTx} ->
                aesc_channels:close_solo(Channel0, PayloadTx, PoI, Height);
            {ok, last_onchain} ->
                aesc_channels:close_solo(Channel0, PoI, Height)
        end,
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

