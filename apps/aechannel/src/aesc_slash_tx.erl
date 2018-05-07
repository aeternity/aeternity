%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel slash transaction
%%% @end
%%%=============================================================================
-module(aesc_slash_tx).

-include("channel_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         accounts/1,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SLASH_TX_VSN, 1).
-define(CHANNEL_SLASH_TX_TYPE, channel_slash_tx).
-define(CHANNEL_SLASH_TX_FEE, 0).

-opaque tx() :: #channel_slash_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      from       := FromPubKey,
      payload    := Payload,
      ttl        := TTL,
      fee        := Fee,
      nonce      := Nonce}) ->
    Tx = #channel_slash_tx{
            channel_id = ChannelId,
            from       = FromPubKey,
            payload    = Payload,
            ttl        = TTL,
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SLASH_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_slash_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_slash_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_slash_tx{from = FromPubKey}) ->
    FromPubKey.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_slash_tx{channel_id = ChannelId,
                        from       = FromPubKey,
                        payload    = Payload,
                        ttl        = TTL,
                        fee        = Fee,
                        nonce        = Nonce}, _Context, Trees, Height,
                                                _ConsensusVersion) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aetx_utils:check_ttl(TTL, Height) end,
         fun() -> check_payload(ChannelId, FromPubKey, Payload, Height, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#channel_slash_tx{channel_id = ChannelId,
                          from       = FromPubKey,
                          payload    = Payload,
                          fee        = Fee,
                          nonce        = Nonce}, _Context, Trees, Height,
                                                  _ConsensusVersion) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Fee, Nonce, Height),
    AccountsTree1           = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    {ok, _SignedTx, StateTx} = deserialize_from_binary(Payload),
    Channel0                 = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1                 = aesc_channels:slash(Channel0, StateTx, Height),
    ChannelsTree1            = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_slash_tx{payload = Payload}) ->
    case deserialize_from_binary(Payload) of
        {ok, SignedState, _StateTx} -> aetx:signers(aetx_sign:tx(SignedState),
                                                    aec_trees:new());
        {error, _Reason}            -> []
    end.

-spec signers(tx(), aec_trees:trees()) -> {ok, list(pubkey())}.
signers(#channel_slash_tx{from = FromPubKey}, _) ->
    {ok, [FromPubKey]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_slash_tx{channel_id = ChannelId,
                            from       = FromPubKey,
                            payload    = Payload,
                            ttl        = TTL,
                            fee        = Fee,
                            nonce      = Nonce}) ->
    {version(),
     [ {channel_id, ChannelId}
     , {from      , FromPubKey}
     , {payload   , Payload}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SLASH_TX_VSN,
            [ {channel_id, ChannelId}
            , {from      , FromPubKey}
            , {payload   , Payload}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    #channel_slash_tx{channel_id = ChannelId,
                      from       = FromPubKey,
                      payload    = Payload,
                      ttl        = TTL,
                      fee        = Fee,
                      nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_slash_tx{channel_id = ChannelId,
                             from       = FromPubKey,
                             payload    = Payload,
                             ttl        = TTL,
                             fee        = Fee,
                             nonce      = Nonce}) ->
    %% TODO: add swagger schema name
    #{<<"vsn">>        => version(),
      <<"channel_id">> => aec_base58c:encode(channel, ChannelId),
      <<"from">>    => aec_base58c:encode(account_pubkey, FromPubKey),
      <<"payload">>    => Payload,
      <<"ttl">>        => TTL,
      <<"fee">>        => Fee,
      <<"nonce">>      => Nonce}.

serialization_template(?CHANNEL_SLASH_TX_VSN) ->
    [ {channel_id, binary}
    , {from      , binary}
    , {payload   , binary}
    , {ttl       , int}
    , {fee       , int}
    , {nonce     , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_payload(aesc_channels:id(), pubkey(), binary(), height(), aec_trees:trees()) ->
                           ok | {error, term()}.
check_payload(ChannelId, FromPubKey, Payload, Height, Trees) ->
    case deserialize_from_binary(Payload) of
        {ok, SignedState, StateTx} ->
            Checks =
                [fun() -> is_peer(FromPubKey, SignedState, Trees) end,
                 fun() -> aetx_sign:verify(SignedState, Trees) end,
                 fun() -> check_channel(ChannelId, StateTx, Height, Trees) end],
            aeu_validation:run(Checks);
        {error, _Reason} = Error ->
            Error
    end.

deserialize_from_binary(Payload) ->
    try
        SignedTx = aetx_sign:deserialize_from_binary(Payload),
        Tx       = aetx_sign:tx(SignedTx),
        case aetx:specialize_type(Tx) of
            {channel_offchain_tx, StateTx} ->
                {ok, SignedTx, StateTx};
            {_Type, _TxBody} ->
                {error, bad_offchain_state_type}
        end
    catch _:_ ->
            {error, payload_deserialization_failed}
    end.

is_peer(AccountPubKey, SignedState, Trees) ->
    Tx = aetx_sign:tx(SignedState),
    case aetx:signers(Tx, Trees) of
        {ok, Signers} ->
            case lists:member(AccountPubKey, Signers) of
                true  -> ok;
                false -> {error, account_not_peer}
            end;
        {error, _Reason}=Err -> Err
    end.

check_channel(ChannelId, StateTx, Height, Trees) ->
    case ChannelId =:= aesc_offchain_tx:channel_id(StateTx) of
        true ->
            ChannelsTree = aec_trees:channels(Trees),
            case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
                none ->
                    {error, channel_does_not_exist};
                {value, Channel} ->
                    Checks =
                        [fun() -> check_solo_closing(Channel, Height) end,
                         fun() -> check_peers_equal(StateTx, Channel) end,
                         fun() -> check_amounts_equal(StateTx, Channel) end,
                         fun() -> check_round(StateTx, Channel) end],
                    aeu_validation:run(Checks)
            end;
        false ->
            {error, bad_state_channel_id}
    end.

check_peers_equal(State, Channel) ->
    case aesc_channels:initiator(Channel) =:= aesc_offchain_tx:initiator(State)
        andalso aesc_channels:responder(Channel) =:= aesc_offchain_tx:responder(State) of
        true ->
            ok;
        false ->
            {error, wrong_channel_peers}
    end.

check_amounts_equal(State, Channel) ->
    ChannelAmount = aesc_channels:total_amount(Channel),
    StateAmount   = aesc_offchain_tx:initiator_amount(State) + aesc_offchain_tx:responder_amount(State),
    case ChannelAmount =:= StateAmount of
        true  -> ok;
        false -> {error, wrong_state_amount}
    end.

check_solo_closing(Channel, Height) ->
    case aesc_channels:is_solo_closing(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

check_round(State, Channel) ->
    case aesc_channels:round(Channel) < aesc_offchain_tx:round(State) of
        true  -> ok;
        false -> {error, state_round_too_small}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SLASH_TX_VSN.
