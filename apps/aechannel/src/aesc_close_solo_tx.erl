%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close solo transaction
%%% @end
%%%=============================================================================
-module(aesc_close_solo_tx).

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
         signers/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         is_verifiable/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CLOSE_SOLO_TX_VSN, 1).
-define(CHANNEL_CLOSE_SOLO_TX_TYPE, channel_close_solo_tx).
-define(CHANNEL_CLOSE_SOLO_TX_FEE, 4).

-opaque tx() :: #channel_close_solo_tx{}.

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
    Tx = #channel_close_solo_tx{
            channel_id = ChannelId,
            from       = FromPubKey,
            payload    = Payload,
            ttl        = TTL,
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLOSE_SOLO_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_solo_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_solo_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_close_solo_tx{from = FromPubKey}) ->
    FromPubKey.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_solo_tx{channel_id = ChannelId,
                             from       = FromPubKey,
                             payload    = Payload,
                             ttl        = TTL,
                             fee        = Fee,
                             nonce        = Nonce}, _Context, Trees, Height,
                                                _ConsensusVersion) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> aetx_utils:check_ttl(TTL, Height) end,
         fun() -> check_payload(ChannelId, FromPubKey, Payload, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#channel_close_solo_tx{channel_id = ChannelId,
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
    Channel1                 = aesc_channels:close_solo(Channel0, StateTx, Height),
    ChannelsTree1            = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_solo_tx{payload = Payload}) ->
    case deserialize_from_binary(Payload) of
        {ok, SignedState, _StateTx} -> aetx:signers(aetx_sign:tx(SignedState));
        {error, _Reason}            -> []
    end.

-spec signers(tx()) -> list(pubkey()).
signers(#channel_close_solo_tx{from = FromPubKey}) ->
    [FromPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_solo_tx{channel_id = ChannelId,
                                 from       = FromPubKey,
                                 payload    = Payload,
                                 ttl        = TTL,
                                 fee        = Fee,
                                 nonce      = Nonce}) ->
    {version(),
     [ {channel_id, ChannelId}
     , {from   , FromPubKey}
     , {payload   , Payload}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_SOLO_TX_VSN,
            [ {channel_id, ChannelId}
            , {from      , FromPubKey}
            , {payload   , Payload}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    #channel_close_solo_tx{channel_id = ChannelId,
                           from       = FromPubKey,
                           payload    = Payload,
                           ttl        = TTL,
                           fee        = Fee,
                           nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_solo_tx{channel_id = ChannelId,
                                  from       = FromPubKey,
                                  payload    = Payload,
                                  ttl        = TTL,
                                  fee        = Fee,
                                  nonce      = Nonce}) ->
    #{<<"data_schema">>=> <<"ChannelCloseSoloTxJSON">>, % swagger schema name
      <<"vsn">>        => version(),
      <<"channel_id">> => aec_base58c:encode(channel, ChannelId),
      <<"from">>    => aec_base58c:encode(account_pubkey, FromPubKey),
      <<"payload">>    => Payload,
      <<"ttl">>        => TTL,
      <<"fee">>        => Fee,
      <<"nonce">>      => Nonce}.

serialization_template(?CHANNEL_CLOSE_SOLO_TX_VSN) ->
    [ {channel_id, binary}
    , {from      , binary}
    , {payload   , binary}
    , {ttl       , int}
    , {fee       , int}
    , {nonce     , int}
    ].

-spec is_verifiable(tx()) -> boolean().
is_verifiable(#channel_close_solo_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, _Channel} -> true;
        {error, not_found} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_payload(aesc_channels:id(), pubkey(), binary(), aec_trees:trees()) ->
                           ok | {error, term()}.
check_payload(ChannelId, FromPubKey, Payload, Trees) ->
    case deserialize_from_binary(Payload) of
        {ok, SignedState, StateTx} ->
            Checks =
                [fun() -> is_peer(FromPubKey, SignedState) end,
                 fun() -> aetx_sign:verify(SignedState) end,
                 fun() -> check_channel(ChannelId, StateTx, Trees) end],
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

is_peer(FromPubKey, SignedState) ->
    Tx = aetx_sign:tx(SignedState),
    case lists:member(FromPubKey, aetx:signers(Tx)) of
        true  -> ok;
        false -> {error, account_not_peer}
    end.

check_channel(ChannelId, StateTx, Trees) ->
    case ChannelId =:= aesc_offchain_tx:channel_id(StateTx) of
        true ->
            aesc_utils:check_active_channel_exists(
              ChannelId, StateTx, Trees);
        false ->
            {error, bad_state_channel_id}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_SOLO_TX_VSN.
