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
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CLOSE_SOLO_TX_VSN, 1).

-opaque tx() :: #channel_close_solo_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      account    := AccountPubKey,
      payload    := Payload,
      fee        := Fee,
      nonce      := Nonce}) ->
    Tx = #channel_close_solo_tx{
            channel_id = ChannelId,
            account    = AccountPubKey,
            payload    = Payload,
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_solo_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_solo_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey().
origin(#channel_close_solo_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_solo_tx{channel_id = ChannelId,
                             account    = AccountPubKey,
                             payload    = Payload,
                             fee        = Fee,
                             nonce      = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_payload(ChannelId, AccountPubKey, Payload, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_close_solo_tx{channel_id = ChannelId,
                               account    = AccountPubKey,
                               payload    = Payload,
                               fee        = Fee,
                               nonce      = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    InitiatorAccount0       = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, InitiatorAccount1} = aec_accounts:spend(InitiatorAccount0, Fee, Nonce, Height),
    AccountsTree1           = aec_accounts_trees:enter(InitiatorAccount1, AccountsTree0),

    State         = aesc_state_signed:state(aesc_state_signed:deserialize(Payload)),
    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:close_solo(Channel0, State),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_solo_tx{payload = Payload}) ->
    %% TODO: Catch errors in deserialization in case someone sends borked payload
    SignedState = aesc_state_signed:deserialize(Payload),
    aesc_state:pubkeys(aesc_state_signed:state(SignedState)).

-spec signers(tx()) -> list(pubkey()).
signers(#channel_close_solo_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> list(map()).
serialize(#channel_close_solo_tx{channel_id = ChannelId,
                                 account    = AccountPubKey,
                                 payload    = Payload,
                                 fee        = Fee,
                                 nonce      = Nonce}) ->
    [#{<<"vsn">>        => version()},
     #{<<"channel_id">> => ChannelId},
     #{<<"account">>    => AccountPubKey},
     #{<<"payload">>    => Payload},
     #{<<"fee">>        => Fee},
     #{<<"nonce">>      => Nonce}].

-spec deserialize(list(map())) -> tx().
deserialize([#{<<"vsn">>        := ?CHANNEL_CLOSE_SOLO_TX_VSN},
             #{<<"channel_id">> := ChannelId},
             #{<<"account">>    := AccountPubKey},
             #{<<"payload">>    := Payload},
             #{<<"fee">>        := Fee},
             #{<<"nonce">>      := Nonce}]) ->
    #channel_close_solo_tx{channel_id = ChannelId,
                           account    = AccountPubKey,
                           payload    = Payload,
                           fee        = Fee,
                           nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_solo_tx{channel_id = ChannelId,
                                  account    = AccountPubKey,
                                  payload    = Payload,
                                  fee        = Fee,
                                  nonce      = Nonce}) ->
    %% TODO: add swagger schema name
    #{<<"vsn">>        => version(),
      <<"channel_id">> => aec_base58c:encode(channel, ChannelId),
      <<"account">>    => aec_base58c:encode(account_pubkey, AccountPubKey),
      <<"payload">>    => Payload,
      <<"fee">>        => Fee,
      <<"nonce">>      => Nonce}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_payload(ChannelId, AccountPubKey, Payload, Trees) ->
    %% TODO: Catch errors in deserialization in case someone sends borked payload
    SignedState = aesc_state_signed:deserialize(Payload),
    State       = aesc_state_signed:state(SignedState),
    Peers       = aesc_state:pubkeys(State),
    Checks =
        [fun() -> is_peer(AccountPubKey, Peers) end,
         fun() -> verify_signatures(SignedState) end,
         fun() -> check_channel(ChannelId, State, Trees) end],
    aeu_validation:run(Checks).

is_peer(AccountPubKey, Peers) ->
    case lists:member(AccountPubKey, Peers) of
        true  -> ok;
        false -> {error, account_not_peers}
    end.

verify_signatures(SignedState) ->
    case aesc_state_signed:verify(SignedState) of
        true  -> ok;
        false -> {error, wrong_payload_signatures}
    end.

check_channel(ChannelId, State, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            Checks =
                [fun() -> check_peers_equal(State, Channel) end,
                 fun() -> check_not_closing(Channel) end],
            aeu_validation:run(Checks)
    end.

check_peers_equal(State, Channel) ->
    case aesc_channels:initiator(Channel) =:= aesc_state:initiator(State)
        andalso aesc_channels:participant(Channel) =:= aesc_state:responder(State) of
        true ->
            ok;
        false ->
            {error, wrong_channel_peers}
    end.

check_not_closing(Channel) ->
    case aesc_channels:is_solo_closing(Channel) of
        true  -> {error, already_closing};
        false -> ok
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_SOLO_TX_VSN.
