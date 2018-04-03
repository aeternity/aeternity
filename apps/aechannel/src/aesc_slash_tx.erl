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
         check/3,
         process/3,
         accounts/1,
         signers/1,
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
      account    := AccountPubKey,
      payload    := Payload,
      fee        := Fee,
      nonce      := Nonce}) ->
    Tx = #channel_slash_tx{
            channel_id = ChannelId,
            account    = AccountPubKey,
            payload    = Payload,
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
origin(#channel_slash_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_slash_tx{channel_id = ChannelId,
                        account    = AccountPubKey,
                        payload    = Payload,
                        fee        = Fee,
                        nonce      = Nonce}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_payload(ChannelId, AccountPubKey, Payload, Height, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_slash_tx{channel_id = ChannelId,
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
    Channel1      = aesc_channels:slash(Channel0, State, Height),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_slash_tx{payload = Payload}) ->
    SignedState = aesc_state_signed:deserialize(Payload),
    aesc_state:pubkeys(aesc_state_signed:state(SignedState)).

-spec signers(tx()) -> list(pubkey()).
signers(#channel_slash_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_slash_tx{channel_id = ChannelId,
                            account    = AccountPubKey,
                            payload    = Payload,
                            fee        = Fee,
                            nonce      = Nonce}) ->
    {version(),
    [ {channel_id, ChannelId}
    , {account   , AccountPubKey}
    , {payload   , Payload}
    , {fee       , Fee}
    , {nonce     , Nonce}
    ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SLASH_TX_VSN,
            [ {channel_id, ChannelId}
            , {account   , AccountPubKey}
            , {payload   , Payload}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    #channel_slash_tx{channel_id = ChannelId,
                      account    = AccountPubKey,
                      payload    = Payload,
                      fee        = Fee,
                      nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_slash_tx{channel_id = ChannelId,
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

serialization_template(?CHANNEL_SLASH_TX_VSN) ->
    [ {channel_id, binary}
    , {account   , binary}
    , {payload   , binary}
    , {fee       , int}
    , {nonce     , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_payload(aesc_channels:id(), pubkey(), binary(), height(), aec_trees:trees()) ->
                           ok | {error, term()}.
check_payload(ChannelId, AccountPubKey, Payload, Height, Trees) ->
    %% TODO: Catch errors in deserialization in case someone sends borked payload
    SignedState = aesc_state_signed:deserialize(Payload),
    State       = aesc_state_signed:state(SignedState),
    Peers       = aesc_state:pubkeys(State),
    Checks =
        [fun() -> is_peer(AccountPubKey, Peers) end,
         fun() -> verify_signatures(SignedState) end,
         fun() -> check_channel(ChannelId, State, Height, Trees) end],
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

check_channel(ChannelId, State, Height, Trees) ->
    ChannelsTree = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelId, ChannelsTree) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            Checks =
                [fun() -> check_peers_equal(State, Channel) end,
                 fun() -> check_amounts_equal(State, Channel) end,
                 fun() -> check_solo_closing(Channel, Height) end,
                 fun() -> check_seq_number(State, Channel) end],
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

check_amounts_equal(State, Channel) ->
    ChannelAmount = aesc_channels:initiator_amount(Channel) + aesc_channels:participant_amount(Channel),
    StateAmount   = aesc_state:initiator_amount(State) + aesc_state:responder_amount(State),
    case ChannelAmount =:= StateAmount of
        true  -> ok;
        false -> {error, wrong_state_amounts}
    end.

check_solo_closing(Channel, Height) ->
    case aesc_channels:is_solo_closing(Channel, Height) of
        true  -> ok;
        false -> {error, channel_not_closing}
    end.

check_seq_number(State, Channel) ->
    case aesc_channels:sequence_number(Channel) < aesc_state:sequence_number(State) of
        true  -> ok;
        false -> {error, state_seq_number_too_small}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SLASH_TX_VSN.
