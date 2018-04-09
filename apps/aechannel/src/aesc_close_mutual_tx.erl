%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close mutual transaction
%%% @end
%%%=============================================================================
-module(aesc_close_mutual_tx).

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
         for_client/1,
         verifiable/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CLOSE_MUTUAL_TX_VSN, 1).
-define(CHANNEL_CLOSE_MUTUAL_TX_TYPE, channel_close_mutual_tx).
-define(CHANNEL_CLOSE_MUTUAL_TX_FEE, 4).

-opaque tx() :: #channel_close_mutual_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id  := ChannelId,
      amount      := Amount,
      fee         := Fee,
      nonce       := Nonce}) ->
    Tx = #channel_close_mutual_tx{
            channel_id  = ChannelId,
            amount      = Amount,
            fee         = Fee,
            nonce       = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_mutual_tx{fee = Fee}) ->
    Fee.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_mutual_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> pubkey() | undefined.
origin(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            aesc_channels:initiator(Channel);
        {error, not_found} -> undefined
    end.

-spec check(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{channel_id  = ChannelId,
                               amount      = Amount,
                               fee         = Fee,
                               nonce       = Nonce}, Trees, Height) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            InitiatorPubKey = aesc_channels:initiator(Channel),
            ParticipantPubKey = aesc_channels:participant(Channel),
            Checks =
                [fun() -> aetx_utils:check_account(InitiatorPubKey, Trees, Height, Nonce, Fee) end,
                 fun() ->
                    case aesc_channels:is_active(Channel) of
                        true -> ok;
                        false -> {error, channel_not_active}
                    end
                end,
                fun() -> check_peer_has_funds(InitiatorPubKey, ParticipantPubKey, ChannelId, Amount, Trees) end],
            case aeu_validation:run(Checks) of
                ok ->
                    {ok, Trees};
                {error, _Reason} = Error ->
                    Error
            end
    end.

-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{channel_id  = ChannelId,
                                 amount      = Amount,
                                 fee         = Fee,
                                 nonce       = Nonce}, Trees, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    InitiatorPubKey = aesc_channels:initiator(Channel0),
    ParticipantPubKey = aesc_channels:participant(Channel0),
    Channel1      = aesc_channels:subtract_funds(Channel0, Amount, InitiatorPubKey),
    Channel2      = aesc_channels:add_funds(Channel1, Amount, ParticipantPubKey),

    InitiatorAccount0         = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    {ok, InitiatorAccount1}   = aec_accounts:spend(InitiatorAccount0, Fee, Nonce, Height),
    {ok, InitiatorAccount2}   = aec_accounts:earn(InitiatorAccount1, aesc_channels:initiator_amount(Channel2), Height),
    ParticipantAccount0       = aec_accounts_trees:get(ParticipantPubKey, AccountsTree0),
    {ok, ParticipantAccount1} = aec_accounts:earn(ParticipantAccount0, aesc_channels:participant_amount(Channel2), Height),

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount2, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ParticipantAccount1, AccountsTree1),

    ChannelsTree1 = aesc_state_tree:delete(aesc_channels:id(Channel2), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            [aesc_channels:initiator(Channel), aesc_channels:participant(Channel)];
        {error, not_found} -> []
    end.

-spec signers(tx()) -> list(pubkey()).
signers(#channel_close_mutual_tx{} = CloseTx) ->
    accounts(CloseTx).

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_mutual_tx{channel_id  = ChannelId,
                                   amount      = Amount,
                                   fee         = Fee,
                                   nonce       = Nonce}) ->
    {version(),
     [ {channel_id , ChannelId}
     , {amount     , Amount}
     , {fee        , Fee}
     , {nonce      , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_MUTUAL_TX_VSN,
            [ {channel_id , ChannelId}
            , {amount     , Amount}
            , {fee        , Fee}
            , {nonce      , Nonce}]) ->
    #channel_close_mutual_tx{channel_id  = ChannelId,
                             amount      = Amount,
                             fee         = Fee,
                             nonce       = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id  = ChannelId,
                                    amount      = Amount,
                                    fee         = Fee,
                                    nonce       = Nonce}) ->
    #{<<"data_schema">> => <<"ChannelCloseMutualTxJSON">>, % swagger schema name
      <<"vsn">>         => version(),
      <<"channel_id">>  => aec_base58c:encode(channel, ChannelId),
      <<"amount">>      => Amount,
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_CLOSE_MUTUAL_TX_VSN) ->
    [ {channel_id , binary}
    , {amount     , int}
    , {fee        , int}
    , {nonce      , int}
    ].

-spec verifiable(tx()) -> boolean().
verifiable(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, _Channel} -> true;
        {error, not_found} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_peer_has_funds(pubkey(), pubkey(), aesc_channels:id(), aesc_channels:amount(), aec_trees:trees()) ->
                                  ok | {error, insufficient_channel_peer_amount}.
check_peer_has_funds(InitiatorPubkey, ParticipantPubKey, ChannelId, Amount, Trees) ->
    case Amount > 0 of
        true  -> aesc_utils:check_peer_has_funds(InitiatorPubkey, ChannelId, Amount, Trees);
        false -> aesc_utils:check_peer_has_funds(ParticipantPubKey, ChannelId, Amount, Trees)
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_VSN.
