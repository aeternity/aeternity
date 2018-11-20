%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel close mutual transaction
%%% @end
%%%=============================================================================
-module(aesc_close_mutual_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         initiator_amount_final/1,
         responder_amount_final/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type vsn() :: non_neg_integer().

-define(CHANNEL_CLOSE_MUTUAL_TX_VSN, 1).
-define(CHANNEL_CLOSE_MUTUAL_TX_TYPE, channel_close_mutual_tx).

-record(channel_close_mutual_tx, {
          channel_id              :: aec_id:id(),
          from_id                 :: aec_id:id(),
          initiator_amount_final  :: non_neg_integer(),
          responder_amount_final  :: non_neg_integer(),
          ttl                     :: aetx:tx_ttl(),
          fee                     :: non_neg_integer(),
          nonce                   :: non_neg_integer()
         }).

-opaque tx() :: #channel_close_mutual_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id              := ChannelId,
      from_id                 := FromId,
      initiator_amount_final  := InitiatorAmount,
      responder_amount_final  := ResponderAmount,
      fee                     := Fee,
      nonce                   := Nonce} = Args) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    Tx = #channel_close_mutual_tx{
            channel_id              = ChannelId,
            from_id                 = FromId,
            initiator_amount_final  = InitiatorAmount,
            responder_amount_final  = ResponderAmount,
            ttl                     = maps:get(ttl, Args, 0),
            fee                     = Fee,
            nonce                   = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_close_mutual_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_close_mutual_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_close_mutual_tx{ttl = Ttl}) ->
    Ttl.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_mutual_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_close_mutual_tx{from_id = FromId}) ->
    aec_id:specialize(FromId, account).

channel_pubkey(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{from_id = FromId,
                               initiator_amount_final = InitiatorAmount,
                               responder_amount_final = ResponderAmount,
                               fee                    = Fee,
                               nonce                  = Nonce} = Tx,
      Trees,_Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    case aesc_state_tree:lookup(ChannelPubKey, aec_trees:channels(Trees)) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            FromIdPubKey = aec_id:specialize(FromId, account),
            InitiatorPubKey = aesc_channels:initiator_pubkey(Channel),
            ResponderPubKey = aesc_channels:responder_pubkey(Channel),
            Checks =
                [% the fee is being split between parties so no check if the
                 % initiator can pay the fee; just a check for the nonce correctness
                 fun() -> ok_or_error(FromIdPubKey == InitiatorPubKey
                                      orelse FromIdPubKey == ResponderPubKey,
                                      illegal_origin)
                 end,
                 fun() -> aetx_utils:check_account(FromIdPubKey, Trees, Nonce, 0) end,
                 fun() ->
                    case aesc_channels:is_active(Channel) of
                        true -> ok;
                        false -> {error, channel_not_active}
                    end
                end,
                fun() -> % check amounts
                    ChannelAmt = aesc_channels:channel_amount(Channel),
                    ok_or_error(ChannelAmt >= InitiatorAmount + ResponderAmount + Fee,
                                wrong_amounts)
                end
                ],
            case aeu_validation:run(Checks) of
                ok ->
                    {ok, Trees};
                {error, _Reason} = Error ->
                    Error
            end
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{initiator_amount_final = InitiatorAmount,
                                 responder_amount_final = ResponderAmount,
                                 ttl                    = _TTL,
                                 fee                    = Fee,
                                 nonce                  = Nonce} = Tx,
        Trees,_Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel         = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    FromIdPubKey    = origin(Tx),
    InitiatorPubKey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubKey = aesc_channels:responder_pubkey(Channel),

    InitiatorAccount0         = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    {ok, InitiatorAccount1}   = aec_accounts:earn(InitiatorAccount0,
                                                   InitiatorAmount),

    ResponderAccount0       = aec_accounts_trees:get(ResponderPubKey, AccountsTree0),
    {ok, ResponderAccount1}  = aec_accounts:earn(ResponderAccount0,
                                                 ResponderAmount),

    {InitiatorAccount, ResponderAccount} =
        update_nonce(FromIdPubKey, InitiatorPubKey, ResponderPubKey,
                     InitiatorAccount1, ResponderAccount1, Nonce),
    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount, AccountsTree1),

    % all coins that are not in the final balance are locked
    % they're sent to the special account that holds locked coins
    % fee is being payed by the channel's balance
    DistributedAmounts = InitiatorAmount + ResponderAmount + Fee,
    MissingCoins = aesc_channels:channel_amount(Channel) - DistributedAmounts,
    AccountsTree3 = aec_accounts_trees:lock_coins(MissingCoins,
                                                  AccountsTree2),

    ChannelsTree = aesc_state_tree:delete(aesc_channels:pubkey(Channel), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree3),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree),
    {ok, Trees2}.

update_nonce(IK, IK, _, I, R, Nonce) ->
    {aec_accounts:set_nonce(I, Nonce), R};
update_nonce(RK, _, RK, I, R, Nonce) ->
    {I, aec_accounts:set_nonce(R, Nonce)}.

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_close_mutual_tx{} = Tx, Trees) ->
    case aec_chain:get_channel(channel_pubkey(Tx), Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_mutual_tx{channel_id             = ChannelId,
                                   from_id                = FromId,
                                   initiator_amount_final = InitiatorAmount,
                                   responder_amount_final = ResponderAmount,
                                   ttl                    = TTL,
                                   fee                    = Fee,
                                   nonce                  = Nonce}) ->
    {version(),
     [ {channel_id              , ChannelId}
     , {from_id                 , FromId}
     , {initiator_amount_final  , InitiatorAmount}
     , {responder_amount_final  , ResponderAmount}
     , {ttl                     , TTL}
     , {fee                     , Fee}
     , {nonce                   , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_MUTUAL_TX_VSN,
            [ {channel_id             , ChannelId}
            , {from_id                , FromId}
            , {initiator_amount_final , InitiatorAmount}
            , {responder_amount_final , ResponderAmount}
            , {ttl                    , TTL}
            , {fee                    , Fee}
            , {nonce                  , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    #channel_close_mutual_tx{channel_id             = ChannelId,
                             from_id                = FromId,
                             initiator_amount_final = InitiatorAmount,
                             responder_amount_final = ResponderAmount,
                             ttl                    = TTL,
                             fee                    = Fee,
                             nonce                  = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id             = ChannelId,
                                    from_id                = FromId,
                                    initiator_amount_final = InitiatorAmount,
                                    responder_amount_final = ResponderAmount,
                                    ttl                    = TTL,
                                    fee                    = Fee,
                                    nonce                  = Nonce}) ->
    #{<<"channel_id">>              => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>                 => aehttp_api_encoder:encode(id_hash, FromId),
      <<"initiator_amount_final">>  => InitiatorAmount,
      <<"responder_amount_final">>  => ResponderAmount,
      <<"ttl">>                     => TTL,
      <<"fee">>                     => Fee,
      <<"nonce">>                   => Nonce}.

serialization_template(?CHANNEL_CLOSE_MUTUAL_TX_VSN) ->
    [ {channel_id             , id}
    , {from_id                , id}
    , {initiator_amount_final , int}
    , {responder_amount_final , int}
    , {ttl                    , int}
    , {fee                    , int}
    , {nonce                  , int}
    ].

-spec initiator_amount_final(tx()) -> integer().
initiator_amount_final(#channel_close_mutual_tx{initiator_amount_final  = Amount}) ->
    Amount.

-spec responder_amount_final(tx()) -> integer().
responder_amount_final(#channel_close_mutual_tx{responder_amount_final  = Amount}) ->
    Amount.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ok_or_error(false, ErrMsg) -> {error, ErrMsg};
ok_or_error(true, _) -> ok.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_VSN.

