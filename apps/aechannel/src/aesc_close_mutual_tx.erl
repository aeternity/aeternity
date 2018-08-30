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
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/6,
         signers/2,
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
      initiator_amount_final  := InitiatorAmount,
      responder_amount_final  := ResponderAmount,
      fee                     := Fee,
      nonce                   := Nonce} = Args) ->
    channel = aec_id:specialize_type(ChannelId),
    Tx = #channel_close_mutual_tx{
            channel_id              = ChannelId,
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

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_close_mutual_tx{ttl = Ttl}) ->
    Ttl.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_close_mutual_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey() | undefined.
origin(#channel_close_mutual_tx{} = Tx) ->
    ChannelId = channel_pubkey(Tx),
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            aesc_channels:initiator_pubkey(Channel);
        {error, not_found} -> undefined
    end.

channel_pubkey(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{initiator_amount_final = InitiatorAmount,
                               responder_amount_final = ResponderAmount,
                               fee                    = Fee,
                               nonce                  = Nonce} = Tx,
      _Context, Trees, _Height, _ConsensusVersion) ->
    ChannelPubKey = channel_pubkey(Tx),
    case aesc_state_tree:lookup(ChannelPubKey, aec_trees:channels(Trees)) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            InitiatorPubKey = aesc_channels:initiator_pubkey(Channel),
            Checks =
                [% the fee is being split between parties so no check if the
                 % initiator can pay the fee; just a check for the nonce correctness
                 fun() -> aetx_utils:check_account(InitiatorPubKey, Trees, Nonce, 0) end,
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

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(),
              non_neg_integer(), binary() | no_tx_hash) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{initiator_amount_final = InitiatorAmount,
                                 responder_amount_final = ResponderAmount,
                                 ttl                    = _TTL,
                                 fee                    = _Fee,
                                 nonce                  = Nonce} = Tx,
        _Context, Trees, _Height, _ConsensusVersion, _TxHash) ->
    ChannelPubKey = channel_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel         = aesc_state_tree:get(ChannelPubKey, ChannelsTree0),
    InitiatorPubKey = aesc_channels:initiator_pubkey(Channel),
    ResponderPubKey = aesc_channels:responder_pubkey(Channel),

    InitiatorAccount0         = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    {ok, InitiatorAccount1}   = aec_accounts:earn(InitiatorAccount0,
                                                   InitiatorAmount),
    InitiatorAccount        = aec_accounts:set_nonce(InitiatorAccount1, Nonce),
    ResponderAccount0       = aec_accounts_trees:get(ResponderPubKey, AccountsTree0),
    {ok, ResponderAccount}  = aec_accounts:earn(ResponderAccount0,
                                                ResponderAmount),

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount, AccountsTree1),

    ChannelsTree = aesc_state_tree:delete(aesc_channels:pubkey(Channel), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree),
    {ok, Trees2}.

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
                                   initiator_amount_final = InitiatorAmount,
                                   responder_amount_final = ResponderAmount,
                                   ttl                    = TTL,
                                   fee                    = Fee,
                                   nonce                  = Nonce}) ->
    {version(),
     [ {channel_id              , ChannelId}
     , {initiator_amount_final  , InitiatorAmount}
     , {responder_amount_final  , ResponderAmount}
     , {ttl                     , TTL}
     , {fee                     , Fee}
     , {nonce                   , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_MUTUAL_TX_VSN,
            [ {channel_id             , ChannelId}
            , {initiator_amount_final , InitiatorAmount}
            , {responder_amount_final , ResponderAmount}
            , {ttl                    , TTL}
            , {fee                    , Fee}
            , {nonce                  , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    #channel_close_mutual_tx{channel_id             = ChannelId,
                             initiator_amount_final = InitiatorAmount,
                             responder_amount_final = ResponderAmount,
                             ttl                    = TTL,
                             fee                    = Fee,
                             nonce                  = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id             = ChannelId,
                                    initiator_amount_final = InitiatorAmount,
                                    responder_amount_final = ResponderAmount,
                                    ttl                    = TTL,
                                    fee                    = Fee,
                                    nonce                  = Nonce}) ->
    #{<<"vsn">>                     => version(),
      <<"channel_id">>              => aec_base58c:encode(id_hash, ChannelId),
      <<"initiator_amount_final">>  => InitiatorAmount,
      <<"responder_amount_final">>  => ResponderAmount,
      <<"ttl">>                     => TTL,
      <<"fee">>                     => Fee,
      <<"nonce">>                   => Nonce}.

serialization_template(?CHANNEL_CLOSE_MUTUAL_TX_VSN) ->
    [ {channel_id             , id}
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

