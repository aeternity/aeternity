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

-define(CHANNEL_CLOSE_MUTUAL_TX_VSN, 1).
-define(CHANNEL_CLOSE_MUTUAL_TX_TYPE, channel_close_mutual_tx).
-define(CHANNEL_CLOSE_MUTUAL_TX_FEE, 4).

-opaque tx() :: #channel_close_mutual_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id        := ChannelId,
      from              := From,
      initiator_amount  := InitiatorAmount,
      responder_amount  := ResponderAmount,
      ttl               := TTL,
      fee               := Fee,
      nonce             := Nonce}) ->
    Tx = #channel_close_mutual_tx{
            channel_id        = ChannelId,
            from              = From,
            initiator_amount  = InitiatorAmount,
            responder_amount  = ResponderAmount,
            ttl               = TTL,
            fee               = Fee,
            nonce             = Nonce},
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

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_close_mutual_tx{channel_id       = ChannelId,
                               from             = From,
                               initiator_amount = InitiatorAmount,
                               responder_amount = ResponderAmount,
                               ttl              = TTL,
                               fee              = Fee,
                               nonce            = Nonce}, _Context, Trees, Height,
                                                _ConsensusVersion) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        none ->
            {error, channel_does_not_exist};
        {value, Channel} ->
            InitiatorPubKey = aesc_channels:initiator(Channel),
            Checks =
                [fun() -> aesc_utils:check_is_peer(From, aesc_channels:peers(Channel)) end,
                 fun() -> aetx_utils:check_account(InitiatorPubKey, Trees, Height, Nonce, Fee) end,
                 fun() ->
                    case aesc_channels:is_active(Channel) of
                        true -> ok;
                        false -> {error, channel_not_active}
                    end
                end,
                fun() -> % check fee
                    ok_or_error(InitiatorAmount + ResponderAmount >= Fee, fee_too_big)
                end,
                fun() -> % check amounts
                    ChannelAmt = aesc_channels:total_amount(Channel),
                    ok_or_error(ChannelAmt =:= InitiatorAmount + ResponderAmount,
                                wrong_amounts)
                end,
                fun() -> % check TTL
                    ok_or_error(TTL >= Height, ttl_expired)
                end
                ],
            case aeu_validation:run(Checks) of
                ok ->
                    {ok, Trees};
                {error, _Reason} = Error ->
                    Error
            end
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), height(), non_neg_integer()) -> {ok, aec_trees:trees()}.
process(#channel_close_mutual_tx{channel_id       = ChannelId,
                                 from             = _From,
                                 initiator_amount = InitiatorAmount0,
                                 responder_amount = ResponderAmount0,
                                 ttl              = _TTL,
                                 fee              = Fee,
                                 nonce            = _Nonce}, _Context, Trees, Height,
                                                  _ConsensusVersion) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    Channel      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    InitiatorPubKey = aesc_channels:initiator(Channel),
    ResponderPubKey = aesc_channels:responder(Channel),

    CFee = ceil(Fee / 2),
    FFee = floor(Fee / 2),

    {InitiatorAmount, ResponderAmount} = subtract_fee(InitiatorAmount0,
                                                      ResponderAmount0,
                                                      CFee, FFee),

    InitiatorAccount0         = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    {ok, InitiatorAccount}    = aec_accounts:earn(InitiatorAccount0,
                                                  InitiatorAmount,
                                                  Height),
    ResponderAccount0       = aec_accounts_trees:get(ResponderPubKey, AccountsTree0),
    {ok, ResponderAccount}  = aec_accounts:earn(ResponderAccount0,
                                                ResponderAmount,
                                                Height),

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount, AccountsTree1),

    ChannelsTree = aesc_state_tree:delete(aesc_channels:id(Channel), ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree),
    {ok, Trees2}.

-spec accounts(tx()) -> list(pubkey()).
accounts(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, Channel} ->
            [aesc_channels:initiator(Channel), aesc_channels:responder(Channel)];
        {error, not_found} -> []
    end.

-spec signers(tx()) -> list(pubkey()).
signers(#channel_close_mutual_tx{} = CloseTx) ->
    accounts(CloseTx).

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_close_mutual_tx{channel_id       = ChannelId,
                                   from             = From,
                                   initiator_amount = InitiatorAmount,
                                   responder_amount = ResponderAmount,
                                   ttl              = TTL,
                                   fee              = Fee,
                                   nonce            = Nonce}) ->
    {version(),
     [ {channel_id        , ChannelId}
     , {from              , From}
     , {initiator_amount  , InitiatorAmount}
     , {responder_amount  , ResponderAmount}
     , {ttl               , TTL}
     , {fee               , Fee}
     , {nonce             , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CLOSE_MUTUAL_TX_VSN,
            [ {channel_id       , ChannelId}
            , {from             , From}
            , {initiator_amount , InitiatorAmount}
            , {responder_amount , ResponderAmount}
            , {ttl              , TTL}
            , {fee              , Fee}
            , {nonce            , Nonce}]) ->
    #channel_close_mutual_tx{channel_id       = ChannelId,
                             from             = From,
                             initiator_amount = InitiatorAmount,
                             responder_amount = ResponderAmount,
                             ttl              = TTL,
                             fee              = Fee,
                             nonce            = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_close_mutual_tx{channel_id  = ChannelId,
                                    from             = From,
                                    initiator_amount = InitiatorAmount,
                                    responder_amount = ResponderAmount,
                                    ttl              = TTL,
                                    fee         = Fee,
                                    nonce       = Nonce}) ->
    #{<<"data_schema">> => <<"ChannelCloseMutualTxJSON">>, % swagger schema name
      <<"vsn">>               => version(),
      <<"channel_id">>        => aec_base58c:encode(channel, ChannelId),
      <<"from">>              => aec_base58c:encode(account_pubkey, From),
      <<"initiator_amount">>  => InitiatorAmount,
      <<"responder_amount">>  => ResponderAmount,
      <<"ttl">>               => TTL,
      <<"fee">>               => Fee,
      <<"nonce">>             => Nonce}.

serialization_template(?CHANNEL_CLOSE_MUTUAL_TX_VSN) ->
    [ {channel_id       , binary}
    , {from             , binary}
    , {initiator_amount , int}
    , {responder_amount , int}
    , {ttl              , int}
    , {fee              , int}
    , {nonce            , int}
    ].

-spec is_verifiable(tx()) -> boolean().
is_verifiable(#channel_close_mutual_tx{channel_id = ChannelId}) ->
    case aec_chain:get_channel(ChannelId) of
        {ok, _Channel} -> true;
        {error, not_found} -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ok_or_error(false, ErrMsg) -> {error, ErrMsg};
ok_or_error(true, _) -> ok.

-spec subtract_fee(InitiatorAmount0  :: non_neg_integer(),
                   ResponderAmount0  :: non_neg_integer(),
                   CeilFee     :: non_neg_integer(),
                   FloorFee     :: non_neg_integer()) ->
    {InitiatorAmount :: non_neg_integer(),
     ResponderAmount :: non_neg_integer()}.
subtract_fee(IAmt, PAmt, CFee, FFee) when IAmt >= CFee andalso PAmt >= FFee ->
    {IAmt - CFee, PAmt - FFee};
subtract_fee(IAmt, PAmt, CFee, FFee) when PAmt >= CFee andalso IAmt >= FFee ->
    {IAmt - FFee, PAmt - CFee};
subtract_fee(IAmt, PAmt, CFee, FFee) when IAmt > PAmt ->
    {IAmt + PAmt - FFee - CFee, 0};
subtract_fee(IAmt, PAmt, CFee, FFee) ->
    {0, IAmt + PAmt - FFee - CFee}.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CLOSE_MUTUAL_TX_VSN.

