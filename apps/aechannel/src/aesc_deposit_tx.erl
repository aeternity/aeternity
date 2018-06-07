%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel deposit transaction
%%% @end
%%%=============================================================================
-module(aesc_deposit_tx).

-include("channel_txs.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         amount/1,
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

-define(CHANNEL_DEPOSIT_TX_VSN, 1).
-define(CHANNEL_DEPOSIT_TX_TYPE, channel_deposit_tx).
-define(CHANNEL_DEPOSIT_TX_FEE, 4).

-opaque tx() :: #channel_deposit_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()} | {error, any()}.
new(#{channel_id  := ChannelId,
      from        := FromPubKey,
      amount      := Amount,
      fee         := Fee,
      nonce       := Nonce} = Args) ->
    try Tx = #channel_deposit_tx{
                channel_id  = ChannelId,
                from        = FromPubKey,
                amount      = Amount,
                ttl         = maps:get(ttl, Args, 0),
                fee         = Fee,
                nonce       = Nonce},
         {ok, aetx:new(?MODULE, Tx)}
    catch
        error:E ->
            {error, E}
    end.

type() ->
    ?CHANNEL_DEPOSIT_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_deposit_tx{fee = Fee}) ->
    Fee.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_deposit_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_deposit_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_deposit_tx{from = FromPubKey}) ->
    FromPubKey.

-spec amount(tx()) -> non_neg_integer().
amount(#channel_deposit_tx{amount = Amount}) ->
    Amount.

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_deposit_tx{channel_id = ChannelId,
                          from       = FromPubKey,
                          amount     = Amount,
                          fee        = Fee,
                          nonce      = Nonce}, _Context, Trees, _Height, _ConsensusVersion) ->
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Amount + Fee) end,
         fun() -> check_channel(ChannelId, FromPubKey, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#channel_deposit_tx{channel_id = ChannelId,
                            from       = FromPubKey,
                            amount     = Amount,
                            fee        = Fee,
                            nonce      = Nonce}, _Context, Trees, _Height, _ConsensusVersion) ->
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Amount + Fee, Nonce),
    AccountsTree1      = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:deposit(Channel0, Amount),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec accounts(tx()) -> list(aec_keys:pubkey()).
accounts(#channel_deposit_tx{from = FromPubKey}) ->
    [FromPubKey].

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_deposit_tx{channel_id = ChannelId}, Trees) ->
    case aec_chain:get_channel(ChannelId, Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator(Channel),
                  aesc_channels:responder(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_deposit_tx{channel_id  = ChannelId,
                              from        = FromPubKey,
                              amount      = Amount,
                              ttl         = TTL,
                              fee         = Fee,
                              nonce       = Nonce}) ->
    {version(),
     [ {channel_id  , ChannelId}
     , {from        , FromPubKey}
     , {amount      , Amount}
     , {ttl         , TTL}
     , {fee         , Fee}
     , {nonce       , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_DEPOSIT_TX_VSN,
            [ {channel_id  , ChannelId}
            , {from        , FromPubKey}
            , {amount      , Amount}
            , {ttl         , TTL}
            , {fee         , Fee}
            , {nonce       , Nonce}]) ->
    #channel_deposit_tx{channel_id  = ChannelId,
                        from        = FromPubKey,
                        amount      = Amount,
                        ttl         = TTL,
                        fee         = Fee,
                        nonce       = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_deposit_tx{channel_id   = ChannelId,
                               from        = FromPubKey,
                               amount       = Amount,
                               ttl          = TTL,
                               fee          = Fee,
                               nonce        = Nonce}) ->
    #{<<"data_schema">>  => <<"ChannelDepositTxJSON">>, % swagger schema name
      <<"vsn">>          => version(),
      <<"channel">>      => aec_base58c:encode(channel, ChannelId),
      <<"from">>         => aec_base58c:encode(account_pubkey, FromPubKey),
      <<"amount">>       => Amount,
      <<"ttl">>          => TTL,
      <<"fee">>          => Fee,
      <<"nonce">>        => Nonce}.

serialization_template(?CHANNEL_DEPOSIT_TX_VSN) ->
    [ {channel_id  , binary}
    , {from        , binary}
    , {amount      , int}
    , {ttl         , int}
    , {fee         , int}
    , {nonce       , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_channel(aesc_channels:id(), aec_keys:pubkey(), aec_trees:trees()) ->
                           ok | {error, atom()}.
check_channel(ChannelId, FromPubKey, Trees) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_is_active(Channel) end,
                 fun() -> aesc_utils:check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end],
            aeu_validation:run(Checks);
        none ->
            {error, channel_does_not_exist}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_DEPOSIT_TX_VSN.
