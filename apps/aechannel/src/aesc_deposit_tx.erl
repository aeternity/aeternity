%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel deposit transaction
%%% @end
%%%=============================================================================
-module(aesc_deposit_tx).

-behavior(aetx).
-behaviour(aesc_signable_transaction).

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
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

% snapshot callbacks
-export([channel_id/1,
         state_hash/1,
         updates/1,
         round/1]).
%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_DEPOSIT_TX_VSN, 1).
-define(CHANNEL_DEPOSIT_TX_TYPE, channel_deposit_tx).
-define(CHANNEL_DEPOSIT_TX_FEE, 4).

-type vsn() :: non_neg_integer().

-record(channel_deposit_tx, {
          channel_id  :: aec_id:id(),
          from        :: aec_id:id(),
          amount      :: non_neg_integer(),
          ttl         :: aetx:tx_ttl(),
          fee         :: non_neg_integer(),
          state_hash  :: binary(),
          round       :: non_neg_integer(),
          nonce       :: non_neg_integer()
         }).

-opaque tx() :: #channel_deposit_tx{}.

-export_type([tx/0]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()} | {error, any()}.
new(#{channel_id  := ChannelId,
      from        := From,
      amount      := Amount,
      fee         := Fee,
      state_hash  := StateHash,
      round       := Round,
      nonce       := Nonce} = Args) ->
    true = aesc_utils:check_state_hash_size(StateHash),
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(From),
    try Tx = #channel_deposit_tx{
                channel_id  = ChannelId,
                from        = From,
                amount      = Amount,
                ttl         = maps:get(ttl, Args, 0),
                fee         = Fee,
                state_hash  = StateHash,
                round       = Round,
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
origin(#channel_deposit_tx{} = Tx) ->
    from_pubkey(Tx).

from(#channel_deposit_tx{from = From}) ->
    From.

from_pubkey(#channel_deposit_tx{from = From}) ->
    aec_id:specialize(From, account).

-spec amount(tx()) -> non_neg_integer().
amount(#channel_deposit_tx{amount = Amount}) ->
    Amount.

channel(#channel_deposit_tx{channel_id = ChannelId}) ->
    ChannelId.

channel_hash(#channel_deposit_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_deposit_tx{amount     = Amount,
                          fee        = Fee,
                          state_hash  = _StateHash,
                          round       = Round,
                          nonce      = Nonce} = Tx, _Context, Trees, _Height, _ConsensusVersion) ->
    ChannelId = channel_hash(Tx),
    FromPubKey = from_pubkey(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(FromPubKey, Trees, Nonce, Amount + Fee) end,
         fun() -> check_channel(ChannelId, FromPubKey, Round, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#channel_deposit_tx{amount     = Amount,
                            fee        = Fee,
                            state_hash = StateHash,
                            round      = Round,
                            nonce      = Nonce} = Tx, _Context, Trees, _Height, _ConsensusVersion) ->
    ChannelId = channel_hash(Tx),
    FromPubKey = from_pubkey(Tx),
    AccountsTree0 = aec_trees:accounts(Trees),
    ChannelsTree0 = aec_trees:channels(Trees),

    FromAccount0       = aec_accounts_trees:get(FromPubKey, AccountsTree0),
    {ok, FromAccount1} = aec_accounts:spend(FromAccount0, Amount + Fee, Nonce),
    AccountsTree1      = aec_accounts_trees:enter(FromAccount1, AccountsTree0),

    Channel0      = aesc_state_tree:get(ChannelId, ChannelsTree0),
    Channel1      = aesc_channels:deposit(Channel0, Amount, Round, StateHash),
    ChannelsTree1 = aesc_state_tree:enter(Channel1, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees, AccountsTree1),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_deposit_tx{} = Tx, Trees) ->
    ChannelId = channel_hash(Tx),
    case aec_chain:get_channel(ChannelId, Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator(Channel),
                  aesc_channels:responder(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_deposit_tx{channel_id  = ChannelId,
                              from        = FromId,
                              amount      = Amount,
                              ttl         = TTL,
                              fee         = Fee,
                              state_hash  = StateHash,
                              round       = Round,
                              nonce       = Nonce}) ->
    {version(),
     [ {channel_id  , ChannelId}
     , {from        , FromId}
     , {amount      , Amount}
     , {ttl         , TTL}
     , {fee         , Fee}
     , {state_hash  , StateHash}
     , {round       , Round}
     , {nonce       , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_DEPOSIT_TX_VSN,
            [ {channel_id  , ChannelId}
            , {from        , FromId}
            , {amount      , Amount}
            , {ttl         , TTL}
            , {fee         , Fee}
            , {state_hash  , StateHash}
            , {round       , Round}
            , {nonce       , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    true = aesc_utils:check_state_hash_size(StateHash),
    #channel_deposit_tx{channel_id  = ChannelId,
                        from        = FromId,
                        amount      = Amount,
                        ttl         = TTL,
                        fee         = Fee,
                        state_hash  = StateHash,
                        round       = Round,
                        nonce       = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_deposit_tx{amount       = Amount,
                               ttl          = TTL,
                               fee          = Fee,
                               state_hash   = StateHash,
                               round        = Round,
                               nonce        = Nonce} = Tx) ->
    #{<<"data_schema">>  => <<"ChannelDepositTxJSON">>, % swagger schema name
      <<"vsn">>          => version(),
      <<"channel">>      => aec_base58c:encode(id_hash, channel(Tx)),
      <<"from">>         => aec_base58c:encode(id_hash, from(Tx)),
      <<"amount">>       => Amount,
      <<"ttl">>          => TTL,
      <<"fee">>          => Fee,
      <<"state_hash">>   => aec_base58c:encode(state, StateHash),
      <<"round">>        => Round,
      <<"nonce">>        => Nonce}.

serialization_template(?CHANNEL_DEPOSIT_TX_VSN) ->
    [ {channel_id  , id}
    , {from        , id}
    , {amount      , int}
    , {ttl         , int}
    , {fee         , int}
    , {state_hash  , binary}
    , {round       , int}
    , {nonce       , int}
    ].

channel_id(#channel_deposit_tx{} = Tx) -> channel_hash(Tx).

state_hash(#channel_deposit_tx{state_hash = StateHash}) -> StateHash.

updates(#channel_deposit_tx{amount = Amount} = Tx) ->
    [aesc_offchain_update:op_deposit(from_pubkey(Tx), Amount)].

round(#channel_deposit_tx{round = Round}) ->
    Round.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_channel(aesc_channels:id(), aec_keys:pubkey(), non_neg_integer(),aec_trees:trees()) ->
                           ok | {error, atom()}.
check_channel(ChannelId, FromPubKey, Round, Trees) ->
    case aesc_state_tree:lookup(ChannelId, aec_trees:channels(Trees)) of
        {value, Channel} ->
            Checks =
                [fun() -> aesc_utils:check_is_active(Channel) end,
                 fun() -> aesc_utils:check_is_peer(FromPubKey, aesc_channels:peers(Channel)) end,
                 fun() -> aesc_utils:check_round_greater_than_last(Channel, Round) end
                ],
            aeu_validation:run(Checks);
        none ->
            {error, channel_does_not_exist}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_DEPOSIT_TX_VSN.
