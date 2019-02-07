%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel withdraw transaction
%%% @end
%%%=============================================================================
-module(aesc_withdraw_tx).

-behavior(aetx).
-behaviour(aesc_signable_transaction).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         amount/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         state_hash/1,
         updates/1,
         round/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_WITHDRAW_TX_VSN, 1).
-define(CHANNEL_WITHDRAW_TX_TYPE, channel_withdraw_tx).

-type vsn() :: non_neg_integer().

%% HERE
-record(channel_withdraw_tx, {
          channel_id  :: aec_id:id(),
          to_id       :: aec_id:id(),
          amount      :: non_neg_integer(),
          ttl         :: aetx:tx_ttl(),
          fee         :: non_neg_integer(),
          state_hash  :: binary(),
          round       :: non_neg_integer(),
          nonce       :: non_neg_integer()
         }).

-opaque tx() :: #channel_withdraw_tx{}.

-export_type([tx/0]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      to_id      := ToId,
      amount     := Amount,
      fee        := Fee,
      state_hash := StateHash,
      round      := Round,
      nonce      := Nonce} = Args) ->
    true = aesc_utils:check_state_hash_size(StateHash),
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(ToId),
    Tx = #channel_withdraw_tx{
            channel_id = ChannelId,
            to_id      = ToId,
            amount     = Amount,
            ttl        = maps:get(ttl, Args, 0),
            fee        = Fee,
            state_hash = StateHash,
            round      = Round,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_WITHDRAW_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_withdraw_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_withdraw_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_withdraw_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_withdraw_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_withdraw_tx{} = Tx) ->
    to_pubkey(Tx).

to_pubkey(#channel_withdraw_tx{to_id = ToId}) ->
    aec_id:specialize(ToId, account).

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_withdraw_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_withdraw_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec amount(tx()) -> non_neg_integer().
amount(#channel_withdraw_tx{amount = Amt}) ->
    Amt.

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_withdraw_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_withdraw_tx{round = Round} = Tx, Trees, Env) ->
    Instructions =
        aec_tx_processor:channel_withdraw_tx_instructions(
          to_pubkey(Tx),
          channel_pubkey(Tx),
          amount(Tx),
          state_hash(Tx),
          Round,
          fee(Tx),
          nonce(Tx)),
    aec_tx_processor:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_withdraw_tx{} = Tx, Trees) ->
    ChannelPubKey = channel_pubkey(Tx),
    case aec_chain:get_channel(ChannelPubKey, Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_withdraw_tx{channel_id = ChannelId,
                               to_id      = ToId,
                               amount     = Amount,
                               ttl        = TTL,
                               fee        = Fee,
                               state_hash = StateHash,
                               round      = Round,
                               nonce      = Nonce}) ->
    {version(),
     [ {channel_id , ChannelId}
     , {to_id      , ToId}
     , {amount     , Amount}
     , {ttl        , TTL}
     , {fee        , Fee}
     , {state_hash , StateHash}
     , {round      , Round}
     , {nonce      , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_WITHDRAW_TX_VSN,
            [ {channel_id , ChannelId}
            , {to_id      , ToId}
            , {amount     , Amount}
            , {ttl        , TTL}
            , {fee        , Fee}
            , {state_hash , StateHash}
            , {round      , Round}
            , {nonce      , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(ToId),
    true = aesc_utils:check_state_hash_size(StateHash),
    #channel_withdraw_tx{channel_id = ChannelId,
                         to_id      = ToId,
                         amount     = Amount,
                         ttl        = TTL,
                         fee        = Fee,
                         state_hash = StateHash,
                         round      = Round,
                         nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_withdraw_tx{channel_id   = ChannelId,
                                to_id        = ToId,
                                amount       = Amount,
                                ttl          = TTL,
                                fee          = Fee,
                                state_hash   = StateHash,
                                round        = Round,
                                nonce        = Nonce}) ->
    #{<<"channel_id">>  => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"to_id">>       => aehttp_api_encoder:encode(id_hash, ToId),
      <<"amount">>      => Amount,
      <<"ttl">>         => TTL,
      <<"fee">>         => Fee,
      <<"state_hash">>  => aehttp_api_encoder:encode(state, StateHash),
      <<"round">>       => Round,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_WITHDRAW_TX_VSN) ->
    [ {channel_id , id}
    , {to_id      , id}
    , {amount     , int}
    , {ttl        , int}
    , {fee        , int}
    , {state_hash , binary}
    , {round      , int}
    , {nonce      , int}
    ].

state_hash(#channel_withdraw_tx{state_hash = StateHash}) -> StateHash.

updates(#channel_withdraw_tx{to_id = ToId, amount = Amount}) ->
    [aesc_offchain_update:op_withdraw(ToId, Amount)].

round(#channel_withdraw_tx{round = Round}) ->
    Round.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_WITHDRAW_TX_VSN.
