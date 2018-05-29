-module(aesc_offchain_tx).

-behaviour(aetx).
-behaviour(aesc_payload).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/5,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%% Getters
-export([channel_id/1,
         initiator/1,
         responder/1,
         updates/1,
         previous_round/1,
         round/1,
         state_hash/1]).

-export([set_value/3]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_OFFCHAIN_TX_VSN, 1).
-define(CHANNEL_OFFCHAIN_TX_TYPE, channel_offchain_tx).
-define(CHANNEL_OFFCHAIN_TX_FEE, 0).   % off-chain

-type vsn() :: non_neg_integer().

-record(channel_offchain_tx, {
          channel_id         :: aec_id:id(),
          initiator          :: aec_id:id(),
          responder          :: aec_id:id(),
          updates            :: [aesc_offchain_state:update()],
          state_hash         :: binary(),
          previous_round     :: non_neg_integer(),
          round              :: non_neg_integer()
         }).

-opaque tx() :: #channel_offchain_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id         := ChannelIdBin,
      initiator          := InitiatorPubKey,
      responder          := ResponderPubKey,
      state_hash         := StateHash,
      updates            := Updates,
      previous_round     := Prev,
      round              := Round}) ->
    Tx = #channel_offchain_tx{
            channel_id         = aec_id:create(channel, ChannelIdBin),
            initiator          = aec_id:create(account, InitiatorPubKey),
            responder          = aec_id:create(account, ResponderPubKey),
            updates            = Updates,
            state_hash         = StateHash,
            previous_round     = Prev,
            round              = Round},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_OFFCHAIN_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    ?CHANNEL_OFFCHAIN_TX_FEE.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    0.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_offchain_tx{round = N}) ->
    N.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_offchain_tx{} = Tx) ->
    initiator(Tx).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_offchain_tx{
         channel_id         = _ChannelId,
         initiator          = _InitiatorPubKey,
         responder          = _ResponderPubKey,
         updates            = _Updates,
         state_hash         = _State,
         previous_round     = _Prev,
         round              = _Round}, _Context, Trees, _Height,
                                                _ConsensusVersion) ->
    %% TODO: implement checks relevant to off-chain
    {ok, Trees}.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()}.
process(#channel_offchain_tx{}, _Context, _Trees, _Height, _ConsensusVersion) ->
    error(off_chain_tx).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_offchain_tx{} = Tx,_Trees) ->
    {ok, [initiator(Tx), responder(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_offchain_tx{
             channel_id         = ChannelId,
             initiator          = InitiatorId,
             responder          = ResponderId,
             updates            = Updates,
             state_hash         = StateHash,
             previous_round     = Prev,
             round              = Round}) ->
    {version(),
     [ {channel_id        , ChannelId}
     , {previous_round    , Prev}
     , {round             , Round}
     , {initiator         , InitiatorId}
     , {responder         , ResponderId}
     , {updates           , Updates}
     , {state_hash        , StateHash}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_OFFCHAIN_TX_VSN,
            [ {channel_id        , ChannelId}
            , {previous_round    , Prev}
            , {round             , Round}
            , {initiator         , InitiatorId}
            , {responder         , ResponderId}
            , {updates           , Updates}
            , {state_hash        , StateHash}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(InitiatorId),
    account = aec_id:specialize_type(ResponderId),
    #channel_offchain_tx{
       channel_id         = ChannelId,
       initiator          = InitiatorId,
       responder          = ResponderId,
       updates            = Updates,
       state_hash         = StateHash,
       previous_round     = Prev,
       round              = Round}.

-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              updates            = Updates,
              state_hash         = StateHash,
              previous_round     = Prev,
              round              = Round} = Tx) ->
    #{<<"vsn">>                => ?CHANNEL_OFFCHAIN_TX_VSN,
      <<"channel_id">>         => aec_base58c:encode(channel, channel_id(Tx)),
      <<"previous_round">>     => Prev,
      <<"round">>              => Round,
      <<"initiator">>          => aec_base58c:encode(
                                    account_pubkey, initiator(Tx)),
      <<"responder">>          => aec_base58c:encode(
                                    account_pubkey, responder(Tx)),
      <<"updates">>            => [aesc_offchain_state:update_for_client(D) || D <- Updates],
      <<"state_hash">>         => aec_base58c:encode(state, StateHash)}.

serialization_template(?CHANNEL_OFFCHAIN_TX_VSN) ->
    [ {channel_id        , id}
    , {previous_round    , int}
    , {round             , int}
    , {initiator         , id}
    , {responder         , id}
    , {updates           , [{int,binary,binary,int}]}
    , {state_hash        , binary}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_offchain_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec initiator(tx()) -> aec_keys:pubkey().
initiator(#channel_offchain_tx{initiator = InitiatorId}) ->
    aec_id:specialize(InitiatorId, account).

-spec responder(tx()) -> aec_keys:pubkey().
responder(#channel_offchain_tx{responder = ResponderId}) ->
    aec_id:specialize(ResponderId, account).

-spec updates(tx()) -> [aesc_offchain_state:update()].
updates(#channel_offchain_tx{updates = Updates}) ->
    Updates.

-spec previous_round(tx()) -> non_neg_integer().
previous_round(#channel_offchain_tx{previous_round = PreviousRound}) ->
    PreviousRound.

-spec round(tx()) -> aesc_channels:seq_number().
round(#channel_offchain_tx{round = Round}) ->
    Round.

-spec state_hash(tx()) -> binary().
state_hash(#channel_offchain_tx{state_hash = StateHash}) ->
    StateHash.

-type settable_field() :: updates
                        | previous_round
                        | round.

-spec set_value(tx(), settable_field(), any()) -> tx().
set_value(#channel_offchain_tx{} = Tx, updates, Updates) when is_list(Updates) ->
    Tx#channel_offchain_tx{updates = Updates};
set_value(#channel_offchain_tx{round = Seq} = Tx, previous_round, N)
  when is_integer(N), N >= 0, N < Seq ->
    Tx#channel_offchain_tx{previous_round = N};
set_value(#channel_offchain_tx{round = Seq} = Tx, round, N)
  when is_integer(N), N >= 0, N > Seq ->
    Tx#channel_offchain_tx{round = N};
set_value(#channel_offchain_tx{} = Tx, state_hash, Hash) when
  is_binary(Hash) ->
    Tx#channel_offchain_tx{state_hash=Hash}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

version() ->
    ?CHANNEL_OFFCHAIN_TX_VSN.
