-module(aesc_offchain_tx).

-behaviour(aetx).
-behaviour(aesc_signable_transaction).

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
         for_client/1
        ]).

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         updates/1,
         round/1,
         state_hash/1]).

-export([set_value/3]).

-compile({no_auto_import, [round/1]}).

-ifdef(TEST).
-export([set_channel_id/2,
         set_round/2,
         set_state_hash/2]).
-endif.
%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_OFFCHAIN_TX_VSN, 1).
-define(CHANNEL_OFFCHAIN_TX_TYPE, channel_offchain_tx).
-define(CHANNEL_OFFCHAIN_TX_FEE, 0).   % off-chain

-type vsn() :: non_neg_integer().

-record(channel_offchain_tx, {
            
          channel_id         :: aec_id:id(),
          updates            :: [aesc_offchain_update:update()],
          state_hash         :: binary(),
          round              :: non_neg_integer()
         }).

-opaque tx() :: #channel_offchain_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id         := ChannelId,
      state_hash         := StateHash,
      updates            := Updates,
      round              := Round}) ->
    channel = aec_id:specialize_type(ChannelId),
    Tx = #channel_offchain_tx{
            channel_id         = ChannelId,
            updates            = Updates,
            state_hash         = StateHash,
            round              = Round},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_OFFCHAIN_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    ?CHANNEL_OFFCHAIN_TX_FEE.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_offchain_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_offchain_tx{}) ->
    %% This tx should never hit the mempool or any block
    0.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_offchain_tx{round = N}) ->
    N.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_offchain_tx{}) ->
    error(do_not_use).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_offchain_tx{
         channel_id         = _ChannelId,
         updates            = _Updates,
         state_hash         = _State,
         round              = _Round},
      Trees,_Env) ->
    %% TODO: implement checks relevant to off-chain
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()}.
process(#channel_offchain_tx{},_Trees, #{}) ->
    error(off_chain_tx).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_offchain_tx{} = Tx, Trees) ->
    case aec_chain:get_channel(channel_pubkey(Tx), Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_does_not_exist}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_offchain_tx{
             channel_id         = ChannelId,
             updates            = Updates0,
             state_hash         = StateHash,
             round              = Round}) ->
    Updates = [aesc_offchain_update:serialize(U) || U <- Updates0],
    {version(),
     [ {channel_id        , ChannelId}
     , {round             , Round}
     , {updates           , Updates}
     , {state_hash        , StateHash}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_OFFCHAIN_TX_VSN,
            [ {channel_id        , ChannelId}
            , {round             , Round}
            , {updates           , Updates0}
            , {state_hash        , StateHash}]) ->
    channel = aec_id:specialize_type(ChannelId),
    Updates = [aesc_offchain_update:deserialize(U) || U <- Updates0],
    #channel_offchain_tx{
       channel_id         = ChannelId,
       updates            = Updates,
       state_hash         = StateHash,
       round              = Round}.

-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              updates            = Updates,
              state_hash         = StateHash,
              channel_id         = ChannelId,
              round              = Round}) ->
    #{<<"channel_id">>         => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"round">>              => Round,
      <<"updates">>            => [aesc_offchain_update:for_client(D) || D <- Updates],
      <<"state_hash">>         => aehttp_api_encoder:encode(state, StateHash)}.

serialization_template(?CHANNEL_OFFCHAIN_TX_VSN) ->
    [ {channel_id        , id}
    , {round             , int}
    , {updates           , [binary]}
    , {state_hash        , binary}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================
-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_offchain_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_offchain_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec updates(tx()) -> [aesc_offchain_update:update()].
updates(#channel_offchain_tx{updates = Updates}) ->
    Updates.

-spec round(tx()) -> aesc_channels:seq_number().
round(#channel_offchain_tx{round = Round}) ->
    Round.

-spec state_hash(tx()) -> binary().
state_hash(#channel_offchain_tx{state_hash = StateHash}) ->
    StateHash.

-type settable_field() :: updates
                        | round.

-spec set_value(tx(), settable_field(), any()) -> tx().
set_value(#channel_offchain_tx{} = Tx, updates, Updates) when is_list(Updates) ->
    Tx#channel_offchain_tx{updates = Updates};
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

%%%===================================================================
%%% Test setters 
%%%===================================================================
-dialyzer({nowarn_function, set_channel_id/2}).
set_channel_id(Tx, ChannelId) ->
    channel = aec_id:specialize_type(ChannelId),
    Tx#channel_offchain_tx{channel_id = ChannelId}.

-dialyzer({nowarn_function, set_round/2}).
set_round(Tx, Round) when is_integer(Round) ->
    Tx#channel_offchain_tx{round = Round}.

-dialyzer({nowarn_function, set_state_hash/2}).
set_state_hash(Tx, Hash) ->
    Tx#channel_offchain_tx{state_hash = Hash}.
