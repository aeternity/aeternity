-module(aesc_offchain_tx).

-behavior(aetx).
-behavior(aesc_signable_transaction).

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
         version/1,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1,
         valid_at_protocol/2
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

-include_lib("aecontract/include/hard_forks.hrl").
%%%===================================================================
%%% Types
%%%===================================================================

-define(INITIAL_VSN, 1).
-define(NO_UPDATES_VSN, 2).

-define(CHANNEL_OFFCHAIN_TX_TYPE, channel_offchain_tx).
-define(CHANNEL_OFFCHAIN_TX_FEE, 0).   % off-chain

-type vsn() :: non_neg_integer().

-record(channel_offchain_tx, {
          channel_id         :: aeser_id:id(),
          updates = none     :: [aesc_offchain_update:update()] | none, 
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
      round              := Round} = Opts) ->
    channel = aeser_id:specialize_type(ChannelId),
    Updates = maps:get(updates, Opts, none),
    Tx = #channel_offchain_tx{
            channel_id         = ChannelId,
            state_hash         = StateHash,
            updates            = Updates,
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
             updates            = UpdatesBins,
             state_hash         = StateHash,
             round              = Round} = Tx) ->
    case version(Tx) of
        ?INITIAL_VSN ->
            {?INITIAL_VSN,
            [ {channel_id        , ChannelId}
            , {round             , Round}
            , {updates           , [aesc_offchain_update:serialize(U) || U <- UpdatesBins]}
            , {state_hash        , StateHash}
            ]};
        ?NO_UPDATES_VSN ->
            {?INITIAL_VSN,
            [ {channel_id        , ChannelId}
            , {round             , Round}
            , {state_hash        , StateHash}
            ]}
    end.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?INITIAL_VSN,
            [ {channel_id        , ChannelId}
            , {round             , Round}
            , {updates           , UpdateBins} 
            , {state_hash        , StateHash}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    #channel_offchain_tx{
       channel_id         = ChannelId,
       updates            = [aesc_offchain_update:deserialize(U) || U <- UpdateBins],
       state_hash         = StateHash,
       round              = Round};
deserialize(?NO_UPDATES_VSN,
            [ {channel_id        , ChannelId}
            , {round             , Round}
            , {state_hash        , StateHash}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    #channel_offchain_tx{
       channel_id         = ChannelId,
       state_hash         = StateHash,
       round              = Round}.


%% off-chain transactions are included on-chain as a serialized payload
%% in a force progress transactions thus the callback
%% aesc_offchain_tx:for_client/1 is never executed for an off-chain tx
%% via HTTP API, but it might be used by State Channel's WebSocket API
%% Note: not documented in swagger.yaml
-spec for_client(tx()) -> map().
for_client(#channel_offchain_tx{
              state_hash         = StateHash,
              channel_id         = ChannelId,
              round              = Round}) ->
    #{<<"channel_id">>         => aeser_api_encoder:encode(id_hash, ChannelId),
      <<"round">>              => Round,
      <<"state_hash">>         => aeser_api_encoder:encode(state, StateHash)}.

serialization_template(?INITIAL_VSN) ->
    [ {channel_id        , id}
    , {round             , int}
    , {updates           , [binary]}
    , {state_hash        , binary}
    ];
serialization_template(?NO_UPDATES_VSN) ->
    [ {channel_id        , id}
    , {round             , int}
    , {state_hash        , binary}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================
-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_offchain_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_offchain_tx{channel_id = ChannelId}) ->
    ChannelId.

% keep it for backwards compatibility of currently running channels
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

version(#channel_offchain_tx{updates = Updates}) ->
    case Updates of
        none -> ?NO_UPDATES_VSN;
        _ when is_list(Updates) -> ?INITIAL_VSN
    end.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, Tx) ->
    case version(Tx) of
        ?INITIAL_VSN -> true;
        ?NO_UPDATES_VSN when Protocol =:= ?FORTUNA_PROTOCOL_VSN -> true;
        _ -> false
    end.

%%%===================================================================
%%% Test setters 
%%%===================================================================

-ifdef(TEST).
set_channel_id(Tx, ChannelId) ->
    channel = aeser_id:specialize_type(ChannelId),
    Tx#channel_offchain_tx{channel_id = ChannelId}.

set_round(Tx, Round) when is_integer(Round) ->
    Tx#channel_offchain_tx{round = Round}.

set_state_hash(Tx, Hash) ->
    Tx#channel_offchain_tx{state_hash = Hash}.
-endif.
