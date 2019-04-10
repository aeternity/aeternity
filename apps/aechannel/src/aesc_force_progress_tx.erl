%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel force progress on-chain transaction
%%% @end
%%%=============================================================================
-module(aesc_force_progress_tx).

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

-export([gas_price/1]).

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         state_hash/1,
         updates/1,
         round/1]).

-include_lib("aecontract/include/hard_forks.hrl").
%%%===================================================================
%%% Types
%%%===================================================================

-define(INITIAL_VSN, 1).
-define(PINNED_BLOCK_VSN, 2).
-define(CHANNEL_FORCE_PROGRESS_TX_TYPE, channel_force_progress_tx).

-type vsn() :: non_neg_integer().

-record(channel_force_progress_tx, {
          channel_id    :: aeser_id:id(),
          from_id       :: aeser_id:id(),
          payload       :: binary(),
          update        :: aesc_offchain_update:update(),
          state_hash    :: binary(),
          round         :: aesc_channels:seq_number(),
          offchain_trees:: aec_trees:trees(),
          ttl           :: aetx:tx_ttl(),
          fee           :: non_neg_integer(),
          nonce         :: non_neg_integer(),
          block_hash    :: aesc_pinned_block:hash()
         }).

-opaque tx() :: #channel_force_progress_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id    := ChannelId,
      from_id       := FromId,
      payload       := Payload,
      update        := Update,
      state_hash    := StateHash,
      round         := Round,
      offchain_trees:= OffChainTrees,
      fee           := Fee,
      nonce         := Nonce} = Args) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    BlockHash = maps:get(block_hash, Args, aesc_pinned_block:no_hash()),
    Tx = #channel_force_progress_tx{
            channel_id    = ChannelId,
            from_id       = FromId,
            payload       = Payload,
            update        = Update,
            state_hash    = StateHash,
            round         = Round,
            offchain_trees= OffChainTrees,
            ttl           = maps:get(ttl, Args, 0),
            fee           = Fee,
            nonce         = Nonce,
            block_hash    = BlockHash},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_FORCE_PROGRESS_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_force_progress_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_force_progress_tx{update = Update}) ->
    case aesc_offchain_update:extract_amounts(Update) of
        {_Amount, _GasPrice, Gas} ->
            Gas;
        not_call ->
            0
    end.

-spec gas_price(tx()) -> aect_contracts:amount() | undefined.
gas_price(#channel_force_progress_tx{update = Update}) ->
    case aesc_offchain_update:extract_amounts(Update) of
        {_Amount, GasPrice, _Gas} ->
            GasPrice;
        not_call ->
            undefined
    end.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_force_progress_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_force_progress_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_force_progress_tx{} = Tx) ->
    from_pubkey(Tx).

channel(#channel_force_progress_tx{channel_id = ChannelId}) ->
    ChannelId.

from_id(#channel_force_progress_tx{from_id = FromId}) ->
    FromId.

from_pubkey(#channel_force_progress_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_force_progress_tx{payload       = Payload,
                                 offchain_trees= OffChainTrees} = Tx,
      Trees, Env) ->
    Height = aetx_env:height(Env),
    case aesc_utils:check_force_progress(Tx, Payload,
                                   OffChainTrees, Height, Trees) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_force_progress_tx{offchain_trees = OffChainTrees} = Tx, Trees, Env) ->
    Height = aetx_env:height(Env),
    {value, STx} = aetx_env:signed_tx(Env),

    TxHash = aetx_sign:hash(STx),
    aesc_utils:process_force_progress(Tx, OffChainTrees, TxHash, Height, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_force_progress_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_force_progress_tx{channel_id     = ChannelId,
                                     from_id        = FromId,
                                     payload        = Payload,
                                     update         = Update,
                                     state_hash     = StateHash,
                                     round          = Round,
                                     offchain_trees = OffChainTrees,
                                     ttl            = TTL,
                                     fee            = Fee,
                                     nonce          = Nonce,
                                     block_hash     = BlockHash} = Tx) ->
    SerializedUpdate = aesc_offchain_update:serialize(Update),
    SerializedTrees = aec_trees:serialize_to_binary(OffChainTrees),
    case version(Tx) of
        ?INITIAL_VSN ->
            {?INITIAL_VSN,
            [ {channel_id    , ChannelId}
            , {from_id       , FromId}
            , {payload       , Payload}
            , {round         , Round}
            , {update        , SerializedUpdate}
            , {state_hash    , StateHash}
            , {offchain_trees, SerializedTrees}
            , {ttl           , TTL}
            , {fee           , Fee}
            , {nonce         , Nonce}
            ]};
        ?PINNED_BLOCK_VSN ->
            {?PINNED_BLOCK_VSN,
            [ {channel_id    , ChannelId}
            , {from_id       , FromId}
            , {payload       , Payload}
            , {round         , Round}
            , {update        , SerializedUpdate}
            , {state_hash    , StateHash}
            , {offchain_trees, SerializedTrees}
            , {ttl           , TTL}
            , {fee           , Fee}
            , {nonce         , Nonce}
            , {block_hash    , aesc_pinned_block:serialize(BlockHash)}
            ]}
    end.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?INITIAL_VSN,
            [ {channel_id     , ChannelId}
            , {from_id        , FromId}
            , {payload        , Payload}
            , {round          , Round}
            , {update         , UpdateBin}
            , {state_hash     , StateHash}
            , {offchain_trees , OffChainTrees}
            , {ttl            , TTL}
            , {fee            , Fee}
            , {nonce          , Nonce}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    Update = aesc_offchain_update:deserialize(UpdateBin),
    #channel_force_progress_tx{channel_id     = ChannelId,
                               from_id        = FromId,
                               payload        = Payload,
                               round          = Round,
                               state_hash     = StateHash,
                               update         = Update,
                               offchain_trees =
                                  aec_trees:deserialize_from_binary_without_backend(OffChainTrees),
                               ttl            = TTL,
                               fee            = Fee,
                               nonce          = Nonce,
                               block_hash     = aesc_pinned_block:no_hash()};
deserialize(?PINNED_BLOCK_VSN,
            [ {channel_id     , ChannelId}
            , {from_id        , FromId}
            , {payload        , Payload}
            , {round          , Round}
            , {update         , UpdateBin}
            , {state_hash     , StateHash}
            , {offchain_trees , OffChainTrees}
            , {ttl            , TTL}
            , {fee            , Fee}
            , {nonce          , Nonce}
            , {block_hash     , BlockHash}
            ]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    Update = aesc_offchain_update:deserialize(UpdateBin),
    #channel_force_progress_tx{channel_id     = ChannelId,
                               from_id        = FromId,
                               payload        = Payload,
                               round          = Round,
                               state_hash     = StateHash,
                               update         = Update,
                               offchain_trees =
                                  aec_trees:deserialize_from_binary_without_backend(OffChainTrees),
                               ttl            = TTL,
                               fee            = Fee,
                               nonce          = Nonce,
                               block_hash     = aesc_pinned_block:deserialize(BlockHash)}.

-spec for_client(tx()) -> map().
for_client(#channel_force_progress_tx{payload       = Payload,
                                      round         = Round,
                                      state_hash    = StateHash,
                                      update        = Update,
                                      offchain_trees= OffChainTrees,
                                      ttl           = TTL,
                                      fee           = Fee,
                                      nonce         = Nonce,
                                      block_hash    = BlockHash} = Tx) ->
    #{<<"channel_id">>    => aeser_api_encoder:encode(id_hash, channel(Tx)),
      <<"from_id">>       => aeser_api_encoder:encode(id_hash, from_id(Tx)),
      <<"payload">>       => aeser_api_encoder:encode(transaction, Payload),
      <<"round">>         => Round,
      <<"update">>        => aesc_offchain_update:for_client(Update),
      <<"state_hash">>    => aeser_api_encoder:encode(state, StateHash),
      <<"offchain_trees">>=> aeser_api_encoder:encode(state_trees,
                                                aec_trees:serialize_to_binary(OffChainTrees)),
      <<"ttl">>           => TTL,
      <<"fee">>           => Fee,
      <<"block_hash">>    => aesc_pinned_block:serialize_for_client(BlockHash),
      <<"nonce">>         => Nonce}.

serialization_template(?INITIAL_VSN) ->
    [ {channel_id     , id}
    , {from_id        , id}
    , {payload        , binary}
    , {round          , int}
    , {update         , binary}
    , {state_hash     , binary}
    , {offchain_trees , binary}
    , {ttl            , int}
    , {fee            , int}
    , {nonce          , int}
    ];
serialization_template(?PINNED_BLOCK_VSN) ->
    [ {channel_id     , id}
    , {from_id        , id}
    , {payload        , binary}
    , {round          , int}
    , {update         , binary}
    , {state_hash     , binary}
    , {offchain_trees , binary}
    , {ttl            , int}
    , {fee            , int}
    , {nonce          , int}
    , {block_hash     , binary}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================
-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_force_progress_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_force_progress_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec updates(tx()) -> [aesc_offchain_update:update()].
updates(#channel_force_progress_tx{update = Update}) ->
    [Update].

-spec round(tx()) -> aesc_channels:seq_number().
round(#channel_force_progress_tx{round = Round}) ->
    Round.

-spec state_hash(tx()) -> binary().
state_hash(#channel_force_progress_tx{state_hash = StateHash}) ->
    StateHash.

-spec version(tx()) -> non_neg_integer().
version(#channel_force_progress_tx{block_hash = BlockHash}) ->
    case BlockHash =:= aesc_pinned_block:no_hash() of
        true ->
            ?INITIAL_VSN;
        false ->
            ?PINNED_BLOCK_VSN
    end.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, #channel_force_progress_tx{payload = Payload} = Tx) ->
    CorrectTxVsn =
        case version(Tx) of
            ?INITIAL_VSN -> true;
            ?PINNED_BLOCK_VSN when Protocol >= ?FORTUNA_PROTOCOL_VSN -> true;
            _ -> false
        end,
    CorrectPayloadVsn =
        case aesc_utils:deserialize_payload(Payload) of
            {error, _} -> false;
            {ok, last_onchain} -> true; %% already on-chain
            {ok, _SignedTx, OffChainTx} ->
                aesc_offchain_tx:valid_at_protocol(Protocol, OffChainTx)
        end,
    CorrectTxVsn andalso CorrectPayloadVsn.

