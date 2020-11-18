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
         entities/1,
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

-export([gas_price/1,
         contract_pubkey_and_caller/1]).

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         state_hash/1,
         updates/1,
         round/1]).

-export([from_db_format/1
        ]).

%% getters
-export([payload/1,
         update/1,
         offchain_trees/1
        ]).

-ifdef(TEST).
-export([ set_payload/2
        , set_nonce/2
        ]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").
%%%===================================================================
%%% Types
%%%===================================================================

-define(INITIAL_VSN, 1).
-define(CHANNEL_FORCE_PROGRESS_TX_TYPE, channel_force_progress_tx).

-type vsn() :: non_neg_integer().

-record(channel_force_progress_tx, {
          channel_id    :: aeser_id:id(),
          from_id       :: aeser_id:id(),
          payload       :: binary(),
          update        :: aesc_offchain_update:update(),
          state_hash    :: binary(),
          round         :: aesc_channels:round(),
          offchain_trees:: aec_trees:trees(),
          ttl           :: aetx:tx_ttl(),
          fee           :: non_neg_integer(),
          nonce         :: non_neg_integer()
         }).

-opaque tx() :: #channel_force_progress_tx{}.

-export_type([tx/0]).

%% Record introduced temporarily during Fortuna development, shipped in releases 2.4.0 and 2.5.0, and potentially stored in DB.
-record(v2_db_record, {	
          channel_id    :: aeser_id:id(),
          from_id       :: aeser_id:id(),
          payload       :: binary(),
          update        :: tuple(),
          state_hash    :: binary(),
          round         :: aesc_channels:seq_number(),
          offchain_trees:: aec_trees:trees(),
          ttl           :: aetx:tx_ttl(),
          fee           :: non_neg_integer(),
          nonce         :: non_neg_integer(),
          block_hash    :: binary()
         }).
%%%===================================================================
%%% Conversion of old db format

-spec from_db_format(tx()) -> tx().
from_db_format(#channel_force_progress_tx{update = Update} = Tx) ->
    case aesc_offchain_update:from_db_format(Update) of
        Update -> Tx; % update is already in new serialization
        NewSUpdate -> Tx#channel_force_progress_tx{update = NewSUpdate}
    end;
from_db_format(Tuple) ->
    case setelement(1, Tuple, v2_db_record) of
        #v2_db_record{
            channel_id    = ChannelId,
            from_id       = FromId,
            payload       = Payload,
            update        = Update,
            state_hash    = StateHash,
            round         = Round,
            offchain_trees= OffChainTrees,
            ttl           = TTL,
            fee           = Fee,
            nonce         = Nonce
          } ->
            channel = aeser_id:specialize_type(ChannelId),
            account = aeser_id:specialize_type(FromId),
            #channel_force_progress_tx{
                channel_id    = ChannelId,
                from_id       = FromId,
                payload       = Payload,
                update        = aesc_offchain_update:from_db_format(Update),
                state_hash    = StateHash,
                round         = Round,
                offchain_trees= OffChainTrees,
                ttl           = TTL,
                fee           = Fee,
                nonce         = Nonce
              };
        _ ->
            error(illegal_db_format)
    end.

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
            nonce         = Nonce},
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

-spec contract_pubkey_and_caller(tx()) ->
    {aect_contracts:pubkey(), aec_keys:pubkey()} | undefined.
contract_pubkey_and_caller(#channel_force_progress_tx{update = Update}) ->
    case aesc_offchain_update:extract_call(Update) of
        {ContractPubkey, Caller} ->
            {ContractPubkey, Caller};
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

-spec entities(tx()) -> [aeser_id:id()].
entities(#channel_force_progress_tx{ channel_id = ChId
                                   , from_id = FromId }) ->
    [FromId, ChId].

channel(#channel_force_progress_tx{channel_id = ChannelId}) ->
    ChannelId.

from_id(#channel_force_progress_tx{from_id = FromId}) ->
    FromId.

from_pubkey(#channel_force_progress_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_force_progress_tx{} = Tx, Trees, Env) ->
    case aesc_utils:check_force_progress(Tx, Trees, Env) of
        ok -> {ok, Trees};
        Err ->
            Err
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_force_progress_tx{ offchain_trees = OffChainTrees
                                  , payload        = Payload} = Tx, Trees, Env) ->
    Height = aetx_env:height(Env),
    {value, STx} = aetx_env:signed_tx(Env),

    TxHash = aetx_sign:hash(STx),
    aesc_utils:process_force_progress(Tx, OffChainTrees, Payload,
                                      TxHash, Height, Trees, Env).

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
                                     nonce          = Nonce} = Tx) ->
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
                               nonce          = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_force_progress_tx{payload       = Payload,
                                      round         = Round,
                                      state_hash    = StateHash,
                                      update        = Update,
                                      offchain_trees= OffChainTrees,
                                      ttl           = TTL,
                                      fee           = Fee,
                                      nonce         = Nonce} = Tx) ->
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

-spec round(tx()) -> aesc_channels:round().
round(#channel_force_progress_tx{round = Round}) ->
    Round.

-spec state_hash(tx()) -> binary().
state_hash(#channel_force_progress_tx{state_hash = StateHash}) ->
    StateHash.

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?INITIAL_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, #channel_force_progress_tx{payload = Payload} = Tx) ->
    CorrectTxVsn =
        case version(Tx) of
            ?INITIAL_VSN -> true
        end,
    CorrectPayloadVsn = aesc_utils:is_payload_valid_at_protocol(Protocol,
                                                                Payload),
    CorrectTxVsn andalso CorrectPayloadVsn.

-spec payload(tx()) -> binary().
payload(#channel_force_progress_tx{payload = Payload}) ->
    Payload.

-spec update(tx()) -> aesc_offchain_update:update().
update(ForceProgress) ->
    [Update] = updates(ForceProgress),
    Update.

-spec offchain_trees(tx()) -> aec_trees:trees().
offchain_trees(#channel_force_progress_tx{offchain_trees = Trees}) ->
    Trees.

-ifdef(TEST).
set_payload(#channel_force_progress_tx{} = FP, Payload) when is_binary(Payload) ->
    FP#channel_force_progress_tx{payload = Payload}.

set_nonce(#channel_force_progress_tx{} = FP, Nonce) when is_integer(Nonce) ->
    FP#channel_force_progress_tx{nonce = Nonce}.
-endif.
