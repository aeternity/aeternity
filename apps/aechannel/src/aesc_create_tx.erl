%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel create transaction
%%% @end
%%%=============================================================================
-module(aesc_create_tx).

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

%% Getters
-export([initiator_id/1,
         initiator_pubkey/1,
         initiator_amount/1,
         channel_reserve/1,
         lock_period/1,
         responder_id/1,
         responder_amount/1,
         responder_pubkey/1,
         delegate_ids/1,
         delegate_pubkeys/1
        ]).

% aesc_signable_transaction callbacks
-export([channel_id/1,
         channel_pubkey/1,
         state_hash/1,
         round/1]).

-ifdef(TEST).
-export([set_state_hash/2]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").
%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CREATE_TX_VSN, 1).
-define(CHANNEL_CREATE_TX_VSN_DELEGATES, 2).
-define(CHANNEL_CREATE_TX_TYPE, channel_create_tx).

-type vsn() :: non_neg_integer().

-record(channel_create_tx, {
          initiator_id       :: aeser_id:id(),
          initiator_amount   :: non_neg_integer(),
          responder_id       :: aeser_id:id(),
          responder_amount   :: non_neg_integer(),
          channel_reserve    :: non_neg_integer(),
          lock_period        :: non_neg_integer(),
          ttl                :: aetx:tx_ttl(),
          fee                :: non_neg_integer(),
          delegate_ids       :: [aeser_id:id()] | {[aeser_id:id()], [aeser_id:id()]},
          state_hash         :: binary(),
          nonce              :: non_neg_integer()
         }).

-opaque tx() :: #channel_create_tx{}.

-export_type([tx/0]).

-compile({no_auto_import, [round/1]}).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{initiator_id       := InitiatorId,
      initiator_amount   := InitiatorAmount,
      responder_id       := ResponderId,
      responder_amount   := ResponderAmount,
      channel_reserve    := ChannelReserve,
      lock_period        := LockPeriod,
      fee                := Fee,
      state_hash         := StateHash,
      nonce              := Nonce} = Args) ->
    true = aesc_utils:check_state_hash_size(StateHash),

    ValidateAccounts =
        fun(Ids) ->
            lists:foreach(fun(D) -> account = aeser_id:specialize_type(D) end, Ids) end,
    %% Note that delegate_ids are required from Iris hardfork on
    DelegateIds =
        case maps:get(delegate_ids, Args, []) of
            L when is_list(L) ->
                ValidateAccounts(L),
                L;
            {IIds, RIds} = DIds when is_list(IIds), is_list(RIds) ->
                ValidateAccounts(IIds),
                ValidateAccounts(RIds),
                DIds
        end,
    account = aeser_id:specialize_type(InitiatorId),
    account = aeser_id:specialize_type(ResponderId),
    Tx = #channel_create_tx{initiator_id       = InitiatorId,
                            responder_id       = ResponderId,
                            initiator_amount   = InitiatorAmount,
                            responder_amount   = ResponderAmount,
                            channel_reserve    = ChannelReserve,
                            lock_period        = LockPeriod,
                            ttl                = maps:get(ttl, Args, 0),
                            fee                = Fee,
                            delegate_ids       = DelegateIds,
                            state_hash         = StateHash,
                            nonce              = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_CREATE_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_create_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_create_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_create_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_create_tx{} = Tx) ->
    initiator_pubkey(Tx).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_create_tx{}, Trees,_Env) ->
    %% Checks are in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_create_tx{} = Tx, Trees, Env) ->
    {value, SignedTx} = aetx_env:signed_tx(Env),
    {ok, ChannelPubkey} = aesc_utils:channel_pubkey(SignedTx),
    Instructions =
        aeprimop:channel_create_tx_instructions(
          initiator_pubkey(Tx),
          initiator_amount(Tx),
          responder_pubkey(Tx),
          responder_amount(Tx),
          channel_reserve(Tx),
          delegate_pubkeys(Tx),
          state_hash(Tx),
          lock_period(Tx),
          fee(Tx),
          nonce(Tx),
          round(Tx),
          ChannelPubkey),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_create_tx{} = Tx, _) ->
    {ok, [initiator_pubkey(Tx), responder_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_create_tx{initiator_id       = InitiatorId,
                             initiator_amount   = InitiatorAmount,
                             responder_id       = ResponderId,
                             responder_amount   = ResponderAmount,
                             channel_reserve    = ChannelReserve,
                             lock_period        = LockPeriod,
                             ttl                = TTL,
                             fee                = Fee,
                             delegate_ids       = DelegateIds,
                             state_hash         = StateHash,
                             nonce              = Nonce} = Tx) ->
    Version = version(Tx),
    Fields =
        case Version of
            ?CHANNEL_CREATE_TX_VSN ->
                [ {initiator_id      , InitiatorId}
                , {initiator_amount  , InitiatorAmount}
                , {responder_id      , ResponderId}
                , {responder_amount  , ResponderAmount}
                , {channel_reserve   , ChannelReserve}
                , {lock_period       , LockPeriod}
                , {ttl               , TTL}
                , {fee               , Fee}
                , {delegate_ids      , DelegateIds}
                , {state_hash        , StateHash}
                , {nonce             , Nonce}
                ];
            ?CHANNEL_CREATE_TX_VSN_DELEGATES ->
                {IIds, RIds} = DelegateIds,
                [ {initiator_id             , InitiatorId}
                , {initiator_amount         , InitiatorAmount}
                , {responder_id             , ResponderId}
                , {responder_amount         , ResponderAmount}
                , {channel_reserve          , ChannelReserve}
                , {lock_period              , LockPeriod}
                , {ttl                      , TTL}
                , {fee                      , Fee}
                , {initiator_delegate_ids   , IIds}
                , {responder_delegate_ids   , RIds}
                , {state_hash               , StateHash}
                , {nonce                    , Nonce}
                ]
        end,
    {Version, Fields}.


-spec deserialize(vsn(), list()) -> tx().
deserialize(Vsn, Fields) ->
    ValidateAccounts =
        fun(Ids) ->
            lists:foreach(fun(D) -> account = aeser_id:specialize_type(D) end, Ids) end,
    case {Vsn, Fields} of
        {?CHANNEL_CREATE_TX_VSN,
            [ {initiator_id      , InitiatorId}
            , {initiator_amount  , InitiatorAmount}
            , {responder_id      , ResponderId}
            , {responder_amount  , ResponderAmount}
            , {channel_reserve   , ChannelReserve}
            , {lock_period       , LockPeriod}
            , {ttl               , TTL}
            , {fee               , Fee}
            , {delegate_ids      , DelegateIds}
            , {state_hash        , StateHash}
            , {nonce             , Nonce}]} ->
            ValidateAccounts(DelegateIds),
            pass;
        {?CHANNEL_CREATE_TX_VSN_DELEGATES,
            [ {initiator_id             , InitiatorId}
            , {initiator_amount         , InitiatorAmount}
            , {responder_id             , ResponderId}
            , {responder_amount         , ResponderAmount}
            , {channel_reserve          , ChannelReserve}
            , {lock_period              , LockPeriod}
            , {ttl                      , TTL}
            , {fee                      , Fee}
            , {initiator_delegate_ids   , IIds}
            , {responder_delegate_ids   , RIds}
            , {state_hash               , StateHash}
            , {nonce                    , Nonce}]} ->
            ValidateAccounts(IIds),
            ValidateAccounts(RIds),
            DelegateIds = {IIds, RIds}
    end,
    account = aeser_id:specialize_type(InitiatorId),
    account = aeser_id:specialize_type(ResponderId),
    true = aesc_utils:check_state_hash_size(StateHash),
    #channel_create_tx{initiator_id       = InitiatorId,
                       initiator_amount   = InitiatorAmount,
                       responder_id       = ResponderId,
                       responder_amount   = ResponderAmount,
                       channel_reserve    = ChannelReserve,
                       lock_period        = LockPeriod,
                       ttl                = TTL,
                       fee                = Fee,
                       delegate_ids       = DelegateIds,
                       state_hash         = StateHash,
                       nonce              = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_create_tx{initiator_id       = InitiatorId,
                              initiator_amount   = InitiatorAmount,
                              responder_id       = ResponderId,
                              responder_amount   = ResponderAmount,
                              channel_reserve    = ChannelReserve,
                              lock_period        = LockPeriod,
                              nonce              = Nonce,
                              ttl                = TTL,
                              delegate_ids       = DelegateIds0,
                              state_hash         = StateHash,
                              fee                = Fee}) ->
    EncodeIds =
        fun(Ids) -> [aeser_api_encoder:encode(id_hash, D) || D <- Ids] end,
    DelegateIds =
        case DelegateIds0 of
            L when is_list(L) -> EncodeIds(L);
            {IL, RL} when is_list(IL), is_list(RL) ->
                #{ <<"initiator">> => EncodeIds(IL)
                 , <<"responder">> => EncodeIds(RL)}
        end,
    #{<<"initiator_id">>       => aeser_api_encoder:encode(id_hash, InitiatorId),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"responder_id">>       => aeser_api_encoder:encode(id_hash, ResponderId),
      <<"responder_amount">>   => ResponderAmount,
      <<"channel_reserve">>    => ChannelReserve,
      <<"lock_period">>        => LockPeriod,
      <<"nonce">>              => Nonce,
      <<"ttl">>                => TTL,
      <<"delegate_ids">>       => DelegateIds,
      <<"state_hash">>         => aeser_api_encoder:encode(state, StateHash),
      <<"fee">>                => Fee}.

serialization_template(?CHANNEL_CREATE_TX_VSN) ->
    [ {initiator_id      , id}
    , {initiator_amount  , int}
    , {responder_id      , id}
    , {responder_amount  , int}
    , {channel_reserve   , int}
    , {lock_period       , int}
    , {ttl               , int}
    , {fee               , int}
    , {delegate_ids      , [id]}
    , {state_hash        , binary}
    , {nonce             , int}
    ];
serialization_template(?CHANNEL_CREATE_TX_VSN_DELEGATES) ->
    [ {initiator_id               , id}
    , {initiator_amount           , int}
    , {responder_id               , id}
    , {responder_amount           , int}
    , {channel_reserve            , int}
    , {lock_period                , int}
    , {ttl                        , int}
    , {fee                        , int}
    , {initiator_delegate_ids     , [id]}
    , {responder_delegate_ids     , [id]}
    , {state_hash                 , binary}
    , {nonce                      , int}
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec initiator_id(tx()) -> aeser_id:id().
initiator_id(#channel_create_tx{initiator_id = InitiatorId}) ->
    InitiatorId.

-spec initiator_pubkey(tx()) -> aec_keys:pubkey().
initiator_pubkey(#channel_create_tx{initiator_id = InitiatorId}) ->
    aeser_id:specialize(InitiatorId, account).

-spec initiator_amount(tx()) -> non_neg_integer().
initiator_amount(#channel_create_tx{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec channel_reserve(tx()) -> non_neg_integer().
channel_reserve(#channel_create_tx{channel_reserve = ChannelReserve}) ->
    ChannelReserve.

-spec lock_period(tx()) -> non_neg_integer().
lock_period(#channel_create_tx{lock_period = LockPeriod}) ->
    LockPeriod.

-spec responder_id(tx()) -> aeser_id:id().
responder_id(#channel_create_tx{responder_id = ResponderId}) ->
    ResponderId.

-spec responder_pubkey(tx()) -> aec_keys:pubkey().
responder_pubkey(#channel_create_tx{responder_id = ResponderId}) ->
    aeser_id:specialize(ResponderId, account).

-spec responder_amount(tx()) -> non_neg_integer().
responder_amount(#channel_create_tx{responder_amount = ResponderAmount}) ->
    ResponderAmount.

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_create_tx{nonce = Nonce} = Tx) ->
    aesc_channels:pubkey(initiator_pubkey(Tx), Nonce, responder_pubkey(Tx)).

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(#channel_create_tx{} = Tx) ->
    Key = channel_pubkey(Tx),
    aeser_id:create(channel, Key).

-spec state_hash(tx()) -> binary().
state_hash(#channel_create_tx{state_hash = StateHash}) -> StateHash.

-spec round(tx()) -> non_neg_integer().
round(#channel_create_tx{}) ->
    1.

delegate_ids(#channel_create_tx{delegate_ids = DelegateIds}) ->
    DelegateIds.

-spec delegate_pubkeys(tx()) -> [aec_keys:pubkey()] | {[aec_keys:pubkey()],
                                                        [aec_keys:pubkey()]}.

delegate_pubkeys(#channel_create_tx{} = Tx) ->
    Specialize = fun(Ids) -> [aeser_id:specialize(D, account) || D <- Ids] end,
    case delegate_ids(Tx) of
        {IL, RL} -> {Specialize(IL), Specialize(RL)};
        L -> Specialize(L)
    end.

-spec version(tx()) -> non_neg_integer().
version(#channel_create_tx{delegate_ids = DelegateIds}) ->
    case DelegateIds of
        L when is_list(L) -> ?CHANNEL_CREATE_TX_VSN;
        {IL, RL} when is_list(IL), is_list(RL) ->
            ?CHANNEL_CREATE_TX_VSN_DELEGATES
    end.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(Protocol, Tx) ->
    case version(Tx) of
        ?CHANNEL_CREATE_TX_VSN when Protocol < ?IRIS_PROTOCOL_VSN -> true;
        ?CHANNEL_CREATE_TX_VSN_DELEGATES when Protocol >= ?IRIS_PROTOCOL_VSN -> true;
        _ -> false
    end.

%%%===================================================================
%%% Test setters 
%%%===================================================================

-ifdef(TEST).
set_state_hash(Tx, Hash) ->
    Tx#channel_create_tx{state_hash = Hash}.
-endif.
