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
         ttl/1,
         nonce/1,
         origin/1,
         check/5,
         process/6,
         signers/2,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
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

% snapshot callbacks
-export([channel_pubkey/1,
         state_hash/1,
         updates/1,
         round/1]).
%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_CREATE_TX_VSN, 1).
-define(CHANNEL_CREATE_TX_TYPE, channel_create_tx).

-type vsn() :: non_neg_integer().

-record(channel_create_tx, {
          initiator_id       :: aec_id:id(),
          initiator_amount   :: non_neg_integer(),
          responder_id       :: aec_id:id(),
          responder_amount   :: non_neg_integer(),
          channel_reserve    :: non_neg_integer(),
          lock_period        :: non_neg_integer(),
          ttl                :: aetx:tx_ttl(),
          fee                :: non_neg_integer(),
          delegate_ids       :: [aec_id:id()],
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
    DelegateIds = maps:get(delegate_ids, Args, []),
    lists:foreach(fun(D) -> account = aec_id:specialize_type(D) end, DelegateIds),
    account = aec_id:specialize_type(InitiatorId),
    account = aec_id:specialize_type(ResponderId),
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

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_create_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_create_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_create_tx{} = Tx) ->
    initiator_pubkey(Tx).

-spec check(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
        {ok, aec_trees:trees()} | {error, term()}.
check(#channel_create_tx{initiator_amount   = InitiatorAmount,
                         responder_amount   = ResponderAmount,
                         channel_reserve    = ChannelReserve,
                         nonce              = Nonce,
                         state_hash         = _StateHash,
                         fee                = Fee} = Tx, _Context, Trees, _Height, _ConsensusVersion) ->
    InitiatorPubKey = initiator_pubkey(Tx),
    ResponderPubKey = responder_pubkey(Tx),
    Checks =
        [fun() -> aetx_utils:check_account(InitiatorPubKey, Trees, Nonce, InitiatorAmount + Fee) end,
         fun() -> aetx_utils:check_account(ResponderPubKey, Trees, ResponderAmount) end,
         fun() -> check_reserve_amount(ChannelReserve, InitiatorAmount, ResponderAmount) end,
         fun() -> check_not_channel(InitiatorPubKey, Nonce, ResponderPubKey, Trees) end],
    case aeu_validation:run(Checks) of
        ok ->
            {ok, Trees};
        {error, _Reason} = Error ->
            Error
    end.

-spec process(tx(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(),
              non_neg_integer(), binary() | no_tx_hash) -> {ok, aec_trees:trees()}.
process(#channel_create_tx{initiator_amount   = InitiatorAmount,
                           responder_amount   = ResponderAmount,
                           fee                = Fee,
                           state_hash         = _StateHash,
                           nonce              = Nonce} = CreateTx, _Context, Trees0, _Height,
        _ConsensusVersion, _TxHash) ->
    InitiatorPubKey = initiator_pubkey(CreateTx),
    ResponderPubKey = responder_pubkey(CreateTx),

    AccountsTree0 = aec_trees:accounts(Trees0),
    ChannelsTree0 = aec_trees:channels(Trees0),

    InitiatorAccount0 = aec_accounts_trees:get(InitiatorPubKey, AccountsTree0),
    ResponderAccount0 = aec_accounts_trees:get(ResponderPubKey, AccountsTree0),

    {ok, InitiatorAccount1} = aec_accounts:spend(InitiatorAccount0, Fee + InitiatorAmount, Nonce),
    {ok, ResponderAccount1} = aec_accounts:spend_without_nonce_bump(ResponderAccount0, ResponderAmount),

    AccountsTree1 = aec_accounts_trees:enter(InitiatorAccount1, AccountsTree0),
    AccountsTree2 = aec_accounts_trees:enter(ResponderAccount1, AccountsTree1),

    Channel       = aesc_channels:new(CreateTx),
    ChannelsTree1 = aesc_state_tree:enter(Channel, ChannelsTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree2),
    Trees2 = aec_trees:set_channels(Trees1, ChannelsTree1),
    {ok, Trees2}.

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
                             nonce              = Nonce}) ->
    {version(),
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
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_CREATE_TX_VSN,
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
            , {nonce             , Nonce}]) ->
    account = aec_id:specialize_type(InitiatorId),
    account = aec_id:specialize_type(ResponderId),
    [account = aec_id:specialize_type(D) || D <- DelegateIds],
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
                              delegate_ids       = DelegateIds,
                              state_hash         = StateHash,
                              fee                = Fee}) ->
    #{<<"data_schema">>        => <<"ChannelCreateTxJSON">>, % swagger schema name
      <<"vsn">>                => version(),
      <<"initiator_id">>       => aec_base58c:encode(id_hash, InitiatorId),
      <<"initiator_amount">>   => InitiatorAmount,
      <<"responder_id">>       => aec_base58c:encode(id_hash, ResponderId),
      <<"responder_amount">>   => ResponderAmount,
      <<"channel_reserve">>    => ChannelReserve,
      <<"lock_period">>        => LockPeriod,
      <<"nonce">>              => Nonce,
      <<"ttl">>                => TTL,
      <<"delegate_ids">>       => [aec_base58c:encode(id_hash, D) || D <- DelegateIds],
      <<"state_hash">>         => aec_base58c:encode(state, StateHash),
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
    ].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec initiator_id(tx()) -> aec_id:id().
initiator_id(#channel_create_tx{initiator_id = InitiatorId}) ->
    InitiatorId.

-spec initiator_pubkey(tx()) -> aec_keys:pubkey().
initiator_pubkey(#channel_create_tx{initiator_id = InitiatorId}) ->
    aec_id:specialize(InitiatorId, account).

-spec initiator_amount(tx()) -> non_neg_integer().
initiator_amount(#channel_create_tx{initiator_amount = InitiatorAmount}) ->
    InitiatorAmount.

-spec channel_reserve(tx()) -> non_neg_integer().
channel_reserve(#channel_create_tx{channel_reserve = ChannelReserve}) ->
    ChannelReserve.

-spec lock_period(tx()) -> non_neg_integer().
lock_period(#channel_create_tx{lock_period = LockPeriod}) ->
    LockPeriod.

-spec responder_id(tx()) -> aec_id:id().
responder_id(#channel_create_tx{responder_id = ResponderId}) ->
    ResponderId.

-spec responder_pubkey(tx()) -> aec_keys:pubkey().
responder_pubkey(#channel_create_tx{responder_id = ResponderId}) ->
    aec_id:specialize(ResponderId, account).

-spec responder_amount(tx()) -> non_neg_integer().
responder_amount(#channel_create_tx{responder_amount = ResponderAmount}) ->
    ResponderAmount.

-spec channel_pubkey(tx()) -> aesc_channels:pubkey().
channel_pubkey(#channel_create_tx{nonce = Nonce} = Tx) ->
    aesc_channels:pubkey(initiator_pubkey(Tx), Nonce, responder_pubkey(Tx)).

-spec state_hash(tx()) -> binary().
state_hash(#channel_create_tx{state_hash = StateHash}) -> StateHash.

updates(#channel_create_tx{}) ->
    [].

round(#channel_create_tx{}) ->
    1.

delegate_ids(#channel_create_tx{delegate_ids = DelegateIds}) ->
    DelegateIds.

-spec delegate_pubkeys(tx()) -> [aec_keys:pubkey()].
delegate_pubkeys(#channel_create_tx{delegate_ids = DelegateIds}) ->
    [aec_id:specialize(D, account) || D <- DelegateIds].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec check_not_channel(aec_keys:pubkey(),non_neg_integer(), aec_keys:pubkey(), aec_trees:trees()) ->
                               ok | {error, channel_exists}.
check_not_channel(InitiatorPubKey, Nonce, ResponderPubKey, Trees) ->
    ChannelPubKey = aesc_channels:pubkey(InitiatorPubKey, Nonce, ResponderPubKey),
    ChannelsTrees = aec_trees:channels(Trees),
    case aesc_state_tree:lookup(ChannelPubKey, ChannelsTrees) of
        {value, _Channel} -> {error, channel_exists};
        none              -> ok
    end.

check_reserve_amount(Reserve, InitiatorAmount,
                     ResponderAmount) when is_integer(Reserve) ->
    case (Reserve =< InitiatorAmount) of
        true ->
            case (Reserve =< ResponderAmount) of
                true  -> ok;
                false -> {error, insufficient_responder_amount}
            end;
        false ->
            {error, insufficient_initiator_amount}
    end.

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_CREATE_TX_VSN.
