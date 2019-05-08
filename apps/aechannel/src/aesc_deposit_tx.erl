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
         gas/1,
         nonce/1,
         origin/1,
         amount/1,
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
         state_hash/1,
         round/1]).

-ifdef(TEST).
-export([set_channel_id/2,
         set_round/2,
         set_state_hash/2]).
-endif.
%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_DEPOSIT_TX_VSN, 1).
-define(CHANNEL_DEPOSIT_TX_TYPE, channel_deposit_tx).

-type vsn() :: non_neg_integer().

-record(channel_deposit_tx, {
          channel_id  :: aeser_id:id(),
          from_id     :: aeser_id:id(),
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
      from_id     := FromId,
      amount      := Amount,
      fee         := Fee,
      state_hash  := StateHash,
      round       := Round,
      nonce       := Nonce} = Args) ->
    true = aesc_utils:check_state_hash_size(StateHash),
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    try Tx = #channel_deposit_tx{
                channel_id  = ChannelId,
                from_id     = FromId,
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

-spec gas(tx()) -> non_neg_integer().
gas(#channel_deposit_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_deposit_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_deposit_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_deposit_tx{} = Tx) ->
    from_pubkey(Tx).

from_pubkey(#channel_deposit_tx{from_id = FromId}) ->
    aeser_id:specialize(FromId, account).

-spec amount(tx()) -> non_neg_integer().
amount(#channel_deposit_tx{amount = Amount}) ->
    Amount.

-spec channel_id(tx()) -> aeser_id:id().
channel_id(#channel_deposit_tx{channel_id = ChannelId}) ->
    ChannelId.

-spec channel_pubkey(tx()) -> aec_keys:pubkey().
channel_pubkey(#channel_deposit_tx{channel_id = ChannelId}) ->
    aeser_id:specialize(ChannelId, channel).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_deposit_tx{}, Trees,_Env) ->
    %% Checks in process/3
    {ok, Trees}.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_deposit_tx{} = Tx, Trees, Env) ->
    Instructions =
        aeprimop:channel_deposit_tx_instructions(
          from_pubkey(Tx),
          channel_pubkey(Tx),
          amount(Tx),
          state_hash(Tx),
          round(Tx),
          fee(Tx),
          nonce(Tx)),
    aeprimop:eval(Instructions, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}
                                        | {error, channel_not_found}.
signers(#channel_deposit_tx{} = Tx, Trees) ->
    ChannelPubKey = channel_pubkey(Tx),
    case aec_chain:get_channel(ChannelPubKey, Trees) of
        {ok, Channel} ->
            {ok, [aesc_channels:initiator_pubkey(Channel),
                  aesc_channels:responder_pubkey(Channel)]};
        {error, not_found} -> {error, channel_not_found}
    end.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_deposit_tx{channel_id  = ChannelId,
                              from_id     = FromId,
                              amount      = Amount,
                              ttl         = TTL,
                              fee         = Fee,
                              state_hash  = StateHash,
                              round       = Round,
                              nonce       = Nonce} = Tx) ->
    {version(Tx),
     [ {channel_id  , ChannelId}
     , {from_id     , FromId}
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
            , {from_id     , FromId}
            , {amount      , Amount}
            , {ttl         , TTL}
            , {fee         , Fee}
            , {state_hash  , StateHash}
            , {round       , Round}
            , {nonce       , Nonce}]) ->
    channel = aeser_id:specialize_type(ChannelId),
    account = aeser_id:specialize_type(FromId),
    true = aesc_utils:check_state_hash_size(StateHash),
    #channel_deposit_tx{channel_id  = ChannelId,
                        from_id     = FromId,
                        amount      = Amount,
                        ttl         = TTL,
                        fee         = Fee,
                        state_hash  = StateHash,
                        round       = Round,
                        nonce       = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_deposit_tx{channel_id   = ChannelId,
                               from_id      = FromId,
                               amount       = Amount,
                               ttl          = TTL,
                               fee          = Fee,
                               state_hash   = StateHash,
                               round        = Round,
                               nonce        = Nonce}) ->
    #{<<"channel_id">>   => aeser_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>      => aeser_api_encoder:encode(id_hash, FromId),
      <<"amount">>       => Amount,
      <<"ttl">>          => TTL,
      <<"fee">>          => Fee,
      <<"state_hash">>   => aeser_api_encoder:encode(state, StateHash),
      <<"round">>        => Round,
      <<"nonce">>        => Nonce}.

serialization_template(?CHANNEL_DEPOSIT_TX_VSN) ->
    [ {channel_id  , id}
    , {from_id     , id}
    , {amount      , int}
    , {ttl         , int}
    , {fee         , int}
    , {state_hash  , binary}
    , {round       , int}
    , {nonce       , int}
    ].

state_hash(#channel_deposit_tx{state_hash = StateHash}) -> StateHash.

round(#channel_deposit_tx{round = Round}) ->
    Round.

-spec version(tx()) -> non_neg_integer().
version(_) ->
    ?CHANNEL_DEPOSIT_TX_VSN.

-spec valid_at_protocol(aec_hard_forks:protocol_vsn(), tx()) -> boolean().
valid_at_protocol(_, _) ->
    true.

%%%===================================================================
%%% Test setters 
%%%===================================================================

-ifdef(TEST).
set_channel_id(Tx, ChannelId) ->
    channel = aeser_id:specialize_type(ChannelId),
    Tx#channel_deposit_tx{channel_id = ChannelId}.

set_round(Tx, Round) when is_integer(Round) ->
    Tx#channel_deposit_tx{round = Round}.

set_state_hash(Tx, Hash) ->
    Tx#channel_deposit_tx{state_hash = Hash}.
-endif.
