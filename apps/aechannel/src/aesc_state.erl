%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    ADT for local state objects
%%% @end
%%%=============================================================================
-module(aesc_state).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([deserialize/1,
         pubkeys/1,
         serialize/1,
         serialize_to_bin/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(LOCAL_STATE_VSN, 1).

-type hash()       :: binary().
-type amount()     :: non_neg_integer().
-type seq_number() :: non_neg_integer().

-record(state, {
          chain_hash       :: hash(),
          initiator_pubkey :: pubkey(),
          responder_pubkey :: pubkey(),
          initiator_amount :: amount(),
          responder_amount :: amount(),
          channel_active   :: boolean(),
          sequence_number  :: seq_number(),
          closed           :: boolean()
         }).
-opaque state() :: #state{}.

-export_type([seq_number/0,
              state/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec deserialize(binary()) -> state().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"vsn">>              := ?LOCAL_STATE_VSN},
     #{<<"chain_hash">>       := ChainHash},
     #{<<"initiator_pubkey">> := InitiatorPubKey},
     #{<<"responder_pubkey">> := ResponderPubKey},
     #{<<"initiator_amount">> := InitiatorAmount},
     #{<<"responder_amount">> := ResponderAmount},
     #{<<"channel_active">>   := ChannelActive},
     #{<<"sequence_number">>  := SeqNumber},
     #{<<"closed">>           := Closed}] = List,
    #state{chain_hash       = ChainHash,
           initiator_pubkey = InitiatorPubKey,
           responder_pubkey = ResponderPubKey,
           initiator_amount = InitiatorAmount,
           responder_amount = ResponderAmount,
           channel_active   = ChannelActive,
           sequence_number  = SeqNumber,
           closed           = Closed}.

-spec pubkeys(state()) -> list(pubkey()).
pubkeys(#state{initiator_pubkey = InitiatorPubKey,
               responder_pubkey = ResponderPubKey}) ->
    [InitiatorPubKey, ResponderPubKey].

-spec serialize(state()) -> binary().
serialize(#state{chain_hash       = ChainHash,
                 initiator_pubkey = InitiatorPubKey,
                 responder_pubkey = ResponderPubKey,
                 initiator_amount = InitiatorAmount,
                 responder_amount = ResponderAmount,
                 channel_active   = ChannelActive,
                 sequence_number  = SeqNumber,
                 closed           = Closed}) ->
    msgpack:pack(
      [#{<<"vsn">>              => ?LOCAL_STATE_VSN},
       #{<<"chain_hash">>       => ChainHash},
       #{<<"initiator_pubkey">> => InitiatorPubKey},
       #{<<"responder_pubkey">> => ResponderPubKey},
       #{<<"initiator_amount">> => InitiatorAmount},
       #{<<"responder_amount">> => ResponderAmount},
       #{<<"channel_active">>   => ChannelActive},
       #{<<"sequence_number">>  => SeqNumber},
       #{<<"closed">>           => Closed}]).

-spec serialize_to_bin(state()) -> binary().
serialize_to_bin(State) ->
    StateMap = serialize(State),
    msgpack:pack(StateMap).

%%%===================================================================
%%% Internal functions
%%%===================================================================
