%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aesc_payload).

-export([channel_id/1
       , state_hash/1
       , updates/1
       , round/1
       , new/1
        ]).

-opaque tx() :: {aesc_offchain_tx, aesc_offchain_tx:tx()}
              | {aesc_create_tx, aesc_create_tx:tx()}
              | {aesc_deposit_tx, aesc_deposit_tx:tx()}
              | {aesc_withdraw_tx, aesc_withdraw_tx:tx()}.

-define(MODS, [aesc_offchain_tx,
               aesc_create_tx,
               aesc_deposit_tx,
               aesc_withdraw_tx]).

-export_type([tx/0]).

-compile({no_auto_import, [round/1]}).

%% -- Behaviour definition ---------------------------------------------------

-callback channel_id(aetx:tx_instance()) -> aesc_channels:id().

-callback state_hash(aetx:tx_instance()) -> binary().

-callback round(aetx:tx_instance()) -> aesc_channels:seq_number().

-callback updates(aetx:tx_instance()) -> [aesc_offchain_state:update()].

-spec channel_id(tx()) -> aesc_channels:id().
channel_id(Tx) -> call_callback(Tx, channel_id).

-spec state_hash(tx()) -> binary().
state_hash(Tx) -> call_callback(Tx, state_hash).

-spec round(tx()) -> aesc_channels:seq_number().
round(Tx) -> call_callback(Tx, round).

-spec updates(tx()) -> [aesc_offchain_state:update()].
updates(Tx) -> call_callback(Tx, updates).

-spec new(aetx:tx()) -> {ok, tx()} | error.
new(Tx) ->
    {Mod, TxI} = aetx:specialize_callback(Tx),
    case lists:member(Mod, ?MODS) of
        true -> {ok, {Mod, TxI}};
        false -> error
    end.

-spec call_callback(tx(), atom()) -> any().
call_callback({Mod, TxI}, Callback) ->
    Mod:Callback(TxI).

