%% -*- mode: erlang; erlang-indent-level: 4; indent-tabs-mode: nil -*-
-module(aesc_chan_db).

-export([ add_index_plugins/1
        , ix_tx2chid/3
        ]).

-include_lib("aecore/include/aec_db.hrl").


add_index_plugins(_Mode) ->
    mnesia_schema:add_index_plugin({tx2chid}, aesc_chan_db, ix_tx2chid).


ix_tx2chid(aec_tx, _Ix, #aec_tx{tx = SignedTx}) ->
    try aetx_sign:tx(SignedTx) of
        Tx ->
            {Mod, TxI} = aetx:specialize_callback(Tx),
            case is_channel_tx(Mod) of
                true ->
                    channel_id(Mod, TxI);
                false ->
                    []
            end
    catch
        error:_ ->
            []
    end.

is_channel_tx(Mod) ->
    case Mod of
        aesc_close_mutual_tx -> true;
        aesc_close_solo_tx -> true;
        aesc_create_tx -> true;
        aesc_deposit_tx -> true;
        aesc_offchain_tx -> true;
        aesc_settle_tx -> true;
        aesc_slash_tx -> true;
        aesc_withdraw_tx -> true;
        _ -> false
    end.

channel_id(Mod, Tx) ->
    case lists:keyfind(channel_id, 1, Mod:serialize(Tx)) of
        {_, Id} ->
            [Id];
        false ->
            []
    end.
