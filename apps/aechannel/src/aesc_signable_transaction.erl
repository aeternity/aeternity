%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aesc_signable_transaction).

%% -- Behaviour definition ---------------------------------------------------

-callback channel_pubkey(aetx:tx_instance()) -> aesc_channels:pubkey().

-callback state_hash(aetx:tx_instance()) -> binary().

-callback round(aetx:tx_instance()) -> aesc_channels:seq_number().

-callback updates(aetx:tx_instance()) -> [aesc_offchain_update:update()].

