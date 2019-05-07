%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aesc_signable_transaction).

%% -- Behaviour definition ---------------------------------------------------

-callback channel_id(aetx:tx_instance()) -> aesc_channels:id().

-callback channel_pubkey(aetx:tx_instance()) -> aesc_channels:pubkey().

-callback state_hash(aetx:tx_instance()) -> binary().

-callback round(aetx:tx_instance()) -> aesc_channels:seq_number().

