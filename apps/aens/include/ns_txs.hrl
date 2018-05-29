%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Records for Naming System transactions
%%% @end
%%%=============================================================================

-record(ns_preclaim_tx, {
          account    :: aec_keys:pubkey(),
          nonce      :: integer(),
          commitment :: binary(),
          fee        :: integer(),
          ttl        :: aec_blocks:height()
         }).

-record(ns_claim_tx, {
          account   :: aec_keys:pubkey(),
          nonce     :: integer(),
          name      :: binary(),
          name_salt :: integer(),
          fee       :: integer(),
          ttl       :: aec_blocks:height()
         }).

-record(ns_update_tx, {
          account    :: aec_keys:pubkey(),
          nonce      :: integer(),
          name_hash  :: binary(),
          name_ttl   :: integer(),
          pointers   :: [{binary(),binary()}],
          client_ttl :: integer(),
          fee        :: integer(),
          ttl        :: aec_blocks:height()
         }).

-record(ns_transfer_tx, {
          account           :: aec_keys:pubkey(),
          nonce             :: integer(),
          name_hash         :: binary(),
          recipient_account :: aec_keys:pubkey(),
          fee               :: integer(),
          ttl               :: aec_blocks:height()
         }).

-record(ns_revoke_tx, {
          account   :: aec_keys:pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          fee       :: integer(),
          ttl       :: aec_blocks:height()
         }).
