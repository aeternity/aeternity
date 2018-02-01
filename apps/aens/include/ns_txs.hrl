%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Records for Naming System transactions
%%% @end
%%%=============================================================================

-record(ns_preclaim_tx, {
          account    :: pubkey(),
          nonce      :: integer(),
          commitment :: binary(),
          fee        :: integer()
         }).

-record(ns_claim_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name      :: binary(),
          name_salt :: integer(),
          fee       :: integer()
         }).

-record(ns_update_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          name_ttl  :: integer(),
          pointers  :: [{binary(),binary()}],
          ttl       :: integer(),
          fee       :: integer()
         }).

-record(ns_transfer_tx, {
          account           :: pubkey(),
          nonce             :: integer(),
          name_hash         :: binary(),
          recipient_account :: pubkey(),
          fee               :: integer()
         }).

-record(ns_revoke_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          fee       :: integer()
         }).
