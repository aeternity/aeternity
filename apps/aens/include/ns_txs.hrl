%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-include_lib("apps/aecore/include/common.hrl").

-record(ns_preclaim_tx, {
          account    :: pubkey(),
          nonce      :: integer(),
          commitment :: binary(),
          fee        :: integer()
         }).

-record(ns_claim_tx, {
          account    :: pubkey(),
          nonce      :: integer(),
          name       :: binary(),
          name_nonce :: integer(),
          fee        :: integer()
         }).

-record(ns_update_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          name_ttl  :: integer(),
          pointers  :: binary(),
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
