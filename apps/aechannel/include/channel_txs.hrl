%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Records for State Channels transactions
%%% @end
%%%=============================================================================
-include_lib("apps/aecore/include/common.hrl").

-record(channel_create_tx, {
          initiator          :: pubkey(),
          initiator_amount   :: non_neg_integer(),
          participant        :: pubkey(),
          participant_amount :: non_neg_integer(),
          lock_period        :: non_neg_integer(),
          fee                :: non_neg_integer(),
          nonce              :: non_neg_integer()
         }).

-record(channel_deposit_tx, {
          channel_id   :: binary(),
          from_account :: pubkey(),
          to_account   :: pubkey(),
          amount       :: non_neg_integer(),
          initiator    :: pubkey(),
          participant  :: pubkey(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).

-record(channel_withdraw_tx, {
          channel_id   :: binary(),
          from_account :: pubkey(),
          to_account   :: pubkey(),
          amount       :: non_neg_integer(),
          initiator    :: pubkey(),
          participant  :: pubkey(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).

-record(channel_close_mutual_tx, {
          channel_id   :: binary(),
          amount       :: integer(),
          initiator    :: pubkey(),
          participant  :: pubkey(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).

-record(channel_close_solo_tx, {
          channel_id :: binary(),
          account    :: pubkey(),
          payload    :: binary(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-record(channel_slash_tx, {
          channel_id :: binary(),
          account    :: pubkey(),
          payload    :: binary(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-record(channel_settle_tx, {
          channel_id :: binary(),
          account    :: pubkey(),
          party      :: pubkey(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).
