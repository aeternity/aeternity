%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Records for State Channels transactions
%%% @end
%%%=============================================================================
-include_lib("apps/aecore/include/common.hrl").

-record(channel_create_tx, {
          initiator          :: pubkey(),
          participant        :: pubkey(),
          initiator_amount   :: integer(),
          participant_amount :: integer(),
          fee                :: non_neg_integer(),
          nonce              :: non_neg_integer()
         }).

-record(channel_deposit_tx, {
          channel_id   :: binary(),
          from_account :: pubkey(),
          to_account   :: pubkey(),
          amount       :: non_neg_integer(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).

-record(channel_withdraw_tx, {
          channel_id   :: binary(),
          from_account :: pubkey(),
          to_account   :: pubkey(),
          amount       :: non_neg_integer(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).

-record(channel_close_mutual_tx, {
          channel_id   :: binary(),
          amount       :: integer(),
          account      :: pubkey(),
          fee          :: non_neg_integer(),
          nonce        :: non_neg_integer()
         }).
