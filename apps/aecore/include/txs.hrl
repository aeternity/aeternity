-record(signed_tx, {
          data            :: term(),
          signatures = [] :: list(binary())}).
-type(signed_tx() :: #signed_tx{}).


%% Basic transactions

-record(coinbase_tx, {
          account = <<>> :: pubkey(),
          nonce = 0      :: non_neg_integer()}).
-type(coinbase_tx() :: #coinbase_tx{}).

-record(spend_tx, {
          from = <<>> :: pubkey(),
          to = <<>>   :: pubkey(),
          amount = 0  :: non_neg_integer(),
          fee = 0     :: non_neg_integer(),
          nonce = 0   :: non_neg_integer()}).
-type(spend_tx() :: #spend_tx{}).


%% Channel-related transactions

-record(channel_new_tx, {
          id = 0          :: non_neg_integer(),
          account1 = <<>> :: pubkey(),
          account2 = <<>> :: pubkey(),
          amount1 = 0     :: non_neg_integer(),
          amount2 = 0     :: non_neg_integer(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer(),
          delay = 0       :: non_neg_integer()}).
-type(channel_new_tx() :: #channel_new_tx{}).

-record(channel_grow_tx, {
          id = 0          :: non_neg_integer(),
          account1 = <<>> :: pubkey(),
          account2 = <<>> :: pubkey(),
          amount1 = 0     :: non_neg_integer(),
          amount2 = 0     :: non_neg_integer(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer(),
          delay = 0       :: non_neg_integer()}).
-type(channel_grow_tx() :: #channel_grow_tx{}).

-record(channel_team_close_tx, {
          id = 0          :: non_neg_integer(),
          account1 = <<>> :: pubkey(),
          account2 = <<>> :: pubkey(),
          amount = 0      :: non_neg_integer(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer()}).
-type(channel_team_close_tx() :: #channel_team_close_tx{}).

-record(channel_solo_close_tx, {
          id = 0         :: non_neg_integer(),
          account = <<>> :: pubkey(),
          fee = 0        :: non_neg_integer(),
          nonce = 0      :: non_neg_integer()}).
-type(channel_solo_close_tx() :: #channel_solo_close_tx{}).

-record(channel_slash_tx, {
          id = 0         :: non_neg_integer(),
          account = <<>> :: pubkey(),
          fee = 0        :: non_neg_integer(),
          nonce = 0      :: non_neg_integer()}).
-type(channel_slash_tx() :: #channel_slash_tx{}).

-record(channel_timeout_tx, {
          id = 0         :: non_neg_integer(),
          account = <<>> :: pubkey(),
          fee = 0        :: non_neg_integer(),
          nonce = 0      :: non_neg_integer()}).
-type(channel_timeout_tx() :: #channel_timeout_tx{}).


%% Oracle-related transactions

-type(oracle_bet() :: true | false | bad_question).

-record(oracle_new_tx, {
          id = 0          :: non_neg_integer(),
          account = <<>>  :: pubkey(),
          question = <<>> :: binary(),
          difficulty = 0  :: non_neg_integer(),
          start_date = 0  :: integer(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer()}).
-type(oracle_new_tx() :: #oracle_new_tx{}).

-record(oracle_bet_tx, {
          id = 0          :: non_neg_integer(),
          account = <<>>  :: pubkey(),
          bet             :: oracle_bet(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer()}).
-type(oracle_bet_tx() :: #oracle_bet_tx{}).

-record(oracle_close_tx, {
          id = 0          :: non_neg_integer(),
          account = <<>>  :: pubkey(),
          fee = 0         :: non_neg_integer(),
          nonce = 0       :: non_neg_integer()}).
-type(oracle_close_tx() :: #oracle_close_tx{}).

-type tx() :: coinbase_tx() | spend_tx() | channel_new_tx() | channel_grow_tx() |
             channel_team_close_tx() | channel_solo_close_tx() | channel_slash_tx() |
             channel_timeout_tx() | oracle_new_tx() | oracle_close_tx().
