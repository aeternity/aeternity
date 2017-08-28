-type(pubkey() :: binary()).

-record(coinbase_tx, {account = <<>> :: pubkey(),
                      nonce = 0      :: non_neg_integer()}).

-record(spend_tx, {from = <<>> :: pubkey(),
                   to = <<>>   :: pubkey(),
                   amount = 0  :: non_neg_integer(),
                   fee = 0     :: non_neg_integer(),
                   nonce = 0   :: non_neg_integer()}).


%% Channel-related transactions

-record(channel_new_tx, {id = 0          :: non_neg_integer(),
                         account1 = <<>> :: pubkey(),
                         account2 = <<>> :: pubkey(),
                         amount1 = 0     :: non_neg_integer(),
                         amount2 = 0     :: non_neg_integer(),
                         fee = 0         :: non_neg_integer(),
                         nonce = 0       :: non_neg_integer(),
                         delay = 0       :: non_neg_integer()}).

-record(channel_grow_tx, {id = 0          :: non_neg_integer(),
                          account1 = <<>> :: pubkey(),
                          account2 = <<>> :: pubkey(),
                          amount1 = 0     :: non_neg_integer(),
                          amount2 = 0     :: non_neg_integer(),
                          fee = 0         :: non_neg_integer(),
                          nonce = 0       :: non_neg_integer(),
                          delay = 0       :: non_neg_integer()}).

-record(channel_team_close_tx, {id = 0          :: non_neg_integer(),
                                account1 = <<>> :: pubkey(),
                                account2 = <<>> :: pubkey(),
                                amount = 0      :: pubkey(),
                                fee = 0         :: non_neg_integer(),
                                nonce = 0       :: non_neg_integer()}).

-record(channel_solo_close_tx, {id = 0         :: non_neg_integer(),
                                account = <<>> :: pubkey(),
                                fee = 0        :: non_neg_integer(),
                                nonce = 0      :: non_neg_integer()}).

-record(channel_slash_tx, {id = 0         :: non_neg_integer(),
                           account = <<>> :: pubkey(),
                           fee = 0        :: non_neg_integer(),
                           nonce = 0      :: non_neg_integer()}).

-record(channel_timeout_tx, {id = 0         :: non_neg_integer(),
                             account = <<>> :: pubkey(),
                             fee = 0        :: non_neg_integer(),
                             nonce = 0      :: non_neg_integer()}).


%% Oracle-related transactions

-type(oracle_bet() :: true | false | bad_question).

-record(oracle_new_tx, {id = 0          :: non_neg_integer(),
                        account = <<>>  :: pubkey(),
                        question = <<>> :: binary(),
                        difficulty = 0  :: non_neg_integer(),
                        start_date = 0  :: integer(),
                        fee = 0         :: non_neg_integer(),
                        nonce = 0       :: non_neg_integer()}).

-record(oracle_bet_tx, {id = 0          :: non_neg_integer(),
                        account = <<>>  :: pubkey(),
                        bet             :: oracle_bet(),
                        fee = 0         :: non_neg_integer(),
                        nonce = 0       :: non_neg_integer()}).

-record(oracle_close_tx, {id = 0          :: non_neg_integer(),
                          account = <<>>  :: pubkey(),
                          fee = 0         :: non_neg_integer(),
                          nonce = 0       :: non_neg_integer()}).
