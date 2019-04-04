-record(aestratum_hash,
        {hash   :: binary(),
         key    :: non_neg_integer()}).

-record(aestratum_share,
        {key    :: non_neg_integer(),
         hash   :: binary(),
         target :: non_neg_integer(), % miner's target
         miner  :: binary()}).        % public key

-record(aestratum_round,
        {key    :: non_neg_integer(),
         val    :: term()}).          % not used, mnesia needs it

-record(aestratum_reward,
        {height  :: non_neg_integer(),
         hash    :: binary(),
         pool    :: #{binary() => float()},
         miners  :: #{binary() => float()},
         amount  :: non_neg_integer(),
         round_key :: non_neg_integer()}).

-record(aestratum_payment,
        {tx_hash :: binary(),
         height  :: non_neg_integer(),
         index   :: non_neg_integer(),
         fee     :: non_neg_integer(),
         gas     :: non_neg_integer(),
         rewards :: map()}).

-define(HASHES_TAB, aestratum_hash).
-define(SHARES_TAB, aestratum_share).
-define(ROUNDS_TAB, aestratum_round).
-define(REWARDS_TAB, aestratum_reward).
-define(PAYMENTS_TAB, aestratum_payment).

-define(TABS, [?HASHES_TAB, ?SHARES_TAB, ?ROUNDS_TAB, ?REWARDS_TAB, ?PAYMENTS_TAB]).
