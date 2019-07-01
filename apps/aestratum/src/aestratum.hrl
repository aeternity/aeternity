-define(PAYMENT_CONTRACT_TESTNET_ADDRESS, <<"ct_jgo43gdpikScyo5munDvsVWBJDugjkMtqPHUfzGJyuWAFFUuB">>).
-define(PAYMENT_CONTRACT_MAINNET_ADDRESS, <<"ct_AhMbfHYPBK8Qu1DkqXwQHcMKZoZAUndyYTNZDnyS1AdWh7X9U">>).

-include_lib("aecore/src/aec_conductor.hrl").

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
         pool    :: transformed | #{binary() => float()},
         miners  :: transformed | #{binary() => float()},
         amount  :: non_neg_integer(),
         round_key :: non_neg_integer()}).

-record(aestratum_payment,
        {id      :: {Height :: non_neg_integer(), Index :: non_neg_integer() | '_'},
         total   :: non_neg_integer() | '_',
         relmap  :: #{binary() => float()} | '_',
         absmap  :: undefined | #{binary() => non_neg_integer()} | '_',
         tx_hash :: undefined | binary() | '_',
         nonce   :: undefined | non_neg_integer() | '_',
         fee     :: undefined | non_neg_integer() | '_',
         gas     :: undefined | non_neg_integer() | '_', % min gas - for transaction
         run_gas :: undefined | non_neg_integer() | '_', % contract gas - for computation
         date    :: undefined | calendar:datetime() | '_'}).

-record(aestratum_candidate,
        {block_hash :: binary(),
         header     :: binary(),
         record     :: #candidate{},
         date       :: calendar:datetime()}).


-define(HASHES_TAB, aestratum_hash).
-define(SHARES_TAB, aestratum_share).
-define(ROUNDS_TAB, aestratum_round).
-define(REWARDS_TAB, aestratum_reward).
-define(PAYMENTS_TAB, aestratum_payment).
-define(CANDIDATES_TAB, aestratum_candidate).

-define(TABS, [?HASHES_TAB, ?SHARES_TAB, ?ROUNDS_TAB,
               ?REWARDS_TAB, ?PAYMENTS_TAB, ?CANDIDATES_TAB]).

-define(PAYMENT_CONTRACT_BATCH_SIZE, 1000).

-define(TXN(Body), mnesia:activity(transaction, fun () -> Body end)).

-define(CFG(Key), aestratum_env:get(Key)).

-define(ENABLED, ?CFG(enabled)).

%% CONNECTION options
-define(HOST,            ?CFG(host)).
-define(PORT,            ?CFG(port)).
-define(TRANSPORT,       ?CFG(transport)).
-define(NUM_ACCEPTORS,   ?CFG(num_acceptors)).
-define(MAX_CONNECTIONS, ?CFG(max_connections)).

%% SESSION options
-define(MAX_JOBS,                    ?CFG(max_jobs)).
-define(MSG_TIMEOUT,                 ?CFG(msg_timeout)).
-define(DESIRED_SOLVE_TIME,          ?CFG(desired_solve_time)).
-define(MAX_SOLVE_TIME,              ?CFG(max_solve_time)).
-define(INITIAL_SHARE_TARGET,        ?CFG(initial_share_target)).
-define(MAX_SHARE_TARGET,            ?CFG(max_share_target)).
-define(EXTRA_NONCE_BYTES,           ?CFG(extra_nonce_bytes)).
-define(SHARE_TARGET_DIFF_THRESHOLD, ?CFG(share_target_diff_threshold)).

%% REWARD options
-define(LAST_N,               ?CFG(last_n)).
-define(CONTRACT_PUBKEY,      ?CFG(contract_pubkey)).
-define(CALLER_ADDRESS,       ?CFG(caller_address)).
-define(CALLER_PRIVKEY,       ?CFG(caller_privkey)).
-define(CALLER_PUBKEY,        ?CFG(caller_pubkey)).
-define(POOL_PERCENT_SUM,     ?CFG(pool_percent_sum)).
-define(POOL_PERCENT_SHARES,  ?CFG(pool_percent_shares)).
-define(POOL_RELATIVE_SHARES, ?CFG(pool_relative_shares)).

%% INTERNAL (not configurable from config)
-define(REWARD_KEYBLOCK_DELAY, ?CFG(reward_keyblock_delay)).
-define(PAYOUT_KEYBLOCK_DELAY, ?CFG(payout_keyblock_delay)).
