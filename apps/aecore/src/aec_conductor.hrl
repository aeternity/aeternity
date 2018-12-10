%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-type(option()  :: {atom(), any()}).
-type(options() :: [option()]).


-record(worker_info, {tag   :: atom(),
                      mon   :: reference(),
                      timer :: {t, timer:tref()} | 'no_timer'}).

-type worker_info() :: #worker_info{}.
-type workers() :: orddict:orddict(pid(), worker_info()).
-type mining_state() :: 'running' | 'stopped'.

-type candidate_hash() :: aec_blocks:block_header_hash().
-record(candidate, {block     :: aec_blocks:block(),
                    nonce     :: aec_pow:nonce() | 'undefined',
                    top_hash  :: binary(),
                    refs = 0  :: non_neg_integer() %% Number of miner workers operating on the candidate
                   }).

-record(consensus, {leader             = false    :: boolean(),
                    micro_block_cycle             :: integer()
                    }).

-type miner_instance()  :: non_neg_integer().
-type instance_state()  :: pid() | 'available'.
-type miner_instances() :: list({miner_instance(), instance_state()}).

-record(state, {key_block_candidates              :: list({candidate_hash(), #candidate{}}) | 'undefined',
                micro_block_candidate             :: #candidate{} | 'undefined',
                blocked_tags            = []      :: ordsets:ordset(atom()),
                keys_ready              = false   :: boolean(),
                mining_state            = running :: mining_state(),
                top_block_hash                    :: binary() | 'undefined',
                top_key_block_hash                :: binary() | 'undefined',
                workers                 = []      :: workers(),
                miner_instances         = []      :: miner_instances(),
                consensus                         :: #consensus{},
                beneficiary                       :: <<_:(32*8)>>,
                fraud_list              = []      :: list({binary(), aec_pof:pof()}),
                pending_key_block                 :: aec_blocks:block() | 'undefined'
               }).
