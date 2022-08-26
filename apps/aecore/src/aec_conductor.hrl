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
-type block_producing_state() :: 'running' | 'stopped'.

-type mining_opts() :: #{ strictly_follow_top => boolean() }.

-type candidate_hash() :: aec_blocks:block_header_hash().
-record(candidate, {block     :: aec_blocks:block(),
                    nonce     :: aec_consensus:key_nonce() | 'undefined',
                    top_hash  :: binary(),
                    refs = 0  :: non_neg_integer() %% Number of miner workers operating on the candidate
                   }).

-record(consensus, {leader             = false    :: boolean(),
                    micro_block_cycle             :: integer(),
                    consensus_module              :: atom()
                    }).

-type instance_state() :: pid() | 'available'.
-record(instance, {id       :: non_neg_integer(),
                   instance :: aeminer_pow:instance() | 'undefined',
                   state    :: instance_state(),
                   config   :: aeminer_pow_cuckoo:config() | 'undefined'
                  }).
-type instance() :: #instance{}.
-type instances() :: list(instance()).

-type mode() :: local_pow | stratum | pos.
-record(state, {key_block_candidates                :: list({candidate_hash(), #candidate{}}) | 'undefined',
                micro_block_candidate               :: #candidate{} | 'undefined',
                blocked_tags            = []        :: ordsets:ordset(atom()),
                keys_ready              = false     :: boolean(),
                block_producing_state   = stopped   :: block_producing_state(),
                mining_opts             = #{}       :: mining_opts(),
                top_block_hash                      :: binary() | 'undefined',
                top_key_block_hash                  :: binary() | 'undefined',
                top_height                          :: aec_blocks:height(),
                workers                 = []        :: workers(),
                instances               = []        :: instances(),
                consensus                           :: #consensus{},
                has_beneficiary         = false     :: boolean(),
                stratum_beneficiary                 :: <<_:(32*8)>> | 'undefined' | fun(() -> <<_:(32*8)>>),
                fraud_list              = []        :: list({binary(), aec_pof:pof()}),
                pending_key_block                   :: aec_blocks:block() | 'undefined',
                mode                    = local_pow :: mode()
               }).
