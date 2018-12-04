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

-record(candidate, {block     :: aec_blocks:block(),
                    bin       :: binary(), %% Serialized for hash
                    nonce     :: aec_pow:nonce() | 'undefined',
                    max_nonce :: aec_pow:nonce() | 'undefined',
                    top_hash  :: binary()
                   }).

-record(consensus, {leader             = false    :: boolean(),
                    micro_block_cycle             :: integer()
                    }).

-record(state, {key_block_candidate                       :: #candidate{} | 'undefined',
                micro_block_candidate                     :: #candidate{} | 'undefined',
                blocked_tags            = []              :: ordsets:ordset(atom()),
                keys_ready              = false           :: boolean(),
                mining_state            = stopped         :: mining_state(),
                top_block_hash                            :: binary() | 'undefined',
                top_key_block_hash                        :: binary() | 'undefined',
                workers                 = []              :: workers(),
                consensus                                 :: #consensus{},
                beneficiary                               :: <<_:(32*8)>> | 'undefined',
                fraud_list              = []              :: list({binary(), aec_pof:pof()}),
                pending_key_block                         :: aec_blocks:block() | 'undefined'
               }).
