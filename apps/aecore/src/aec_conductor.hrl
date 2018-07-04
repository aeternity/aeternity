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

-define(NG_MICROBLOCK_TIMOUT, 100).

-record(candidate, {block     :: aec_blocks:block(),
                    bin       :: binary(), %% Serialized for hash
                    nonce     :: aec_pow:nonce() | 'undefined',
                    max_nonce :: aec_pow:nonce() | 'undefined',
                    top_hash  :: binary()
                   }).

-record(consensus, {leader             = false                 :: boolean(),
                    micro_block_cycle  = ?NG_MICROBLOCK_TIMOUT :: integer()
                    }).

-record(state, {key_block_candidate                       :: #candidate{} | 'undefined',
                micro_block_candidate                     :: #candidate{} | 'undefined',
                new_micro_candidate_available = false     :: boolean(),
                blocked_tags            = []              :: ordsets:ordset(atom()),
                keys_ready              = false           :: boolean(),
                mining_state            = running         :: mining_state(),
                top_block_hash                            :: binary() | 'undefined',
                top_key_block_hash                        :: binary() | 'undefined',
                workers                 = []              :: workers(),
                consensus                                 :: #consensus{},
                beneficiary                               :: <<_:(32*8)>>  % Maybe move beneficiary out of conductor's state
               }).
