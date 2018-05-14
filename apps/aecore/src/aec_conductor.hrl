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

-define(NG_MICROBLOCK_TIMOUT, 3000).

-record(candidate, {block     :: aec_blocks:block(),
                    bin       :: binary(), %% Serialized for hash
                    nonce     :: aec_pow:nonce(),
                    max_nonce :: aec_pow:nonce(),
                    top_hash  :: binary()
                   }).

-record(consensus, {height :: integer(),
                    leader            = false                 :: boolean(),
                    leader_key                                :: binary() | undefined,
                    micro_block_cycle = ?NG_MICROBLOCK_TIMOUT :: integer()
                    }).

-record(state, {block_candidate                     :: #candidate{} | 'undefined',
                micro_block_candidate               :: #candidate{} | undefined,
                blocked_tags            = []        :: ordsets:ordset(atom()),
                fetch_new_txs_from_pool = true      :: boolean(),
                keys_ready              = false     :: boolean(),
                mining_state            = running   :: mining_state(),
                seen_top_block_hash                 :: binary() | 'undefined',
                workers                 = []        :: workers(),
                consensus                           :: #consensus{}
               }).
