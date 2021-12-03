-define(LOG_DEBUG(Fun, Arity, Params), lager:debug("~p/~p ~p", [Fun, Arity, Params])).

-define(NOT_SET_BLOCK_HASH, <<0:32/unit:8>>).

-define(DEFAULT_MINIMUM_DEPTH_STRATEGY, txfee).

% Number of blocks until a fork will be considered active
-define(FORK_MINIMUM_DEPTH, 4).

-define(REESTABLISH_OPTS_KEYS,
    [ existing_channel_id
    , offchain_tx
    , existing_fsm_id_wrapper
    ]).

-define(CONNECT_OPTS_KEYS,
    [ host
    , port
    , noise
    ]).

-define(DEFAULT_TIMEOUTS,
    #{ accept         => 120000
     , funding_create => 120000
     , funding_sign   => 120000
     , funding_lock   => 360000
     , idle           => 600000
     , sign           => 500000
     }).

%% This map needs to contain all recognized tags.
-define(DEFAULT_REPORT_FLAGS,
    #{ info         => true
     , update       => true
     , leave        => true
     , conflict     => true
     , message      => true
     , error        => true
     , debug        => false
     , on_chain_tx  => true
     , min_depth    => true
     }).

-define(TIMER_SUBST(State),
    case State of
        accepted                -> funding_create;
        awaiting_initial_state  -> accept;
        awaiting_leave_ack      -> accept;
        awaiting_locked         -> funding_lock;
        awaiting_connect        -> accept;
        awaiting_open           -> idle;
        awaiting_reestablish    -> idle;
        awaiting_signature      -> sign;
        awaiting_update_ack     -> accept;
        channel_closed          -> idle;
        channel_closing         -> idle;
        dep_half_signed         -> funding_sign;
        dep_signed              -> funding_lock;
        half_signed             -> funding_sign;
        initialized             -> accept;
        mutual_closing          -> accept;
        mutual_closed           -> idle;
        open                    -> idle;
        reestablish_init        -> accept;
        signed                  -> funding_lock;
        wdraw_half_signed       -> funding_sign;
        wdraw_signed            -> funding_lock
    end).

% Useful for debugging
-define(GEN_STATEM_OPTS, []).
%-define(GEN_STATEM_OPTS, [{debug, [trace]}]).

-define(WATCH_FND, funding).
-define(WATCH_DEP, deposit).
-define(WATCH_WDRAW, withdraw).
-define(WATCH_CLOSED, closed).
-define(WATCH_SNAPSHOT_SOLO, snapshot_solo).
-define(WATCH_FORCE_PROGRESS, force_progress_tx).
-define(MIN_DEPTH, min_depth).
-define(MAX_ASSUMPTIONS, 10000).

-define(NO_OP, no_op).

-define(UPDATE_CAST(R), R==?UPDATE; R==?DEP_CREATED; R==?WDRAW_CREATED).
-define(UPDATE_REQ(R),
          R==upd_transfer
        ; R==upd_deposit
        ; R==upd_withdraw
        ; R==upd_create_contract
        ; R==upd_call_contract).

-define(KNOWN_MSG_TYPE(T), T =:= ?CH_OPEN
                         ; T =:= ?CH_ACCEPT
                         ; T =:= ?FND_CREATED
                         ; T =:= ?FND_SIGNED
                         ; T =:= ?FND_LOCKED
                         ; T =:= ?UPDATE
                         ; T =:= ?UPDATE_ACK
                         ; T =:= ?UPDATE_ERR
                         ; T =:= ?DEP_CREATED
                         ; T =:= ?DEP_SIGNED
                         ; T =:= ?DEP_LOCKED
                         ; T =:= ?DEP_ERR
                         ; T =:= ?WDRAW_CREATED
                         ; T =:= ?WDRAW_SIGNED
                         ; T =:= ?WDRAW_LOCKED
                         ; T =:= ?WDRAW_ERR
                         ; T =:= ?INBAND_MSG
                         ; T =:= ?ERROR
                         ; T =:= disconnect
                         ; T =:= ?LEAVE
                         ; T =:= ?LEAVE_ACK
                         ; T =:= ?SHUTDOWN
                         ; T =:= ?SHUTDOWN_ACK
                         ; T =:= ?SHUTDOWN_ERR
                         ; T =:= ?CH_REESTABL
                         ; T =:= ?CH_REEST_ACK).

-define(WATCHER_EVENT(T), T =:= ?CHANNEL_CHANGED
                        ; T =:= ?CHANNEL_UNLOCKED
                        ; T =:= ?MIN_DEPTH_ACHIEVED
                        ; T =:= ?CHANNEL_CLOSING
                        ; T =:= ?CHANNEL_CLOSED).

-define(KEEP, 10).

-define(TRANSITION_STATE(S), S =:= awaiting_signature
                           ; S =:= awaiting_open
                           ; S =:= awaiting_locked
                           ; S =:= awaiting_update_ack
                           ; S =:= awaiting_leave_ack
                           ; S =:= mutual_closing ).

-record(bh_delta, { not_older_than  :: integer()
                  , not_newer_than  :: integer()
                  , pick            :: integer()
                  }).

%% ==================================================================
%% Records and Types

-record(data, { role                            :: role()
              , channel_status                  :: undefined | attached | open | closing
              , cur_statem_state                :: undefined | atom()
              , state                           :: aesc_offchain_state:state() | function()
              , session                         :: undefined | pid()
              , client                          :: undefined | pid()
              , client_mref                     :: undefined | reference()
              , client_connected = true         :: boolean()
              , client_may_disconnect = false   :: boolean()
              , peer_connected = false          :: boolean()
              , opts                            :: map()
              , fsm_id_wrapper                  :: undefined | aesc_fsm_id:wrapper()
              , channel_id                      :: undefined | binary()
              , on_chain_id                     :: undefined | binary()
              , create_tx                       :: undefined | any()
              , watcher_registered = false      :: boolean()
              , block_hash_delta = #bh_delta{}  :: #bh_delta{}
              %% we keep the latest operation so we can perform according
              %% checks
              , op = ?NO_OP                   :: latest_op()
              , chain_op = ?NO_OP             :: latest_chain_op()
              , ongoing_update = false        :: boolean()
              , error_msg_type                :: undefined | error_msg_type()
              , last_reported_update          :: undefined | non_neg_integer()
              , last_channel_change           :: undefined | binary()          %% on-chain tx hash
              , past_assumptions = gb_sets:new() :: gb_sets:set(binary())
              , log                           :: log()
              , strict_checks = true          :: boolean()
              }).

-opaque data() :: #data{ opts :: opts() }.

-type log() :: aesc_window:window({Op :: atom(), Type :: atom(), erlang:timestamp(), Msg :: term()}).

-opaque state_name() :: awaiting_open
                      | awaiting_reestablish
                      | initialized
                      | reestablish_init
                      | awaiting_signature
                      | accepted
                      | half_signed
                      | dep_half_signed
                      | dep_signed
                      | wdraw_half_signed
                      | wdraw_signed
                      | awaiting_locked
                      | awaiting_initial_state
                      | awaiting_update_ack
                      | awaiting_leave_ack
                      | signed
                      | open
                      | mutual_closing
                      | channel_closing
                      | channel_closed.

-type sign_tag() :: create_tx
                  | slash_tx
                  | deposit_tx
                  | withdraw_tx
                  | force_progress_tx
                  | snapshot_solo_tx
                  | close_solo_tx
                  | settle_tx
                  | ?FND_CREATED
                  | ?DEP_CREATED
                  | ?WDRAW_CREATED
                  | ?UPDATE
                  | ?UPDATE_ACK
                  | ?SHUTDOWN
                  | ?SHUTDOWN_ACK.

-type error_msg_type() :: ?UPDATE_ERR
                        | ?DEP_ERR
                        | ?WDRAW_ERR.

-type minimum_depth_factor()   :: non_neg_integer().
-type minimum_depth_strategy() :: txfee | plain.

-opaque opts() :: #{ role                   := initiator | responder
                   , minimum_depth          => minimum_depth_factor()
                   , minimum_depth_strategy => minimum_depth_strategy()
                   , timeouts               := #{state_name() := pos_integer()}
                   , report                 := #{atom() := boolean()}
                   , log_keep               := non_neg_integer()
                   , initiator              := aec_keys:pubkey()
                   , responder              := aec_keys:pubkey()
                   }.

-record(op_data, { signed_tx  :: aetx_sign:signed_tx()
                 , updates    :: [aesc_offchain_update:update()]
                 , block_hash :: aec_blocks:block_header_hash()
                 }).

-record(op_sign, { tag  :: sign_tag()
                 , data :: #op_data{}
                 }).

-record(op_ack, { tag  :: sign_tag()
                , data :: #op_data{}
                }).

-record(op_lock, { tag  :: create | deposit | withdraw
                 , data :: #op_data{}
                 }).

-record(op_min_depth, { tag     :: ?WATCH_FND
                                 | ?WATCH_DEP
                                 | ?WATCH_WDRAW
                                 | ?WATCH_CLOSED
                                 | ?WATCH_SNAPSHOT_SOLO
                      , tx_hash :: binary()
                      , data    :: #op_data{}
                      }).

-record(op_watch, { type    :: unlock | close
                  , tx_hash :: binary()
                  , data    :: #op_data{}
                  }).

-record(op_reestablish, { offchain_tx :: aetx_sign:signed_tx() | undefined
                        , mode = restart :: restart | remain | {connect, map()}
                        }).

-record(op_close, { data :: #op_data{}
                  }).

-type latest_op() :: ?NO_OP % no pending op
                   | #op_sign{}
                   | #op_ack{}
                   | #op_lock{}
                   | #op_watch{}
                   | #op_reestablish{}
                   | #op_close{}.

-type latest_chain_op() :: ?NO_OP % no pending op
                         | #op_min_depth{}.

-define(DEFAULT_FSM_TX_GAS, 20000).

-define(DEFAULT_FSM_TX_TTL_DELTA, 100).

-type next_fsm_state() :: {next_state, atom(), #data{}, list()}.
-type keep_fsm_state() :: {keep_state, #data{}, list()}.

%% TODO: Make this configurable
%% No need for a stronger password policy
%% This check is only here to ensure that someone doesn't enter a 1-2 character password
-define(STATE_PASSWORD_MINIMUM_LENGTH, 6).

%% cancelable tags for an action initiated by this FSM's client. This cancels
%% sign requests
-define(CANCEL_SIGN_TAGS, [ slash_tx
                          , deposit_tx
                          , withdraw_tx
                          , force_progress_tx
                          , snapshot_solo_tx
                          , close_solo_tx
                          , settle_tx
                          , ?UPDATE
                          , ?SHUTDOWN]).

%% cancelable tags for an action initiated by the other party. This cancels
%% acknowledge requests
-define(CANCEL_ACK_TAGS,  [ ?DEP_CREATED
                          , ?WDRAW_CREATED
                          , ?UPDATE_ACK
                          , ?SHUTDOWN_ACK]).

-define(SOLO_TRANSACTIONS, [ aesc_close_solo_tx,
                             aesc_slash_tx,
                             aesc_settle_tx,
                             aesc_snapshot_solo_tx,
                             aesc_force_progress_tx]).

