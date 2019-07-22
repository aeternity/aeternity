-define(LOG_DEBUG(Fun, Arity, Params), lager:debug("~p/~p ~p", [Fun, Arity, Params])).

-define(NOT_SET_BLOCK_HASH, <<0:32/unit:8>>).

% Number of blocks until an opening tx should be considered final
-define(MINIMUM_DEPTH, 4).

-define(REESTABLISH_OPTS_KEYS,
    [ existing_channel_id
    , offchain_tx
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
     }).

-define(TIMER_SUBST(State),
    case State of
        accepted                -> funding_create;
        awaiting_initial_state  -> accept;
        awaiting_leave_ack      -> accept;
        awaiting_locked         -> funding_lock;
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
-define(MIN_DEPTH, min_depth).

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
                         ; T =:= disconnect
                         ; T =:= ?LEAVE
                         ; T =:= ?LEAVE_ACK
                         ; T =:= ?SHUTDOWN
                         ; T =:= ?SHUTDOWN_ACK
                         ; T =:= ?CH_REESTABL
                         ; T =:= ?CH_REEST_ACK).

-define(WATCHER_EVENT(T), T =:= ?CHANNEL_CHANGED
                        ; T =:= ?CHANNEL_UNLOCKED
                        ; T =:= ?MIN_DEPTH_ACHIEVED
                        ; T =:= ?CHANNEL_CLOSING
                        ; T =:= ?CHANNEL_CLOSED).

-define(KEEP, 10).

-define(TRANSITION_STATE(S),  S=:=awaiting_signature
                            ; S=:=awaiting_open
                            ; S=:=awaiting_locked
                            ; S=:=awaiting_update_ack
                            ; S=:=awaiting_leave_ack
                            ; S=:=mutual_closing ).

-ifdef(TEST).
-define(LOG_CAUGHT(Err), lager:debug("CAUGHT ~p / ~p", [Err, erlang:get_stacktrace()])).
-else.
-define(LOG_CAUGHT(Err), lager:debug("CAUGHT ~p", [Err])).
-endif.

%% ==================================================================
%% Records and Types

-record(data, { role                   :: role()
              , channel_status         :: undefined | attached | open | closing
              , cur_statem_state       :: undefined | atom()
              , state                  :: aesc_offchain_state:state() | function()
              , session                :: pid()
              , client                 :: pid()
              , opts                   :: map()
              , channel_id             :: undefined | binary()
              , on_chain_id            :: undefined | binary()
              , create_tx              :: undefined | any()
              , watcher                :: undefined | pid()
              , latest = undefined     :: latest_op()
              , ongoing_update = false :: boolean()
              , last_reported_update   :: undefined | non_neg_integer()
              , log                    :: log()
              , strict_checks = true   :: boolean()
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

-type role() :: initiator | responder.
-type sign_tag() :: create_tx
                  | funding_created
                  | slash_tx
                  | deposit_tx
                  | withdraw_tx
                  | close_solo_tx
                  | settle_tx
                  | ?FND_CREATED
                  | ?DEP_CREATED
                  | ?WDRAW_CREATED
                  | ?UPDATE
                  | ?UPDATE_ACK
                  | ?SHUTDOWN
                  | ?SHUTDOWN_ACK.

-opaque opts() :: #{ minimum_depth => non_neg_integer() %% Defaulted for responder, not for initiator.
                   , timeouts := #{state_name() := pos_integer()
                   , report := map()
                   , log_keep := non_neg_integer()
                   , initiator := aec_keys:pubkey()
                   , responder := aec_keys:pubkey()
                   }}.

-type latest_op() :: undefined % no pending op
                   | {sign | ack, sign_tag(), aetx_sign:signed_tx(), [aesc_offchain_update:update()]}
                   | {create | shutdown | deposit | withdraw, aetx_sign:signed_tx(),
                      [aesc_offchain_update:update()]}
                   | {?MIN_DEPTH, ?WATCH_FND
                                | ?WATCH_DEP
                                | ?WATCH_WDRAW
                                | ?WATCH_CLOSED,
                      TxHash :: binary(), aetx_sign:signed_tx(),
                      [aesc_offchain_update:update()]}
                   | {watch, unlock
                           | close,
                      TxHash :: binary(), aetx_sign:signed_tx(),
                      [aesc_offchain_update:update()]}
                   | {reestablish, OffChainTx :: aetx_sign:signed_tx()}.
