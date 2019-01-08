%% Message codes for ae state channels

-define(CH_OPEN      , channel_open).
-define(CH_ACCEPT    , channel_accept).
-define(CH_REESTABL  , channel_reestablish).
-define(CH_REEST_ACK , channel_reest_ack).
-define(FND_CREATED  , funding_created).
-define(FND_SIGNED   , funding_signed).
-define(FND_LOCKED   , funding_locked).
-define(UPDATE       , update).
-define(UPDATE_ACK   , update_ack).
-define(UPDATE_ERR   , update_error).
-define(DEP_CREATED  , deposit_created).
-define(DEP_SIGNED   , deposit_signed).
-define(DEP_LOCKED   , deposit_locked).
-define(DEP_ERR      , deposit_error).
-define(WDRAW_CREATED, withdraw_created).
-define(WDRAW_SIGNED , withdraw_signed).
-define(WDRAW_LOCKED , withdraw_locked).
-define(WDRAW_ERR    , withdraw_error).
-define(ERROR        , error).
-define(LEAVE        , leave).
-define(LEAVE_ACK    , leave_ack).
-define(SHUTDOWN     , shutdown).
-define(SHUTDOWN_ACK , shutdown_ack).
-define(INBAND_MSG   , inband_msg).

-define(ID_CH_OPEN      , 1).
-define(ID_CH_ACCEPT    , 2).
-define(ID_CH_REESTABL  , 3).
-define(ID_CH_REEST_ACK , 4).
-define(ID_FND_CREATED  , 5).
-define(ID_FND_SIGNED   , 6).
-define(ID_FND_LOCKED   , 7).
-define(ID_UPDATE       , 8).
-define(ID_UPDATE_ACK   , 9).
-define(ID_UPDATE_ERR   , 10).
-define(ID_DEP_CREATED  , 11).
-define(ID_DEP_SIGNED   , 12).
-define(ID_DEP_LOCKED   , 13).
-define(ID_DEP_ERR      , 14).
-define(ID_WDRAW_CREATED, 15).
-define(ID_WDRAW_SIGNED , 16).
-define(ID_WDRAW_LOCKED , 17).
-define(ID_WDRAW_ERR    , 18).
-define(ID_LEAVE        , 94).
-define(ID_LEAVE_ACK    , 95).
-define(ID_INBAND_MSG   , 96).
-define(ID_ERROR        , 97).
-define(ID_SHUTDOWN     , 98).
-define(ID_SHUTDOWN_ACK , 99).

%% Non-encoded cast types
-define(DISCONNECT, disconnect).
-define(SIGNED, signed).
-define(SETTLE, settle).
-define(MIN_DEPTH_ACHIEVED, minimum_depth_achieved).
-define(CHANNEL_CHANGED, channel_changed).
-define(CHANNEL_CLOSING, channel_closing).
-define(CHANNEL_UNLOCKED, channel_unlocked).

%% Error codes
-define(ERR_VALIDATION, 1).
-define(ERR_CONFLICT  , 2).
