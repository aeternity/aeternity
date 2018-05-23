%% Message codes for ae state channels

-define(CH_OPEN      , channel_open).
-define(CH_ACCEPT    , channel_accept).
-define(CH_REESTABL  , channel_reestablish).
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
-define(SHUTDOWN     , shutdown).
-define(SHUTDOWN_ACK , shutdown_ack).
-define(INBAND_MSG   , inband_msg).

-define(ID_CH_OPEN      , 1).
-define(ID_CH_ACCEPT    , 2).
-define(ID_CH_REESTABL  , 3).
-define(ID_FND_CREATED  , 4).
-define(ID_FND_SIGNED   , 5).
-define(ID_FND_LOCKED   , 6).
-define(ID_UPDATE       , 7).
-define(ID_UPDATE_ACK   , 8).
-define(ID_UPDATE_ERR   , 9).
-define(ID_DEP_CREATED  , 10).
-define(ID_DEP_SIGNED   , 11).
-define(ID_DEP_LOCKED   , 12).
-define(ID_DEP_ERR      , 13).
-define(ID_WDRAW_CREATED, 14).
-define(ID_WDRAW_SIGNED , 15).
-define(ID_WDRAW_LOCKED , 16).
-define(ID_WDRAW_ERR    , 17).
-define(ID_INBAND_MSG   , 96).
-define(ID_ERROR        , 97).
-define(ID_SHUTDOWN     , 98).
-define(ID_SHUTDOWN_ACK , 99).

%% Non-encoded cast types
-define(DISCONNECT, disconnect).
-define(SIGNED, signed).
-define(MIN_DEPTH_ACHIEVED, minimum_depth_achieved).
