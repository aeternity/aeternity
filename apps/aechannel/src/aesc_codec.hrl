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
-define(ID_INBAND_MSG   , 96).
-define(ID_ERROR        , 97).
-define(ID_SHUTDOWN     , 98).
-define(ID_SHUTDOWN_ACK , 99).

