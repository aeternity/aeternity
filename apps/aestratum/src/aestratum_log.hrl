
-define(LOG(Level, Fmt, Args), lager:log(aestratum_lager_event, Level, self(), Fmt, Args)).

-define(DEBUG(Fmt, Args),    ?LOG(debug, Fmt, Args)).
-define(INFO(Fmt, Args),     ?LOG(info, Fmt, Args)).
-define(ERROR(Fmt, Args),    ?LOG(error, Fmt, Args)).
-define(WARN(Fmt, Args),     ?LOG(warn, Fmt, Args)).
-define(CRITICAL(Fmt, Args), ?LOG(critical, Fmt, Args)).
