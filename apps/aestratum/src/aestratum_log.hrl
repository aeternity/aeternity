
-define(DEBUG(Fmt, Args), lager:log(aestratum_lager_event, debug, [], Fmt, Args)).
-define(INFO(Fmt, Args), lager:log(aestratum_lager_event, info, [], Fmt, Args)).
-define(ERROR(Fmt, Args), lager:log(aestratum_lager_event, error, [], Fmt, Args)).
-define(WARN(Fmt, Args), lager:log(aestratum_lager_event, warning, [], Fmt, Args)).
-define(CRITICAL(Fmt, Args), lager:log(aestratum_lager_event, critical, [], Fmt, Args)).
