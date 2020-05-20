%%%=============================================================================
%%% @doc
%%%    Helper functions to work with the system's logging.
%%% @end
%%%=============================================================================
-module(aeu_log).

-export([ set_console_log_level/1
        ]).

%% @doc Set log level for configured console log backends. Returns previously
%% configured level. The log level must be supported by lager, otherwise this
%% function will fail.
set_console_log_level(Level) ->
    PrevLevel = lager:get_loglevel(lager_console_backend),
    ok = lager:set_loglevel(lager_console_backend, Level),
    ok = lager:set_loglevel(epoch_sync_lager_event, lager_console_backend, undefined, Level),
    PrevLevel.
