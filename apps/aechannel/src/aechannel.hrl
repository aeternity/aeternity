-define(KNOWN_ROLES, [initiator, responder]).

-ifdef(TEST).
-define(CATCH_LOG(Err), ?_catch_(error, Err, ST)
        ST1 = case erlang:function_exported(?MODULE, pr_stacktrace, 1) of
            true ->
                apply(?MODULE, pr_stacktrace, [ST]);
            false ->
                ST
        end,
        lager:debug("CAUGHT ~p / ~p", [Err, ST1]),
       ).
-define(CATCH_LOG(Err, Prefix), ?_catch_(error, Err, ST)
        ST1 = case erlang:function_exported(?MODULE, pr_stacktrace, 1) of
            true ->
                apply(?MODULE, pr_stacktrace, [ST]);
            false ->
                ST
        end,
        lager:debug("~s: CAUGHT ~p / ~p", [Prefix, Err, ST1]),
       ).
-else.
% When not testing we don't use the stracktrace, therefore we don't acquire it
% in the first place.
-define(CATCH_LOG(Err), ?_catch_(error, Err)
        lager:debug("CAUGHT ~p", [Err]),
       ).
-define(CATCH_LOG(Err, Prefix), ?_catch_(error, Err)
        lager:debug("~s: CAUGHT ~p", [Prefix, Err]),
       ).
-endif.

%% ==================================================================
%% Records and Types

-type role() :: initiator | responder.
