-define(KNOWN_ROLES, [initiator, responder]).

-ifdef(TEST).
-define(CATCH_LOG(Err), ?_catch_(error, Err, ST)
        case is_function(pr_stacktrace, 1) of
            true ->
                ST1 = apply(?MODULE, pr_stacktrace, [ST]),
                lager:debug("CAUGHT ~p / ~p", [Err, ST1]);
            false ->
                lager:debug("CAUGHT ~p / ~p", [Err, ST])
        end,
       ).
-define(CATCH_LOG(Err, Prefix), ?_catch_(error, Err, ST)
        case is_function(pr_stacktrace, 1) of
            true ->
                ST1 = apply(?MODULE, pr_stacktrace, [ST]),
                lager:debug("~s: CAUGHT ~p / ~p", [Prefix, Err, ST1]);
            false ->
                lager:debug("~s: CAUGHT ~p / ~p", [Prefix, Err, ST])
        end,
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
