% Get the stacktrace in a way that is backwards compatible
% Luckily OTP_RELEASE was introduced in the same version as the
% new preferred way of getting the stacktrace.
-ifdef(OTP_RELEASE).
-define(_catch_(ErrorType, Error, ErrorStackTrace),
        catch ErrorType:Error:ErrorStackTrace ->).
-else.
-define(_catch_(ErrorType, Error, ErrorStackTrace),
        catch ErrorType:Error ->
            ErrorStackTrace = erlang:get_stacktrace(),).
-endif.

