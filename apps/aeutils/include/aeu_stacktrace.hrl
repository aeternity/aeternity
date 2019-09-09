% Get the stacktrace in a way that is backwards compatible
% Luckily OTP_RELEASE was introduced in the same version as the
% new preferred way of getting the stacktrace.
% A _catch_/2 macro is provided for consistency in cases where the stacktrace
% is not needed.
-ifdef(OTP_RELEASE).
-define(_catch_(ErrorType, Error),
        catch ErrorType:Error ->).
-define(_catch_(ErrorType, Error, ErrorStackTrace),
        catch ErrorType:Error:ErrorStackTrace ->).
-else.
-define(_catch_(ErrorType, Error),
        catch ErrorType:Error ->).
-define(_catch_(ErrorType, Error, ErrorStackTrace),
        catch ErrorType:Error ->
            ErrorStackTrace = erlang:get_stacktrace(),).
-endif.
