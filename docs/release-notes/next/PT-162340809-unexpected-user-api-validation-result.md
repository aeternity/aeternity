* Fine tunes the validation of user API requests. The request body must be smaller than approx. 5 MB (was 8 MB) and must be received within 10 s (was 15 s).
* Fine tunes the severity of the log message in case of unexpected validation result of user HTTP API request - from error to warning.
