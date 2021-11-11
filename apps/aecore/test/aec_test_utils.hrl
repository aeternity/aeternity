-define(LOG(D, Fmt, Args), aec_test_utils:log(Fmt, Args, ?LINE, D)).
-define(LOG(Fmt, Args), aec_test_utils:log(Fmt, Args, ?LINE, true)).
              
