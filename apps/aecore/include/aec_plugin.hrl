
-define(PLUGGABLE(Args, Default),
        case aec_plugin:get_module(?MODULE) of
            undefined ->
                Default;
            __M ->
                case erlang:function_exported(
                       __M, ?FUNCTION_NAME, ?FUNCTION_ARITY) of
                    true ->
                        __M:?FUNCTION_NAME Args;
                    false ->
                        Default
                end
        end).
