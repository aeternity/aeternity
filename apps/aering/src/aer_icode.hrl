
-type type() :: term().
-record(arg, {name::string(), type::type()}).

-type expr() :: term().
-type arg() :: #arg{name::string(), type::type()}.
-type arg_list() :: [arg()].

-record(fun_dec, { name :: string()
                 , args :: arg_list()
                 , body :: expr()}).

-record(var_ref, { name :: string()}).

