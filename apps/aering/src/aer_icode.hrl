
-type type() :: term().
-record(arg, {name::string(), type::type()}).

-type expr() :: term().
-type arg() :: #arg{name::string(), type::type()}.
-type arg_list() :: [arg()].

-record(fun_dec, { name :: string()
                 , args :: arg_list()
                 , body :: expr()}).

-record(var_ref, { name :: string()}).

-record(integer, {value :: integer()}).

-record(tuple,   {cpts  :: [expr()]}).

-record(list,    {elems :: [expr()]}).

-record(unop,    { op   :: term()
		 , rand :: expr()}).

-record(binop,   { op   :: term()
		 , left :: expr()
		 , right :: expr()}).

-record(ifte,    { decision :: expr()
		 , then :: expr()
		 , else :: expr()}).

-record(switch,  { expr  :: expr()
		 , cases :: [{expr(),expr()}]}).

-record(funcall, { function :: expr()
		 , args     :: [expr()]}).

-record(lambda,  { args :: arg_list(),
		   body :: expr()}).

-record(missing_field, { format :: string()
		       , args   :: [term()]}).
