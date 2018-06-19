
-type type() :: word | string | typerep | function
              | {list, type()}
              | {option, type()}
              | {tuple, [type()]}.

-define(TYPEREP_WORD_TAG,   0).
-define(TYPEREP_STRING_TAG, 1).
-define(TYPEREP_LIST_TAG,   2).
-define(TYPEREP_TUPLE_TAG,  3).
-define(TYPEREP_OPTION_TAG, 4).

-record(arg, {name::string(), type::type()}).

-type expr() :: term().
-type arg() :: #arg{name::string(), type::type()}.
-type arg_list() :: [arg()].

-record(fun_dec, { name :: string()
                 , args :: arg_list()
                 , body :: expr()}).

-record(var_ref, { name :: string() | {builtin, atom() | tuple()}}).

-record(prim_call_contract,
    { gas      :: expr()
    , address  :: expr()
    , value    :: expr()
    , arg      :: expr()
    , arg_type :: type()
    , out_type :: type() }).

-record(prim_balance,    { address :: expr() }).
-record(prim_block_hash, { height :: expr() }).
-record(prim_put,        { state :: expr() }).

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
