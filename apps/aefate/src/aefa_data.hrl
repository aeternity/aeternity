%% Common definitions.

-define(FATE_UNIT,       {tuple, {}}).
-define(FATE_TUPLE(T),   {tuple, T}).
-define(FATE_ADDRESS(A), {address, A}).

-ifdef(CHECKFATEDATA).

%% Types
-define(FATE_INTEGER_T, {integer,  integer()}).
-define(FATE_BYTE_T,    {integer,  0..255}).
-define(FATE_BOOLEAN_T, {boolean, true | false}).
-define(FATE_NIL_T,     {nil}).
-define(FATE_LIST_T(T), {list,    list(A::T) | ?FATE_NIL_T}).
-define(FATE_UNIT_T,    {tuple,   {}}).
-define(FATE_MAP_T,     {map,     #{ fate_none_map_type() => fate_type() }}).
-define(FATE_STRING_T,  {string,  binary()}).
-define(FATE_ADDRESS_T, {address, <<_:256>>}).
-define(FATE_VARIANT_T(T), {variant, ?FATE_BYTE_T, fate_tuple(T)}).
-define(FATE_VOID_T,    {void}).

%% Values
-define(FATE_TRUE,    {boolean, true}).
-define(FATE_FALSE,   {boolean, false}).
-define(FATE_NIL,     {nil}).
-define(FATE_VOID,    {void}).
-define(FATE_EMPTY_STRING, {string, <<>>}).
-define(FATE_STRING(S),    {string, S}).

-define(IS_FATE_INTEGER(X), (is_tuple(X) andalso (integer == element(1, X)))).
-define(IS_FATE_LIST(X), (is_tuple(X) andalso (list == element(1, X)))).
-define(IS_FATE_STRING(X), (is_tuple(X) andalso (string == element(1, X)))).
-define(IS_FATE_MAP(X), (is_tuple(X) andalso (map == element(1, X)))).
-define(FATE_GET(T, X), (begin {T, _} = X, element(2, X) end) ).
-define(FATE_INTEGER_VALUE(X), ?FATE_GET(integer, X)).
-define(FATE_LIST_VALUE(X), ?FATE_GET(list, X)).
-define(FATE_MAP_VALUE(X), ?FATE_GET(map, X)).
-define(FATE_STRING_VALUE(X), ?FATE_GET(string, X)).
-define(FATE_MAP_SIZE(X), (is_tuple(X) andalso (map == element(1, X)) andalso map_size(element(2, X)))).
-define(FATE_STRING_SIZE(X), (is_tuple(X) andalso (string == element(1, X)) andalso (byte_size(element(2, X))))).

-define(MAKE_FATE_INTEGER(X), {integer, X}).
-define(MAKE_FATE_LIST(X),    {list, X}).
-define(MAKE_FATE_MAP(X),     {map, X}).
-define(MAKE_FATE_STRING(X),  {string, X}).


-else. %% No extra tagging on Erlang values.

-define(FATE_INTEGER_T, integer()).
-define(FATE_BYTE_T,    0..255).
-define(FATE_BOOLEAN_T, true | false).
-define(FATE_NIL_T,     []).
-define(FATE_LIST_T(T), list(A::T) | fate_nil()).
-define(FATE_UNIT_T,    {tuple, {}}).
-define(FATE_MAP_T,     #{ fate_none_map_type() => fate_type() }).
-define(FATE_STRING_T,  binary()).
-define(FATE_ADDRESS_T, {address, <<_:256>>}).
-define(FATE_VARIANT_T(T), {variant, ?FATE_BYTE_T, fate_tuple(T)}).
-define(FATE_VOID_T,    void).

-define(IS_FATE_INTEGER(X), is_integer(X)).
-define(IS_FATE_LIST(X), (is_list(X))).
-define(IS_FATE_STRING(X), (is_binary(X))).
-define(IS_FATE_MAP(X), (is_map(X))).

-define(FATE_INTEGER_VALUE(X), (X)).
-define(FATE_LIST_VALUE(X), (X)).
-define(FATE_STRING_VALUE(X), (X)).
-define(FATE_MAP_VALUE(X), (X)).
-define(FATE_MAP_SIZE(X), (map_size(X))).
-define(FATE_STRING_SIZE(X), (byte_size(X))).
-define(FATE_TRUE,  true).
-define(FATE_FALSE, false).
-define(FATE_NIL,   []).
-define(FATE_VOID,  void).
-define(FATE_EMPTY_STRING, <<>>).
-define(FATE_STRING(S), S).

-define(MAKE_FATE_INTEGER(X), X).
-define(MAKE_FATE_LIST(X), X).
-define(MAKE_FATE_MAP(X),  X).
-define(MAKE_FATE_STRING(X),  X).

-endif.

-define(FATE_VARIANT(Tag,T), {variant, ?MAKE_FATE_INTEGER(Tag), ?FATE_TUPLE(T)}).
-define(IS_FATE_TUPLE(X), (is_tuple(X) andalso (tuple == element(1, X) andalso is_tuple(element(2, X))))).
-define(IS_FATE_ADDRESS(X), (is_tuple(X) andalso (address == element(1, X) andalso is_binary(element(2, X))))).
-define(FATE_TUPLE_T(T), {tuple, T}). %% Can this be expresses more precise?

