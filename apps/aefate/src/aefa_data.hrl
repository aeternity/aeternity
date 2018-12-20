
-define(FATE_INTEGER_T, integer()).
-define(FATE_BYTE_T,    0..255).
-define(FATE_BOOLEAN_T, true | false).
-define(FATE_NIL_T,     []).
-define(FATE_LIST_T,    list()).
-define(FATE_UNIT_T,    {tuple, {}}).
-define(FATE_MAP_T,     #{ fate_type() => fate_type() }).
-define(FATE_STRING_T,  binary()).
-define(FATE_ADDRESS_T, {address, <<_:256>>}).
-define(FATE_VARIANT_T, {variant, ?FATE_BYTE_T, tuple()}).
-define(FATE_VOID_T,    void).
-define(FATE_TUPLE_T,   {tuple, tuple()}).

-define(IS_FATE_INTEGER(X), is_integer(X)).
-define(IS_FATE_LIST(X), (is_list(X))).
-define(IS_FATE_STRING(X), (is_binary(X))).
-define(IS_FATE_MAP(X), (is_map(X))).
-define(IS_FATE_TUPLE(X), (is_tuple(X) andalso (tuple == element(1, X) andalso is_tuple(element(2, X))))).
-define(IS_FATE_ADDRESS(X), (is_tuple(X) andalso (address == element(1, X) andalso is_binary(element(2, X))))).

-define(FATE_UNIT,       {tuple, {}}).
-define(FATE_TUPLE(T),   {tuple, T}).
-define(FATE_ADDRESS(A), {address, A}).
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
-define(FATE_VARIANT(Tag,T), {variant, Tag, T}).

-define(MAKE_FATE_INTEGER(X), X).
-define(MAKE_FATE_LIST(X), X).
-define(MAKE_FATE_MAP(X),  X).
-define(MAKE_FATE_STRING(X),  X).



