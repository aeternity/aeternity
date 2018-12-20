%% First draft of FATE data representation.
%% Very likely to change.
%%
-include("aefa_data.hrl").

-module(aefa_data).

-type fate_integer() :: ?FATE_INTEGER_T.
-type fate_boolean() :: ?FATE_BOOLEAN_T.
-type fate_nil()     :: ?FATE_NIL_T.
-type fate_list()    :: ?FATE_LIST_T.
-type fate_unit()    :: ?FATE_UNIT_T.
-type fate_map()     :: ?FATE_MAP_T.
-type fate_string()  :: ?FATE_STRING_T.
-type fate_address() :: ?FATE_ADDRESS_T.
-type fate_variant() :: ?FATE_VARIANT_T.

-type fate_void()    :: ?FATE_VOID_T.

-type fate_tuple()   :: ?FATE_TUPLE_T.

-type fate_type() ::
        fate_boolean()
      | fate_integer()
      | fate_nil()
      | fate_list()
      | fate_unit()
      | fate_tuple()
      | fate_string()
      | fate_address()
      | fate_variant()
      | fate_map()
      | fate_list()
      | fate_tuple()
      | fate_variant()
      | fate_void(). %% Not sure we need this.

-export_type([fate_type/0]).
-export([ make_integer/1
        , make_boolean/1
        , make_list/1
        , make_variant/2
        , make_tuple/1
        , make_string/1
        , make_map/1
        , make_address/1
        , make_unit/0
        ]).
-export([format/1]).


make_integer(I) when is_integer(I) -> ?MAKE_FATE_INTEGER(I).
make_boolean(true)  -> ?FATE_TRUE;
make_boolean(false) -> ?FATE_FALSE.
make_list([]) -> ?FATE_NIL;
make_list(L) -> ?MAKE_FATE_LIST(L).
make_string(S) when is_list(S) ->
    ?FATE_STRING(list_to_binary(lists:flatten(S)));
make_string(S) when is_binary(S) -> ?FATE_STRING(S).
make_unit() -> ?FATE_UNIT.
make_tuple(T) -> ?FATE_TUPLE(T).
make_map(M) -> ?MAKE_FATE_MAP(M).
make_address(A) -> ?FATE_ADDRESS(A).

make_variant(Tag, Values) when is_integer(Tag)
                               , 0 =< Tag
                               , Tag < 256
                               , is_tuple(Values) ->
    ?FATE_VARIANT(Tag, Values).

-spec format(fate_type()) -> iolist().
format(I) when ?IS_FATE_INTEGER(I) -> integer_to_list(?MAKE_FATE_INTEGER(I));
format(?FATE_VOID) -> "void";
format(?FATE_TRUE) -> "true";
format(?FATE_FALSE) -> "false";
format(?FATE_NIL) -> "[]";
format(L) when ?IS_FATE_LIST(L) -> format_list(?FATE_LIST_VALUE(L));
format(?FATE_UNIT) -> "unit";
format(?FATE_TUPLE(T)) ->
    "{ " ++ [format(E) ++ " " || E <- tuple_to_list(T)] ++ "}";
format(S) when ?IS_FATE_STRING(S) -> [S];
format(?FATE_VARIANT(Tag, T)) ->
    "( " ++ integer_to_list(Tag) ++ ", "
        ++ [format(E) ++ " " || E <- tuple_to_list(T)]
        ++ " )";
format(M) when ?IS_FATE_MAP(M) ->
    "#{ "
        ++ format_kvs(maps:to_list(?FATE_MAP_VALUE(M)))
        ++" }";
format(?FATE_ADDRESS(Address)) -> base58:binary_to_base58(Address);
format(V) -> exit({not_a_fate_type, V}).

format_list([]) -> " ]";
format_list([E]) -> format(E) ++ " ]";
format_list([H|T]) -> format(H) ++ ", " ++ format_list(T).

format_kvs([]) -> "";
format_kvs([{K,V}]) -> "( " ++ format(K) ++ " => " ++ format(V) ++ " )";
format_kvs([{K,V} | Rest]) ->
    "( " ++ format(K) ++ " => " ++  format(V) ++ " ), " ++ format_kvs(Rest).
