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
        , make_bits/1
        , make_unit/0
        , tuple_to_list/1
        , decode/1
        , encode/1
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
make_bits(I) when is_integer(I) -> ?FATE_BITS(I).

make_variant(Tag, Values) when is_integer(Tag)
                               , 0 =< Tag
                               , Tag < 256
                               , is_tuple(Values) ->
    ?FATE_VARIANT(Tag, Values).

tuple_to_list(?FATE_TUPLE(T)) -> erlang:tuple_to_list(T).

%% Encode is a convinience function for testing, encoding an Erlang term
%% to a Fate term, but it can not distinguish between e.g. 32-byte strings
%% and addresses. Use the make_X functions instead.
encode({bits, Term}) when is_integer(Term) -> make_bits(Term);
encode(Term) when is_integer(Term) -> make_integer(Term);
encode(Term) when is_boolean(Term) -> make_boolean(Term);
encode(Term) when is_list(Term) -> make_list([encode(E) || E <- Term]);
encode(Term) when is_tuple(Term) ->
    make_tuple(list_to_tuple([encode(E) || E <- erlang:tuple_to_list(Term)]));
encode(Term) when is_map(Term) ->
    make_map(maps:from_list([{encode(K), encode(V)} || {K,V} <- maps:to_list(Term)]));
%% TODO: this breaks down for 32 bytes strings.
encode(Term) when is_binary(Term), size(Term) =:= 32 -> make_address(Term);
encode(Term) when is_binary(Term) -> make_string(Term).



decode(I) when ?IS_FATE_INTEGER(I) -> I;
decode(?FATE_TRUE)  -> true;
decode(?FATE_FALSE) -> false;
decode(L) when ?IS_FATE_LIST(L) -> [decode(E) || E <- L];
decode(?FATE_TUPLE(T)) -> erlang:list_to_tuple([decode(E) || E <- T]);
decode(S) when ?IS_FATE_STRING(S) -> binary_to_list(S);
decode(M) when ?IS_FATE_MAP(M) ->
    maps:from_list([{decode(K), decode(V)} || {K, V} <- maps:to_list(M)]);
decode(?FATE_ADDRESS(Address)) -> Address;
decode(?FATE_BITS(Bits)) -> {bits, Bits}.

-spec format(fate_type()) -> iolist().
format(I) when ?IS_FATE_INTEGER(I) -> integer_to_list(?MAKE_FATE_INTEGER(I));
format(?FATE_VOID) -> "void";
format(?FATE_TRUE) -> "true";
format(?FATE_FALSE) -> "false";
format(?FATE_NIL) -> "[]";
format(L) when ?IS_FATE_LIST(L) -> format_list(?FATE_LIST_VALUE(L));
format(?FATE_UNIT) -> "unit";
format(?FATE_TUPLE(T)) ->
    "{ " ++ [format(E) ++ " " || E <- erlang:tuple_to_list(T)] ++ "}";
format(S) when ?IS_FATE_STRING(S) -> [S];
format(?FATE_VARIANT(Tag, T)) ->
    "( " ++ integer_to_list(Tag) ++ ", "
        ++ [format(E) ++ " " || E <- erlang:tuple_to_list(T)]
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
