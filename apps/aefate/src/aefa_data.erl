%% First draft of FATE data representation.
%% Very likely to change.
%%

-module(aefa_data).

-type fate_integer() :: integer().
-type fate_boolean() :: true | false.
-type fate_nil() :: [].
-type fate_list() :: list(A::fate_type()) | fate_nil().
-type fate_zero_tuple() :: {tuple, {}}.
-type fate_tuple() :: {tuple, T::tuple()}. %% Can this be expresses more precise?
-type fate_map() :: #{ A::fate_none_map_type() => B::fate_type()}.
-type fate_string() :: binary().
-type fate_address() :: {address, <<_:256>>}.
-type fate_variant() :: {variant, fate_integer(), fate_tuple()}.

-type fate_void() :: void.

-type fate_none_map_type() ::
        fate_boolean()
      | fate_integer()
      | fate_nil()
      | fate_list()
      | fate_zero_tuple()
      | fate_tuple()
      | fate_string()
      | fate_address()
      | fate_variant().

-type fate_type() :: fate_none_map_type() | fate_map() | fate_void().

-export_type([fate_type/0]).
-export([format/1]).


-spec format(fate_type()) -> iolist().
format(I) when is_integer(I) ->
    integer_to_list(I);
format(true) -> "true";
format(false) -> "false";
format([]) -> "[]";
format([E]) -> "[ " ++ format(E) ++ " ]";
format(L) when is_list(L) -> "[ " ++ format_list(L);
format({tuple, {}}) -> "unit";
format({tuple, T}) -> "{ " ++ [format(E) ++ " " || E <- tuple_to_list(T)] ++ "}";
format(S) when is_binary(S) -> S;
format({variant, Tag, T}) -> "( " ++ format(Tag) ++ ", " ++ format(tuple_to_list(T)) ++ " )";
format(#{} = M) -> "#{ " ++ format_kvs(maps:to_list(M)) ++" }";
format(V) -> exit({not_a_fate_type, V}).

format_list([]) -> " ]";
format_list([E]) -> format(E) ++ " ]";
format_list([H|T]) -> format(H) ++ ", " ++ format_list(T).

format_kvs([]) -> "";
format_kvs([{K,V}]) -> "( " ++ format(K) ++ format(V) ++ " )";
format_kvs([{K,V} | Rest]) -> "( " ++ format(K) ++ format(V) ++ " ), " ++ format_kvs(Rest).
