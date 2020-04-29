-module(sc_ws_utils).

-export([ check_params/1
        , check_params/2
        , read_f/1
        , readmap_f/1
        , put_f/0
        , read_param/3
        , check_type/3 ]).

-export([patterns/0]).

%%%==================================================================
%%% Trace settings
%%%==================================================================

patterns() ->
    [{?MODULE, F, A, []} || {F, A} <- [ {read_param, 3}
                                      , {check_type, 3}
                                      ]].


check_params(Checks) ->
    check_params(Checks, #{}).

check_params(Checks, Acc) ->
    lists:foldl(
      fun(_, {error, _} = Err) -> Err;
         (Fun, Acc1) ->
              Fun(Acc1)
      end, Acc, Checks).

read_f(Params) ->
    fun(KeyBin, Key, Opts) ->
            fun(M) ->
                    case (read_param(KeyBin, Key, Opts))(Params) of
                        not_set -> M;
                        {ok, Val} -> maps:put(Key, Val, M);
                        {error, _} = Err -> Err
                    end
            end
    end.

readmap_f(Params) ->
    fun(MapName, Prefix, Opts) ->
            fun(Name) ->
                    NameBin = atom_to_binary(Name, utf8),
                    Key = <<Prefix/binary, "_", NameBin/binary>>,
                    fun(M) ->
                            OldVal = maps:get(MapName, M, #{}),
                            case (read_param(Key, Name, Opts))(Params) of
                                not_set -> M;
                                {ok, Val} -> maps:put(MapName, maps:put(Name, Val, OldVal), M);
                                {error, _} = Err -> Err
                            end
                    end
            end
    end.

put_f() ->
    fun(K, V) ->
            fun(M) -> maps:put(K, V, M) end
    end.

-spec read_param(binary(), atom(), map()) -> fun((map()) -> {ok, term()} |
                                                            not_set |
                                                            {error, {atom(), atom()}}).
read_param(ParamName, RecordField, Options) ->
    fun(Params) ->
        Mandatory = maps:get(mandatory, Options, true),
        case maps:get(ParamName, Params, undefined) of
            undefined when Mandatory ->
                {error, {RecordField, missing}};
            undefined when not Mandatory ->
                case maps:find(default, Options) of
                    {ok, Default} ->
                        check_type(Options, Default, RecordField);
                    error ->
                        not_set
                end;
            Val ->
                check_type(Options, Val, RecordField)
        end
    end.

check_type(Options, Val0, RecordField) ->
    Type = maps:get(type, Options, binary),
    try parse_by_type(Type, Val0, RecordField) of
        {error, _} = Err -> Err;
        {ok, Val} ->
            case maps:get(enum, Options, undefined) of
                undefined ->  {ok, Val};
                AllowedVals when is_list(AllowedVals) ->
                    case lists:member(Val, AllowedVals) of
                        true -> {ok, Val};
                        false ->
                            {error, {RecordField, invalid}}
                    end
            end
    catch
        error:_ -> {error, {RecordField, invalid}}
    end.

parse_by_type(binary, V, _) when is_binary(V) ->
    {ok, V};
parse_by_type(boolean, V, _) when is_binary(V) ->
    case V of
        <<"true">>  -> {ok, true};
        <<"false">> -> {ok, false};
        _           -> {error, not_bool}
    end;
parse_by_type(string, V, _) when is_binary(V) ->
    {ok, binary_to_list(V)};
parse_by_type(atom, V, _) when is_binary(V) ->
    {ok, binary_to_existing_atom(V, utf8)};
parse_by_type(integer, V, _) when is_binary(V) ->
    {ok, list_to_integer(binary_to_list(V))};
parse_by_type(integer, V, _) when is_integer(V) ->
    {ok, V};
parse_by_type({hash, Type}, V, RecordField) when is_binary(V) ->
    case aeser_api_encoder:safe_decode(Type, V) of
        {error, _} ->
            {error, {RecordField, broken_encoding}};
        {ok, _} = OK -> OK
    end;
parse_by_type(fsm_id, V, _RecordField) when is_binary(V) ->
    case aeser_api_encoder:safe_decode(bytearray, V) of
        {error, _} ->
            %% This also includes the broken_encoding message
            {error, invalid_fsm_id};
        {ok, Result} when byte_size(Result) /= 32 ->
            {error, invalid_fsm_id};
        {ok, Result} ->
            {ok, aesc_fsm_id:from_binary(Result)}
    end;
parse_by_type({alt, L}, V, RecordField) when is_list(L) ->
    lists:foldl(
      fun(_, {ok,_} = Ok) -> Ok;
         (Alt, Acc) when is_map(Alt) ->
              case check_type(Alt, V, RecordField) of
                  {ok, _} = Ok -> Ok;
                  {error,_} -> Acc
              end
      end, {error, {RecordField, invalid}}, L);
parse_by_type(serialized_tx, V, RecordField) when is_binary(V) ->
    case aeser_api_encoder:safe_decode(transaction, V) of
        {ok, TxBin} ->
            try {ok, aetx_sign:deserialize_from_binary(TxBin)}
            catch
                error:_ ->
                    {error, {RecordField, invalid_tx_serialization}}
            end;
        {error, _} ->
            {error, {RecordField, broken_encoding}}
    end;
parse_by_type({list, Opts}, V, RecordField) when is_list(V) ->
    ThrowTag = {?MODULE, ?LINE},
    try V1 = lists:map(
               fun(X) ->
                       case check_type(Opts, X, RecordField) of
                           {ok, X1} ->
                               X1;
                           {error, _} = Err ->
                               throw({ThrowTag, Err})
                       end
               end, V),
         {ok, V1}
    catch
        throw:{ThrowTag, Err} ->
            Err
    end.
