-module(aec_tx_pool_failures).

-export([limit/2,
         settings/0]).

-ifdef(TEST).
-export([set/1]).
-endif.

-define(ENABLED, <<"enabled">>).
-define(COMMON, <<"common">>).
-define(DEFAULT, <<"fallback">>).
-define(DEFAULT_DEFAULT_VALUE, 20).

-define(ETS_TABLE, ?MODULE).
-define(ETS_KEY, settings).

-spec limit(aetx_sign:signed_tx(), atom()) -> {ok, non_neg_integer()} | no_limit.
limit(SignedTx, Error) ->
    {Type, _Tx} = aetx:specialize_type(aetx_sign:tx(SignedTx)),
    Settings = settings(),
    IsEnabled = maps:get(?ENABLED, Settings, true),
    case IsEnabled of
        false -> no_limit;
        true ->
            ErrorName = error_to_setting_name(Error),
            TxType = tx_type_to_setting_name(Type),
            GeneralSettings = maps:get(?COMMON, Settings, #{}),
            TxSettings = maps:merge(GeneralSettings, maps:get(TxType, Settings, #{})),
            {ok, _R} = lookup_setting(TxSettings, ErrorName)
    end.

settings() ->
    try
        [{?ETS_KEY, Settings}] = ets:lookup(?ETS_TABLE, ?ETS_KEY),
        Settings
    catch
        _:_ ->
            ensure_table(),
            S = load_settings(),
            true = ets:insert(?ETS_TABLE, {?ETS_KEY, S}),
            S
    end.

lookup_setting(Map, Key) ->
    case maps:find(Key, Map) of
        {ok, Val} -> {ok, Val};
        error -> maps:find(?DEFAULT, Map)
    end.

tx_type_to_setting_name(TxNameAtom) -> atom_to_binary(TxNameAtom, utf8).

error_to_setting_name(Event) when is_atom(Event) ->
    atom_to_binary(Event, utf8);
error_to_setting_name(Event) when is_tuple(Event) ->
    Reason =
        case element(1, Event) of
            bad_label         -> invalid_name;
            invalid_codepoint -> invalid_name;
            Other             -> Other
        end,
    atom_to_binary(Reason, utf8);
error_to_setting_name(Event) ->
    lager:error("?p:error_to_setting_name(~p)", [?MODULE, Event]),
    <<"unknown_error">>.

load_settings() ->
    Settings0 =
        case aeu_env:find_config([<<"mempool">>, <<"tx_failures">>],
                                          [user_config]) of
            {ok, S} -> S;
            undefined -> #{}
        end,
    {ok, Defaults} = aeu_env:schema_default_values([<<"mempool">>, <<"tx_failures">>]),
    MapFold =
        fun(MergeFun, M1, M2) ->
            maps:fold(
                fun(K, V1, Acc) ->
                    Val =
                        case maps:find(K, Acc) of
                            {ok, V2} -> MergeFun(V1, V2);
                            error -> V1
                        end,
                    maps:put(K, Val, Acc)
                end,
                M2, M1) %% M2 overwrites M1
        end,
    DeepMerge =
        fun Merge(V1, V2) when is_map(V2) ->
                  MapFold(Merge, V1, V2);
            Merge(_V1, V2) -> V2
        end,
    Settings = MapFold(DeepMerge, Defaults, Settings0),
    %% ensure there is a default
    case lookup_setting(maps:get(?COMMON, Settings, #{}), ?DEFAULT) of
        error ->
            maps:put(?COMMON,
                     maps:put(?DEFAULT, ?DEFAULT_DEFAULT_VALUE, maps:get(?COMMON, Settings, #{})),
                     Settings);
        {ok, _} -> Settings
    end.

ensure_table() ->
    case ets:whereis(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [set, named_table, {read_concurrency, false}, public]);
        _ -> pass
    end.

-ifdef(TEST).
set(S) ->
    ensure_table(),
    true = ets:insert(?ETS_TABLE, {?ETS_KEY, S}).
-endif.
