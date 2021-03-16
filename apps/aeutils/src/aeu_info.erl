-module(aeu_info).

-export([get_version/0,
         get_revision/0,
         get_os/0,
         vendor/0]).

-define(VERSION_FILE, <<"VERSION">>).
-define(REVISION_FILE, <<"REVISION">>).

-define(ETS_TABLE, aeu_info_file_cache).

get_version() ->
    cached_file(?VERSION_FILE).

get_revision() ->
    cached_file(?REVISION_FILE).

get_os() ->
    Bin = fun(A) when is_atom(A)    -> atom_to_binary(A, utf8)
          end,
    {OSFamily, OSName} = os:type(),
    BOSFamily = Bin(OSFamily),
    BOSName = Bin(OSName),
    <<BOSFamily/binary, ":", BOSName/binary>>.

vendor() ->
    <<"Aeternity reference node">>.

%% Internals

read_trimmed_file(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            <<>>;
        {ok, Content} ->
            trim_ending_whitespace(Content)
    end.

trim_ending_whitespace(Binary) ->
    re:replace(Binary, "\\s+$", "", [{return, binary}, global]).

cached_file(Filename) ->
    EtsKey = {file, Filename},
    try
        [{EtsKey, Data}] = ets:lookup(?ETS_TABLE, EtsKey),
        Data
    catch
        _:_ ->
            case ets:whereis(?ETS_TABLE) of
                undefined ->
                    ets:new(?ETS_TABLE, [named_table, {read_concurrency, true}, public]);
                _ -> pass
            end,
            ReadData = read_trimmed_file(Filename),
            ets:insert(?ETS_TABLE, {EtsKey, ReadData}),
            ReadData
    end.

