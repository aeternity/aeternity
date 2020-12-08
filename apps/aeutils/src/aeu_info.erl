-module(aeu_info).

-export([get_version/0,
         get_revision/0,
         get_os/0]).

-define(VERSION_FILE, <<"VERSION">>).
-define(REVISION_FILE, <<"REVISION">>).

get_version() ->
    read_trimmed_file(?VERSION_FILE).

get_revision() ->
    read_trimmed_file(?REVISION_FILE).

get_os() ->
    Bin = fun(A) when is_atom(A) -> atom_to_binary(A, utf8);
              (I) when is_integer(I) -> integer_to_binary(I)
          end,
    {OSFamily, OSName} = os:type(),
    BOSFamily = Bin(OSFamily),
    BOSName = Bin(OSName),
    OSVsnBin =
        case os:version() of
            {V1, V2, V3} ->
                BV1 = Bin(V1),
                BV2 = Bin(V2),
                BV3 = Bin(V3),
                <<BV1/binary,".",BV2/binary,".",BV3/binary>>;
            Str -> list_to_binary(Str)
        end,
    <<BOSFamily/binary, ":", BOSName/binary, "-", OSVsnBin/binary>>.

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
