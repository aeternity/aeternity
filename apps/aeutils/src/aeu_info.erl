-module(aeu_info).

-export([get_version/0,
         get_revision/0,
         get_os/0,
         vendor/0]).

-define(VERSION_FILE, <<"VERSION">>).
-define(REVISION_FILE, <<"REVISION">>).

get_version() ->
    read_trimmed_file(?VERSION_FILE).

get_revision() ->
    read_trimmed_file(?REVISION_FILE).

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
