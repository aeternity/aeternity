-module(aeu_info).

-export([get_version/0,
         get_revision/0]).

-define(VERSION_FILE, <<"VERSION">>).
-define(REVISION_FILE, <<"REVISION">>).

get_version() ->
    read_trimmed_file(?VERSION_FILE).

get_revision() ->
    read_trimmed_file(?REVISION_FILE).

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
