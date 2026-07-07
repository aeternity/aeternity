%%%-------------------------------------------------------------------
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    CI gate on apps/aefate/src/aefa_stores_ceres.erl: it is the frozen
%%%    Iris..Ceres FATE-store replay reference, so any edit to it changes
%%%    replay of already-forked blocks. Compares a normalized copy of the
%%%    file against a committed golden sha256, rather than diffing against
%%%    the live aefa_stores.erl (which is expected to diverge from Salus
%%%    onward). Normalization strips the `-module(...).` line and any
%%%    `%% FROZEN ...` banner; everything else must match byte-for-byte.
%%%    Re-baselining (only for a legitimately new frozen protocol version)
%%%    needs a new golden regenerated via normalize/1 + hex_sha256/1 below.
%%%    See check_stores_ceres_freeze.sh for the shell-equivalent CI check.
%%% @end
%%%-------------------------------------------------------------------
-module(aefa_stores_ceres_freeze_test).

-include_lib("eunit/include/eunit.hrl").

-export([ normalize/1
        , hex_sha256/1
        , ceres_src_path/0
        ]).

%% Golden: sha256 (hex) of the frozen aefa_stores_ceres.erl, normalized.
-define(FREEZE_GOLDEN_SHA256,
        <<"6aef651ad2dd45f83342ea07f87ce2f1f82d95a1b6c8411674cfaf4b8ab42f93">>).

freeze_equivalence_test() ->
    {ok, CeresBin} = file:read_file(ceres_src_path()),
    ActualHash = hex_sha256(normalize(CeresBin)),
    ?assertEqual(
       {aefa_stores_ceres_unchanged_since_freeze, ?FREEZE_GOLDEN_SHA256},
       {aefa_stores_ceres_unchanged_since_freeze, ActualHash}).

%% -- helpers (exported for re-baselining / the shell CI-check twin) --

-spec ceres_src_path() -> file:filename().
ceres_src_path() ->
    filename:join(code:lib_dir(aefate), "src/aefa_stores_ceres.erl").

%% Strips the `-module(...).` line and any `%% FROZEN ...` banner line;
%% keeps everything else, including blank lines, as-is.
-spec normalize(binary()) -> binary().
normalize(Bin) when is_binary(Bin) ->
    Lines = binary:split(Bin, [<<"\n">>], [global]),
    Kept = [L || L <- Lines,
                 not is_module_line(L),
                 not is_frozen_banner_line(L)],
    iolist_to_binary(lists:join(<<"\n">>, Kept)).

is_module_line(Line) ->
    match(re:run(Line, "^-module\\(.+\\)\\.\\s*$")).

is_frozen_banner_line(Line) ->
    match(re:run(Line, "^\\s*%% FROZEN")).

match({match, _}) -> true;
match(nomatch)    -> false.

-spec hex_sha256(binary()) -> binary().
hex_sha256(Bin) ->
    <<N:256/big-unsigned-integer>> = crypto:hash(sha256, Bin),
    list_to_binary(io_lib:format("~64.16.0b", [N])).
