%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Help functions for running eunit tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_test_utils).

-export([ get_config/1
        , get_post_storage/1
        , get_eeevm_run_config/1
        , get_eeevm_run_config/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

get_config(TestName) ->
    Bin = get_config_file(TestName),
    Res = jsx:decode(Bin, [return_maps]),
    validate_config_format(Res),
    maps:get(atom_to_binary(TestName, latin1), Res).

get_eeevm_run_config(SpecIn) ->
    get_eeevm_run_config(SpecIn, #{}).

get_eeevm_run_config(SpecIn, Extra) ->
    #{<<"exec">> := #{ <<"code">> := Code
                     , <<"gas">>  := Gas}} = SpecIn,
    Extra#{ code => binhex_to_bin(Code)
          , gas  => binhex_to_int(Gas)
          }.

get_post_storage(#{<<"post">> := Post} =_SpecIn) ->
    [{_Hash, #{<<"storage">> := Storage}}] = maps:to_list(Post),
    maps:from_list([{binhex_to_int(Pos), binhex_to_int(Val)}
                    || {Pos, Val} <- maps:to_list(Storage)]).

%%====================================================================
%% Internal functions
%%====================================================================

binhex_to_int(<<"0x", Bin/binary>>) ->
  binary_to_integer(Bin, 16).

binhex_to_bin(<<"0x", Bin/binary>>) ->
  << <<(hex_to_int(X)):4>> || <<X:8>> <= Bin>>.

hex_to_int(X) when $A =< X, X =< $F -> 10 + X - $A;
hex_to_int(X) when $a =< X, X =< $f -> 10 + X - $a;
hex_to_int(X) when $0 =< X, X =< $9 -> X - $0.


get_config_file(TestName) ->
    FileName = config_filename(TestName),
    case file:read_file(FileName) of
        {ok, Bin} -> Bin;
        Other -> error({could_not_read_file, TestName, FileName, Other})
    end.


config_filename(TestName) ->
    LibDir = code:lib_dir(aevm),
    filename:join([LibDir, "test", "configs", atom_to_list(TestName) ++ ".json"]).

-define(CONFIG_FIELDS, [ <<"_info">>
                       , <<"callcreates">>
                       , <<"env">>
                       , <<"exec">>
                       , <<"gas">>
                       , <<"logs">>
                       , <<"out">>
                       , <<"post">>
                       , <<"pre">>
                       ]).

validate_config_format(Config) ->
    maps:map(fun(_TestCase, Data) -> validate_testcase_config_format(Data) end, Config).

validate_testcase_config_format(Data) ->
    Keys = maps:keys(Data),
    case Keys -- ?CONFIG_FIELDS of
        [] -> ok;
        Illegal -> error({illegal_config_fields, Illegal})
    end,
    case ?CONFIG_FIELDS -- Keys of
        [] -> ok;
        Missing -> error({missing_config_fields, Missing})
    end.
