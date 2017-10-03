%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Help functions for running eunit tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_test_utils).

-export([ get_config/2
        , get_post_storage/1
        , is_external_available/0
        ]).

%%====================================================================
%% API functions
%%====================================================================

is_external_available() ->
    case external_dir() of
      false -> false;
      DirPath ->
        VMTests = filename:join([DirPath, "etherium_tests", "VMTests"]),
        filelib:is_dir(VMTests)
    end.

get_config(DirPath, TestName) ->
    Bin    = get_config_file(DirPath, TestName),
    Json   = jsx:decode(Bin, [return_maps, {labels, attempt_atom}]),
    Config = build_config(Json),
    maps:get(TestName, Config).

get_post_storage(#{post := Post}) ->
    %% TODO: The hash needs to be checked
    [{_, #{storage := Storage}}] = maps:to_list(Post),
    Storage.

%%====================================================================
%% Internal functions
%%====================================================================

external_dir() ->
    os:getenv("AEVM_EXTERNAL_TEST_DIR").

atom_or_binint_to_int(Bin) when is_binary(Bin) ->
    binint_to_int(Bin);
atom_or_binint_to_int(Atom) when is_atom(Atom) ->
    binint_to_int(atom_to_binary(Atom, utf8)).

to_atom(Atom) when is_atom(Atom) ->
    Atom;
to_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8).


binint_to_int(<<"0x", Bin/binary>>) ->
    binary_to_integer(Bin, 16);
binint_to_int(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin, 16).

binint_to_bin(<<"0x", Bin/binary>>) ->
    << <<(hex_to_int(X)):4>> || <<X:8>> <= Bin>>;
binint_to_bin(<<"0", _/binary>> = Bin) ->
    %% Don't know what to do.
    %% Is this an attempt to pad?
    error({unexpected, Bin});
binint_to_bin(Bin) when is_binary(Bin) ->
    Int = binary_to_integer(Bin),
    binary:encode_unsigned(Int).

hex_to_int(X) when $A =< X, X =< $F -> 10 + X - $A;
hex_to_int(X) when $a =< X, X =< $f -> 10 + X - $a;
hex_to_int(X) when $0 =< X, X =< $9 -> X - $0.


get_config_file(DirPath, TestName) ->
    FileName = config_filename(DirPath, TestName),
    case file:read_file(FileName) of
        {ok, Bin} -> Bin;
        Other -> error({could_not_read_file, TestName, FileName, Other})
    end.

config_filename(DirPath, TestName) ->
    filename:join([ external_dir()
                  , "etherium_tests"
                  , DirPath
                  , atom_to_list(TestName) ++ ".json"]).

build_config(ConfigIn) ->
    try build_config(config_structure(), ConfigIn)
    catch throw:{TagLine, X} -> error({TagLine, X, ConfigIn})
    end.

build_config(#{'$multiple_atom' := Body}, #{} = Spec) ->
    maps:from_list([{to_atom(X), build_config(Body, Y)}
                    || {X, Y} <- maps:to_list(Spec)]);
build_config(#{'$multiple_bin_int' := Body}, #{} = Spec) ->
    maps:from_list([{atom_or_binint_to_int(X), build_config_body(Body, Y)}
                    || {X, Y} <- maps:to_list(Spec)]);
build_config(#{} = Map, #{} = Spec) ->
    maps:from_list(build_config_lists(lists:sort(maps:to_list(Map)),
                                      lists:sort(maps:to_list(Spec))));
build_config([Body], [Spec|Left]) ->
    [build_config(Body, Spec)|build_config([Body], Left)];
build_config([_], []) ->
    [].

build_config_lists([{X, Body}|Left], [{X, Spec}|SpecLeft]) ->
    [{X, build_config_body(Body, Spec)}|build_config_lists(Left, SpecLeft)];
build_config_lists([], []) ->
    [];
build_config_lists([{X, _}|Left], Spec) ->
    case get_default_for_optional_config_field(X) of
        {ok, Default} -> [{X, Default}|build_config_lists(Left, Spec)];
        leave_out     -> build_config_lists(Left, Spec);
        error -> throw({missing, X})
    end;
build_config_lists([], [{X, _}|_]) ->
    throw({unknown, X}).

build_config_body(string, Bin) when is_binary(Bin) -> Bin;
build_config_body(data_array, Data) ->
    build_data_array(Data);
build_config_body(data_array_int, Data) ->
    binary:decode_unsigned(build_data_array(Data));
build_config_body(bin_int, Bin) when is_binary(Bin) ->
    binint_to_int(Bin);
build_config_body(#{} = Body, #{} = Spec) ->
    build_config(Body, Spec);
build_config_body([_] = Body, Spec) when is_list(Spec) ->
    build_config(Body, Spec);
build_config_body(unclear, X) ->
    X.


build_data_array(Data) ->
    build_data_array(Data, <<>>).

build_data_array(Int, Acc) when is_integer(Int), Int >= 0 ->
    <<Acc/binary, Int:64>>;
build_data_array(Bin, Acc) when is_binary(Bin) ->
    Tail = binint_to_bin(Bin),
    <<Acc/binary, Tail/binary >>;
build_data_array([Bin|T], Acc) when is_binary(Bin) ->
    New = binint_to_bin(Bin),
    build_data_array(T, <<Acc/binary, New/binary>>);
build_data_array([Int|T], Acc) when is_integer(Int) ->
    build_data_array(T, <<Acc/binary, Int:64>>);
build_data_array([], Acc) ->
    Acc.

-define(OPTIONAL_CONFIG_FIELDS, [ {callcreates, default, []}
                                , {gas, default, <<"0x00">>}
                                , {logs, leave_out}
                                , {out, leave_out}
                                , {post, leave_out}
                                ]).

get_default_for_optional_config_field(X) ->
    case lists:keyfind(X, 1, ?OPTIONAL_CONFIG_FIELDS) of
        {_, default, Default} -> {ok, Default};
        {_, leave_out} -> leave_out;
        false -> error
    end.

config_structure() ->
    #{'$multiple_atom' =>
          #{ '_info' => #{ comment => string
                         , filledwith => string
                         , source => string
                         }
           , callcreates => [#{ data => data_array
                              , destination => bin_int
                              , gaslimit => bin_int
                              , value => bin_int
                              }
                            ]
           , env => #{ currentCoinbase => bin_int
                     , currentDifficulty => bin_int
                     , currentGasLimit => bin_int
                     , currentNumber => bin_int
                     , currentTimestamp => bin_int
                     }
           , exec => #{ address => bin_int
                      , caller => bin_int
                      , code => data_array
                      , data => data_array
                      , gas => bin_int
                      , gasPrice => bin_int
                      , origin => bin_int
                      , value => bin_int}
           , gas => bin_int
           , logs => unclear
           , out => data_array
           , post => #{'$multiple_bin_int' =>
                           #{ balance => bin_int
                            , code => data_array
                            , nonce => bin_int
                            , storage =>
                                  #{'$multiple_bin_int' => data_array_int}
                            }
                      }
           , pre => #{'$multiple_bin_int' =>
                          #{ balance => bin_int
                           , code => data_array
                           , nonce => bin_int
                           , storage =>
                                 #{'$multiple_bin_int' => data_array_int}
                           }
                     }
           }
     }.
