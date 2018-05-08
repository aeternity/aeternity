%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Help functions for running eunit tests for aevm
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_test_utils).

-export([ testcase_generate/2
        , testcase_generate/3
        ]).

-define(EUNIT_NOAUTO, true).
-include_lib("eunit/include/eunit.hrl").

-define(opt_format(___Opts__, ___Fmt___, ___Args___),
        case maps:get(trace, ___Opts__, false) of
            true -> ?debugFmt(___Fmt___, ___Args___);
            _    -> ok
        end).

-define(wrap_run(___Expr___),
        try ___Expr___
        catch ___X___:___Y___ ->
                error({___X___, ___Y___, erlang:get_stacktrace()})
        end).

%%====================================================================
%% API functions
%%====================================================================

-spec testcase_generate(string(), [atom()]) -> EunitTestSpec :: any().

testcase_generate(Path, Tests) ->
  testcase_generate(Path, Tests, #{}).

-spec testcase_generate(string(), [atom()], function() | map()) ->
                               EunitTestSpec :: any().

testcase_generate(Path, Tests, Opts) ->
    case is_external_available() of
        true ->
            {foreachx
            , fun get_config/1
            , fun(_, _) -> ok end
            , [{{Path, TestName, expand_opts(Opts, TestName)}, fun testcase/2}
               || TestName <- Tests]
            };
        false ->
            []
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Running the actual test case

testcase({Path, Name, Opts}, #{ pre := Pre, exec := Exec} = Spec) ->
    { Path ++ "/" ++ atom_to_list(Name)
    , fun() ->
              %% Set up the store for the contract.
              #{ address := Address } = Exec,
              Store = get_store(Address, Pre),
              Spec1 = Spec#{ exec => Exec#{ store => Store}},

              ?opt_format(Opts, "Setting up state: ~w~n", [Name]),
              InitState = init_state(Spec1, Opts),
              ?opt_format(Opts, "Init state: ~p~n", [InitState]),
              ?opt_format(Opts, "Running: ~w~n", [Name]),
              case ?wrap_run(run_eeevm(InitState)) of
                  {ok, State} ->
                      %% Executed to completion'
                      ?opt_format(Opts, "Checking: ~w~n", [Name]),
                      ?opt_format(Opts, "State of ~w: ~p~n", [Name, State]),
                      validate_storage(State, Spec1),
                      validate_out(State, Spec1),
                      validate_gas(State, Spec1, Opts),
                      validate_callcreates(State, Spec1),
                      {ok, State};
                  {error, What, State} ->
                      %% Handle execution exceptions gracefully here.
                      %% Some testcases are not supposed to work.
                      %% This is implicitly flagged in the config
                      %% by leaving out some fields.
                      %% TODO: Check the config
                      io:format("Error ~p~n", [What]),
                      validate_no_post(Spec1),
                      {error, State}
              end
      end
    }.

get_store(Address, State) ->
    case State of
        #{ Address := #{ storage := Store }} ->
            aevm_eeevm_store:to_binary(#{ storage => Store});
        _ -> #{}
    end.


validate_no_post(#{post := _} = Spec) ->
    error({should_have_succeeded, Spec});
validate_no_post(#{}) ->
    ok.

validate_storage(State, #{exec := #{address := Addr}} = Spec) ->
    case Spec of
	#{ post := Post} ->
	    PostStorage =
		case maps:get(Addr, Post, undefined) of
		    undefined -> #{};
		    #{storage := S}  -> S
		end,
	    Storage = aevm_eeevm_state:storage(State),
	    ?assertEqual(PostStorage, Storage);
	_ -> true
    end.

validate_out(State, #{out := SpecOut} =_Spec) ->
    Out  = aevm_eeevm_state:out(State),
    ?assertEqual(SpecOut, Out);
validate_out(_State, _Spec) -> true.

validate_gas(_State, #{} =_Spec, #{validate_gas := false}) ->
    ok;
validate_gas(State, #{gas := SpecGas} =_Spec,_Opts) ->
    Out  = aevm_eeevm_state:gas(State),
    ?assertEqual(SpecGas, Out),
    ok;
validate_gas(_State, #{} =_Spec,_Opts) ->
    %% TODO: Make sure that this was an exception.
    ok.

validate_callcreates(State, #{callcreates := []} =_Spec) ->
    Callcreates = aevm_eeevm_state:callcreates(State),
    ?assertEqual([], Callcreates);
validate_callcreates(State, #{callcreates := CallcreatesSpec} =_Spec) ->
    Callcreates = aevm_eeevm_state:callcreates(State),
    ?assertEqual(CallcreatesSpec, Callcreates).


%%--------------------------------------------------------------------
%% Interfacing to aevm_eevm

init_state(Spec, Opts) ->
    aevm_eeevm_state:init(Spec, Opts).

run_eeevm(State) ->
    aevm_eeevm:eval(State).

%%--------------------------------------------------------------------
%% Expanding the opts that is sent to the aevm_eeevm:run/1

expand_opts(#{} = Map, TestName) when is_atom(TestName) ->
    Map;
expand_opts(Fun, TestName) when is_function(Fun, 1), is_atom(TestName) ->
    Fun(TestName);
expand_opts(Fun, TestName) ->
    error({illegal_test_specs, Fun, TestName}).

%%--------------------------------------------------------------------
%% Check if the external tests are available, and only generate the
%% tests if the config files are there. We don't want to fail if there
%% are no tests to be run.

is_external_available() ->
    case external_dir() of
      false -> false;
      DirPath ->
        VMTests = filename:join([DirPath, "ethereum_tests"]),
        filelib:is_dir(VMTests)
    end.

external_dir() ->
    os:getenv("AEVM_EXTERNAL_TEST_DIR").

%%--------------------------------------------------------------------
%% Read a test config file

get_config({DirPath, TestName,_Opts}) ->
    Bin    = get_config_file(DirPath, TestName),
    Json   = jsx:decode(Bin, [return_maps, {labels, attempt_atom}]),
    Config = build_config(Json),
    TestConfig = maps:get(TestName, Config),
    DefaultEnv = #{ chainState => aevm_ethereum_test_chain:new_state(TestConfig)
                  , chainAPI => aevm_ethereum_test_chain
                  },
    maps:update_with(env, fun(Env) -> maps:merge(DefaultEnv, Env) end,
                     TestConfig).

get_config_file(DirPath, TestName) ->
    FileName = config_filename(DirPath, TestName),
    case file:read_file(FileName) of
        {ok, Bin} -> Bin;
        Other -> error({could_not_read_file, TestName, FileName, Other})
    end.

config_filename(DirPath, TestName) ->
    filename:join([ external_dir()
                  , "ethereum_tests"
                  , DirPath
                  , atom_to_list(TestName) ++ ".json"]).

%%--------------------------------------------------------------------
%% Build the test config by simultaneously traverse the defined config
%% structure and the json config from file. The json values are
%% translated to the internal representation.

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

%%--------------------------------------------------------------------
%% Define the structure of a test config.

-define(OPTIONAL_CONFIG_FIELDS, [ {callcreates, default, []}
                                , {gas, leave_out}
                                , {logs, leave_out}
                                , {out, leave_out}
                                , {post, leave_out}
                                , {sourceHash, default, <<>>}
                                , {lllcversion, default, <<>>}
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
                         , sourceHash => string
                         , lllcversion => string
                         }
           , callcreates => [#{ data => data_array
                              , destination => bin_int
                              , gasLimit => bin_int
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

%%--------------------------------------------------------------------
%% Primitive operations for building the test config.

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
