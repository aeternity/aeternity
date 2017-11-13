%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    A library providing Cuckoo Cycle PoW generation and verification.
%%%    Using (as an independent OS process) the C/C++ Cuckoo Cycle implementation of
%%%    John Tromp:  https://github.com/tromp/cuckoo
%%%    White paper: https://github.com/tromp/cuckoo/blob/master/doc/cuckoo.pdf?raw=true
%%% @end
%%%-------------------------------------------------------------------
-module(aec_pow_cuckoo).

-behaviour(aec_pow).


-export([generate/3,
         verify/4]).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_CUCKOO_ENV, {mean30, "-t 5", 30}). %% -m 7 caused crash

-ifdef(TEST).
-define(debug(F, A), ok).
-define(info(F, A), ?debugFmt(F, A)).
-define(error(F, A), ?debugFmt(F, A)).
-else.
-define(debug(F, A), epoch_pow_cuckoo:debug(F, A)).
-define(info(F, A),  epoch_pow_cuckoo:info(F, A)).
-define(error(F, A), epoch_pow_cuckoo:error(F, A)).
-endif.

-record(state, {os_pid :: integer() | undefined,
                buffer = [] :: string(),
                target :: aec_pow:sci_int() | undefined,
                parser :: output_parser_fun()}).

-type pow_cuckoo_solution() :: [integer()].
-type output_parser_fun() :: fun((list(string()), #state{}) ->
                                      {'ok', term()} | {'error', term()}).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation with default settings
%%
%% According to my experiments, increasing the number of trims from the default
%% 7 in John Tromp's code does not really affect speed, reducing it causes failure.
%%
%% Measured execution times (seconds) for 7 trims for threads:
%%   1: 44.61 46.92 46.41
%%   2: 15.17 15.75 19.19
%%   3:  9.35 10.92  9.73
%%   4: 10.66  7.83 10.02
%%   5:  7.41  7.47  7.32
%%  10:  7.27  6.70  6.38
%%  20:  6.25  6.74  6.41
%%
%%  Very slow below 3 threads, not improving significantly above 5, let us take 5.
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Target :: aec_pow:sci_int(),
               Nonce :: integer()) -> aec_pow:pow_result().
generate(Data, Target, Nonce) ->
    %% Hash Data and convert the resulting binary to a base64 string for Cuckoo
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    generate_int(Hash, Nonce, Target).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Target) when is_list(Evd) ->
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    case test_target(Evd, Target) of
        true ->
            verify(Hash, Nonce, Evd);
        false ->
            false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation: use the hash provided
%%------------------------------------------------------------------------------
-spec generate_int(Hash :: string(), Nonce :: integer(), Target :: aec_pow:sci_int()) ->
                          {'ok', Nonce2 :: integer(), Solution :: pow_cuckoo_solution()} |
                          {'error', term()}.
generate_int(Hash, Nonce, Target) ->
    case generate_single(Hash, Nonce, Target) of
        {ok, Soln} ->
            {ok, {Nonce, Soln}};
        {error, no_value} ->
            ?debug("No cuckoo solution found~n", []),
            {error, no_solution};
        {error, Reason} ->
            %% Executable failed (segfault, not found, etc.): let miner decide
            {error, {runtime, Reason}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Proof of Work generation, a single attempt
%% @end
%%------------------------------------------------------------------------------
-spec generate_single(Hash :: string(), Nonce :: integer(), Target :: aec_pow:sci_int()) ->
                             {'ok', Solution :: pow_cuckoo_solution()} |
                             {'error', term()}.
generate_single(Hash, Nonce, Target) ->
    BinDir = filename:join([code:priv_dir(ae_cuckoo_cycle_pow_executables), "bin"]),
    {Exe, Extra, _} = application:get_env(aecore, aec_pow_cuckoo, ?DEFAULT_CUCKOO_ENV),
    %% -s makes mean miner print out the solution. We do not make it
    %% Algo-dependent in order to avoid wiring-in executable names. Lean miner
    %% will complain but ignores it.
    ?info("Executing cmd: ~p~n", [lists:concat(export_ld_lib_path() ++ ["./", Exe, " -h ", Hash, " -n ", Nonce, " -s ", Extra])]),
    try exec:run(
          lists:concat(export_ld_lib_path() ++
                           ["./", Exe, " -h ", Hash, " -n ", Nonce, " -s ", Extra]),
          [{stdout, self()},
           {stderr, self()},
           {kill_timeout, 1},
           {cd, BinDir},
           {env, [{"SHELL", "/bin/sh"}]},
           monitor]) of
        {ok, _ErlPid, OsPid} ->
            wait_for_result(#state{os_pid = OsPid,
                                   buffer = [],
                                   parser = fun parse_generation_result/2,
                                   target = Target})
    catch
        C:E ->
            {error, {unknown, {C, E}}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Proof of Work verification (without difficulty check)
%%
%%   This function returns {error, term()} in the unlikely case that
%%   it does not manage to determine an outcome for the verification
%%   of the PoW (e.g. verifier terminates abnormally).
%% @end
%%------------------------------------------------------------------------------
-spec verify(Hash :: string(), Nonce :: integer(),
             Soln :: aec_pow:pow_evidence()) -> boolean().
verify(Hash, Nonce, Soln) ->
    BinDir = filename:join([code:priv_dir(ae_cuckoo_cycle_pow_executables), "bin"]),
    {_, _, Size} = application:get_env(aecore, aec_pow_cuckoo, ?DEFAULT_CUCKOO_ENV),
    SolnStr = solution_to_hex(Soln),
    ?info("Executing: ~p~n", [lists:concat(export_ld_lib_path() ++ ["./verify", Size, " -h ", Hash, " -n ", Nonce])]),
    try exec:run(
              lists:concat(export_ld_lib_path() ++
                               ["./verify", Size, " -h ", Hash, " -n ", Nonce]),
              [{stdout, self()},
               {stderr, self()},
               stdin,
               {kill_timeout, 1}, %% Kills exe with SIGTERM, then with SIGKILL if needed
               {cd, BinDir},
               {env, [{"SHELL", "/bin/sh"}]},
               monitor]) of
        {ok, _ErlPid, OsPid} ->
            exec:send(OsPid, list_to_binary(SolnStr)),
            exec:send(OsPid, eof),
            ParserState = #state{os_pid = OsPid,
                                        buffer = [],
                                        parser = fun parse_verification_result/2},
            case wait_for_result(ParserState) of
                {ok, Value} when is_boolean(Value) ->
                    Value;
                {error, _Reason} = Err ->
                    Err
            end
    catch
        C:E ->
            {error, {unknown, {C, E}}}
    end.

export_ld_lib_path() ->
    LdPathVar = case os:type() of
                 {unix, darwin} -> "DYLD_LIBRARY_PATH";
                 {unix, _}      -> "LD_LIBRARY_PATH"
                end,
    ["export ", LdPathVar, "=../lib:$", LdPathVar, "; "].

%%------------------------------------------------------------------------------
%% @doc
%%   Receive and process notifications about the fate of the process and its
%%   output. The receieved stdout tends to be in large chunks, we keep a buffer
%%   for the last line fragment w/o NL.
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_result(#state{}) -> {'ok', term()} | {'error', term()}.
wait_for_result(#state{os_pid = OsPid,
                       buffer = Buffer} = State) ->
    receive
        {stdout, OsPid, Msg} ->
            Str = binary_to_list(Msg),
            {Lines, NewBuffer} = handle_fragmented_lines(Str, Buffer),
            (State#state.parser)(Lines, State#state{buffer = NewBuffer});
        {stderr, OsPid, Msg} ->
            ?error("ERROR: ~s~n", [Msg]),
            wait_for_result(State);
        {'DOWN', OsPid, process, _, normal} ->
            %% Process ended but no value found
            {error, no_value};
        {'DOWN', OsPid, process, _, Reason} ->
            %% Abnormal termination
            Reason2 = case Reason of
                          {exit_status, ExStat} -> exec:status(ExStat);
                          _                     -> Reason
                      end,
            ?error("OS process died: ~p~n", [Reason2]),
            {error, {execution_failed, Reason2}};
        Other ->
            ?debug("Ignoring message ~p~n", [Other]),
            wait_for_result(State)
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Prepend the first new incoming line with the last line fragment stored
%%   in Buffer and replace Buffer with the possible new line fragment at the
%%   end of Str.
%% @end
%%------------------------------------------------------------------------------
-spec handle_fragmented_lines(string(), string()) -> {list(string()), string()}.
handle_fragmented_lines(Str, Buffer) ->
    Lines = string:tokens(Str, "\n"),

    %% Add previous truncated line if present to first line
    Lines2 =
        case Buffer of
            [] ->
                Lines;
            _ ->
                [Line1 | More] = Lines,
                [Buffer ++ Line1 | More]
        end,

    %% Keep last fraction (w/o NL) in buffer
    case lists:last(Str) of
        $\n ->
            {Lines2, ""};
        _ ->
            {L3, [Bf]} = lists:split(length(Lines) - 1, Lines2),
            {L3, Bf}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Parse miner output
%% @end
%%------------------------------------------------------------------------------
-spec parse_generation_result(list(string()), #state{}) ->
                                     {'ok', Solution :: pow_cuckoo_solution()} |
                                     {'error', term()}.
parse_generation_result([], State) ->
    wait_for_result(State);
parse_generation_result(["Solution" ++ ValuesStr | Rest], #state{os_pid = OsPid,
                                                                 target = Target} = State) ->
    Soln = [list_to_integer(V, 16) || V <- string:tokens(ValuesStr, " ")],
    case test_target(Soln, Target) of
        true ->
            ?debug("Solution found: ~p~n", [Soln]),
            stop_execution(OsPid),
            {ok, Soln};
        false ->
            %% failed to meet target: go on, we may find another solution
            ?debug("Failed to meet target~n", []),
            parse_generation_result(Rest, State)
    end;
parse_generation_result([Msg | T], State) ->
    ?debug("~s~n", [Msg]),
    parse_generation_result(T, State).

%%------------------------------------------------------------------------------
%% @doc
%%   Parse verifyer output
%% @end
%%------------------------------------------------------------------------------
-spec parse_verification_result(list(string()), #state{}) ->
                                       {'ok', boolean()} | {'error', term()}.
parse_verification_result([], State) ->
    wait_for_result(State);
parse_verification_result(["Verified with cyclehash" ++ _ | _Rest], #state{os_pid = OsPid}) ->
    ?debug("PoW verified.~n", []),
    stop_execution(OsPid),
    {ok, true};
parse_verification_result(["FAILED due to" ++ Reason | _Rest], #state{os_pid = OsPid}) ->
    ?error("PoW verification failed: ~s~n", [Reason]),
    stop_execution(OsPid),
    {ok, false};
parse_verification_result([Msg | T], State) ->
    ?debug("~s~n", [Msg]),
    parse_verification_result(T, State).

%%------------------------------------------------------------------------------
%% @doc
%%   Ask erlexec to stop the OS process
%% @end
%%------------------------------------------------------------------------------
-spec stop_execution(integer()) -> ok.
stop_execution(OsPid) ->
    case exec:stop(OsPid) of
        {error, Reason} ->
            ?debug("Failed to stop mining OS process ~p: ~p (may have already finished).~n",
                   [OsPid, Reason]);
        R ->
            ?debug("Mining OS process ~p stopped successfully: ~p~n",
                   [OsPid, R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   The Cuckoo solution is a list of uint32 integers unless the graph size is
%%   greater than 33 (when it needs u64 to store). Hash result for difficulty
%%   control accordingly.
%% @end
%%------------------------------------------------------------------------------
-spec get_node_size() -> integer().
get_node_size() ->
    case application:get_env(aecore, aec_pow_cuckoo, ?DEFAULT_CUCKOO_ENV) of
        %% Refs:
        %% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/Makefile#L214-L215
        %% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/cuckoo.h#L26-L30
        {_, _, L} when is_integer(L), L > 32 ->
            8;
        {_, _, L} when is_integer(L), L > 0 ->
            4
    end.

%%------------------------------------------------------------------------------
%% White paper, section 9: rather than adjusting the nodes/edges ratio, a
%% hash-based target is suggested: the sha256 hash of the cycle nonces
%% is restricted to be under the target value (0 < target < 2^256).
%%------------------------------------------------------------------------------
-spec test_target(Soln :: pow_cuckoo_solution(), Target :: aec_pow:sci_int()) ->
                             boolean().
test_target(Soln, Target) ->
    NodeSize = get_node_size(),
    Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8, <<>>),
    Hash = aec_sha256:hash(Bin),
    aec_pow:test_target(Hash, Target).


%%------------------------------------------------------------------------------
%% Convert solution (a list of 42 numbers) to a binary
%% in a languauge-independent way
%%------------------------------------------------------------------------------
-spec solution_to_binary(Soln :: pow_cuckoo_solution(), Bits :: integer(),
                         Acc :: binary()) -> binary().
solution_to_binary([], _Bits, Acc) ->
    Acc;
solution_to_binary([H | T], Bits, Acc) ->
    solution_to_binary(T, Bits, <<Acc/binary, H:Bits>>).

-spec solution_to_hex(list(integer())) -> string().
solution_to_hex(Sol) ->
    solution_to_hex(Sol, []).

-spec solution_to_hex(list(integer()), list(string())) -> string().
solution_to_hex([], Acc) ->
    string:join(lists:reverse(Acc), " ");
solution_to_hex([H | T], Acc) when is_integer(H) ->
    solution_to_hex(T, [lists:flatten(io_lib:format("~8.16.0b", [H])) | Acc]).
