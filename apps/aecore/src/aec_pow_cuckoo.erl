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
-include("pow.hrl").

-define(DEFAULT_CUCKOO_ENV, {"mean28s-generic", "-t 5", 28}).

-define(debug(F, A), epoch_pow_cuckoo:debug(F, A)).
-define(info(F, A),  epoch_pow_cuckoo:info(F, A)).
-define(error(F, A), epoch_pow_cuckoo:error(F, A)).

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
-spec generate(Data :: aec_hash:hashable(), Target :: aec_pow:sci_int(),
               Nonce :: aec_pow:nonce()) -> aec_pow:pow_result().
generate(Data, Target, Nonce) when Nonce >= 0,
                                   Nonce =< ?MAX_NONCE ->
    %% Hash Data and convert the resulting binary to a base64 string for Cuckoo
    %% Since this hash is purely internal, we don't use base58
    Hash = aec_hash:hash(pow, Data),
    ?debug("Generating solution for data hash ~p and nonce ~p with target ~p.",
           [Hash, Nonce, Target]),
    generate_int(Hash, Nonce, Target).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_hash:hashable(), Nonce :: aec_pow:nonce(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Target) when is_list(Evd),
                                      Nonce >= 0, Nonce =< ?MAX_NONCE ->
    Hash = aec_hash:hash(pow, Data),
    case test_target(Evd, Target) of
        true ->
            verify_proof(Hash, Nonce, Evd);
        false ->
            false
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Options handling
%%------------------------------------------------------------------------------

get_options() ->
    {_, _, _} = aeu_env:get_env(aecore, aec_pow_cuckoo, ?DEFAULT_CUCKOO_ENV).

get_miner_options() ->
    case
        {aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"miner">>, <<"executable">>]),
         aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"miner">>, <<"extra_args">>])
        }
    of
        {{ok, BinB}, {ok, ExtraArgsB}} ->
            {binary_to_list(BinB), binary_to_list(ExtraArgsB)};
        {undefined, undefined} -> %% Both or neither - enforced by user config schema.
            {Bin, ExtraArgs, _} = get_options(),
            {Bin, ExtraArgs}
    end.

get_node_bits() ->
    case aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"miner">>, <<"node_bits">>]) of
        {ok, NodeBits} ->
            NodeBits;
        undefined ->
            {_, _, NodeBits} = get_options(),
            NodeBits
    end.

%%------------------------------------------------------------------------------
%% Proof of Work generation: use the hash provided
%%------------------------------------------------------------------------------
-spec generate_int(Hash :: binary(), Nonce :: aec_pow:nonce(), Target :: aec_pow:sci_int()) ->
                          {'ok', Nonce2 :: aec_pow:nonce(),
                           Solution :: pow_cuckoo_solution()} |
                          {'error', term()}.
generate_int(Hash, Nonce, Target) ->
    Header = pack_header_and_nonce(Hash, Nonce),

    case generate_int(Header, Target) of
        {ok, Soln} ->
            {ok, {Nonce, Soln}};
        {error, no_value} ->
            ?debug("No cuckoo solution found", []),
            {error, no_solution};
        {error, Reason} ->
            %% Executable failed (segfault, not found, etc.): let miner decide
            {error, {runtime, Reason}}
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   Proof of Work generation from a hex-encoded 80-byte buffer
%% @end
%%------------------------------------------------------------------------------
-spec generate_int(Headr :: string(), Target :: aec_pow:sci_int()) ->
                          {'ok', Solution :: pow_cuckoo_solution()} |
                          {'error', term()}.
generate_int(Header, Target) ->
    {MinerBin, MinerExtraArgs} = get_miner_options(),
    generate_int(Header, Target, MinerBin, MinerExtraArgs).

generate_int(Header, Target, MinerBin, MinerExtraArgs) ->
    BinDir = aecuckoo:bin_dir(),
    Cmd = lists:concat([ld_lib_env(), " ",  "./", MinerBin,
                        " -h ", Header, " ", MinerExtraArgs]),
    ?info("Executing cmd: ~p", [Cmd]),
    Old = process_flag(trap_exit, true),
    try exec:run(Cmd,
                 [{stdout, self()},
                  {stderr, self()},
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
    after
        process_flag(trap_exit, Old),
        receive
            {'EXIT',_From, shutdown} -> exit(shutdown)
        after 0 -> ok
        end
    end.

-define(POW_OK, ok).
-define(POW_TOO_BIG, {error, nonce_too_big}).
-define(POW_TOO_SMALL, {error, nonces_not_ascending}).
-define(POW_NON_MATCHING, {error, endpoints_do_not_match_up}).
-define(POW_BRANCH, {error, branch_in_cycle}).
-define(POW_DEAD_END, {error, cycle_dead_ends}).
-define(POW_SHORT_CYCLE, {error, cycle_too_short}).
-define(PROOFSIZE, 42).
%%------------------------------------------------------------------------------
%% @doc
%%   Proof of Work verification (difficulty check should be done before calling
%%   this function)
%% @end
%%------------------------------------------------------------------------------
-spec verify_proof(Hash :: binary(), Nonce :: aec_pow:nonce(),
                   Solution :: aec_pow:pow_evidence()) -> boolean().
verify_proof(Hash, Nonce, Solution) ->
    verify_proof(Hash, Nonce, Solution, get_node_bits()).

verify_proof(Hash, Nonce, Solution, NodeBits) ->
    %% Cuckoo has an 80 byte header, we have to use that as well
    %% packed Hash + Nonce = 56 bytes, add 24 bytes of 0:s
    Header0 = pack_header_and_nonce(Hash, Nonce),
    Header = <<(list_to_binary(Header0))/binary, 0:(8*24)>>,
    {K0, K1, K2, K3} = aeu_siphash24:create_keys(Header),

    EdgeMask = (1 bsl (NodeBits - 1)) - 1,
    try
        %% Generate Uv pairs representing endpoints by hashing the proof
        %% XOR points together: for a closed cycle they must match somewhere
        %% making one of the XORs zero.
        {Xor0, Xor1, _, Uvs} =
            lists:foldl(
              fun(N, _) when N > EdgeMask ->
                      throw({?POW_TOO_BIG, N});
                 (N, {_Xor0, _Xor1, PrevN, _Uvs}) when N =< PrevN ->
                      throw({?POW_TOO_SMALL, N, PrevN});
                 (N, {Xor0C, Xor1C, _PrevN, UvsC}) ->
                      Uv0 = sipnode(K0, K1, K2, K3, N, 0, EdgeMask),
                      Uv1 = sipnode(K0, K1, K2, K3, N, 1, EdgeMask),
                      {Xor0C bxor Uv0, Xor1C bxor Uv1, N, [{Uv0, Uv1} | UvsC]}
               end, {16#0, 16#0, -1, []}, Solution),
        case Xor0 bor Xor1 of
            0 ->
                %% check cycle
                case check_cycle(Uvs) of
                    ok -> true;
                    {error, E} -> throw(E)
                end;
            _ ->
                %% matching endpoints imply zero xors
                throw(?POW_NON_MATCHING)
        end
    catch
        throw:{error, Reason} ->
            epoch_pow_cuckoo:error("Proof verification failed for ~p: ~p", [Solution, Reason]),
            false
    end.

sipnode(K0, K1, K2, K3, Proof, UOrV, EdgeMask) ->
    SipHash = aeu_siphash24:hash(K0, K1, K2, K3, 2*Proof + UOrV) band EdgeMask,
    (SipHash bsl 1) bor UOrV.

check_cycle(Nodes0) ->
  Nodes = lists:keysort(2, Nodes0),
  {Evens0, Odds} = lists:unzip(Nodes),
  Evens  = lists:sort(Evens0), %% Odd nodes are already sorted...
  UEvens = lists:usort(Evens),
  UOdds  = lists:usort(Odds),
  %% Check that all nodes appear exactly twice (i.e. each node has
  %% exactly two edges).
  case length(UEvens) == (?PROOFSIZE div 2) andalso
        length(UOdds) == (?PROOFSIZE div 2) andalso
        UOdds == Odds -- UOdds andalso UEvens == Evens -- UEvens of
      false ->
          {error, ?POW_BRANCH};
      true  ->
          [{X0, Y0}, {X1, Y0} | Nodes1] = Nodes,
          check_cycle(X0, X1, Nodes1)
  end.

%% If we reach the end in the last step everything is fine
check_cycle(X, X, []) ->
    ok;
%% If we reach the end too early the cycle is too short
check_cycle(X, X, _)  ->
    {error, ?POW_SHORT_CYCLE};
check_cycle(XEnd, XNext, Nodes) ->
    %% Find the outbound edge for XNext and follow that edge
    %% to an odd node and back again to NewXNext
    case find_node(XNext, Nodes, []) of
        Err = {error, _}            -> Err;
        {XNext, NewXNext, NewNodes} -> check_cycle(XEnd, NewXNext, NewNodes)
    end.

find_node(_, [], _Acc) ->
    {error, ?POW_DEAD_END};
find_node(X, [{X, Y}, {X1, Y} | Nodes], Acc) ->
    {X, X1, Nodes ++ Acc};
find_node(X, [{X1, Y}, {X, Y} | Nodes], Acc) ->
    {X, X1, Nodes ++ Acc};
find_node(X, [{X, _Y} | _], _Acc) ->
  {error, ?POW_DEAD_END};
find_node(X, [N1, N2 | Nodes], Acc) ->
  find_node(X, Nodes, [N1, N2 | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%%   Creates the Cuckoo buffer (hex encoded) from a base64-encoded hash and a
%%   uint64 nonce.
%%   Since this hash is purely internal, we don't use base58.
%% @end
%%------------------------------------------------------------------------------
-spec pack_header_and_nonce(binary(), aec_pow:nonce()) -> string().
pack_header_and_nonce(Hash, Nonce) when byte_size(Hash) == 32 ->
    %% Cuckoo originally uses 32-bit nonces inserted at the end of its 80-byte buffer.
    %% This buffer is hashed into the keys used by the main algorithm.
    %%
    %% We insert our 64-bit Nonce right after the hash of the block header We
    %% base64-encode both the hash of the block header and the nonce and pass
    %% the resulting command-line friendly string with the -h option to Cuckoo.
    %%
    %% The SHA256 hash is 32 bytes (44 chars base64-encoded), the nonce is 8 bytes
    %% (12 chars base64-encoded). That leaves plenty of room (80 - 56 = 24
    %% bytes) for cuckoo to put its nonce (which will be 0 in our case) in.
    %%
    %% (Base64 encoding: see RFC 3548, Section 3:
    %% https://tools.ietf.org/html/rfc3548#page-4
    %% converts every triplet of bytes to 4 characters: from N bytes to 4*ceil(N/3)
    %% bytes.)
    %%
    %% Like Cuckoo, we use little-endian for the nonce here.
    NonceStr = base64:encode_to_string(<<Nonce:64/little-unsigned-integer>>),
    HashStr  = base64:encode_to_string(Hash),
    %% Cuckoo will automatically fill bytes not given with -h option to 0, thus
    %% we need only return the two base64 encoded strings concatenated.
    %% 44 + 12 = 56 bytes
    HashStr ++ NonceStr.

%%------------------------------------------------------------------------------
%% @doc
%%   Prefix setting load path to command so that blake2b.so is found in priv/lib
%% @end
%%------------------------------------------------------------------------------
-spec ld_lib_env() -> string().
ld_lib_env() ->
    LdPathVar = case os:type() of
                 {unix, darwin} -> "DYLD_LIBRARY_PATH";
                 {unix, _}      -> "LD_LIBRARY_PATH"
                end,
    "env " ++ LdPathVar ++ "=../lib:$" ++ LdPathVar.

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
            ?error("ERROR: ~s", [Msg]),
            wait_for_result(State);
        {'EXIT',_From, shutdown} ->
            %% Someone is telling us to stop
            stop_execution(OsPid),
            exit(shutdown);
        {'DOWN', OsPid, process, _, normal} ->
            %% Process ended but no value found
            {error, no_value};
        {'DOWN', OsPid, process, _, Reason} ->
            %% Abnormal termination
            Reason2 = case Reason of
                          {exit_status, ExStat} -> exec:status(ExStat);
                          _                     -> Reason
                      end,
            ?error("OS process died: ~p", [Reason2]),
            {error, {execution_failed, Reason2}}
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
            ?debug("Solution found: ~p", [Soln]),
            stop_execution(OsPid),
            {ok, Soln};
        false ->
            %% failed to meet target: go on, we may find another solution
            ?debug("Failed to meet target (~p)", [Target]),
            parse_generation_result(Rest, State)
    end;
parse_generation_result([Msg | T], State) ->
    ?debug("~s", [Msg]),
    parse_generation_result(T, State).

%%------------------------------------------------------------------------------
%% @doc
%%   Ask erlexec to stop the OS process
%% @end
%%------------------------------------------------------------------------------
-spec stop_execution(integer()) -> ok.
stop_execution(OsPid) ->
    case exec:kill(OsPid, 9) of
        {error, Reason} ->
            ?debug("Failed to stop mining OS process ~p: ~p (may have already finished).",
                   [OsPid, Reason]);
        R ->
            ?debug("Mining OS process ~p stopped successfully: ~p",
                   [OsPid, R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%%   The Cuckoo solution is a list of uint32 integers unless the graph size is
%%   greater than 33 (when it needs u64 to store). Hash result for difficulty
%%   control accordingly.
%% @end
%%------------------------------------------------------------------------------
-spec get_node_size() -> non_neg_integer().
get_node_size() ->
    node_size(get_node_bits()).

%% Refs:
%% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/Makefile#L214-L215
%% * https://github.com/tromp/cuckoo/blob/488c03f5dbbfdac6d2d3a7e1d0746c9a7dafc48f/src/cuckoo.h#L26-L30
-spec node_size(non_neg_integer()) -> non_neg_integer().
node_size(NodeBits) when is_integer(NodeBits), NodeBits > 32 -> 8;
node_size(NodeBits) when is_integer(NodeBits), NodeBits >  0 -> 4.

%%------------------------------------------------------------------------------
%% White paper, section 9: rather than adjusting the nodes/edges ratio, a
%% hash-based target is suggested: the sha256 hash of the cycle nonces
%% is restricted to be under the target value (0 < target < 2^256).
%%------------------------------------------------------------------------------
-spec test_target(Soln :: pow_cuckoo_solution(), Target :: aec_pow:sci_int()) ->
                             boolean().
test_target(Soln, Target) ->
    test_target(Soln, Target, get_node_size()).

test_target(Soln, Target, NodeSize) ->
    Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8, <<>>),
    Hash = aec_hash:hash(pow, Bin),
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

