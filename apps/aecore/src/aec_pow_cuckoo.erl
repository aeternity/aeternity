%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    A library providing Cuckoo Cycle PoW generation and verification.
%%%    A NIF interface to the C/C++ Cuckoo Cycle implementation of
%%%    John Tromp:  https://github.com/tromp/cuckoo
%%%    White paper: https://github.com/tromp/cuckoo/blob/master/doc/cuckoo.pdf?raw=true
%%% @end
%%%-------------------------------------------------------------------
-module(aec_pow_cuckoo).

-behaviour(aec_pow).

-export([generate/3,
         generate/6,
         verify/4]).


-ifdef(TEST).
-compile(export_all).
-endif.

-on_load(init/0).

-type pow_cuckoo_solution() :: [integer()].


%%%=============================================================================
%%% NIF initialization
%%%=============================================================================

init() ->
    ok = erlang:load_nif(filename:join([code:priv_dir(aecore),
                                        "aec_pow_cuckoo_nif"]), 0).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation with default settings, multiple attempts
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
               Retries :: integer()) -> aec_pow:pow_result().
generate(Data, Target, Retries) ->
    Nonce = aec_pow:pick_nonce(),
    generate(Data, Nonce, Target, 7, 5, Retries).

%%------------------------------------------------------------------------------
%% Proof of Work generation, all params adjustable
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Nonce :: integer(),
               Target :: aec_pow:sci_int(), Trims :: integer(),
               Threads :: integer(), Retries :: integer()) ->
                      aec_pow:pow_result().
generate(Data, Nonce, Target, Trims, Threads, Retries) ->
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    generate_hashed(Hash, Nonce, Target, Trims, Threads, Retries).

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
%% Proof of Work generation: use the hash provided and try consecutive nonces
%%------------------------------------------------------------------------------
generate_hashed(_Hash, _Nonce, _Target, _Trims, _Threads, 0) ->
    {error, generation_count_exhausted};
generate_hashed(Hash, Nonce, Target, Trims, Threads, Retries) when Retries > 0 ->
    case generate_single(Hash, Nonce, Trims, Threads) of
        {error, no_solutions} ->
            generate_hashed(Hash, Nonce + 1, Target, Trims, Threads, Retries - 1);
        {ok, Soln} ->
            case test_target(Soln, Target) of
                true ->
                    {ok, {Nonce, Soln}};
                false ->
                    generate_hashed(Hash, Nonce + 1, Target, Trims, Threads, Retries - 1)
            end
    end.

%%------------------------------------------------------------------------------
%% Proof of Work generation, a single attempt
%%------------------------------------------------------------------------------
-spec generate_single(Header :: string(), Nonce :: integer(), Trims :: integer(),
                      Threads :: integer()) ->
                             {'ok', Solution :: pow_cuckoo_solution()} |
                             {'error', 'no_solutions'}.
generate_single(_Header, _Nonce, _Trims, _Threads) ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Proof of Work verification (without difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Hash :: string(), Nonce :: integer(),
             Soln :: aec_pow:pow_evidence()) -> boolean().
verify(_Hash, _Nonce, _Soln) ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Fetch the size of solution elements
%%------------------------------------------------------------------------------
-spec get_node_size() -> integer().
get_node_size() ->
    erlang:nif_error(nif_library_not_loaded).

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
