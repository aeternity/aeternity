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

-export([generate/6,
         verify/4,
         recalculate_difficulty/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-on_load(init/0).

-type pow_cuckoo_solution() :: [integer()].
-type pow_cuckoo_result() :: {'ok', Soln :: pow_cuckoo_solution()} | {'error', atom()}.

-export_type([pow_cuckoo_result/0,
              pow_cuckoo_solution/0]).

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
%% Proof of Work generation, multiple attempts
%%------------------------------------------------------------------------------
-spec generate(Data :: binary(), Nonce :: integer(), Difficulty :: aec_sha256:sci_int(),
               Trims :: integer(), Threads :: integer(), Retries :: integer()) ->
                      pow_cuckoo_result().
generate(Data, Nonce, Difficulty, Trims, Threads, Retries) ->
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    generate_hashed(Hash, Nonce, Difficulty, Trims, Threads, Retries).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Data :: binary(), Nonce :: integer(),
             Soln :: pow_cuckoo_solution(), Difficulty :: aec_sha256:sci_int()) ->
                    boolean().
verify(Data, Nonce, Soln, Difficulty) ->
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    case test_difficulty(Soln, Difficulty) of
        true ->
            verify(Hash, Nonce, Soln);
        false ->
            false
    end.

%%------------------------------------------------------------------------------
%% Adjust difficulty so that generation of new blocks proceeds at the expected pace
%%------------------------------------------------------------------------------
-spec recalculate_difficulty(aec_sha256:sci_int(), integer(), integer()) ->
                                    aec_sha256:sci_int().
recalculate_difficulty(Difficulty, Expected, Actual) ->
    DiffInt = aec_sha256:scientific_to_integer(Difficulty),
    aec_sha256:integer_to_scientific(max(1, DiffInt * Expected div Actual)).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation: use the hash provided and try consecutive nonces
%%------------------------------------------------------------------------------
generate_hashed(_Hash, _Nonce, _Difficulty, _Trims, _Threads, 0) ->
    {error, generation_count_exhausted};
generate_hashed(Hash, Nonce, Difficulty, Trims, Threads, Retries) when Retries > 0 ->
    case generate(Hash, Nonce, Trims, Threads) of
        {error, no_solutions} ->
            generate_hashed(Hash, Nonce + 1, Difficulty, Trims, Threads, Retries - 1);
        {ok, Soln} = Result ->
            case test_difficulty(Soln, Difficulty) of
                true ->
                    Result;
                false ->
                    generate_hashed(Hash, Nonce + 1, Difficulty, Trims, Threads, Retries - 1)
            end
    end.

%%------------------------------------------------------------------------------
%% Proof of Work generation, a single attempt
%%------------------------------------------------------------------------------
-spec generate(Header :: string(), Nonce :: integer(), Trims :: integer(),
               Threads :: integer()) -> pow_cuckoo_result().
generate(_Header, _Nonce, _Trims, _Threads) ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Proof of Work verification (without difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Hash :: string(), Nonce :: integer(),
             Soln :: pow_cuckoo_solution()) -> boolean().
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
%% hash-based difficulty is suggested: the sha256 hash of the cycle nonces
%% is restricted to be under the difficulty value (0 < difficulty < 2^256).
%%------------------------------------------------------------------------------
-spec test_difficulty(Soln :: pow_cuckoo_solution(), Difficulty :: aec_sha256:sci_int()) ->
                             boolean().
test_difficulty(Soln, Difficulty) ->
    NodeSize = get_node_size(),
    Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8, <<>>),
    Hash = aec_sha256:hash(Bin),
    aec_sha256:binary_to_scientific(Hash) < Difficulty.

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
