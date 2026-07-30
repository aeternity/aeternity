%%%=============================================================================
%%% @copyright (C) 2026, Aeternity Anstalt
%%% @doc
%%%    Historical-block replay equivalence harness: drives blocks through the
%%%    real block-application path (`aec_chain_state') and checks that the
%%%    recomputed state root equals the root each block header declares. Two
%%%    entry points: `replay_insert/1' inserts blocks (oldest first) into a
%%%    fresh DB via the follower/validator path; `replay_repair_range/2'
%%%    recomputes a height range in place over an already-populated DB via
%%%    `repair_block_state/1'. Never mines, signs, gossips, or connects to a
%%%    peer — only recomputes state locally.
%%% @end
%%%=============================================================================
-module(aec_replay_harness).

-export([ replay_insert/1
        , replay_repair_range/2
        , summarize/1
        , format_entry/1
        , print_report/1
        , ok_entries/1
        ]).

-export_type([entry/0, report/0]).

-type status() :: ok
                 | {mismatch, Got :: binary() | undefined, Expected :: binary()}
                 | {error, term()}.

-type entry() :: #{ height   := non_neg_integer()
                   , hash     := binary() | undefined
                   , type     := key | micro
                   , expected := binary()
                   , status   := status()
                   , micros   := non_neg_integer()
                   }.

-type report() :: {ok, [entry()]} | {error, entry(), [entry()]}.

%%%===================================================================
%%% Sequential insert into a fresh/empty chain DB
%%%===================================================================

%% Feed blocks (oldest first) through the real validator path, stopping
%% at the first non-ok status.
-spec replay_insert([aec_blocks:block()]) -> report().
replay_insert(Blocks) when is_list(Blocks) ->
    replay_insert(Blocks, []).

replay_insert([], Acc) ->
    {ok, lists:reverse(Acc)};
replay_insert([Block | Rest], Acc) ->
    Height = safe_height(Block),
    Type = aec_blocks:type(Block),
    Hash = safe_hash(Block),
    Expected = aec_blocks:root_hash(Block),
    {Micros, Raw} = timer:tc(fun() -> safe_call(fun() -> aec_chain_state:insert_block(Block) end) end),
    Status = classify(Raw),
    Entry = #{height => Height, hash => Hash, type => Type,
              expected => Expected, status => Status, micros => Micros},
    case Status of
        ok -> replay_insert(Rest, [Entry | Acc]);
        _  -> {error, Entry, lists:reverse(Acc)}
    end.

%%%===================================================================
%%% Repair-in-place over an already-populated chain DB
%%%===================================================================

%% Recompute each block's state ascending from FromHeightExcl+1 to
%% ToHeightIncl, so each height reads the previous height's freshly
%% recomputed trees. FromHeightExcl itself is left as the starting
%% parent state.
-spec replay_repair_range(non_neg_integer(), non_neg_integer()) -> report().
replay_repair_range(FromHeightExcl, ToHeightIncl)
  when is_integer(FromHeightExcl), is_integer(ToHeightIncl),
       ToHeightIncl >= FromHeightExcl ->
    Heights = lists:seq(FromHeightExcl + 1, ToHeightIncl),
    repair_heights(Heights, []).

repair_heights([], Acc) ->
    {ok, lists:reverse(Acc)};
repair_heights([Height | Rest], Acc) ->
    case aec_chain:get_generation_by_height(Height, backward) of
        error ->
            Entry = #{height => Height, hash => undefined, type => key,
                      expected => <<>>, status => {error, no_such_generation},
                      micros => 0},
            {error, Entry, lists:reverse(Acc)};
        {ok, #{key_block := KeyBlock, micro_blocks := MicroBlocks}} ->
            BlocksInOrder = MicroBlocks ++ [KeyBlock],
            case repair_blocks(BlocksInOrder, Acc) of
                {ok, Acc1}            -> repair_heights(Rest, Acc1);
                {error, _, _} = Error -> Error
            end
    end.

repair_blocks([], Acc) ->
    {ok, Acc};
repair_blocks([Block | Rest], Acc) ->
    Height = safe_height(Block),
    Type = aec_blocks:type(Block),
    Hash = safe_hash(Block),
    Expected = aec_blocks:root_hash(Block),
    {Micros, Raw} = timer:tc(fun() -> safe_call(fun() -> aec_chain_state:repair_block_state(Block) end) end),
    Status = classify(Raw),
    Entry = #{height => Height, hash => Hash, type => Type,
              expected => Expected, status => Status, micros => Micros},
    case Status of
        ok -> repair_blocks(Rest, [Entry | Acc]);
        _  -> {error, Entry, lists:reverse(Acc)}
    end.

%%%===================================================================
%%% Result classification — tolerant of however deeply aec_chain_state
%%% wraps its abort reason; never assert on the exact shape.
%%%===================================================================

safe_call(F) ->
    try F() catch C:R:St -> {caught, C, R, St} end.

classify({ok, _})                    -> ok;
classify({ok, _, _})                 -> ok;
classify({pof, _, _})                -> ok;
classify({pof, _, _, _})             -> ok;
classify(ok)                         -> ok;
classify({error, already_in_db})     -> ok;
classify({error, Reason}) ->
    case find_mismatch(Reason) of
        {Got, Exp} -> {mismatch, Got, Exp};
        none       -> {error, Reason}
    end;
classify({caught, C, R, St})         -> {error, {caught, C, R, St}};
classify(Other)                      -> {error, Other}.

find_mismatch({root_hash_mismatch, Got, Exp})    -> {Got, Exp};
find_mismatch({aec_chain_state_error, Inner})    -> find_mismatch(Inner);
find_mismatch(_)                                 -> none.

safe_height(Block) -> aec_blocks:height(Block).

safe_hash(Block) ->
    case aec_blocks:hash_internal_representation(Block) of
        {ok, H} -> H;
        _       -> undefined
    end.

%%%===================================================================
%%% Reporting
%%%===================================================================

-spec ok_entries(report()) -> [entry()].
ok_entries({ok, Entries})       -> Entries;
ok_entries({error, _, Entries}) -> Entries.

-spec summarize(report()) -> map().
summarize({ok, Entries}) ->
    #{ result => pass
     , n_blocks => length(Entries)
     , n_key => length([1 || #{type := key} <- Entries])
     , n_micro => length([1 || #{type := micro} <- Entries])
     , total_micros => lists:sum([M || #{micros := M} <- Entries])
     };
summarize({error, Bad, Entries}) ->
    #{ result => fail
     , n_blocks_ok => length(Entries)
     , failed_at => Bad
     }.

-spec format_entry(entry()) -> string().
format_entry(#{height := H, hash := Hash, type := T, expected := Exp,
               status := Status, micros := Us}) ->
    io_lib:format(
      "height=~p type=~p hash=~s expected_root=~s status=~s (~p us)",
      [H, T, short(Hash), short(Exp), fmt_status(Status), Us]).

fmt_status(ok) -> "OK";
fmt_status({mismatch, Got, Exp}) ->
    io_lib:format("ROOT_HASH_MISMATCH got=~s expected=~s", [short(Got), short(Exp)]);
fmt_status({error, Reason}) ->
    io_lib:format("ERROR ~p", [Reason]).

short(undefined) -> "undefined";
short(Bin) when is_binary(Bin), byte_size(Bin) > 8 ->
    <<Head:8/binary, _/binary>> = Bin,
    [binary_to_list(base64_url(Head)), "..."];
short(Bin) when is_binary(Bin) -> binary_to_list(base64_url(Bin));
short(Other) -> io_lib:format("~p", [Other]).

base64_url(Bin) -> base64:encode(Bin).

-spec print_report(report()) -> ok.
print_report({ok, Entries} = R) ->
    #{n_blocks := N, n_key := NK, n_micro := NM, total_micros := Us} = summarize(R),
    io:format("REPLAY PASS: ~p/~p blocks byte-identical (key=~p micro=~p, ~p us total)~n",
               [N, N, NK, NM, Us]),
    lists:foreach(fun(E) -> io:format("  ~s~n", [format_entry(E)]) end, Entries),
    ok;
print_report({error, Bad, Entries} = R) ->
    #{n_blocks_ok := N} = summarize(R),
    io:format(standard_error,
               "REPLAY FAIL after ~p good blocks — DIVERGENCE:~n  ~s~n",
               [N, format_entry(Bad)]),
    io:format(standard_error, "Last 5 good blocks before divergence:~n", []),
    lists:foreach(fun(E) -> io:format(standard_error, "  ~s~n", [format_entry(E)]) end,
                  lists:sublist(lists:reverse(Entries), 5)),
    ok.
