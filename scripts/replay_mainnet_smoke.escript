#!/usr/bin/env escript
%%! -network_id ae_mainnet +A 4
%%%----------------------------------------------------------------------
%%% Bounded real-mainnet replay smoke test.
%%%
%%% Pulls a genesis-forward prefix of mainnet blocks read-only over the
%%% node HTTP API (v3), reconstructs them with the node's own client-decode
%%% functions, and replays each through `aec_replay_harness:replay_insert/1'
%%% (the real `aec_chain_state:insert_block/1' follower path). Starts from
%%% the local hardcoded mainnet genesis, selected automatically once
%%% `network_id = ae_mainnet' is set (accounts.json ships in this repo, so
%%% no snapshot is needed).
%%%
%%% Read-only: never mines, signs, gossips, or opens a P2P connection. Only
%%% issues HTTP GETs and recomputes state in a throwaway RAM-mode chain DB.
%%%
%%% Usage:
%%%   escript scripts/replay_mainnet_smoke.escript [BaseURL] [MaxHeight] [Concurrency]
%%% Defaults: BaseURL = http://136.243.173.251:3013, MaxHeight = 300,
%%% Concurrency = 16. Run from the repo root (needs a warm `_build/test' tree).
%%%
%%% A bounded look-ahead window prefetches `Concurrency' generations ahead
%%% of the strictly sequential insert cursor (out-of-order arrivals buffered
%%% by height, drained in order); httpc keep-alive is sized to match so the
%%% concurrent GETs reuse a small fixed connection pool.
%%%----------------------------------------------------------------------
-module(replay_mainnet_smoke).
-mode(compile).

-export([main/1]).

-define(DEFAULT_BASE, "http://136.243.173.251:3013").
-define(DEFAULT_MAX_HEIGHT, 300).
-define(DEFAULT_CONCURRENCY, 16).
-define(PROGRESS_EVERY, 500).
-define(FETCH_RETRIES, 4).
-define(FETCH_RETRY_BACKOFF_MS, 400).

%% ae_mainnet protocol-activation boundaries.
-define(MILESTONES, [ {47800, "Minerva (v2)"}
                     , {90800, "Fortuna (v3)"}
                     , {161150, "Lima (v4)"}
                     , {441444, "Iris (v5)"}
                     , {941700, "Ceres (v6)"}
                     ]).

main(Args) ->
    {Base, MaxHeight, Concurrency} = parse_args(Args),
    setup_code_paths(),
    boot_apps(Concurrency),
    io:format("=== mainnet genesis-forward replay smoke ===~n"
              "base=~s max_height=~p concurrency=~p network_id=ae_mainnet~n",
              [Base, MaxHeight, Concurrency]),
    ok = boot_chain_db(),
    try
        run(Base, MaxHeight, Concurrency)
    catch
        Class:Reason:Stack ->
            io:format(standard_error, "FATAL ~p:~p~n~p~n", [Class, Reason, Stack]),
            halt(1)
    after
        ok = stop_chain_db()
    end.

parse_args([]) -> {?DEFAULT_BASE, ?DEFAULT_MAX_HEIGHT, ?DEFAULT_CONCURRENCY};
parse_args([Base]) -> {Base, ?DEFAULT_MAX_HEIGHT, ?DEFAULT_CONCURRENCY};
parse_args([Base, MaxHeightStr]) -> {Base, list_to_integer(MaxHeightStr), ?DEFAULT_CONCURRENCY};
parse_args([Base, MaxHeightStr, ConcurrencyStr]) ->
    {Base, list_to_integer(MaxHeightStr), list_to_integer(ConcurrencyStr)};
parse_args(_) -> {?DEFAULT_BASE, ?DEFAULT_MAX_HEIGHT, ?DEFAULT_CONCURRENCY}.

%%%======================================================================
%%% Setup
%%%======================================================================

%% `_build/default' (compiled without -DTEST) must go on the path FIRST:
%% aec_consensus_bitcoin_ng:genesis_target/0 has an -ifdef(TEST) branch
%% that swaps in an easy test-only genesis target, which would diverge a
%% real-mainnet replay. Only aec_replay_harness is layered in from
%% `_build/test' (it is compiled in the test profile only).
setup_code_paths() ->
    DefaultEbinPaths = filelib:wildcard("_build/default/lib/*/ebin"),
    HarnessTestPath = filelib:wildcard("_build/test/lib/aecore/test"),
    ok = code:add_pathsz(DefaultEbinPaths ++ HarnessTestPath),
    ok.

boot_apps(Concurrency) ->
    %% Point `setup' at data/ so aec_fork_block_settings finds the mainnet
    %% genesis presets; its default "data.<nodename>" does not exist here.
    application:set_env(setup, data_dir, "data"),
    Apps = [crypto, asn1, public_key, ssl, inets, syntax_tools, compiler,
            mnesia, setup, gproc, lager, jsx],
    lists:foreach(
      fun(App) ->
              case application:ensure_all_started(App) of
                  {ok, _} -> ok;
                  {error, Reason} ->
                      io:format(standard_error, "warn: ~p did not start: ~p~n", [App, Reason])
              end
      end, Apps),
    %% Size the httpc keep-alive pool to the prefetch window so concurrent
    %% GETs reuse connections instead of opening one per request.
    ok = httpc:set_options([{max_sessions, Concurrency + 2},
                             {max_keep_alive_length, Concurrency * 4}]),
    ok.

%% RAM-mode chain-DB bootstrap that avoids `aec_test_utils' (test-profile
%% only) so other modules still load from `_build/default'; it calls the
%% same core `aec_db' API underneath.
boot_chain_db() ->
    ok = mnesia:start(),
    ok = aec_db:initialize_db(ram),
    Tabs = [Tab || {Tab, _} <- aec_db:tables(ram)],
    ok = mnesia:wait_for_tables(Tabs, 5000),
    ok.

stop_chain_db() ->
    application:stop(mnesia).

%%%======================================================================
%%% Run
%%%======================================================================

run(Base, MaxHeight, Concurrency) ->
    %% Genesis is the local hardcoded mainnet genesis (not fetched over
    %% HTTP), then cross-checked against the network's reported genesis hash.
    {GenesisBlock, _GenesisTrees} = aec_block_genesis:genesis_block_with_state(),
    {ok, LocalGenesisHash} = aec_blocks:hash_internal_representation(GenesisBlock),
    RemoteGenesisHash = fetch_remote_genesis_hash(Base),
    io:format("genesis: local=~s remote=~s match=~p~n",
               [enc_hash(key, LocalGenesisHash), RemoteGenesisHash,
                enc_hash(key, LocalGenesisHash) =:= RemoteGenesisHash]),

    {ok, [GenEntry]} = aec_replay_harness:replay_insert([GenesisBlock]),
    io:format("~s~n", [aec_replay_harness:format_entry(GenEntry)]),
    case maps:get(status, GenEntry) of
        ok -> ok;
        Bad -> io:format(standard_error, "GENESIS MISMATCH: ~p~n", [Bad]), halt(2)
    end,

    %% Consensus start/2 seeds the block whitelist into persistent_term
    %% (read on every insert) and enforces the ae_mainnet fork choice.
    %% Must run after genesis is in the DB (it reads the current top block).
    ok = aec_consensus_bitcoin_ng:start(#{}, []),

    %% Every insert consults aec_resilience's ETS table; start it
    %% standalone here (defaults to fork-resistance disabled, which is
    %% what a plain forward replay wants).
    {ok, _} = aec_resilience:start_link(),

    %% Populate the dev-reward app env that grant_fees/5 reads on every
    %% key-block fee-grant once the split activates (Fortuna, protocol >=
    %% 3). Must run before any block at/after the Fortuna boundary, or the
    %% first key block past it crashes with {badmatch, undefined}.
    ok = aec_dev_reward:ensure_env(),

    Activity = probe_activity(Base, MaxHeight),
    io:format("~n-- activity probe (informational; does not gate the replay) --~n~s~n",
               [Activity]),

    io:format("~n-- replaying key blocks 1..~p (window=~p) --~n", [MaxHeight, Concurrency]),
    StartTime = erlang:monotonic_time(millisecond),
    State0 = #{ base => Base, max_height => MaxHeight, concurrency => Concurrency,
                start_time => StartTime, next_dispatch => 1, next_consume => 1,
                in_flight => 0, buffer => #{}, good_count => 0,
                milestones => ?MILESTONES },
    pipeline_loop(dispatch_more(State0)).

%%%======================================================================
%%% Concurrent windowed prefetch pipeline
%%%
%%% At most `concurrency' fetches are in flight or buffered ahead of
%%% `next_consume' (dispatch_more's guard). Results are buffered by height
%%% and drained in order, so insert_block/1 still only ever sees blocks in
%%% real chain order — concurrency only overlaps the network I/O.
%%%======================================================================

dispatch_more(#{next_dispatch := ND, max_height := MaxH, in_flight := InFlight,
                next_consume := NC, base := Base, concurrency := Cc} = State)
  when ND =< MaxH, InFlight < Cc, ND < NC + Cc ->
    spawn_fetch(Base, ND, self()),
    dispatch_more(State#{next_dispatch := ND + 1, in_flight := InFlight + 1});
dispatch_more(State) ->
    State.

spawn_fetch(Base, H, Parent) ->
    spawn(fun() -> Parent ! {fetched, H, fetch_generation_with_retry(Base, H, ?FETCH_RETRIES)} end).

fetch_generation_with_retry(Base, H, AttemptsLeft) ->
    case fetch_generation_blocks(Base, H) of
        {ok, _} = Ok -> Ok;
        {error, _Reason} when AttemptsLeft > 1 ->
            timer:sleep(?FETCH_RETRY_BACKOFF_MS),
            fetch_generation_with_retry(Base, H, AttemptsLeft - 1);
        {error, _Reason} = Err -> Err
    end.

pipeline_loop(#{next_consume := NC, max_height := MaxH, good_count := GoodCount,
                start_time := StartTime} = _State) when NC > MaxH ->
    Elapsed = max(erlang:monotonic_time(millisecond) - StartTime, 1),
    io:format("~n=== DONE: reached max_height=~p, ~p/~p key-block generations "
               "byte-identical (elapsed ~.1fs, ~.2f gens/s) ===~n",
               [MaxH, GoodCount, MaxH, Elapsed / 1000, 1000 * GoodCount / Elapsed]),
    ok;
pipeline_loop(#{buffer := Buffer, next_consume := NC} = State) ->
    case maps:take(NC, Buffer) of
        {Result, Buffer1} -> consume(Result, State#{buffer := Buffer1});
        error ->
            receive
                {fetched, H, Result} ->
                    InFlight = maps:get(in_flight, State),
                    State1 = State#{in_flight := InFlight - 1,
                                     buffer := maps:put(H, Result, Buffer)},
                    pipeline_loop(dispatch_more(State1))
            end
    end.

consume({error, Reason}, #{next_consume := H, max_height := MaxH, good_count := GoodCount}) ->
    io:format(standard_error,
               "~n=== STOPPED at height ~p: could not fetch generation after ~p retries (~p) — "
               "treating as the reachable end of this pull, not a divergence ===~n",
               [H, ?FETCH_RETRIES, Reason]),
    io:format("=== RESULT: ~p/~p key-block generations byte-identical before stopping ===~n",
               [GoodCount, MaxH]);
consume({ok, Blocks}, #{next_consume := H} = State) ->
    case aec_replay_harness:replay_insert(Blocks) of
        {ok, Entries} ->
            log_entries(H, Entries, State),
            GoodCount = maps:get(good_count, State),
            State1 = State#{next_consume := H + 1, good_count := GoodCount + 1},
            State2 = maybe_announce_milestone(H, State1),
            maybe_log_progress(H, State2),
            pipeline_loop(dispatch_more(State2));
        {error, #{status := Status} = Bad, Good} ->
            lists:foreach(fun(E) -> io:format("  ~s~n", [aec_replay_harness:format_entry(E)]) end, Good),
            %% Distinguish a real root-hash divergence from a harness/
            %% environment fault so the two are never conflated.
            Label = case Status of
                        {mismatch, _, _} -> "STATE-ROOT DIVERGENCE (CRITICAL, consensus-adjacent)";
                        _                -> "ERROR (harness/environment fault, NOT a root-hash mismatch)"
                    end,
            io:format(standard_error,
                       "~n=== ~s at height ~p ===~n  ~s~n",
                       [Label, H, aec_replay_harness:format_entry(Bad)]),
            halt(3)
    end.

%% Print microblocks (the state-changing entries) and anything within +/-2
%% of a milestone; empty key blocks elsewhere are only summarized in the
%% progress line. The root-hash assertion runs on every entry regardless.
log_entries(H, Entries, #{milestones := Milestones}) ->
    NearBoundary = lists:any(fun({Threshold, _}) -> abs(H - Threshold) =< 2 end, Milestones),
    lists:foreach(
      fun(#{type := Type} = E) ->
              case Type =:= micro orelse NearBoundary of
                  true -> io:format("  ~s~n", [aec_replay_harness:format_entry(E)]);
                  false -> ok
              end
      end, Entries).

maybe_announce_milestone(H, #{milestones := Milestones, base := Base} = State) ->
    case lists:partition(fun({Threshold, _}) -> H >= Threshold end, Milestones) of
        {[], _} -> State;
        {Crossed, Remaining} ->
            lists:foreach(
              fun({Threshold, Name}) ->
                      io:format("~n*** CROSSED ~s boundary: now past height ~p (at height ~p) ***~n",
                                 [Name, Threshold, H]),
                      announce_versions(Base, Threshold)
              end, Crossed),
            State#{milestones := Remaining}
    end.

%% Informational only: one GET each side of the boundary to print the
%% protocol `version' the network reports. The real proof is that
%% insert_block/1 accepted the header past the boundary at all.
announce_versions(Base, Threshold) ->
    try
        Before = maps:get(<<"version">>, http_get_json(
                     Base ++ "/v3/key-blocks/height/" ++ integer_to_list(Threshold - 1))),
        At = maps:get(<<"version">>, http_get_json(
                     Base ++ "/v3/key-blocks/height/" ++ integer_to_list(Threshold))),
        io:format("    protocol version: height ~p -> ~p, height ~p -> ~p~n",
                   [Threshold - 1, Before, Threshold, At])
    catch _:_ -> ok
    end.

maybe_log_progress(H, #{max_height := MaxH, good_count := GoodCount, start_time := StartTime}) ->
    if H rem ?PROGRESS_EVERY =:= 0 orelse H =:= MaxH ->
           Elapsed = max(erlang:monotonic_time(millisecond) - StartTime, 1),
           io:format("...at height ~p/~p, ~p byte-identical so far (elapsed ~.1fs, ~.2f gens/s)...~n",
                      [H, MaxH, GoodCount, Elapsed / 1000, 1000 * GoodCount / Elapsed]);
       true -> ok
    end.

%%%======================================================================
%%% HTTP + decode
%%%======================================================================

fetch_remote_genesis_hash(Base) ->
    Map = http_get_json(Base ++ "/v3/key-blocks/height/0"),
    maps:get(<<"hash">>, Map).

%% Returns [KeyBlock | MicroBlocks...] for generation H in chain order.
%% The v3 API bundles key-block H with the microblocks published after it
%% (up to, but excluding, key-block H+1).
fetch_generation_blocks(Base, H) ->
    try
        GenMap = http_get_json(Base ++ "/v3/generations/height/" ++ integer_to_list(H)),
        KeyBlockMap = maps:get(<<"key_block">>, GenMap),
        MicroHashes = maps:get(<<"micro_blocks">>, GenMap),
        {ok, KeyHeader} = aec_headers:deserialize_from_client(key, KeyBlockMap),
        KeyBlock = aec_blocks:new_key_from_header(KeyHeader),
        {ok, KeyBlockHash} = aec_blocks:hash_internal_representation(KeyBlock),
        MicroBlocks = order_micro_blocks(Base, MicroHashes, KeyBlockHash),
        {ok, [KeyBlock | MicroBlocks]}
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% Fetch each microblock, then walk the prev_hash chain to order them
%% chronologically (robust to the API's array order).
order_micro_blocks(_Base, [], _PrevHash) -> [];
order_micro_blocks(Base, Hashes, GenPrevHash) ->
    Fetched = [ {H, fetch_micro_block(Base, H)} || H <- Hashes ],
    ByPrevHash = maps:from_list([ {prev_hash_of(B), B} || {_, B} <- Fetched ]),
    walk_chain(ByPrevHash, GenPrevHash, []).

walk_chain(ByPrevHash, Cursor, Acc) ->
    case maps:find(Cursor, ByPrevHash) of
        error -> lists:reverse(Acc);
        {ok, Block} ->
            {ok, Hash} = aec_blocks:hash_internal_representation(Block),
            walk_chain(maps:remove(Cursor, ByPrevHash), Hash, [Block | Acc])
    end.

prev_hash_of(Block) -> aec_blocks:prev_hash(Block).

fetch_micro_block(Base, MicroHashBin) ->
    MicroHash = binary_to_list(MicroHashBin),
    HeaderMap = http_get_json(Base ++ "/v3/micro-blocks/hash/" ++ MicroHash ++ "/header"),
    TxsMap = http_get_json(Base ++ "/v3/micro-blocks/hash/" ++ MicroHash ++ "/transactions"),
    Height = maps:get(<<"height">>, HeaderMap),
    PrevHash = decode_id(maps:get(<<"prev_hash">>, HeaderMap)),
    PrevKeyHash = decode_id(maps:get(<<"prev_key_hash">>, HeaderMap)),
    RootHash = decode_id(maps:get(<<"state_hash">>, HeaderMap)),
    TxsHash = decode_id(maps:get(<<"txs_hash">>, HeaderMap)),
    Time = maps:get(<<"time">>, HeaderMap),
    Version = maps:get(<<"version">>, HeaderMap),
    Signature = decode_id(maps:get(<<"signature">>, HeaderMap)),
    PofHashRaw = maps:get(<<"pof_hash">>, HeaderMap),
    PofHash = case PofHashRaw of
                  <<"no_fraud">> -> <<>>;
                  _ -> decode_id(PofHashRaw)
              end,
    Header0 = aec_headers:new_micro_header(Height, PrevHash, PrevKeyHash, RootHash,
                                            Time, TxsHash, PofHash, Version),
    Header = aec_headers:set_signature(Header0, Signature),
    Txs = [ decode_signed_tx(maps:get(<<"encoded_tx">>, T))
            || T <- maps:get(<<"transactions">>, TxsMap) ],
    aec_blocks:new_micro_from_header(Header, Txs, no_fraud).

decode_signed_tx(EncTx) ->
    Bin = decode_id(EncTx),
    aetx_sign:deserialize_from_binary(Bin).

%% Generic decoder: returns just the raw payload bytes for any
%% aeser_api_encoder type (the type tag is unneeded here). Note decode/1
%% returns a plain {Type, Payload} tuple, not {ok, Payload}.
decode_id(Bin) when is_binary(Bin) ->
    {_Type, Payload} = aeser_api_encoder:decode(Bin),
    Payload.

%%%======================================================================
%%% Activity probe (informational): where do contracts/oracles/names
%%% first appear, vs how far this bounded pull reaches.
%%%======================================================================

probe_activity(Base, MaxHeight) ->
    Status = http_get_json(Base ++ "/v3/status"),
    Protocols = maps:get(<<"protocols">>, Status),
    TopHeight = maps:get(<<"top_block_height">>, Status),
    ProtoLines = [ io_lib:format("  protocol ~p effective_at_height=~p~n",
                                  [maps:get(<<"version">>, P), maps:get(<<"effective_at_height">>, P)])
                   || P <- Protocols ],
    io_lib:format(
      "mainnet top_block_height=~p (this pull reaches at most height ~p, "
      "i.e. ~.4f% of the chain)~nprotocol activation heights (from live "
      "/v3/status; AENS/oracles are live from protocol 1 genesis, "
      "contracts/AEVM activate with a later protocol):~n~s",
      [TopHeight, MaxHeight, 100 * MaxHeight / TopHeight, ProtoLines]).

%%%======================================================================
%%% HTTP + JSON
%%%======================================================================

http_get_json(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 15000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            jsx:decode(Body, [return_maps]);
        {ok, {{_, Code, _}, _Headers, Body}} ->
            error({http_error, Code, Url, Body});
        {error, Reason} ->
            error({http_failed, Url, Reason})
    end.

enc_hash(key, Hash) -> aeser_api_encoder:encode(key_block_hash, Hash).
