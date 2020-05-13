-module(aec_chain_metrics_probe).
-behavior(exometer_probe).

-export([behaviour/0,
         probe_init/3,
         probe_terminate/1,
         probe_setopts/3,
         probe_update/2,
         probe_get_value/2,
         probe_get_datapoints/1,
         probe_reset/1,
         probe_sample/1,
         probe_handle_msg/2,
         probe_code_change/3]).

-export([ad_hoc_spec/0]).

-ifdef(TEST).
-export([sample/0]).
-endif.

-include_lib("exometer_core/include/exometer.hrl").
-include("blocks.hrl").

-define(DATAPOINTS, [ total_difficulty
                    , n_forks
                    , rel_fork_heights
                    , tot_fork_heights ]).

-record(st, {
          last_time_block_created,
          last_time_top_changed,
          datapoints = ?DATAPOINTS,
          data = [],
          ref
         }).

ad_hoc_spec() ->
    [{module, ?MODULE},
     {type, probe},
     {sample_interval, 5000}].



-spec behaviour() -> exometer:behaviour().
behaviour() ->
    probe.

probe_init(_, _, Opts) ->
    lager:debug("chain_metrics probe initialized", []),
    aec_events:subscribe(block_created),
    aec_events:subscribe(top_changed),
    DP = proplists:get_value(datapoints, Opts, ?DATAPOINTS),
    {ok, #st{datapoints = DP}}.

probe_terminate(Reason) ->
    lager:debug("chain_metrics probe terminating: ~p", [Reason]),
    ok.

probe_get_value(DPs, #st{data = Data0,
                         datapoints = DPs0} = S) ->
    Data1 = if Data0 =:= undefined ->
                    sample();
               true -> Data0
            end,
    DPs1 = if DPs =:= default -> DPs0;
              true -> DPs
           end,
    {ok, probe_get_value_(Data1, DPs1), S#st{data = Data1}}.

probe_get_value_(Data, DPs) ->
    [D || {K,_} = D <- Data,
          lists:member(K, DPs)].

probe_get_datapoints(#st{datapoints = DPs}) ->
    {ok, DPs}.

probe_update(_, _) ->
    {error, not_supported}.

probe_reset(S) ->
    {ok, S#st{data = []}}.

probe_sample(#st{} = S) ->
    {_Pid, Ref} = spawn_monitor(
                    fun() ->
                            exit({sample, sample()})
                    end),
    {ok, S#st{ref = Ref}}.

probe_setopts(_Entry, Opts, S) ->
    DPs = proplists:get_value(datapoints, Opts, S#st.datapoints),
    {ok, S#st{datapoints = DPs}}.

probe_handle_msg({'DOWN', Ref, _, _, SampleRes}, #st{ref = Ref} = S) ->
    case SampleRes of
        {sample, Data} ->
            {ok, S#st{ref = undefined, data = Data}};
        Other ->
            lager:debug("sampler died: ~p", [Other]),
            {ok, S#st{ref = undefined}}
    end;
probe_handle_msg({gproc_ps_event, block_created, #{time := T}},
                 #st{last_time_block_created = LastT} = S) ->
    if LastT =:= undefined ->
            ok;
       true ->
            Diff = max(0, timer:now_diff(T, LastT) div 1000),
            aec_metrics:try_update([ae,epoch,aecore,mining,interval], Diff)
    end,
    {ok, S#st{last_time_block_created = T}};
probe_handle_msg({gproc_ps_event, top_changed, #{time := T}},
                 #st{last_time_top_changed = LastT} = S) ->
    if LastT =:= undefined ->
            ok;
       true ->
            Diff = max(0, timer:now_diff(T, LastT) div 1000),
            aec_metrics:try_update(
              [ae,epoch,aecore,chain,top_change,interval], Diff)
    end,
    {ok, S#st{last_time_top_changed = T}};
probe_handle_msg(_Msg, S) ->
    lager:debug("Unknown msg: ~p", [_Msg]),
    {ok, S}.

probe_code_change(_, S, _) ->
    {ok, S}.

sample() ->
    aec_db:ensure_activity(
      async_dirty,
      fun() ->
              [ {total_difficulty, total_difficulty()}
              | forks_at_height() ]
      end).

total_difficulty() ->
    try aec_chain:difficulty_at_top_block() of
        {ok, V} -> V
    catch
        error:_ -> 0
    end.

forks_at_height() ->
    NHeights = 2,
    aec_db:ensure_activity(
      async_dirty,
      fun() ->
              Top = aec_chain:top_block_hash(),
              Height = height(Top),
              Found =
                  lists:foldl(
                    fun forks_at_height/2, #{},
                    lists:seq(Height, Height - NHeights + 1, -1)),
              group_forks(Found, Top)
      end).

forks_at_height(Height, Acc) ->
    Found = aec_db:find_headers_and_hash_at_height(Height),
    KeyBlocks = [Hash || {Hdr, Hash} <- Found,
                         aec_headers:type(Hdr) == key ],
    lists:foldl(
      fun(BHash, Acc1) ->
              case aec_db:find_block_fork_id(BHash) of
                  none ->
                      Acc1;
                  {value, Id} ->
                      %% Since we (may) include >1 heights, and inspect heights
                      %% in descending order, Acc takes priority
                      case maps:is_key(Id, Acc1) of
                          true -> Acc1;
                          false ->
                              case aec_db:find_header(Id) of
                                  {value, FH} ->
                                      FI = #{ height => aec_headers:height(FH)
                                            , prev   => aec_headers:prev_key_hash(FH) },
                                      Acc1#{ Id => #{ height     => Height
                                                    , block_hash => BHash
                                                    , fork_info  => FI } };
                                  none ->
                                      %% ??
                                      Acc1
                              end
                      end
              end
      end, Acc, KeyBlocks).

group_forks(Forks, Top) ->
    {value, MainForkId} = aec_db:find_block_fork_id(Top),
    {#{ height := TopHeight
      , fork_info := #{height := MFHeight}} = MF, Rest} = maps:take(MainForkId, Forks),
    Acc0 = [#{ value => TopHeight
             , tags  => [{id, fmt_tag(MainForkId)}] }],
    {RelHeights, TotHeights} =
        maps:fold(
          fun(FId, #{ height := Height
                    , block_hash := BHash } = Info, {RHs, THs} = Acc) ->
                  case aec_chain_state:find_common_ancestor(Top, BHash) of
                      {ok, ForkHash} ->
                          I = #{ tags => [{id, fmt_tag(FId)}] },
                          ForkHeight = height(ForkHash),
                          { [I#{ value => Height - ForkHeight } | RHs]
                          , [I#{ value => Height } | THs] };
                      {error, _} ->
                          lager:debug("No common ancestor: ~p, ~p",
                                      [FId, MainForkId]),
                          Acc
                  end
          end, {Acc0, Acc0}, Rest),
    [ {rel_fork_heights, RelHeights}
    , {tot_fork_heights, TotHeights}
    , {n_forks, length(TotHeights)} ].

height(Hash) ->
    {value, Hdr} = aec_db:find_header(Hash),
    aec_headers:height(Hdr).

fmt_tag(Hash) ->
    aeser_api_encoder:encode(key_block_hash, Hash).
