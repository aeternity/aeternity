%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%% @doc
%%% Aggregator for statistics about remote peers
%%% It tries to connect to as much peers as possible and tracks stats about peers
%%% @end
%%%-------------------------------------------------------------------
-module(aec_peer_analytics).

-behaviour(gen_server).

%% Genserver
-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% Peer stats
-export([ log_peer_status/4
        , log_temporary_peer_status/4
        , get_stats/0
        , get_stats_for_client/0
        , enabled/0
        , pending_requests/0
        ]).

-type peer_stat() :: #{ host => string() | binary()
                      , port => aec_peer:peer()
                      , first_seen => non_neg_integer()
                      , last_seen => non_neg_integer()
                      , genesis_hash => binary()
                      , top_hash => binary()
                      , difficulty => non_neg_integer()
                      , info => any()
                      }.
-type peer_pub() :: binary().
-type state() :: #{ enabled => boolean()
                  , stats => #{ aec_keys:pubkey() => peer_stat() }
                  , probes_pids => #{ peer_pub() => pid() }
                  , probes_mref => #{ reference() => peer_pub() }
                  }.

log_temporary_peer_status(S, GHash, THash, Diff) ->
    gen_server:cast(?MODULE, {log_temporary_peer_status, S, GHash, THash, Diff}).

log_peer_status(S, GHash, THash, Diff) ->
    gen_server:cast(?MODULE, {log_peer_status, S, GHash, THash, Diff}).

log_peer_info(Pub, NetID, Ver, Os, Rev, Vendor) ->
    gen_server:call(?MODULE, {log_peer_info, Pub, NetID, Ver, Os, Rev, Vendor}).

log_peer_info_unknown(Pub) ->
    gen_server:call(?MODULE, {log_peer_info_unknown, Pub}).

get_stats() -> gen_server:call(?MODULE, get_stats).

get_stats_for_client() ->
    maps:fold(fun(K, V, Acc) ->
        Acc#{ aeser_api_encoder:encode(peer_pubkey, K) => encode_peer_details_for_client(V) }
              end, #{}, get_stats()).

encode_peer_details_for_client(#{ host := Host
                                 , port := Port
                                 , first_seen := FT
                                 , last_seen := LT
                                 , genesis_hash := GH
                                 , top_hash := TH
                                 , difficulty := D
                                 , info := Info}) ->
    EInfo = encode_peer_info_for_client(Info),
    EStatus = #{ <<"host">> => Host
               , <<"port">> => Port
               , <<"first_seen">> => FT
               , <<"last_seen">> => LT
               , <<"genesis_hash">> => aeser_api_encoder:encode(key_block_hash, GH)
               , <<"top_hash">> => aeser_api_encoder:encode(key_block_hash, TH)
               , <<"top_difficulty">> => D
               },
    maps:merge(EStatus, EInfo).

encode_peer_info_for_client(unresolved) -> #{};
encode_peer_info_for_client(unknown) -> #{};
encode_peer_info_for_client(#{ network_id := NetID
                             , version := Ver
                             , os := Os
                             , revision := Rev
                             , vendor := Vendor
                             }) ->
    #{ <<"network_id">> => NetID
     , <<"node_version">> => Ver
     , <<"node_revision">> => Rev
     , <<"node_vendor">> => Vendor
     , <<"node_os">> => Os
     }.

pending_requests() -> gen_server:call(?MODULE, pending_requests).

enabled() ->
    {ok, Enabled} =
        aeu_env:find_config( [<<"sync">>, <<"peer_analytics">>]
                           , [user_config, schema_default, {value,false}]),
    Enabled.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Ask the nodes for their version every hour
time_between_version_probes() -> 60 * 60.

%% Maximum inactivity in seconds after which we garbage collect the stats
max_allowed_peer_inactivity() -> 24 * 60 * 60. %% If a node was last seen a day ago then garbage collect it

-spec init(any()) -> {ok, state()}.
init(_) ->
    Enabled = enabled(),
    case Enabled of
        true ->
            lager:info("Enabling detailed peer statistics"),
            application:set_env(aecore, sync_max_outbound, 1000000),
            application:set_env(aecore, sync_max_inbound, 1000000);
        false -> lager:info("Detailed peer statistics are disabled")
    end,
    timer:send_interval((max_allowed_peer_inactivity() + 5 * 60) * 1000, sweep),
    {ok, #{enabled => Enabled, stats => #{}, probes_pids => #{}, probes_mref => #{}}}.

terminate(_Reason, _St) -> ok.
code_change(_FromVsn, St, _Extra) -> {ok, St}.

handle_call({log_peer_info, Pub, NetID, Ver, Os, Rev, Vendor}, _From, #{ stats := Stats } = St) ->
    Info = #{ network_id => NetID
            , version => Ver
            , os => Os
            , revision => Rev
            , vendor => Vendor
            },
    {reply, ok, St#{stats => update_peer_info(Stats, Pub, Info)}};
handle_call({log_peer_info_unknown, Pub}, _From, #{ stats := Stats } = St) ->
    {reply, ok, St#{stats => update_peer_info(Stats, Pub, unknown)}};
handle_call(get_stats, _From, #{stats := Stats } = St) -> {reply, Stats, St};
handle_call(pending_requests, _From, #{probes_pids := PPids} = St) -> {reply, maps:size(PPids) /= 0, St};
handle_call(Msg, _From, #{} = St) ->
    lager:debug("Unhandled call in peer analytics ~p", [Msg]),
    {noreply, St}.

handle_info({'DOWN', MRef, process, _, _}, #{probes_pids := PPids, probes_mref := PRefs} = St) ->
    Pub = maps:get(MRef, PRefs),
    {noreply, St#{probes_pids := maps:remove(Pub, PPids), probes_mref := maps:remove(MRef, PRefs)}};
handle_info(sweep, #{stats := Stats} = St) ->
    %% Garbage collected stats don't have a version probe pending
    Max = max_allowed_peer_inactivity(),
    {noreply, St#{stats => maps:filter(
        fun (_, #{first_seen := FT, last_seen := LT}) when LT-FT =< Max -> true;
            (_, _) -> false end,
        Stats)}}.

handle_cast(_Msg, #{enabled := false} = St) -> {noreply, St};
handle_cast({log_temporary_peer_status, #{r_pubkey := Pub, host := Host, port := Port}, GHash, THash, Diff}, St) ->
    St1 = log_ping(St, Pub, Host, Port, GHash, THash, Diff),
    {noreply, St1};
handle_cast({log_peer_status, #{r_pubkey := Pub, host := Host, port := Port}, GHash, THash, Diff}, St) ->
    St1 = log_ping(St, Pub, Host, Port, GHash, THash, Diff),
    St2 = maybe_schedule_version_probe(St1, Pub),
    {noreply, St2};
handle_cast(Msg, #{} = St) ->
    lager:debug("Unhandled cast in peer analytics ~p", [Msg]),
    {noreply, St}.

log_ping(#{stats := Stats} = St, Pub, Host, Port, GHash, THash, Diff) ->
    PS0 = maps:get(Pub, Stats, #{ info => unresolved, first_seen => unix_time() }),
    PS = PS0#{ host => Host
             , port => Port
             , last_seen => unix_time()
             , genesis_hash => GHash
             , top_hash => THash
             , difficulty => Diff
             },
    St#{stats => Stats#{ Pub => PS }}.

unix_time() ->
    erlang:monotonic_time(second) + erlang:time_offset(second).

maybe_schedule_version_probe(#{stats := Stats, probes_pids := PPids, probes_mref := MRefs} = St, Pub) ->
    #{first_seen := First, last_seen := Last, info := Info} = maps:get(Pub, Stats),
    case Last - First > time_between_version_probes() orelse Info =:= unresolved of
        false -> St;
        true ->
            case maps:find(Pub, PPids) of
                {ok, _} -> St; %% Already scheduled
                error ->
                    {Pid, MRef} = spawn_monitor(fun() ->
                        lager:debug("Scheduling peer version probe"),
                        case aec_peer_connection:get_node_info(Pub, 10000) of
                            {ok, #{network_id := NetID, node_version := Ver, os := Os, revision := Rev, vendor := Vendor}} ->
                                log_peer_info(Pub, NetID, Ver, Os, Rev, Vendor);
                            {error, no_connection} -> ok;
                            {error, _} ->
                                log_peer_info_unknown(Pub)
                        end end),
                    St#{probes_pids := maps:put(Pub, Pid, PPids), probes_mref := maps:put(MRef, Pub, MRefs)}
            end
    end.

update_peer_info(Stats, Pub, Key) ->
    maps:update_with(Pub, fun(V) -> V#{info => Key} end, Stats).
