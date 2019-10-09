-module(aec_hard_forks).

-export([ensure_env/0]).
-export([protocols/0,
         protocol_effective_at_height/1
        ]).

-ifdef(TEST).
-export([sorted_protocol_versions/0]).
-endif.

-include("blocks.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-define(is_version(V), (is_integer(V) andalso (V >= 0))).
-define(is_height(H), (is_integer(H) andalso (H >= ?GENESIS_HEIGHT))).

-type version() :: non_neg_integer().

%% Maps consensus protocol version to minimum height at which such
%% version is effective.  The height must be strictly increasing with
%% the version.
-type protocol_vsn() :: pos_integer()
                      | ?LIMA_PROTOCOL_VSN
                      | ?FORTUNA_PROTOCOL_VSN
                      | ?MINERVA_PROTOCOL_VSN
                      | ?ROMA_PROTOCOL_VSN.
-type protocols() :: #{protocol_vsn() => aec_blocks:height()}.

-type fork() :: #{signalling_start_height := aec_blocks:height(),
                  signalling_end_height   := aec_blocks:height(),
                  signalling_block_count  := pos_integer(),
                  info_field              := non_neg_integer(),
                  version                 := version()}.

-export_type([ protocols/0
             , protocol_vsn/0
             , fork/0
             ]).
%%%===================================================================
%%% API
%%%===================================================================

-spec protocols() -> protocols().
protocols() ->
    NetworkId = aec_governance:get_network_id(),
    protocols_from_network_id(NetworkId).

-spec ensure_env() -> ok.
ensure_env() ->
    NetworkId = aec_governance:get_network_id(),
    Protocols = protocols(),
    ForkConfig = fork_config(),
    assert_protocols(Protocols),
    assert_fork(NetworkId, ForkConfig, Protocols),
    case fork_from_network_id(NetworkId) of
        Fork when Fork =/= undefined ->
            application:set_env(aecore, fork, Fork);
        undefined ->
            ok
    end.

%% This function is supposed to be used only when:
%% - a new block is being added to the database (in aec_conductor);
%% - a new key block candidate is prepared (aec_block_key_candidate).
%% The function shouldn't be used elsewhere (apart from tests). With community
%% forks the function can return different protocol versions at the same height.
-spec protocol_effective_at_height(aec_blocks:height()) -> version().
protocol_effective_at_height(H) ->
    protocol_effective_at_height(H, protocols()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

protocols_from_network_id(<<"ae_mainnet">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     , ?MINERVA_PROTOCOL_VSN  => 47800
     , ?FORTUNA_PROTOCOL_VSN => 90800
     , ?LIMA_PROTOCOL_VSN => 161150
    };
protocols_from_network_id(<<"ae_uat">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     , ?MINERVA_PROTOCOL_VSN  => 40900
     , ?FORTUNA_PROTOCOL_VSN => 82900
     , ?LIMA_PROTOCOL_VSN =>  154300
     };
protocols_from_network_id(<<"local_roma_testnet">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     %%, ?MINERVA_PROTOCOL_VSN  => Excluded for testing old protocol
     %%, ?FORTUNA_PROTOCOL_VSN  => Excluded for testing old protocol
     %%, ?LIMA_PROTOCOL_VSN     => Excluded for testing old protocol
     };
protocols_from_network_id(<<"local_minerva_testnet">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     , ?MINERVA_PROTOCOL_VSN  => 1
     %%, ?FORTUNA_PROTOCOL_VSN  => Excluded for testing old protocol
     %%, ?LIMA_PROTOCOL_VSN     => Excluded for testing old protocol
     };
protocols_from_network_id(<<"local_lima_testnet">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     %%, ?MINERVA_PROTOCOL_VSN  => Excluded for testing new protocol
     %%, ?FORTUNA_PROTOCOL_VSN  => Excluded for testing new protocol
     , ?LIMA_PROTOCOL_VSN     => 1
     };
protocols_from_network_id(_ID) ->
    case aeu_env:user_map_or_env([<<"chain">>, <<"hard_forks">>], aecore, hard_forks, undefined) of
        undefined ->
            #{ ?ROMA_PROTOCOL_VSN     => 0
             , ?FORTUNA_PROTOCOL_VSN  => 1 %% Update after switching to LIMA
             };
        M when is_map(M) ->
            maps:fold(fun(K, V, Acc) ->
                              Acc#{binary_to_integer(K) => V}
                      end, #{}, M)
    end.

fork_from_network_id(<<"ae_mainnet">>) ->
    case fork_config() of
        #{enabled := true}  -> mainnet_fork_config();
        #{enabled := false} -> undefined;
        %% If no config in config file, signalling is enabled by default.
        undefined           -> mainnet_fork_config()
    end;
fork_from_network_id(<<"ae_uat">>) ->
    case fork_config() of
        #{enabled := true}  -> testnet_fork_config();
        #{enabled := false} -> undefined;
        undefined           -> testnet_fork_config()
    end;
fork_from_network_id(_Id) ->
    case fork_config() of
        #{enabled := true} = Config ->
            maps:without([enabled], Config);
        _Other ->
            undefined
    end.

mainnet_fork_config() ->
    #{signalling_start_height => undefined,
      signalling_end_height => undefined,
      signalling_block_count => undefined,
      info_field => undefined,
      version => undefined}.

testnet_fork_config() ->
    #{signalling_start_height => undefined,
      signalling_end_height => undefined,
      signalling_block_count => undefined,
      info_field => undefined,
      version => undefined}.

%% Exported for tests
sorted_protocol_versions() ->
    lists:sort(maps:keys(protocols())).

protocols_sorted_by_version(Ps) ->
    lists:keysort(1, maps:to_list(Ps)).

protocol_effective_at_height(H, Protocols) ->
    assert_height(H),
    SortedProtocols = protocols_sorted_by_version(Protocols),
    %% Find the last protocol version effective before or at the
    %% specified height: that is the one effective at the specified
    %% height.  This assumes that the height is strictly increasing
    %% with the version so also assert that for the sake of clarity.
    ProtocolsEffectiveSinceBeforeOrAtHeight = [_|_] =
        lists:takewhile(fun({_, HH}) -> H >= HH end, SortedProtocols),
    {V, _} = lists:last(ProtocolsEffectiveSinceBeforeOrAtHeight),
    V.

fork_config() ->
    case aeu_env:user_map([<<"fork_management">>, <<"fork">>]) of
        {ok, Config} ->
            maps:fold(fun(K, V, Acc) -> maps:put(conv_fork_config_key(K), V, Acc) end,
                      maps:new(), Config);
        undefined ->
            undefined
    end.

conv_fork_config_key(<<"enabled">>)                 -> enabled;
conv_fork_config_key(<<"signalling_start_height">>) -> signalling_start_height;
conv_fork_config_key(<<"signalling_end_height">>)   -> signalling_end_height;
conv_fork_config_key(<<"signalling_block_count">>)  -> signalling_block_count;
conv_fork_config_key(<<"info_field">>)              -> info_field;
conv_fork_config_key(<<"version">>)                 -> version.

assert_protocols(M) ->
    GenesisVersion = aec_block_genesis:version(),
    Vs = [GenesisVersion | _] = sorted_protocol_versions(),
    %% Assert versions are sorted.
    Vs = lists:sort(Vs),
    %% Assert versions are distinct.
    [] = lists:sort(Vs) -- lists:usort(Vs),
    {[], _} = {maps:keys(M) -- Vs, check_no_extra_protocol_versions},
    {[], _} = {Vs -- maps:keys(M), check_no_missing_protocol_versions},
    assert_heights_strictly_increasing(M),
    ok.

assert_height(H) ->
    case is_integer(H) andalso H >= aec_block_genesis:height() of
        true  -> ok;
        false -> error({illegal_height, H})
    end.

assert_heights_strictly_increasing(Ps) ->
    GenesisVersion = aec_block_genesis:version(),
    GenesisHeight = aec_block_genesis:height(),
    [{GenesisVersion, GenesisHeight} = G | VHs] =
        protocols_sorted_by_version(Ps),
    _ = lists:foldl(
          fun(P, PrevP) ->
                  {true, _} =
                      {is_valid_next_protocol(P, PrevP),
                       {check_protocol_height_strictly_increasing,
                        {PrevP, P}}},
                  P end,
          G, VHs),
    ok.

is_valid_next_protocol({CurV, CurH}, {PrevV, PrevH}) when CurV > PrevV,
                                                          CurH > PrevH ->
    true;
is_valid_next_protocol(_, _) ->
    false.

assert_fork(<<"ae_mainnet">>, Config, _Protocols) ->
    assert_only_enabled_key(Config);
assert_fork(<<"ae_uat">>, Config, _Protocols) ->
    assert_only_enabled_key(Config);
assert_fork(_Id, #{enabled := _Enabled,
                   signalling_start_height := SignallingStartHeight,
                   signalling_end_height := SignallingEndHeight,
                   signalling_block_count := SignallingBlockCount,
                   info_field := _InfoField,
                   version := Version}, Protocols) ->
    SortedProtocols = protocols_sorted_by_version(Protocols),
    {PrevVersion, PrevForkHeight} = lists:last(SortedProtocols),
    assert_fork_signalling_interval(SignallingStartHeight, SignallingEndHeight, PrevForkHeight),
    SignallingInterval = SignallingEndHeight - SignallingStartHeight,
    assert_fork_signalling_block_count(SignallingInterval, SignallingBlockCount),
    assert_fork_version(Version, PrevVersion);
assert_fork(_Id, undefined, _Protocols) ->
    ok.

assert_only_enabled_key(Config) when Config =/= undefined ->
    %% ae_mainnet and ae_uat are allowed to have just 'enabled' key in the config.
    case maps:size(maps:without([enabled], Config)) of
        0      -> ok;
        _Other -> error(illegal_fork_signalling_config)
    end;
assert_only_enabled_key(undefined) ->
    ok.

assert_fork_signalling_interval(SignallingStartHeight, SignallingEndHeight, PrevForkHeight)
  when (PrevForkHeight < SignallingStartHeight) andalso
       (SignallingStartHeight < SignallingEndHeight) ->
    ok;
assert_fork_signalling_interval(SignallingStartHeight, SignallingEndHeight, _PrevForkHeight) ->
    error({illegal_fork_signalling_interval, SignallingStartHeight, SignallingEndHeight}).

assert_fork_signalling_block_count(SignallingInterval, SignallingBlockCount)
  when SignallingBlockCount =< SignallingInterval ->
    ok;
assert_fork_signalling_block_count(_SignallingInterval, SignallingBlockCount) ->
    error({illegal_fork_signalling_block_count, SignallingBlockCount}).

assert_fork_version(Version, PrevVersion)
  when (Version > ?MINERVA_PROTOCOL_VSN) andalso
       (Version > PrevVersion) ->
    ok;
assert_fork_version(Version, _PrevVersion) ->
    error({illegal_fork_version, Version}).
