-module(aec_hard_forks).

-export([ensure_env/0]).
-export([protocols/0,
         fork/1,
         protocol_effective_at_height/1,
         protocol_effective_at_height/2
        ]).

-ifdef(TEST).
-export([sorted_protocol_versions/0]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").

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
                  fork_height             := aec_blocks:height(),
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

%% If there is a fork specified in the config (fork_management > fork) and the
%% Height parameter is less than or equal the fork height, the info about
%% the fork is returned.
-spec fork(aec_blocks:height()) -> fork() | undefined.
fork(Height) ->
    fork_from_height(Height).

-spec ensure_env() -> ok.
ensure_env() ->
    NetworkId = aec_governance:get_network_id(),
    Protocols = protocols_from_network_id(NetworkId),
    ForkConfig = fork_config(),
    assert_protocols(Protocols),
    ensure_fork_env(ForkConfig, Protocols).

-spec protocol_effective_at_height(aec_blocks:height()) ->
                                          {ok, version()} | {error, term()}.
protocol_effective_at_height(Height) ->
    protocol_effective_at_height(Height, protocols()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

protocols_from_network_id(<<"ae_mainnet">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     , ?MINERVA_PROTOCOL_VSN  => 47800
     , ?FORTUNA_PROTOCOL_VSN => 90800
%%%  , ?LIMA_PROTOCOL_VSN =>  Not yet decided
     };
protocols_from_network_id(<<"ae_uat">>) ->
    #{ ?ROMA_PROTOCOL_VSN     => 0
     , ?MINERVA_PROTOCOL_VSN  => 40900
     , ?FORTUNA_PROTOCOL_VSN => 82900
%%%  , ?LIMA_PROTOCOL_VSN =>  Not yet decided
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

fork_from_height(Height) ->
    case fork_config() of
        #{fork_height := ForkHeight} = Fork ->
            case Height =< ForkHeight of
                true  -> Fork;
                false -> undefined
            end;
        undefined ->
            undefined
    end.

%% Exported for tests
sorted_protocol_versions() ->
    lists:sort(maps:keys(protocols())).

protocols_sorted_by_version(Protocols) ->
    lists:keysort(1, maps:to_list(Protocols)).

protocol_effective_at_height(Height, Protocols) ->
    assert_height(Height),
    SortedProtocols = protocols_sorted_by_version(Protocols),
    %% Find the last protocol version effective before or at the
    %% specified height: that is the one effective at the specified
    %% height.  This assumes that the height is strictly increasing
    %% with the version so also assert that for the sake of clarity.
    ProtocolsEffectiveSinceBeforeOrAtHeight = [_|_] =
        lists:takewhile(fun({_, H}) -> Height >= H end, SortedProtocols),
    {Protocol, _ForkHeight} = lists:last(ProtocolsEffectiveSinceBeforeOrAtHeight),
    maybe_protocol_from_fork(aeu_env:get_env(aecore, fork), Protocol, Height).

maybe_protocol_from_fork(undefined, Protocol, _Height) ->
    %% No community fork configured.
    {ok, Protocol};
maybe_protocol_from_fork({ok, #{fork_height := ForkHeight}}, Protocol, Height)
  when Height < ForkHeight ->
    %% Height is below community fork height, so the last protocol before the
    %% community fork is returned.
    {ok, Protocol};
maybe_protocol_from_fork({ok, #{fork_height := ForkHeight} = Fork}, Protocol, Height)
  when Height >= ForkHeight ->
    %% Height is equal or greated than community fork height. The fork
    %% signalling result needs to be taken from aec_fork_signalling module for
    %% the last key block before the fork height.
    case aec_chain:get_key_block_by_height(ForkHeight - 1) of
        {ok, Block} ->
            {ok, BlockHash} = aec_blocks:hash_internal_representation(Block),
            case aec_fork_signalling:get_fork_result(Block, BlockHash, Fork) of
                {ok, true} ->
                    {ok, maps:get(version, Fork)};
                {ok, false} ->
                    {ok, Protocol};
                {error, pending_protocol} = Err ->
                    Err
            end;
        {error, _Rsn} ->
            {error, last_block_before_fork_not_found}
    end.

fork_config() ->
    case aeu_env:user_map([<<"fork_management">>, <<"fork">>]) of
        {ok, #{<<"signalling_start_height">> := SignallingStartHeight,
               <<"signalling_end_height">> := SignallingEndHeight,
               <<"signalling_block_count">> := SignallingBlockCount,
               <<"fork_height">> := ForkHeight,
               <<"info_field">> := InfoField,
               <<"version">> := Version}} ->
            #{signalling_start_height => SignallingStartHeight,
              signalling_end_height => SignallingEndHeight,
              signalling_block_count => SignallingBlockCount,
              fork_height => ForkHeight,
              info_field => InfoField,
              %% TODO: encoded info_field?
              version => Version};
        undefined ->
            undefined
    end.

assert_protocols(Protocols) ->
    GP = aec_block_genesis:version(),
    Ps = [GP | _] = sorted_protocol_versions(),
    %% Assert versions are sorted.
    Ps = lists:sort(Ps),
    %% Assert versions are distinct.
    [] = lists:sort(Ps) -- lists:usort(Ps),
    {[], _} = {maps:keys(Protocols) -- Ps, check_no_extra_protocol_versions},
    {[], _} = {Ps -- maps:keys(Protocols), check_no_missing_protocol_versions},
    assert_heights_strictly_increasing(Protocols),
    ok.

assert_height(Height) ->
    case is_integer(Height) andalso (Height >= aec_block_genesis:height()) of
        true  -> ok;
        false -> error({illegal_height, Height})
    end.

assert_heights_strictly_increasing(Ps) ->
    GenesisVersion = aec_block_genesis:version(),
    GenesisHeight = aec_block_genesis:height(),
    [{GenesisVersion, GenesisHeight} = G | VHs] = protocols_sorted_by_version(Ps),
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

ensure_fork_env(#{signalling_start_height := SignallingStartHeight,
                  signalling_end_height := SignallingEndHeight,
                  signalling_block_count := SignallingBlockCount,
                  fork_height := ForkHeight, info_field := _InfoField,
                  version := Version} = Fork, Protocols) ->
    SortedProtocols = protocols_sorted_by_version(Protocols),
    {PrevVersion, PrevForkHeight} = lists:last(SortedProtocols),
    assert_fork_signalling_interval(SignallingStartHeight, SignallingEndHeight, PrevForkHeight),
    SignallingInterval = SignallingEndHeight - SignallingStartHeight,
    assert_fork_signalling_block_count(SignallingInterval, SignallingBlockCount),
    assert_fork_height(SignallingEndHeight, ForkHeight),
    assert_fork_version(Version, PrevVersion),
    application:set_env(aecore, fork, Fork);
ensure_fork_env(undefined, _Protocols) ->
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

assert_fork_height(SignallingEndHeight, ForkHeight)
  when ForkHeight > SignallingEndHeight ->
    ok;
assert_fork_height(_SignallingEndHeight, ForkHeight) ->
    error({illegal_fork_height, ForkHeight}).

assert_fork_version(Version, PrevVersion)
  when (Version > ?MINERVA_PROTOCOL_VSN) andalso
       (Version > PrevVersion) ->
    ok;
assert_fork_version(Version, _PrevVersion) ->
    error({illegal_fork_version, Version}).
