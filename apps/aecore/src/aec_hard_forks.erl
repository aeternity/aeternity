-module(aec_hard_forks).

-export([check_env/0]).
-export([protocols/0,
         check_protocol_version_validity/2,
         protocol_effective_at_height/1
        ]).

-ifdef(TEST).
-export([check_protocol_version_validity/3,
         sorted_protocol_versions/0
        ]).
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

-export_type([ protocols/0
             , protocol_vsn/0
             ]).
%%%===================================================================
%%% API
%%%===================================================================

-spec protocols() -> protocols().
protocols() ->
    NetworkId = aec_governance:get_network_id(),
    protocols_from_network_id(NetworkId).

-spec check_env() -> ok.
check_env() ->
    assert_protocols(protocols()).

-spec check_protocol_version_validity(version(), aec_blocks:height()) ->
                                         ok | {error, Reason} when
    Reason :: unknown_protocol_version
            | {protocol_version_mismatch, ExpectedVersion::non_neg_integer()}.
check_protocol_version_validity(Version, Height) ->
    check_protocol_version_validity(Version, Height, protocols()).

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


%% Exported for tests
check_protocol_version_validity(Version, Height, Protocols) ->
    case maps:is_key(Version, Protocols) of
        false ->
            {error, unknown_protocol_version};
        true ->
            case protocol_effective_at_height(Height, Protocols) of
                Version -> ok;
                Other   -> {error, {protocol_version_mismatch, Other}}
            end
    end.

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
