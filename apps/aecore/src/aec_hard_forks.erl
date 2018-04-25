-module(aec_hard_forks).

-export([check_env/0]).
-export([protocols/1,
         is_known_protocol/2,
         protocol_effective_at_height/1,
         protocol_effective_at_height/2]).

-include("common.hrl").
-include("blocks.hrl").

-define(is_version(V), (is_integer(V) andalso (V >= 0))).
-define(is_height(H), (is_integer(H) andalso (H >= ?GENESIS_HEIGHT))).

-type version() :: non_neg_integer().

%%%===================================================================
%%% API
%%%===================================================================

check_env() ->
    Ps = aec_governance:protocols(),
    Ps = protocols(Ps),
    ok.

-spec protocols(#{version() => height()}) -> aec_governance:protocols().
protocols(M) ->
    Vs = sorted_versions(),
    {[], _} = {maps:keys(M) -- Vs, check_no_extra_protocol_versions},
    {[], _} = {Vs -- maps:keys(M), check_no_missing_protocol_versions},
    assert_heights_strictly_increasing(M),
    M.

-spec is_known_protocol(version(), aec_governance:protocols()) -> boolean().
is_known_protocol(V, Protocols) when ?is_version(V) ->
    maps:is_key(V, Protocols).

-spec protocol_effective_at_height(height()) -> version().
protocol_effective_at_height(H) ->
    protocol_effective_at_height(H, protocols(aec_governance:protocols())).

-spec protocol_effective_at_height(height(), aec_governance:protocols()) ->
                                          version().
protocol_effective_at_height(H, Protocols) when ?is_height(H) ->
    SortedProtocols = protocols_sorted_by_version(Protocols),
    %% Find the last protocol version effective before or at the
    %% specified height: that is the one effective at the specified
    %% height.  This assumes that the height is strictly increasing
    %% with the version so also assert that for the sake of clarity.
    Protocols = protocols(Protocols), %% Height is increasing.
    ProtocolsEffectiveSinceBeforeOrAtHeight = [_|_] =
        lists:takewhile(fun({_, HH}) -> H >= HH end, SortedProtocols),
    {V, _} = lists:last(ProtocolsEffectiveSinceBeforeOrAtHeight),
    V.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sorted_versions() ->
    Vs = [?GENESIS_VERSION | _] = aec_governance:sorted_protocol_versions(),
    %% Assert versions are sorted.
    Vs = lists:sort(Vs),
    %% Assert versions are distinct.
    [] = lists:sort(Vs) -- lists:usort(Vs),
    Vs.

protocols_sorted_by_version(Ps) ->
    lists:keysort(1, maps:to_list(Ps)).

assert_heights_strictly_increasing(Ps) ->
    [{?GENESIS_VERSION, ?GENESIS_HEIGHT} = G | VHs] =
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
