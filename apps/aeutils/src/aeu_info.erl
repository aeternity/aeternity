-module(aeu_info).

-export([get_version/0,
         get_revision/0,
         get_os/0,
         get_status/0,
         vendor/0,
         block_info/0]).

-define(VERSION_FILE, <<"VERSION">>).
-define(REVISION_FILE, <<"REVISION">>).

-define(ETS_TABLE, aeu_info_file_cache).

get_version() ->
    cached_file(?VERSION_FILE).

get_revision() ->
    cached_file(?REVISION_FILE).

block_info() ->
    try
        cached_file(block_info,
                    fun(block_info) ->
                            binary_to_integer(
                              re:replace(
                                cached_file(?VERSION_FILE),
                                "^(\\d+)\\.(\\d+)\\.(\\d+).*", "\\1\\2\\3",
                                [{return, binary}, global]))
                    end)
    catch
        error:Error:ST ->
            lager:debug("CAUGHT error:~p / ~p", [Error, ST]),
            error(Error)
    end.

get_os() ->
    Bin = fun(A) when is_atom(A)    -> atom_to_binary(A, utf8)
          end,
    {OSFamily, OSName} = os:type(),
    BOSFamily = Bin(OSFamily),
    BOSName = Bin(OSName),
    <<BOSFamily/binary, ":", BOSName/binary>>.

vendor() ->
    <<"Aeternity reference node">>.

get_status() ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    GenesisBlockHash = aec_consensus:get_genesis_hash(),
    Difficulty = aec_blocks:difficulty(TopKeyBlock),
    {Syncing, SyncProgress, _} = aec_sync:sync_progress(),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = aec_peers:count(peers),
    PendingTxsCount = aec_tx_pool:size(),
    {ok, PeerPubkey} = aec_keys:peer_pubkey(),
    TopBlock = aec_chain:top_block(),
    TopBlockHeight = aec_blocks:height(TopBlock),
    Protocol = aec_hard_forks:protocol_effective_at_height(TopBlockHeight),
    ProtocolName = aec_hard_forks:protocol_vsn_name(Protocol),
    ProtocolStr = integer_to_list(Protocol) ++ " (" ++ atom_to_list(ProtocolName) ++ ")",
    TopBlockHash = aec_chain:top_key_block_hash(),
    Status =
     [{<<"Genesis block Hash">>          , aeser_api_encoder:encode(key_block_hash, GenesisBlockHash)},
      {<<"Difficulty">>                  , Difficulty},
      {<<"Syncing">>                     , Syncing},
      {<<"Sync progress">>               , SyncProgress},
      {<<"Node version">>                , NodeVersion},
      {<<"Node revision">>               , NodeRevision},
      {<<"Peer count">>                  , PeerCount},
      {<<"Peer connections (inbound)">>  , aec_peers:count(inbound)},
      {<<"Peer connections (outbound)">> , aec_peers:count(outbound)},
      {<<"Pending transactions count">>  , PendingTxsCount},
      {<<"Network id">>                  , aec_governance:get_network_id()},
      {<<"Peer pubkey">>                 , aeser_api_encoder:encode(peer_pubkey, PeerPubkey)},
      {<<"Top key block hash">>          , aeser_api_encoder:encode(key_block_hash, TopBlockHash)},
      {<<"Top block height">>            , TopBlockHeight},
      {<<"Top block protocol">>          , list_to_binary(ProtocolStr)}
     ],
    {ok, Status}.

%% Internals

read_trimmed_file(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            <<>>;
        {ok, Content} ->
            trim_ending_whitespace(Content)
    end.

trim_ending_whitespace(Binary) ->
    re:replace(Binary, "\\s+$", "", [{return, binary}, global]).

cached_file(Filename) ->
    try
        Res = cached_file(Filename, fun read_trimmed_file/1),
        lager:debug("~p -> ~p", [Filename, Res]),
        Res
    catch
        error:E:ST ->
            lager:debug("CAUGHT error:~p:~p", [E, ST]),
            error(E)
    end.

cached_file(Filename, ReadFun) ->
    EtsKey = {file, Filename},
    try
        [{EtsKey, Data}] = ets:lookup(?ETS_TABLE, EtsKey),
        Data
    catch
        _:_ ->
            lager:debug("~p not cached in ets", [Filename]),
            case ets:whereis(?ETS_TABLE) of
                undefined ->
                    ets:new(?ETS_TABLE, [named_table, {read_concurrency, true}, public]);
                _ -> pass
            end,
            ReadData = ReadFun(Filename),
            lager:debug("ReadData (~p) = ~p", [Filename, ReadData]),
            ets:insert(?ETS_TABLE, {EtsKey, ReadData}),
            ReadData
    end.

