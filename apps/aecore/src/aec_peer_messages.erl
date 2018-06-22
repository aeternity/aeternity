%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding serialization code for P2P messages
%%% @end
%%%=============================================================================
-module(aec_peer_messages).

-export([ deserialize/2
        , serialize/2
        , serialize/3
        , serialize_response/2
        , tag/1]).

-include("aec_peer_messages.hrl").

serialize_response(Type, {ok, Object}) ->
    SerObj = serialize(Type, Object),
    serialize(response, #{ result => true,
                           type   => tag(Type),
                           object => SerObj });
serialize_response(Type, {error, Reason}) ->
    serialize(response, #{ result => false,
                           type   => tag(Type),
                           reason => to_binary(Reason) }).

serialize(Msg, Data) ->
    serialize(Msg, Data, latest_vsn(Msg)).

serialize(ping, Ping, Vsn = ?PING_VSN) ->
    Flds = [ {port, maps:get(port, Ping)}
           , {share, maps:get(share, Ping)}
           , {genesis_hash, maps:get(genesis_hash, Ping)}
           , {difficulty, serialize(double, maps:get(difficulty, Ping), ?VSN_1)}
           , {best_hash, maps:get(best_hash, Ping)}
           , {sync_allowed, maps:get(sync_allowed, Ping)}
           , {peers, serialize(peers, maps:get(peers, Ping), ?VSN_1)} ],
    serialize_flds(ping, Vsn, Flds);
serialize(get_header_by_hash, GetHeader, Vsn = ?GET_HEADER_BY_HASH_VSN) ->
    #{ hash := Hash } = GetHeader,
    serialize_flds(get_header_by_hash, Vsn, [{hash, Hash}]);
serialize(get_header_by_height, GetHeader, Vsn = ?GET_HEADER_BY_HEIGHT_VSN) ->
    #{ height := Height, top_hash := TopHash } = GetHeader,
    serialize_flds(get_header_by_height, Vsn, [{height, Height}, {top_hash, TopHash}]);
serialize(header, Header, Vsn = ?HEADER_VSN) ->
    #{ hdr := Hdr } = Header,
    serialize_flds(header, Vsn, [{hdr, Hdr}]);
serialize(get_n_successors, GetNSucc, Vsn = ?GET_N_SUCCESSORS_VSN) ->
    #{ from_hash := FromHash, target_hash := TargetHash, n := N } = GetNSucc,
    serialize_flds(get_n_successors, Vsn,
              [{from_hash, FromHash}, {target_hash, TargetHash}, {n, N}]);
serialize(header_hashes, HeaderHashes, Vsn = ?HEADER_HASHES_VSN) ->
    #{ header_hashes := HHs } = HeaderHashes,
    serialize_flds(header_hashes, Vsn, [{header_hashes, HHs}]);
serialize(get_block, GetBlock, Vsn = ?GET_BLOCK_VSN) ->
    #{ hash := Hash } = GetBlock,
    serialize_flds(get_block, Vsn, [{hash, Hash}]);
serialize(block, Block, Vsn = ?BLOCK_VSN) ->
    #{ block := Blk } = Block,
    serialize_flds(block, Vsn, [{block,Blk}]);
serialize(get_generation, GetGeneration, Vsn = ?GET_GENERATION_VSN) ->
    #{ hash := Hash, forward := Fwd } = GetGeneration,
    serialize_flds(get_generation, Vsn, [{hash, Hash}, {forward, Fwd}]);
serialize(generation, Generation, Vsn = ?GENERATION_VSN) ->
    #{ key_block := KeyBlk, micro_blocks := MicroBlks, forward := Fwd } = Generation,
    serialize_flds(generation, Vsn, [{key_block, KeyBlk}, {micro_blocks, MicroBlks}, {forward, Fwd}]);
serialize(txs, TxMap, Vsn = ?TXS_VSN) ->
    #{ txs := Txs } = TxMap,
    serialize_flds(txs, Vsn, [{txs, Txs}]);
serialize(txps_init, _TxpsInit, Vsn = ?TX_POOL_SYNC_INIT_VSN) ->
    serialize_flds(txps_init, Vsn, []);
serialize(txps_unfold, TxpsUnfold, Vsn = ?TX_POOL_SYNC_UNFOLD_VSN) ->
    #{ unfolds := Unfolds } = TxpsUnfold,
    serialize_flds(txps_unfold, Vsn, [{unfolds, Unfolds}]);
serialize(txps_get, TxpsGet, Vsn = ?TX_POOL_SYNC_GET_VSN) ->
    #{ tx_hashes := TxHashes } = TxpsGet,
    serialize_flds(txps_get, Vsn, [{tx_hashes, TxHashes}]);
serialize(txps_finish, TxpsFinish, Vsn = ?TX_POOL_SYNC_FINISH_VSN) ->
    #{ done := Done } = TxpsFinish,
    serialize_flds(txps_finish, Vsn, [{done, Done}]);
serialize(double, D, ?VSN_1) ->
    float_to_binary(D);
serialize(peers, Peers, Vsn = ?VSN_1) ->
    [ serialize(peer, Peer, Vsn) || Peer <- Peers ];
serialize(peer, Peer, ?VSN_1) ->
    %% Don't pollute the encoding with lots of Vsn-fields...
    Template = serialization_template(peer, ?PING_VSN),
    Flds = [ {host,   maps:get(host, Peer)}
           , {port,   maps:get(port, Peer)}
           , {pubkey, maps:get(pubkey, Peer)} ],
    aeu_rlp:encode(aec_serialization:encode_fields(Template, Flds));
serialize(response, Response, Vsn = ?RESPONSE_VSN) ->
    serialize_flds(response, Vsn,
              [ {result, maps:get(result, Response)}
              , {type,   maps:get(type, Response)}
              , {reason, maps:get(reason, Response, <<>>)}
              , {object, maps:get(object, Response, <<>>)} ]);
serialize(close, _, Vsn = ?CLOSE_VSN) ->
    serialize_flds(close, Vsn, []).

serialize_flds(Type, Vsn, Flds) ->
    Template = [{vsn, int} | serialization_template(Type, Vsn)],
    List = aec_serialization:encode_fields(Template, [{vsn, Vsn} | Flds]),
    aeu_rlp:encode(List).

deserialize(Type, Binary) ->
    try
        [VsnBin | Fields] = aeu_rlp:decode(Binary),
        [{vsn, Vsn}] = aec_serialization:decode_fields([{vsn, int}], [VsnBin]),
        deserialize(rev_tag(Type), Vsn, Fields)
    catch _:Reason ->
        {error, Reason}
    end.

tag(ping)                 -> ?MSG_PING;
tag(get_header_by_hash)   -> ?MSG_GET_HEADER_BY_HASH;
tag(get_header_by_height) -> ?MSG_GET_HEADER_BY_HEIGHT;
tag(header)               -> ?MSG_HEADER;
tag(get_n_successors)     -> ?MSG_GET_N_SUCCESSORS;
tag(header_hashes)        -> ?MSG_HEADER_HASHES;
tag(get_block)            -> ?MSG_GET_BLOCK;
tag(block)                -> ?MSG_BLOCK;
tag(get_generation)       -> ?MSG_GET_GENERATION;
tag(generation)           -> ?MSG_GENERATION;
tag(txs)                  -> ?MSG_TXS;
tag(response)             -> ?MSG_P2P_RESPONSE;
tag(txps_init)            -> ?MSG_TX_POOL_SYNC_INIT;
tag(txps_unfold)          -> ?MSG_TX_POOL_SYNC_UNFOLD;
tag(txps_get)             -> ?MSG_TX_POOL_SYNC_GET;
tag(txps_finish)          -> ?MSG_TX_POOL_SYNC_FINISH;
tag(close)                -> ?MSG_CLOSE.

rev_tag(?MSG_PING)                 -> ping;
rev_tag(?MSG_GET_HEADER_BY_HASH)   -> get_header_by_hash;
rev_tag(?MSG_GET_HEADER_BY_HEIGHT) -> get_header_by_height;
rev_tag(?MSG_HEADER)               -> header;
rev_tag(?MSG_GET_N_SUCCESSORS)     -> get_n_successors;
rev_tag(?MSG_HEADER_HASHES)        -> header_hashes;
rev_tag(?MSG_GET_BLOCK)            -> get_block;
rev_tag(?MSG_BLOCK)                -> block;
rev_tag(?MSG_GET_GENERATION)       -> get_generation;
rev_tag(?MSG_GENERATION)           -> generation;
rev_tag(?MSG_TXS)                  -> txs;
rev_tag(?MSG_P2P_RESPONSE)         -> response;
rev_tag(?MSG_TX_POOL_SYNC_INIT)    -> txps_init;
rev_tag(?MSG_TX_POOL_SYNC_UNFOLD)  -> txps_unfold;
rev_tag(?MSG_TX_POOL_SYNC_GET)     -> txps_get;
rev_tag(?MSG_TX_POOL_SYNC_FINISH)  -> txps_finish;
rev_tag(?MSG_CLOSE)                -> close.

latest_vsn(ping)                 -> ?PING_VSN;
latest_vsn(get_header_by_hash)   -> ?GET_HEADER_BY_HASH_VSN;
latest_vsn(get_header_by_height) -> ?GET_HEADER_BY_HEIGHT_VSN;
latest_vsn(header)               -> ?HEADER_VSN;
latest_vsn(get_n_successors)     -> ?GET_N_SUCCESSORS_VSN;
latest_vsn(header_hashes)        -> ?HEADER_HASHES_VSN;
latest_vsn(get_block)            -> ?GET_BLOCK_VSN;
latest_vsn(block)                -> ?BLOCK_VSN;
latest_vsn(get_generation)       -> ?GET_GENERATION_VSN;
latest_vsn(generation)           -> ?GENERATION_VSN;
latest_vsn(txs)                  -> ?TXS_VSN;
latest_vsn(response)             -> ?RESPONSE_VSN;
latest_vsn(txps_init)            -> ?TX_POOL_SYNC_INIT_VSN;
latest_vsn(txps_unfold)          -> ?TX_POOL_SYNC_UNFOLD_VSN;
latest_vsn(txps_get)             -> ?TX_POOL_SYNC_GET_VSN;
latest_vsn(txps_finish)          -> ?TX_POOL_SYNC_FINISH_VSN;
latest_vsn(close)                -> ?CLOSE_VSN.

deserialize(ping, Vsn, PingFlds) when Vsn == ?PING_VSN ->
    PingData =
        [ {port, _Port}
        , {share, _Share}
        , {genesis_hash, _GenHash}
        , {difficulty, DifficultyBin}
        , {best_hash, _TopHash}
        , {sync_allowed, _SyncAllowed}
        , {peers, PeersBin} ] = aec_serialization:decode_fields(
                                    serialization_template(ping, Vsn),
                                    PingFlds),
    Peers = deserialize(peers, Vsn, PeersBin),
    Difficulty = binary_to_float(DifficultyBin),
    PingData1 = replace_keys(PingData, [{peers, Peers}, {difficulty, Difficulty}]),
    {ping, Vsn, maps:from_list(PingData1)};
deserialize(get_header_by_hash, Vsn, GetHeaderFlds) when Vsn == ?GET_HEADER_BY_HASH_VSN ->
    [{hash, Hash}] = aec_serialization:decode_fields(
                         serialization_template(get_header_by_hash, Vsn), GetHeaderFlds),
    {get_header_by_hash, Vsn, #{ hash => Hash }};
deserialize(get_header_by_height, Vsn, GetHeaderFlds) when Vsn == ?VSN_1 ->
    [{height, Height}] = aec_serialization:decode_fields(
                             serialization_template(get_header_by_height, Vsn),
                             GetHeaderFlds),
    {get_header_by_height, Vsn, #{ height => Height }};
deserialize(get_header_by_height, Vsn, GetHeaderFlds) when Vsn == ?GET_HEADER_BY_HEIGHT_VSN ->
    [{height, Height}, {top_hash, TopHash}] =
        aec_serialization:decode_fields(serialization_template(get_header_by_height, Vsn),
                                        GetHeaderFlds),
    {get_header_by_height, Vsn, #{ height => Height, top_hash => TopHash }};
deserialize(header, Vsn, HeaderFlds) when Vsn == ?HEADER_VSN ->
    [{hdr, Hdr}] = aec_serialization:decode_fields(
                       serialization_template(header, Vsn), HeaderFlds),
    {header, Vsn, #{ hdr => Hdr }};
deserialize(get_n_successors, Vsn, GetNFlds) when Vsn == ?VSN_1 ->
    [ {hash, Hash}
    , {n, N} ] = aec_serialization:decode_fields(
                     serialization_template(get_n_successors, Vsn), GetNFlds),
    {get_n_successors, Vsn, #{ hash => Hash, n => N }};
deserialize(get_n_successors, Vsn, GetNFlds) when Vsn == ?GET_N_SUCCESSORS_VSN ->
    [ {from_hash, FromHash}
    , {target_hash, TargetHash}
    , {n, N} ] = aec_serialization:decode_fields(
                     serialization_template(get_n_successors, Vsn), GetNFlds),
    {get_n_successors, Vsn, #{ from_hash => FromHash, target_hash => TargetHash, n => N }};
deserialize(header_hashes, Vsn, HeaderHashesFlds) when Vsn == ?HEADER_HASHES_VSN ->
    [{header_hashes, HHs}] = aec_serialization:decode_fields(
                       serialization_template(header_hashes, Vsn), HeaderHashesFlds),
    {header_hashes, Vsn, #{ header_hashes => HHs }};
deserialize(get_block, Vsn, GetBlockFlds) when Vsn == ?GET_BLOCK_VSN ->
    [{hash, Hash}] =  aec_serialization:decode_fields(
                         serialization_template(get_block, Vsn), GetBlockFlds),
    {get_block, Vsn, #{ hash => Hash }};
deserialize(block, Vsn, BlockFlds) when Vsn == ?BLOCK_VSN ->
    [{block, Block}] =
        aec_serialization:decode_fields(serialization_template(block, Vsn), BlockFlds),
    {block, Vsn, #{ block => Block }};
deserialize(get_generation, Vsn, GetGenFlds) when Vsn == ?GET_GENERATION_VSN ->
    [{hash, Hash}, {forward, Fwd}] =
        aec_serialization:decode_fields(serialization_template(get_generation, Vsn), GetGenFlds),
    {get_generation, Vsn, #{ hash => Hash, forward => Fwd }};
deserialize(generation, Vsn, GenerationFlds) when Vsn == ?GENERATION_VSN ->
    [{key_block, KeyBlock}, {micro_blocks, MicroBlocks}, {forward, Fwd}] =
        aec_serialization:decode_fields(serialization_template(generation, Vsn), GenerationFlds),
    {generation, Vsn, #{ key_block => KeyBlock, micro_blocks => MicroBlocks, forward => Fwd }};
deserialize(txs, Vsn, TxsFlds) when Vsn == ?TXS_VSN ->
    [{txs, Txs}] = aec_serialization:decode_fields(
                       serialization_template(txs, Vsn), TxsFlds),
    {txs, Vsn, #{ txs => Txs }};
deserialize(txps_init, Vsn, TxpsInitFlds) when Vsn == ?TX_POOL_SYNC_INIT_VSN ->
    [] = aec_serialization:decode_fields(
             serialization_template(txps_init, Vsn), TxpsInitFlds),
    {txps_init, Vsn, #{}};
deserialize(txps_unfold, Vsn, TxpsUnfoldFlds) when Vsn == ?TX_POOL_SYNC_UNFOLD_VSN ->
    [{unfolds, Unfolds}] = aec_serialization:decode_fields(
                               serialization_template(txps_unfold, Vsn), TxpsUnfoldFlds),
    {txps_unfold, Vsn, #{ unfolds => Unfolds }};
deserialize(txps_get, Vsn, TxpsGetFlds) when Vsn == ?TX_POOL_SYNC_GET_VSN ->
    [{tx_hashes, TxHashes}] = aec_serialization:decode_fields(
                               serialization_template(txps_get, Vsn), TxpsGetFlds),
    {txps_get, Vsn, #{ tx_hashes => TxHashes }};
deserialize(txps_finish, Vsn, TxpsFinishFlds) when Vsn == ?TX_POOL_SYNC_FINISH_VSN ->
    [{done, Done}] = aec_serialization:decode_fields(
                         serialization_template(txps_finish, Vsn), TxpsFinishFlds),
    {txps_finish, Vsn, #{ done => Done }};
deserialize(peers, Vsn, PeerBins) ->
    [ deserialize(peer, Vsn, aeu_rlp:decode(PeerBin)) || PeerBin <- PeerBins ];
deserialize(peer, Vsn, PeerFlds) ->
    PeerData =
        [ {host, _Host}
        , {port, _Port}
        , {pubkey, _PK} ] = aec_serialization:decode_fields(
                                serialization_template(peer, Vsn),
                                PeerFlds),
    maps:from_list(PeerData);
deserialize(response, Vsn, RspFlds) when Vsn == ?RESPONSE_VSN ->
    [ {result, Result}
    , {type,   Type}
    , {reason, Reason}
    , {object, Object} ] = aec_serialization:decode_fields(
                               serialization_template(response, ?RESPONSE_VSN),
                               RspFlds),
    R = #{ result => Result, type => rev_tag(Type) },
    case Result of
        true ->
            {response, Vsn, R#{ msg => deserialize(Type, Object) }};
        false ->
            {response, Vsn, R#{ reason => Reason }}
    end;
deserialize(close, Vsn, _CloseFlds) when Vsn == ?CLOSE_VSN ->
    {close, Vsn, #{}}.

serialization_template(ping, ?PING_VSN) ->
    [ {port, int}
    , {share, int}
    , {genesis_hash, binary}
    , {difficulty, binary}
    , {best_hash, binary}
    , {sync_allowed, bool}
    , {peers, [binary]} ];
serialization_template(get_header_by_hash, ?GET_HEADER_BY_HASH_VSN) ->
    [{hash, binary}];
serialization_template(get_header_by_height, ?VSN_1) ->
    [{height, int}];
serialization_template(get_header_by_height, ?GET_HEADER_BY_HEIGHT_VSN) ->
    [{height, int}, {top_hash, binary}];
serialization_template(header, ?HEADER_VSN) ->
    [{hdr, binary}];
serialization_template(get_n_successors, ?VSN_1) ->
    [{hash, binary}, {n, int}];
serialization_template(get_n_successors, ?GET_N_SUCCESSORS_VSN) ->
    [{from_hash, binary}, {target_hash, binary}, {n, int}];
serialization_template(header_hashes, ?HEADER_HASHES_VSN) ->
    [{header_hashes, [binary]}];
serialization_template(get_block, ?GET_BLOCK_VSN) ->
    [{hash, binary}];
serialization_template(block, ?BLOCK_VSN) ->
    [{block, binary}];
serialization_template(get_generation, ?GET_GENERATION_VSN) ->
    [{hash, binary}, {forward, bool}];
serialization_template(generation, ?GENERATION_VSN) ->
    [{key_block, binary}, {micro_blocks, [binary]}, {forward, bool}];
serialization_template(txs, ?TXS_VSN) ->
    [{txs, [binary]}];
serialization_template(txps_init, ?TX_POOL_SYNC_INIT_VSN) ->
    [];
serialization_template(txps_unfold, ?TX_POOL_SYNC_UNFOLD_VSN) ->
    [{unfolds, [binary]}];
serialization_template(txps_get, ?TX_POOL_SYNC_GET_VSN) ->
    [{tx_hashes, [binary]}];
serialization_template(txps_finish, ?TX_POOL_SYNC_FINISH_VSN) ->
    [{done, bool}];
serialization_template(peer, ?PEER_VSN) ->
    [ {host, binary}
    , {port, int}
    , {pubkey, binary} ];
serialization_template(response, ?RESPONSE_VSN) ->
    [ {result, bool}
    , {type, int}
    , {reason, binary}
    , {object, binary} ];
serialization_template(close, ?CLOSE_VSN) ->
    [ ].

%% -- Local functions  -------------------------------------------------------
replace_keys(PropList, KeyValues) ->
    lists:foldl(fun(KV = {K, _}, PL) -> lists:keyreplace(K, 1, PL, KV) end,
                PropList, KeyValues).

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(Bin) when is_binary(Bin) ->
    Bin.

