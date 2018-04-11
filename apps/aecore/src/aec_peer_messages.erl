%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module holding serialization code for P2P messages
%%% @end
%%%=============================================================================
-module(aec_peer_messages).

-export([serialize/2, serialize_response/2, deserialize/2]).

-include("aec_peer_messages.hrl").

-define(RESPONSE_VSN, 1).
-define(PING_VSN, 1).
-define(GET_MEMPOOL_VSN, 1).
-define(MEMPOOL_VSN, 1).
-define(GET_HEADER_BY_HASH_VSN, 1).
-define(GET_HEADER_BY_HEIGHT_VSN, 1).
-define(HEADER_VSN, 1).
-define(GET_N_SUCCESSORS_VSN, 1).
-define(HEADER_HASHES_VSN, 1).
-define(GET_BLOCK_VSN, 1).
-define(BLOCK_VSN, 1).
-define(TX_VSN, 1).
-define(PEER_VSN, 1).

serialize_response(Type, {ok, Object}) ->
    SerObj = serialize(Type, Object),
    serialize(response, #{ result => <<"ok">>,
                           type   => tag(Type),
                           object => SerObj });
serialize_response(Type, {error, Reason}) ->
    serialize(response, #{ result => <<"error">>,
                           type   => tag(Type),
                           reason => to_binary(Reason) }).

serialize(ping, Ping) ->
    Flds = [ {port, maps:get(port, Ping)}
           , {share, maps:get(share, Ping)}
           , {genesis_hash, maps:get(genesis_hash, Ping)}
           , {difficulty, serialize(double, maps:get(difficulty, Ping))}
           , {best_hash, maps:get(best_hash, Ping)}
           , {peers, serialize(peers, maps:get(peers, Ping))} ],
    serialize(ping, ?PING_VSN, Flds);
serialize(get_mempool, _MemPool) ->
    serialize(get_mempool, ?GET_MEMPOOL_VSN, []);
serialize(mempool, MemPool) ->
    #{ txs := Txs } = MemPool,
    serialize(mempool, ?MEMPOOL_VSN, [{txs, Txs}]);
serialize(get_header_by_hash, GetHeader) ->
    #{ hash := Hash } = GetHeader,
    serialize(get_header_by_hash, ?GET_HEADER_BY_HASH_VSN, [{hash, Hash}]);
serialize(get_header_by_height, GetHeader) ->
    #{ height := Height } = GetHeader,
    serialize(get_header_by_height, ?GET_HEADER_BY_HEIGHT_VSN, [{height, Height}]);
serialize(header, Header) ->
    #{ hdr := Hdr } = Header,
    serialize(header, ?HEADER_VSN, [{hdr, Hdr}]);
serialize(get_n_successors, GetNSucc) ->
    #{ hash := Hash, n := N } = GetNSucc,
    serialize(get_n_successors, ?GET_N_SUCCESSORS_VSN, [{hash, Hash}, {n, N}]);
serialize(header_hashes, HeaderHashes) ->
    #{ header_hashes := HHs } = HeaderHashes,
    serialize(header_hashes, ?HEADER_HASHES_VSN, [{header_hashes, HHs}]);
serialize(get_block, GetBlock) ->
    #{ hash := Hash } = GetBlock,
    serialize(get_block, ?GET_BLOCK_VSN, [{hash, Hash}]);
serialize(block, Block) ->
    #{ block := Blk } = Block,
    serialize(block, ?BLOCK_VSN, [{block, Blk}]);
serialize(tx, TxMap) ->
    #{ tx := Tx } = TxMap,
    serialize(tx, ?TX_VSN, [{tx, Tx}]);
serialize(double, D) ->
    float_to_binary(D);
serialize(peers, Peers) ->
    [ serialize(peer, Peer) || Peer <- Peers ];
serialize(peer, Peer) ->
    %% Don't pollute the encoding with lots of Vsn-fields...
    Template = serialization_template(peer, ?PING_VSN),
    Flds = [ {host,   maps:get(host, Peer)}
           , {port,   maps:get(port, Peer)}
           , {pubkey, maps:get(pubkey, Peer)} ],
    aeu_rlp:encode(aec_serialization:encode_fields(Template, Flds));
serialize(response, Response) ->
    serialize(response, ?RESPONSE_VSN,
              [ {result, maps:get(result, Response)}
              , {type,   maps:get(type, Response)}
              , {reason, maps:get(reason, Response, <<>>)}
              , {object, maps:get(object, Response, <<>>)} ]).


serialize(Type, Vsn, Flds) ->
    Template = [{vsn, int} | serialization_template(Type, Vsn)],
    List = aec_serialization:encode_fields(Template, [{vsn, Vsn} | Flds]),
    aeu_rlp:encode(List).

deserialize(Type, Binary) ->
    try
        [VsnBin | Fields] = aeu_rlp:decode(Binary),
        [{vsn, Vsn}] = aec_serialization:decode_fields([{vsn, int}], [VsnBin]),
        lager:debug("deserilaize ~p", [{rev_tag(Type), Vsn, Fields}]),
        deserialize(rev_tag(Type), Vsn, Fields)
    catch _:Reason ->
        {error, Reason}
    end.

tag(ping)                 -> ?MSG_PING;
tag(get_mempool)          -> ?MSG_GET_MEMPOOL;
tag(mempool)              -> ?MSG_MEMPOOL;
tag(get_header_by_hash)   -> ?MSG_GET_HEADER_BY_HASH;
tag(get_header_by_height) -> ?MSG_GET_HEADER_BY_HEIGHT;
tag(header)               -> ?MSG_HEADER;
tag(get_n_successors)     -> ?MSG_GET_N_SUCCESSORS;
tag(header_hashes)        -> ?MSG_HEADER_HASHES;
tag(get_block)            -> ?MSG_GET_BLOCK;
tag(block)                -> ?MSG_BLOCK;
tag(tx)                   -> ?MSG_TX;
tag(response)             -> ?MSG_P2P_RESPONSE.

rev_tag(?MSG_PING)                 -> ping;
rev_tag(?MSG_GET_MEMPOOL)          -> get_mempool;
rev_tag(?MSG_MEMPOOL)              -> mempool;
rev_tag(?MSG_GET_HEADER_BY_HASH)   -> get_header_by_hash;
rev_tag(?MSG_GET_HEADER_BY_HEIGHT) -> get_header_by_height;
rev_tag(?MSG_HEADER)               -> header;
rev_tag(?MSG_GET_N_SUCCESSORS)     -> get_n_successors;
rev_tag(?MSG_HEADER_HASHES)        -> header_hashes;
rev_tag(?MSG_GET_BLOCK)            -> get_block;
rev_tag(?MSG_BLOCK)                -> block;
rev_tag(?MSG_TX)                   -> tx;
rev_tag(?MSG_P2P_RESPONSE)         -> response.

deserialize(ping, Vsn, PingFlds) when Vsn == ?PING_VSN ->
    PingData =
        [ {port, _Port}
        , {share, _Share}
        , {genesis_hash, _GenHash}
        , {difficulty, DifficultyBin}
        , {best_hash, _TopHash}
        , {peers, PeersBin} ] = aec_serialization:decode_fields(
                                    serialization_template(ping, Vsn),
                                    PingFlds),
    Peers = deserialize(peers, Vsn, PeersBin),
    Difficulty = binary_to_float(DifficultyBin),
    PingData1 = replace_keys(PingData, [{peers, Peers}, {difficulty, Difficulty}]),
    {ping, Vsn, maps:from_list(PingData1)};
deserialize(get_mempool, Vsn, GetMempoolFlds) when Vsn == ?GET_MEMPOOL_VSN ->
    [] = aec_serialization:decode_fields(
            serialization_template(get_mempool, Vsn), GetMempoolFlds),
    {get_mempool, Vsn, #{}};
deserialize(mempool, Vsn, MempoolFlds) when Vsn == ?MEMPOOL_VSN ->
    [{txs, Txs}] = aec_serialization:decode_fields(
                                    serialization_template(mempool, Vsn),
                                    MempoolFlds),
    {mempool, Vsn, #{ txs => Txs }};
deserialize(get_header_by_hash, Vsn, GetHeaderFlds) when Vsn == ?GET_HEADER_BY_HASH_VSN ->
    [{hash, Hash}] = aec_serialization:decode_fields(
                         serialization_template(get_header_by_hash, Vsn), GetHeaderFlds),
    {get_header_by_hash, Vsn, #{ hash => Hash }};
deserialize(get_header_by_height, Vsn, GetHeaderFlds) when Vsn == ?GET_HEADER_BY_HEIGHT_VSN ->
    [{height, Height}] = aec_serialization:decode_fields(
                             serialization_template(get_header_by_height, Vsn),
                             GetHeaderFlds),
    {get_header_by_height, Vsn, #{ height => Height }};
deserialize(header, Vsn, HeaderFlds) when Vsn == ?HEADER_VSN ->
    [{hdr, Hdr}] = aec_serialization:decode_fields(
                       serialization_template(header, Vsn), HeaderFlds),
    {header, Vsn, #{ hdr => Hdr }};
deserialize(get_n_successors, Vsn, GetNFlds) when Vsn == ?GET_N_SUCCESSORS_VSN ->
    [ {hash, Hash}
    , {n, N} ] = aec_serialization:decode_fields(
                     serialization_template(get_n_successors, Vsn), GetNFlds),
    {get_n_successors, Vsn, #{ hash => Hash, n => N }};
deserialize(header_hashes, Vsn, HeaderHashesFlds) when Vsn == ?HEADER_HASHES_VSN ->
    [{header_hashes, HHs}] = aec_serialization:decode_fields(
                       serialization_template(header_hashes, Vsn), HeaderHashesFlds),
    {header_hashes, Vsn, #{ header_hashes => HHs }};
deserialize(get_block, Vsn, GetBlockFlds) when Vsn == ?GET_BLOCK_VSN ->
    [{hash, Hash}] =  aec_serialization:decode_fields(
                         serialization_template(get_block, Vsn), GetBlockFlds),
    {get_block, Vsn, #{ hash => Hash }};
deserialize(block, Vsn, BlockFlds) when Vsn == ?BLOCK_VSN ->
    [{block, Block}] =  aec_serialization:decode_fields(
                         serialization_template(block, Vsn), BlockFlds),
    {block, Vsn, #{ block => Block }};
deserialize(tx, Vsn, TxFlds) when Vsn == ?TX_VSN ->
    [{tx, Tx}] =  aec_serialization:decode_fields(
                         serialization_template(tx, Vsn), TxFlds),
    {tx, Vsn, #{ tx => Tx }};
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
        <<"ok">> ->
            {response, Vsn, R#{ msg => deserialize(Type, Object) }};
        <<"error">> ->
            {response, Vsn, R#{ reason => Reason }}
    end.

serialization_template(ping, ?PING_VSN) ->
    [ {port, int}
    , {share, int}
    , {genesis_hash, binary}
    , {difficulty, binary}
    , {best_hash, binary}
    , {peers, [binary]} ];
serialization_template(get_mempool, ?GET_MEMPOOL_VSN) ->
    [];
serialization_template(mempool, ?MEMPOOL_VSN) ->
    [{txs, [binary]}];
serialization_template(get_header_by_hash, ?GET_HEADER_BY_HASH_VSN) ->
    [{hash, binary}];
serialization_template(get_header_by_height, ?GET_HEADER_BY_HEIGHT_VSN) ->
    [{height, int}];
serialization_template(header, ?HEADER_VSN) ->
    [{hdr, binary}];
serialization_template(get_n_successors, ?GET_N_SUCCESSORS_VSN) ->
    [{hash, binary}, {n, int}];
serialization_template(header_hashes, ?HEADER_HASHES_VSN) ->
    [{header_hashes, [binary]}];
serialization_template(get_block, ?GET_BLOCK_VSN) ->
    [{hash, binary}];
serialization_template(block, ?BLOCK_VSN) ->
    [{block, binary}];
serialization_template(tx, ?TX_VSN) ->
    [{tx, binary}];
serialization_template(peer, ?PEER_VSN) ->
    [ {host, binary}
    , {port, int}
    , {pubkey, binary} ];
serialization_template(response, ?RESPONSE_VSN) ->
    [ {result, binary}
    , {type, int}
    , {reason, binary}
    , {object, binary} ].

%% -- Local functions  -------------------------------------------------------
replace_keys(PropList, KeyValues) ->
    lists:foldl(fun(KV = {K, _}, PL) -> lists:keyreplace(K, 1, PL, KV) end,
                PropList, KeyValues).

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_binary(Bin) when is_binary(Bin) ->
    Bin.

