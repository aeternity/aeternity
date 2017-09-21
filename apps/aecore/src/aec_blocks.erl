-module(aec_blocks).

%% API
-export([prev_hash/1,
         height/1,
         trees/1,
         target/1,
         difficulty/1,
         set_nonce/3,
         new/3,
         to_header/1,
         serialize_for_network/1,
         deserialize_from_network/1,
         hash_internal_representation/1]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export_type([block_serialized_for_network/0,
              block_deserialized_from_network/0]).

-include("common.hrl").
-include("blocks.hrl").
-include("txs.hrl").

-define(CURRENT_BLOCK_VERSION, 1).

-type block_serialized_for_network() :: binary().
-type block_deserialized_from_network() :: #block{trees :: DummyTrees::trees()}.

prev_hash(Block) ->
    Block#block.prev_hash.

height(Block) ->
    Block#block.height.

trees(Block) ->
    Block#block.trees.

target(Block) ->
    Block#block.target.

difficulty(Block) ->
    aec_pow:target_to_difficulty(target(Block)).

%% Sets the evidence of PoW,too,  for Cuckoo Cycle
set_nonce(Block, Nonce, Evd) ->
    Block#block{nonce = Nonce,
                pow_evidence = Evd}.

-spec new(block(), list(signed_tx()), trees()) -> {ok, block()} | {error, term()}.
new(LastBlock, Txs, Trees0) ->
    LastBlockHeight = height(LastBlock),
    {ok, LastBlockHeaderHash} = hash_internal_representation(LastBlock),
    Height = LastBlockHeight + 1,
    case aec_tx:apply_signed(Txs, Trees0, Height) of
        {ok, Trees} ->
            {ok, #block{height = Height,
                        prev_hash = LastBlockHeaderHash,
                        root_hash = aec_trees:all_trees_hash(Trees),
                        trees = Trees,
                        txs = Txs,
                        target = target(LastBlock),
                        time = aeu_time:now_in_msecs(),
                        version = ?CURRENT_BLOCK_VERSION}};
        {error, _Reason} = Error ->
            Error
    end.

-spec to_header(block()) -> header().
to_header(#block{height = Height,
                 prev_hash = PrevHash,
                 root_hash = RootHash,
                 target = Target,
                 nonce = Nonce,
                 time = Time,
                 version = Version,
                 pow_evidence = Evd}) ->
    #header{height = Height,
            prev_hash = PrevHash,
            root_hash = RootHash,
            target = Target,
            nonce = Nonce,
            time = Time,
            pow_evidence = Evd,
            version = Version}.

-spec serialize_for_network(BlockInternalRepresentation) ->
                                   {ok, block_serialized_for_network()} when
      BlockInternalRepresentation :: block()
                                   | block_deserialized_from_network().
serialize_for_network(B = #block{}) ->
    {ok, jsx:encode(serialize_to_map(B))}.

serialize_to_map(B = #block{}) ->
    #{<<"height">> => height(B),
      <<"prev-hash">> => base64:encode(prev_hash(B)),
      <<"root-hash">> => base64:encode(B#block.root_hash),
      <<"target">> => B#block.target,
      <<"nonce">> => B#block.nonce,
      <<"time">> => B#block.time,
      <<"version">> => B#block.version,
      <<"pow-evidence">> => serialize_pow_evidence(
			      B#block.pow_evidence),
      <<"txs">> => base64:encode(term_to_binary(B#block.txs))
     }.

-spec deserialize_from_network(block_serialized_for_network()) ->
                                      {ok, block_deserialized_from_network()}.
deserialize_from_network(B) when is_binary(B) ->
    deserialize_from_map(jsx:decode(B, [return_maps])).

deserialize_from_map(#{<<"height">> := Height,
		       <<"prev-hash">> := PrevHash,
		       <<"root-hash">> := RootHash,
		       <<"target">> := Target,
		       <<"nonce">> := Nonce,
		       <<"time">> := Time,
		       <<"version">> := Version,
		       <<"pow-evidence">> := PowEvidence,
		       <<"txs">> := Txs}) ->
    {ok, #block{
	    height = Height,
	    prev_hash = base64:decode(PrevHash),
	    root_hash = base64:decode(RootHash),
	    target = Target,
	    nonce = Nonce,
	    time = Time,
	    version = Version,
	    txs = binary_to_term(base64:decode(Txs)),
	    pow_evidence = deserialize_pow_evidence(PowEvidence)}}.

serialize_pow_evidence(Ev) ->
    base64:encode(term_to_binary(Ev)).

deserialize_pow_evidence(Bin) ->
    binary_to_term(base64:decode(Bin)).

-spec hash_internal_representation(block()) -> {ok, block_header_hash()}.
hash_internal_representation(B = #block{}) ->
    aec_headers:hash_header(to_header(B)).
