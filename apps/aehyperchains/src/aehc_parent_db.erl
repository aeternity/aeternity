%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Persistent storage for parent chain data
%%% @end
%%%-------------------------------------------------------------------

-module(aehc_parent_db).

%% API
-export([ get_commitment/1
        , get_parent_block/1
        , get_parent_blocks/2
        , get_candidates_in_election_cycle/2
        , write_parent_block/2
        , find_parent_block/1
        ]).

-export([ write_parent_state/2
        , get_parent_state/1
        ]).

-export([ get_parent_block_state/1
        , write_parent_block_state/2
        ]).

-export([ find_delegates_node/1
        , write_delegates_node/2
        ]).

-export([
          table_specs/1
        , check_tables/1
        ]).

-define(TAB(Record),
        {Record, aec_db:tab(Mode, Record, record_info(fields, Record), [])}).
-define(TAB(Record, Extra),
        {Record, aec_db:tab(Mode, Record, record_info(fields, Record), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), aec_db:ensure_transaction(fun() -> Expr end)).
-define(t(Expr, ErrorKeys), aec_db:ensure_transaction(fun() -> Expr end, ErrorKeys)).

-include("../../aecore/include/blocks.hrl").
-include("aehc_parent_db.hrl").
-include("aehc_utils.hrl").

table_specs(Mode) ->
    [ ?TAB(hc_db_pogf)
    , ?TAB(hc_db_commitment_header)
    , ?TAB(hc_db_parent_block_header)
    , ?TAB(hc_db_parent_block_state)
    , ?TAB(hc_db_parent_state)
    , ?TAB(hc_db_delegate_state)
    ].

check_tables(Acc) ->
    lists:foldl(
      fun({Tab, Spec}, Acc1) ->
              aec_db:check_table(Tab, Spec, Acc1)
      end, Acc, table_specs(disc)).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_commitment(commitment_hash()) -> aehc_commitment:commitment().
get_commitment(CommitmentHash) ->
    NoFraud = aehc_pogf:hash(no_pogf),
    ?t(begin
       [#hc_db_commitment_header{value = DBCommitmentHeader}]
           = get_commitment_header(CommitmentHash),
       CommitmentHeader = aehc_commitment_header:from_db(DBCommitmentHeader),
       case aehc_commitment_header:hc_pogf_hash(CommitmentHeader) of
            NoFraud ->
                aehc_commitment:new(CommitmentHeader, no_pogf);
            PoGFHash ->
                [#hc_db_pogf{value = DBPoGF}] = get_pogf(PoGFHash),
                aehc_commitment:new(CommitmentHeader, aehc_pogf:from_db(DBPoGF))
       end
    end).

-spec get_parent_block(binary()) -> aehc_parent_block:parent_block().
get_parent_block(ParentBlockHash) ->
    ?t(begin
           [#hc_db_parent_block_header{value = DBParentHeader}]
                = get_parent_header(ParentBlockHash),
           ParentBlockHeader = aehc_parent_block:header_from_db(DBParentHeader),
           CommitmentHashes = aehc_parent_block:commitment_hashes(ParentBlockHeader),
           Commitments = get_commitments(CommitmentHashes),

           aehc_parent_block:new_block(ParentBlockHeader, Commitments)
    end).

-spec find_parent_block(binary()) -> 'none' | {'value', aehc_parent_block:parent_block()}.
find_parent_block(ParentBlockHash) ->
    ?t(case get_parent_header(ParentBlockHash) of
           [#hc_db_parent_block_header{value = DBParentHeader}] ->
               ParentBlockHeader = aehc_parent_block:header_from_db(DBParentHeader),
               CommitmentHashes = aehc_parent_block:commitment_hashes(ParentBlockHeader),
               Commitments = get_commitments(CommitmentHashes),

               aehc_parent_block:new_block(ParentBlockHeader, Commitments);
           [] ->
               none
       end).

get_parent_header(ParentBlockHash) ->
    mnesia:read(hc_db_parent_block_header, ParentBlockHash).

get_commitment_header(CommitmentHash) ->
    mnesia:read(hc_db_commitment_header, CommitmentHash).

get_pogf(PoGFHash) ->
    mnesia:read(hc_db_pogf, PoGFHash).

get_commitments(CommitmentHashes) ->
    NoFraud = aehc_pogf:hash(no_pogf),
    [begin
         [#hc_db_commitment_header{value = DBCommitmentHeader}]
             = get_commitment_header(CommitmentHash),
         CommitmentHeader =
             aehc_commitment_header:from_db(DBCommitmentHeader),
         case aehc_commitment_header:hc_pogf_hash(CommitmentHeader) of
             NoFraud ->
                 aehc_commitment:new(CommitmentHeader, no_pogf);
             PoGFHash ->
                 [#hc_db_pogf{value = DBPoGF}]
                     = get_pogf(PoGFHash),
                 PoGFF = aehc_pogf:from_db(DBPoGF),
                 aehc_commitment:new(CommitmentHeader, PoGFF)
         end
     end || CommitmentHash <- CommitmentHashes].

-spec get_parent_blocks(non_neg_integer(), binary()) -> [aehc_parent_block:aehc_parent_block()].
get_parent_blocks(Height, Hash) ->
    get_parent_blocks(Hash, [], Height).

get_parent_blocks(_Hash, Acc, 0) ->
    Acc;
get_parent_blocks(Hash, Acc, Height) ->
    Block = get_parent_block(Hash),
    case aehc_parent_block:prev_hash_block(Block) of
        Hash ->
            Acc;
        PrevHash ->
            get_parent_blocks(PrevHash, [Block|Acc], Height - 1)
    end.

-spec get_candidates_in_election_cycle(non_neg_integer(), binary()) -> [aehc_commitment:commitment()].
get_candidates_in_election_cycle(_Height, ParentBlockHash) ->
    %% First retrieve the "genesis" hash for the provided Block Hash
    %% Hyperchains might during their lifetime change their parent chain or
    %% even use multiple parent chains. The Election period can be changed
    %% during the lifetime of the hyperchain. We define the parent chain
    %% "genesis" hash as a configuration key. Please note that this genesis hash
    %% is not the real genesis hash of the parent chain - it is only
    %% an indicator where commitments will start to be submitted
    %% In our DB the "genesis" parent block has it's prev hash set to itself

    %% CommitmentStart = aehc_parent_chain:pinpointed_hash(ParentBlockHash),
    %% The height determines the protocol - protocol determines which parent chains are used
    %% ElectionPeriod = aehc_utils:election_period_for_hash(Height, CommitmentStart)
    ElectionPeriod = 1,
    get_candidates_in_election_cycle_(ParentBlockHash, [], ElectionPeriod).

get_candidates_in_election_cycle_(_ParentBlockHash, Acc, 0) ->
    lists:flatten(Acc);
get_candidates_in_election_cycle_(ParentBlockHash, Acc, N) ->
    Block = get_parent_block(ParentBlockHash),
    Candidates = aehc_parent_block:commitments_in_block(Block),
    case aehc_parent_block:prev_hash_block(Block) of
        ParentBlockHash ->
            %% Pinpointed block "Genesis"
            lists:flatten([Candidates | Acc]);
        PrevHash ->
            get_candidates_in_election_cycle_(PrevHash, [Candidates | Acc], N-1)
    end.

%%% Block record

-spec write_parent_block(aehc_parent_block:parent_block(), aehc_parent_trees:trees()) -> ok.
write_parent_block(ParentBlock, Trees) ->
    ?t(begin
           [
               ok = Fun(ParentBlock)|| Fun <- [ fun write_parent_block_header/1
                                              , fun write_commitments/1
                                              , fun write_pogf/1
                                              ]
           ],
           ok = write_parent_block_state(ParentBlock, Trees)
       end
    ).

write_parent_block_header(ParentBlock) ->
    ParentBlockHeader = aehc_parent_block:block_header(ParentBlock),
    DBHeader = #hc_db_parent_block_header{ key = aehc_parent_block:hash_block(ParentBlock)
        , value = ParentBlockHeader
    },
    mnesia:write(DBHeader).

write_commitments(ParentBlock) ->
    ParentBlockHeader = aehc_parent_block:block_header(ParentBlock),
    Commitments = aehc_parent_block:commitments_in_block(ParentBlock),
    CommitmentHashes = aehc_parent_block:commitment_hashes(ParentBlockHeader),
    DBCommitments = lists:map(
        fun({K,V}) ->
            #hc_db_commitment_header{key = K, value = aehc_commitment:header(V)}
        end, lists:zip(CommitmentHashes, Commitments)),
    [ok = mnesia:write(DBCommitment) || DBCommitment <- DBCommitments],
    ok.

write_pogf(ParentBlock) ->
    Commitments = aehc_parent_block:commitments_in_block(ParentBlock),
    DBPoGFs = lists:filtermap(
        fun(El) ->
            aehc_commitment:has_pogf(El) andalso
                {true, #hc_db_pogf{ key = aehc_commitment:pogf_hash(El)
                    , value = aehc_commitment:pogf(El)
                }}
        end, Commitments),
    [ok = mnesia:write(DBPoGF) || DBPoGF <- DBPoGFs],
    ok.

write_parent_block_state(ParentBlock, Trees) ->

    Hash = aehc_parent_block:hash_block(ParentBlock),
    Trees2 = aehc_parent_trees:serialize_for_db(aehc_parent_trees:commit_to_db(Trees)),
    ParentBlockState = #hc_db_parent_block_state{ key = Hash, value = Trees2 },
    mnesia:write(ParentBlockState).

get_parent_block_state(Hash) ->
    ?t(begin
           [#hc_db_parent_block_state{value = Trees}] =
               mnesia:read(hc_db_parent_block_state, Hash),
           aehc_parent_trees:deserialize_from_db(Trees)
       end).

%%%===================================================================
%%% Chain state API
%%%===================================================================

-spec get_parent_state(binary()) -> aehc_parent_state:parent_state() | undefined.
get_parent_state(Pointer) ->
    ?t(case mnesia:read(hc_db_parent_state, Pointer) of
           [#hc_db_parent_state{value = Value}] ->
               Value;
           _ ->
               undefined
       end).

-spec write_parent_state(binary(), aehc_parent_state:parent_state()) -> ok.
write_parent_state(Pointer, ParentState) ->
    ?t(mnesia:write(#hc_db_parent_state{key = Pointer, value = ParentState}),
        []).

%%%===================================================================
%%% State trees API
%%%===================================================================

find_delegates_node(Hash) ->
    case ?t(mnesia:read(hc_db_delegate_state, Hash)) of
        [#hc_db_delegate_state{value = Node}] -> {value, Node};
        [] -> none
    end.

write_delegates_node(Hash, Node) ->
    ?t(mnesia:write(#hc_db_delegate_state{key = Hash, value = Node}),
        [{hc_db_delegate_state, Hash}]).
%% TODO TO initiate parent genesis with trees
