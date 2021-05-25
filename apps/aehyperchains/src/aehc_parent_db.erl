%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2020, Aeternity Anstalt
%%% @doc
%%% Persistent storage for parent chain data
%%% @end
%%%-------------------------------------------------------------------
-module(aehc_parent_db).

%% API
-export([get_commitment/1
        , get_parent_top_block/1
        , get_parent_block/1
        , get_candidates_in_election_cycle/2
        , write_parent_block/1]).

-export([write_parent_chain_view/2
        , get_parent_chain_view/1]).

-export([table_specs/1
        , check_tables/1]).

-define(TAB(Record),
    {Record, aec_db:tab(Mode, Record, record_info(fields, Record), [])}).
-define(TAB(Record, Extra),
    {Record, aec_db:tab(Mode, Record, record_info(fields, Record), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), aec_db:ensure_transaction(fun() -> Expr end)).
-define(t(Expr, ErrorKeys), aec_db:ensure_transaction(fun() -> Expr end, ErrorKeys)).

-include_lib("aehyperchains/include/aehc_types.hrl").

-record(hc_db_pogf, {key, value}).
-record(hc_db_commitment_header, {key, value}).
-record(hc_db_parent_block_header, {key, value}).
-record(hc_db_parent_chain_view, {key, value}).

%%-type hc_db_pogf() :: #hc_db_pogf{}.
%%-type hc_db_commitment_header() :: #hc_db_commitment_header{}.
%%-type hc_db_parent_block_header() :: #hc_db_parent_block_header{}.
%%-type hc_db_parent_chain_view() :: #hc_db_parent_chain_view{}.

-type mode() :: disc | ram | map().


%% API

%%TODO Convert to proper records. Mixing records and tuples is never right.
-spec table_specs(mode()) -> [tuple()].
table_specs(Mode) ->
    [
        ?TAB(hc_db_pogf)
        , ?TAB(hc_db_commitment_header)
        , ?TAB(hc_db_parent_block_header)
        , ?TAB(hc_db_parent_chain_view)
    ].

check_tables(Acc) ->
    lists:foldl(
        fun({Tab, Spec}, Acc1) ->
            aec_db:check_table(Tab, Spec, Acc1)
        end, Acc, table_specs(disc)).

-spec get_commitment(commitment_hash()) -> aehc_commitment:commitment().
get_commitment(CommitmentHash) ->
    NoFraud = aehc_pogf:hash(no_pogf),
    ?t(begin
           [#hc_db_commitment_header{value = DBCommitmentHeader}]
               = mnesia:read(hc_db_commitment_header, CommitmentHash),
           CommitmentHeader = aehc_commitment_header:from_db(DBCommitmentHeader),
           case aehc_commitment_header:hc_pogf_hash(CommitmentHeader) of
               NoFraud ->
                   aehc_commitment:new(CommitmentHeader, no_pogf);
               PoGFHash ->
                   [#hc_db_pogf{value = DBPoGF}] = mnesia:read(hc_db_pogf, PoGFHash),
                   aehc_commitment:new(CommitmentHeader, aehc_pogf:from_db(DBPoGF))
           end
       end).

-spec get_parent_block(binary()) -> aehc_parent_block:parent_block().
get_parent_block(ParentBlockHash) ->
    NoFraud = aehc_pogf:hash(no_pogf),
    ?t(begin
           [#hc_db_parent_block_header{value = DBParentHeader}]
               = mnesia:read(hc_db_parent_block_header, ParentBlockHash),
           ParentBlockHeader = aehc_parent_block:header_from_db(DBParentHeader),
           CommitmentHashes = aehc_parent_block:commitment_hashes(ParentBlockHeader),
           Commitments = [begin
                              [#hc_db_commitment_header{value = DBCommitmentHeader}]
                                  = mnesia:read(hc_db_commitment_header, CommitmentHash),
                              CommitmentHeader =
                                  aehc_commitment_header:from_db(DBCommitmentHeader),
                              case aehc_commitment_header:hc_pogf_hash(CommitmentHeader) of
                                  NoFraud ->
                                      aehc_commitment:new(CommitmentHeader, no_pogf);
                                  PoGFHash ->
                                      [#hc_db_pogf{value = DBPoGF}]
                                          = mnesia:read(hc_db_pogf, PoGFHash),
                                      PoGFF = aehc_pogf:from_db(DBPoGF),
                                      aehc_commitment:new(CommitmentHeader, PoGFF)
                              end
                          end || CommitmentHash <- CommitmentHashes],
           aehc_parent_block:new_block(ParentBlockHeader, Commitments)
       end).

-spec get_candidates_in_election_cycle(non_neg_integer(), hash()) ->
    [aehc_commitment:commitment()].
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

-spec get_candidates_in_election_cycle_(hash(), [[aehc_commitment:commitment()]], non_neg_integer()) ->
    [aehc_commitment:commitment()].
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
            get_candidates_in_election_cycle_(PrevHash, [Candidates | Acc], N - 1)
    end.

-spec write_parent_block(aehc_parent_block:parent_block()) -> ok.
write_parent_block(ParentBlock) ->
    ParentBlockHeader = aehc_parent_block:block_header(ParentBlock),
    DBHeader = #hc_db_parent_block_header
    {
        key = aehc_parent_block:hash_block(ParentBlock)
        , value = ParentBlockHeader
    },
    Commitments = aehc_parent_block:commitments_in_block(ParentBlock),
    CommitmentHashes = aehc_parent_block:commitment_hashes(ParentBlockHeader),
    DBCommitments = lists:map(
        fun({K, V}) ->
            #hc_db_commitment_header{key = K, value = aehc_commitment:header(V)}
        end, lists:zip(CommitmentHashes, Commitments)),
    DBPoGFs = lists:filtermap(
        fun(El) ->
            aehc_commitment:has_pogf(El) andalso
                {true, #hc_db_pogf{key = aehc_commitment:pogf_hash(El)
                    , value = aehc_commitment:pogf(El)
                }}
        end, Commitments),
    ?t(begin
           mnesia:write(DBHeader),
           [mnesia:write(DBCommitment) || DBCommitment <- DBCommitments],
           [mnesia:write(DBPoGF) || DBPoGF <- DBPoGFs]
       end).

%% This entry determines traversing path within the current db log (the current range between genesis and top);
-spec write_parent_chain_view(binary(), binary()) -> ok.
write_parent_chain_view(GenesisHash, TopHash) when is_binary(GenesisHash),
    is_binary(TopHash) ->
    ?t(mnesia:write(#hc_db_parent_chain_view{key = GenesisHash, value = TopHash}),
        [{hc_db_parent_chain_view, GenesisHash}]).

-spec get_parent_chain_view(binary()) -> binary() | undefined.
get_parent_chain_view(GenesisHash) ->
    ?t(case mnesia:read(hc_db_parent_chain_view, GenesisHash) of
           [#hc_db_parent_chain_view{value = Value}] ->
               Value;
           _ ->
               undefined
       end).

%% Get the parent tob block of a particular parent view;
-spec get_parent_top_block(binary()) -> aehc_parent_block:parent_block() | undefined.
get_parent_top_block(GenesisHash) ->
    ?t(get_parent_block(get_parent_chain_view(GenesisHash))).
