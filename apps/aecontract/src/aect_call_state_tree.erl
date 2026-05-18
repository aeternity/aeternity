%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for keeping a call and return value for one block
%%% @end
%%%-------------------------------------------------------------------

-module(aect_call_state_tree).

%% API
-export([ commit_to_db/1
        , empty/0
        , empty_with_backend/0
        , enter_auth_call/2
        , get_call/3
        , insert_call/2
        , lookup_call/3
        , new_with_backend/1
        , new_with_dirty_backend/1
        , iterator/1
        , prune/2
        , prune_without_backend/1
        , root_hash/1
        , flush_call_batch/1
        , db/1 ]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , serialize_to_client/1
        , from_db_format/1
        ]).

-export([record_fields/1]).

-ifdef(TEST).
-export([to_list/1]).
-endif.

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type call_tree() :: aeu_mtrees:mtree().

%% Per-microblock deferred call writes. 'new' = insert_call
%% (aeu_mtrees:insert, errors on duplicate); 'update' = enter_auth_call
%% (aeu_mtrees:enter). Calls are append-only within a block (call ids
%% are unique) and the whole tree is pruned every block, so there is no
%% tombstone arm.
-type call_entry() :: {new | update, aect_call:call()}.
-type call_batch() :: #{binary() => call_entry()}.

-record(call_tree, {
          calls      = aeu_mtrees:empty() :: call_tree(),
          call_batch = #{}                :: call_batch()
    }).

-opaque tree() :: #call_tree{}.

-define(VSN, 1).
-define(PUB_SIZE, 32).

%% ==================================================================
%% Tracing support
record_fields(call_tree) -> record_info(fields, call_tree);
record_fields(_        ) -> no.
%% ==================================================================


%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #call_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    new_with_backend(empty).

-spec from_db_format(tree()) -> tree().
from_db_format(Tree = #call_tree{calls = CtTree}) ->
    Tree#call_tree{calls = aeu_mtrees:from_db_format(CtTree)}.

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:calls_backend()),
    #call_tree{calls = CtTree}.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:dirty_calls_backend()),
    #call_tree{calls = CtTree}.

%% A new block always starts with an empty calls tree.
%% Calls and return values are only kept for one block.
-spec prune(aec_blocks:height(), aec_trees:trees()) -> aec_trees:trees().
prune(_,Trees) ->
    CTree = aec_trees:calls(Trees),
    Empty = case has_backend(CTree) of
                true  -> empty_with_backend();
                false -> empty()
            end,
    aec_trees:set_calls(Trees, Empty).

-spec prune_without_backend(aec_trees:trees()) -> aec_trees:trees().
prune_without_backend(Trees) ->
    aec_trees:set_calls(Trees, empty()).

has_backend(#call_tree{calls = CtTree}) ->
    aeu_mtrees:has_backend(CtTree).

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree) ->
    CtCallId = aect_call:ct_call_id(Call),
    add_call(insert, CtCallId, Call, Tree).

-spec enter_auth_call(aect_call:call(), tree()) -> tree().
enter_auth_call(Call, Tree) ->
    CallerId = aect_call:caller_pubkey(Call),
    add_call(enter, CallerId, Call, Tree).

-spec lookup_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    {value, aect_call:call()} | none.
lookup_call(CtId, CallId, #call_tree{ calls = Calls, call_batch = Batch }) ->
    Key = call_tree_id(CtId, CallId),
    case maps:find(Key, Batch) of
        {ok, {_Tag, Call}} -> {value, Call};
        error ->
            case aeu_mtrees:lookup(Key, Calls) of
                {value, Val} -> {value, aect_call:deserialize(CallId, Val)};
                none         -> none
            end
    end.

-spec iterator(tree()) -> aeu_mtrees:iterator().
iterator(Tree) ->
    #call_tree{calls = CtTree} = flush_call_batch(Tree),
    aeu_mtrees:iterator(CtTree).

-spec get_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    aect_call:call().
get_call(CtPubkey, CallId, #call_tree{ calls = CtTree, call_batch = Batch }) ->
    Key = call_tree_id(CtPubkey, CallId),
    case maps:find(Key, Batch) of
        {ok, {_Tag, Call}} -> Call;
        error -> aect_call:deserialize(CallId, aeu_mtrees:get(Key, CtTree))
    end.

-ifdef(TEST).
to_list(Tree) ->
    F = fun(K, SerCall, CallsIn) ->
                [{K, aect_call:deserialize(call_id(K), SerCall)} | CallsIn]
        end,
    aeu_mtrees:fold(F, [], iterator(Tree)).

call_id(<<_:?PUB_SIZE/unit:8, CallId/binary>> = _CallTreeId) ->
    CallId.
-endif.

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(Tree) ->
    #call_tree{calls = CtTree} = flush_call_batch(Tree),
    aeu_mtrees:root_hash(CtTree).

%% Flush all pending call writes to the MPT.  Called once at microblock
%% end via aec_trees:flush_state_batches/1.  O(1) fast-path when empty.
%% Order-independent (call ids are unique) ⇒ root identical to the
%% per-tx-write reference.
-spec flush_call_batch(tree()) -> tree().
flush_call_batch(#call_tree{call_batch = Batch} = Tree)
  when map_size(Batch) =:= 0 ->
    Tree;
flush_call_batch(#call_tree{calls = CtTree, call_batch = Batch} = Tree) ->
    CtTree1 = maps:fold(
        fun(Key, {new, Call}, Acc) ->
                aeu_mtrees:insert(Key, aect_call:serialize(Call), Acc);
           (Key, {update, Call}, Acc) ->
                aeu_mtrees:enter(Key, aect_call:serialize(Call), Acc)
        end, CtTree, Batch),
    Tree#call_tree{calls = CtTree1, call_batch = #{}}.

%% WARNING: returns the backing MPT db of the *materialised* calls tree
%% only — it does NOT reflect entries still pending in `call_batch'.
%% Backend identity only; never read call state without a flush
%% (root_hash/1, commit_to_db/1 flush internally).
-spec db(tree()) -> {ok, aeu_mp_trees:db()}.
db(#call_tree{calls = CtTree}) ->
    aeu_mtrees:db(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(Tree) ->
    #call_tree{calls = CtTree} = FT = flush_call_batch(Tree),
    FT#call_tree{calls = aeu_mtrees:commit_to_db(CtTree)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(Tree) ->
    #call_tree{calls = CtTree} = flush_call_batch(Tree),
    Bin = aeu_mtrees:serialize(CtTree),
    aeser_chain_objects:serialize(
        calls_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{calls, Bin}]).

-spec serialize_to_client(tree()) -> binary().
serialize_to_client(#call_tree{} = Tree) ->
    TreeBinary = to_binary_without_backend(Tree),
    aeser_api_encoder:encode(call_state_tree, TreeBinary).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{calls, CallsBin}] =
        aeser_chain_objects:deserialize(calls_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    #call_tree{calls = aeu_mtrees:deserialize(CallsBin)}.

serialization_template(?VSN) ->
    [{calls, binary}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

call_tree_id(ContractId, CallId) ->
    <<ContractId/binary, CallId/binary>>.

add_call(How, CtId, Call, Tree = #call_tree{ call_batch = Batch }) ->
    CallId     = aect_call:id(Call),
    CallTreeId = call_tree_id(CtId, CallId),
    %% Defer the MPT write; accumulate in the microblock-level batch.
    %% Keep the insert/enter distinction so flush preserves the
    %% error-on-duplicate invariant for `insert_call' (`new'); an
    %% `enter' over an already-`new' key stays `new'.
    Tag = case {How, maps:find(CallTreeId, Batch)} of
              {insert, _}              -> new;
              {enter,  {ok, {new, _}}} -> new;
              {enter,  _}              -> update
          end,
    Tree#call_tree{ call_batch = Batch#{CallTreeId => {Tag, Call}} }.
