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
        , get_call/3
        , insert_call/2
        , lookup_call/3
        , new_with_backend/1
        , iterator/1
        , prune/2
        , prune_without_backend/1
        , root_hash/1]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , serialize_to_client/1
        ]).

-ifdef(TEST).
-export([to_list/1]).
-endif.

-export_type([tree/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type call_tree() :: aeu_mtrees:mtree().

-record(call_tree, {
          calls = aeu_mtrees:empty() :: call_tree()
    }).

-opaque tree() :: #call_tree{}.

-define(VSN, 1).
%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #call_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    new_with_backend(empty).

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:calls_backend()),
    #call_tree{calls = CtTree}.

%% A new block always starts with an empty calls tree.
%% Calls and return values are only keept for one block.
-spec prune(aec_blocks:height(), aec_trees:trees()) -> aec_trees:trees().
prune(_,Trees) ->
    aec_trees:set_calls(Trees, empty_with_backend()).

-spec prune_without_backend(aec_trees:trees()) -> aec_trees:trees().
prune_without_backend(Trees) ->
    aec_trees:set_calls(Trees, empty()).

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree = #call_tree{ calls = CtTree}) ->
    %% Construct the Id to store in the tree.
    CtId       = aect_call:contract_pubkey(Call),
    CallId     = aect_call:id(Call),
    CallTreeId = call_tree_id(CtId, CallId),

    %% Insert the new call into the history
    Serialized = aect_call:serialize(Call),
    CtTree1    = aeu_mtrees:insert(CallTreeId, Serialized, CtTree),

    %% Update the calls tree
    Tree#call_tree{ calls = CtTree1}.

-spec lookup_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    {value, aect_call:call()} | none.
lookup_call(CtPubkey, CallId, Tree) ->
    case aeu_mtrees:lookup(call_tree_id(CtPubkey, CallId), Tree#call_tree.calls) of
        {value, Val} -> {value, aect_call:deserialize(Val)};
        none         -> none
    end.

-spec iterator(tree()) -> aeu_mtrees:iterator().
iterator(Tree) ->
    aeu_mtrees:iterator(Tree#call_tree.calls).

-spec get_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    aect_call:call().
get_call(CtPubkey, CallId, #call_tree{ calls = CtTree }) ->
    CallTreeId = call_tree_id(CtPubkey, CallId),
    aect_call:deserialize(aeu_mtrees:get(CallTreeId, CtTree)).

-ifdef(TEST).
to_list(Tree) ->
    F = fun(K, SerCall, CallsIn) ->
                [{K, aect_call:deserialize(SerCall)} | CallsIn]
        end,
    aeu_mtrees:fold(F, [], iterator(Tree)).
-endif.

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#call_tree{calls = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#call_tree{calls = CtTree} = Tree) ->
    Tree#call_tree{calls = aeu_mtrees:commit_to_db(CtTree)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(#call_tree{calls = Tree}) ->
    Bin = aeu_mtrees:serialize(Tree),
    aec_object_serialization:serialize(
        calls_mtree,
        ?VSN,
        serialization_template(?VSN),
        [{calls, Bin}]).

-spec serialize_to_client(tree()) -> binary().
serialize_to_client(#call_tree{} = Tree) ->
    TreeBinary = to_binary_without_backend(Tree),
    aehttp_api_encoder:encode(call_state_tree, TreeBinary).

-spec from_binary_without_backend(binary()) -> tree().
from_binary_without_backend(Bin) ->
    [{calls, CallsBin}] =
        aec_object_serialization:deserialize(calls_mtree, ?VSN,
                                             serialization_template(?VSN), Bin),
    #call_tree{calls = aeu_mtrees:deserialize(CallsBin)}.

serialization_template(?VSN) ->
    [{calls, binary}].
%%%===================================================================
%%% Internal functions
%%%===================================================================

call_tree_id(ContractId, CallId) ->
    <<ContractId/binary, CallId/binary>>.
