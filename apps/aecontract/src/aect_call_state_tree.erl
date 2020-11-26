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
        , proxy_tree/1
        , get_mtree/1
        , set_mtree/2
        , iterator/1
        , prune/2
        , prune_without_backend/1
        , root_hash/1]).

-export([ from_binary_without_backend/1
        , to_binary_without_backend/1
        , serialize_to_client/1
        ]).

-export([ record_fields/1
        , pp_term/1 ]).

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
-define(PUB_SIZE, 32).

%% ==================================================================
%% Tracing support
record_fields(call_tree) -> record_info(fields, call_tree);
record_fields(_        ) -> no.

pp_term(Term) ->
    aeu_mp_trees:tree_pp_term(Term, '$calls', fun aect_call:deserialize/2).

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

-spec new_with_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:calls_backend()),
    #call_tree{calls = CtTree}.

-spec new_with_dirty_backend(aeu_mtrees:root_hash() | 'empty') -> tree().
new_with_dirty_backend(Hash) ->
    CtTree = aeu_mtrees:new_with_backend(Hash, aec_db_backends:dirty_calls_backend()),
    #call_tree{calls = CtTree}.

-spec proxy_tree(call_tree()) -> tree().
proxy_tree(Tree) ->
    #call_tree{calls = Tree}.

-spec get_mtree(tree()) -> aeu_mtrees:mtree().
get_mtree(#call_tree{calls = Tree}) ->
    Tree.

-spec set_mtree(aeu_mtrees:mtree(), tree()) -> tree().
set_mtree(Tree, #call_tree{} = T) ->
    T#call_tree{calls = Tree}.

%% A new block always starts with an empty calls tree.
%% Calls and return values are only keept for one block.
-spec prune(aec_blocks:height(), aec_trees:trees()) -> aec_trees:trees().
prune(_,Trees) ->
    aec_trees:set_calls(Trees, empty_with_backend()).

-spec prune_without_backend(aec_trees:trees()) -> aec_trees:trees().
prune_without_backend(Trees) ->
    aec_trees:set_calls(Trees, empty()).

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree) ->
    CtId = aect_call:contract_pubkey(Call),
    add_call(insert, CtId, Call, Tree).

-spec enter_auth_call(aect_call:call(), tree()) -> tree().
enter_auth_call(Call, Tree) ->
    CtId       = aect_call:caller_pubkey(Call),
    add_call(enter, CtId, Call, Tree).

-spec lookup_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    {value, aect_call:call()} | none.
lookup_call(CtId, CallId, #call_tree{ calls = Calls }) ->
    case aeu_mtrees:lookup(call_tree_id(CtId, CallId), Calls) of
        {value, Val} -> {value, aect_call:deserialize(CallId, Val)};
        none         -> none
    end.

-spec iterator(tree()) -> aeu_mtrees:iterator().
iterator(Tree) ->
    aeu_mtrees:iterator(Tree#call_tree.calls).

-spec get_call(aect_contracts:pubkey(), aect_call:id(), tree()) ->
    aect_call:call().
get_call(CtPubkey, CallId, #call_tree{ calls = CtTree }) ->
    aect_call:deserialize(CallId, aeu_mtrees:get(call_tree_id(CtPubkey, CallId), CtTree)).

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
root_hash(#call_tree{calls = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#call_tree{calls = CtTree} = Tree) ->
    Tree#call_tree{calls = aeu_mtrees:commit_to_db(CtTree)}.

-spec to_binary_without_backend(tree()) -> binary().
to_binary_without_backend(#call_tree{calls = Tree}) ->
    Bin = aeu_mtrees:serialize(Tree),
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

add_call(How, CtId, Call, Tree = #call_tree{ calls = CtTree }) ->
    CallId     = aect_call:id(Call),
    CallTreeId = call_tree_id(CtId, CallId),
    Serialized = aect_call:serialize(Call),
    %% Insert the new call into the history
    %% io:format("~p: ~p\n", [How, CallTreeId]),
    CtTree1 =
        case How of
            insert -> aeu_mtrees:insert(CallTreeId, Serialized, CtTree);
            enter  -> aeu_mtrees:enter(CallTreeId, Serialized, CtTree)
        end,

    %% Update the calls tree
    Tree#call_tree{ calls = CtTree1}.
