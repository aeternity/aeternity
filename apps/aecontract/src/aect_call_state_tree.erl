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
        , lookup_call/2
        , prune/2
        , root_hash/1]).

-export_type([tree/0]).

-include_lib("apps/aecore/include/common.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type call_tree() :: aeu_mtrees:mtree().

-record(call_tree, {
          calls = aeu_mtrees:empty() :: call_tree()
    }).

-opaque tree() :: #call_tree{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    #call_tree{}.

-spec empty_with_backend() -> tree().
empty_with_backend() ->
    CtTree = aeu_mtrees:empty_with_backend(aec_db_backends:calls_backend()),
    #call_tree{calls = CtTree}.

%% A new block always starts with an empty calls tree.
%% Calls and return values are only keept for one block.
-spec prune(height(), aec_trees:trees()) -> aec_trees:trees().
prune(_,Trees) ->
    aec_trees:set_calls(Trees, empty_with_backend()).

-spec insert_call(aect_call:call(), tree()) -> tree().
insert_call(Call, Tree = #call_tree{ calls = CtTree}) ->
    %% Construct the Id to store in the tree.
    CtId       = aect_call:contract_address(Call),
    CallId     = aect_call:id(Call),
    CallTreeId = <<CtId/binary, CallId/binary>>,

    %% Insert the new call into the history
    Serialized = aect_call:serialize(Call),
    CtTree1    = aeu_mtrees:insert(CallTreeId, Serialized, CtTree),

    %% Update the calls tree
    Tree#call_tree{ calls = CtTree1}.

-spec lookup_call(aect_call:id(), tree()) -> {value, aect_call:call()} | none.
lookup_call(Id, Tree) ->
    case aeu_mtrees:lookup(Id, Tree#call_tree.calls) of
        {value, Val} -> {value, aect_call:deserialize(Val)};
        none         -> none
    end.

-spec get_call(aect_contracts:id(), aect_call:id(), tree()) -> aect_call:call().
get_call(CtId, CallId, #call_tree{ calls = CtTree }) ->
    CallTreeId = <<CtId/binary, CallId/binary>>,
    aect_call:deserialize(aeu_mtrees:get(CallTreeId, CtTree)).

%% -- Hashing --

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#call_tree{calls = CtTree}) ->
    aeu_mtrees:root_hash(CtTree).

%% -- Commit to db --

-spec commit_to_db(tree()) -> tree().
commit_to_db(#call_tree{calls = CtTree} = Tree) ->
    Tree#call_tree{calls = aeu_mtrees:commit_to_db(CtTree)}.
