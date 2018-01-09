%%%=============================================================================
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%=============================================================================

-module(aens_utils).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

%% API
-export([check_name_claimed_and_owned/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_name_claimed_and_owned(binary(), pubkey(), trees()) -> ok | {error, term()}.
check_name_claimed_and_owned(NameHash, AccountPubKey, Trees) ->
    NamesTree = aec_trees:ns(Trees),
    case aens_state_tree:lookup_name(NameHash, NamesTree) of
        {value, Name} ->
            Checks =
                [fun() -> check_name_owner(Name, AccountPubKey) end,
                 fun() -> check_claimed_status(Name) end],
            aeu_validation:run(Checks);
        none ->
            {error, name_does_not_exist}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_name_owner(Name, AccountPubKey) ->
    case aens_names:owner(Name) =:= AccountPubKey of
        true  -> ok;
        false -> {error, name_not_owned}
    end.

check_claimed_status(Name) ->
    case aens_names:status(Name) of
        claimed -> ok;
        revoked -> {error, name_revoked}
    end.
