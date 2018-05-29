%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% Proof of inclusion ADT.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_poi).

-export([ add_poi/3
        , new/1
        , root_hash/1
        , verify/3
        , lookup/2
        ]).

-export([ from_serialization_format/1
        , serialization_format/1
        , serialization_format_template/0
        ]).

-export([ proof_db_get/2
        , proof_db_put/3
        , proof_db_commit/2
        ]).

-export_type([ poi/0
             ]).

-include("blocks.hrl").

-define(HASH_BYTES, 32).

-record(aec_poi, { proof     :: aeu_mp_trees_db:db()
                 , root_hash :: binary()
                 }).

-opaque poi()     :: #aec_poi{}.

-type key() :: binary().
-type value() :: binary().
-type proof_value() :: aeu_rlp:encodable().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(state_hash()) -> poi().
new(<<_:?HASH_BYTES/binary>> = Hash) ->
    #aec_poi{ proof     = new_proof_db()
            , root_hash = Hash
            }.

-spec root_hash(poi()) -> state_hash().
root_hash(#aec_poi{root_hash = Hash}) ->
    Hash.

-spec add_poi(key(), aeu_mtrees:mtree(), poi()) ->
                     {'ok', value(), poi()}
                   | {'error', 'not_present' | 'wrong_root_hash'}.
add_poi(Key, Tree, #aec_poi{root_hash = RootHash} = Poi) ->
    case aeu_mtrees:root_hash(Tree) of
        {ok, RootHash} ->
            Proof = Poi#aec_poi.proof,
            case aeu_mtrees:lookup_with_proof(Key, Tree, Proof) of
                none -> {error, not_present};
                {value_and_proof, Value, NewProof} ->
                    { ok
                    , Value
                    , Poi#aec_poi{ proof = NewProof}
                    }
            end;
        Other ->
            lager:debug("Root hash: ~p\nOther: ~p\n", [RootHash, Other]),
            {error, wrong_root_hash}
    end.

-spec verify(key(), term(), poi()) -> 'ok' | {'error', term()}.
verify(Key, Value, #aec_poi{root_hash = RootHash} = Poi) ->
    case aeu_mtrees:verify_proof(Key, Value, RootHash, Poi#aec_poi.proof) of
        {ok, verified} -> ok;
        {error, _} = E -> E
    end.

-spec lookup(key(), poi()) -> {'ok', term()} | {'error', term()}.
lookup(Key, #aec_poi{root_hash = RootHash} = Poi) ->
    case aeu_mtrees:lookup_proof(Key, RootHash, Poi#aec_poi.proof) of
        {ok, Val} -> {ok, Val};
        {error, _} = E -> E
    end.

-spec serialization_format(poi()) -> {state_hash(), [{key(), proof_value()}]}.
serialization_format(#aec_poi{ root_hash = Hash
                             , proof = ProofDb}) ->
    { Hash
    , proof_serialize_to_list(ProofDb)
    }.

-spec serialization_format_template() -> {'binary', [{'binary', ['binary']}]}.
serialization_format_template() ->
    { binary
    , [{binary, [binary]}]
    }.

-spec from_serialization_format({state_hash(),  [{key(), proof_value()}]}) ->
                                       poi().
from_serialization_format({<<_:?HASH_BYTES/binary>> = Hash, ProofList}) ->
    #aec_poi{ root_hash = Hash
            , proof = proof_from_list(ProofList)
            }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Proof

new_proof_db() ->
    aeu_mp_trees_db:new(proof_db_spec()).

proof_db_spec() ->
    #{ handle => gb_trees
     , cache  => gb_trees:empty()
     , get    => {?MODULE, proof_db_get}
     , put    => {?MODULE, proof_db_put}
     , commit => {?MODULE, proof_db_commit}
     }.

proof_db_get(Key, Proof) ->
    gb_trees:lookup(Key, Proof).

proof_db_put(Key, Val, Proof) ->
    gb_trees:enter(Key, Val, Proof).

proof_db_commit(_Cache,_DB) ->
    error(no_commits_in_proof).

proof_serialize_to_list(Proof) ->
    gb_trees:to_list(aeu_mp_trees_db:get_cache(Proof)).

proof_from_list(ProofList) ->
    case build_proof_list(ProofList, new_proof_db()) of
        {ok, Proof} -> Proof;
        error -> error(illegal_proof)
    end.

build_proof_list([{Key, Val}|Left], Proof) when is_binary(Key),
                                                is_list(Val) ->
    case lists:all(fun is_binary/1, Val) of
        true -> build_proof_list(Left, aeu_mp_trees_db:put(Key, Val, Proof));
        false -> error
    end;
build_proof_list([], Proof) ->
    {ok, Proof};
build_proof_list(_, _) ->
    error.



