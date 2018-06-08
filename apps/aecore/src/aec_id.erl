%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for identifiers
%%% @end
%%%-------------------------------------------------------------------

-module(aec_id).

-export([ create/2
        , specialize/1
        , specialize/2
        ]).

-record(id, { tag
            , val
            }).

-type tag() :: 'account' | 'name' | 'commitment'.
-type val() :: <<_:256>>.
-opaque(id() :: #id{}).

-export_type([ id/0
             , tag/0
             , val/0
             ]).

-define(PUB_SIZE, 32).
-define(IS_TAG(___TAG___), ___TAG___ =:= account;
                           ___TAG___ =:= name;
                           ___TAG___ =:= commitment
       ).
-define(IS_VAL(___VAL___), byte_size(___VAL___) =:= 32).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(tag(), val()) -> id().
create(Tag, Val) when ?IS_TAG(Tag), ?IS_VAL(Val) ->
    #id{ tag = Tag
       , val = Val};
create(Tag, Val) when ?IS_VAL(Val) ->
    error({illegal_tag, Tag});
create(Tag, Val) when ?IS_TAG(Tag)->
    error({illegal_val, Val}).


-spec specialize(id()) -> {tag(), val()}.
specialize(#id{tag = Tag, val = Val}) ->
    {Tag, Val}.

-spec specialize(id(), tag()) -> val().
specialize(#id{tag = Tag, val = Val}, Tag) when ?IS_TAG(Tag), ?IS_VAL(Val) ->
    Val.
