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

%% For aec_serialization
-export([ encode/1
        , decode/1
        ]).

-record(id, { tag
            , val
            }).

-type tag() :: 'account' | 'oracle' | 'name' | 'commitment' | 'contract'.
-type val() :: <<_:256>>.
-opaque(id() :: #id{}).

-export_type([ id/0
             , tag/0
             , val/0
             ]).

-define(PUB_SIZE, 32).
-define(TAG_SIZE, 1).
-define(SERIALIZED_SIZE, 33). %% ?TAG_SIZE + ?PUB_SIZE

-define(IS_TAG(___TAG___), ___TAG___ =:= account;
                           ___TAG___ =:= oracle;
                           ___TAG___ =:= name;
                           ___TAG___ =:= commitment;
                           ___TAG___ =:= contract
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

-spec encode(id()) -> binary().
encode(#id{tag = Tag, val = Val}) ->
    Res = <<(encode_tag(Tag)):?TAG_SIZE/unit:8, Val/binary>>,
    true = ?SERIALIZED_SIZE =:= byte_size(Res),
    Res.

-spec decode(binary()) -> id().
decode(<<Tag:?TAG_SIZE/unit:8, Val:?PUB_SIZE/binary>>) ->
    #id{ tag = decode_tag(Tag)
       , val = Val}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

encode_tag(account)    -> 1;
encode_tag(name)       -> 2;
encode_tag(commitment) -> 3;
encode_tag(oracle)     -> 4;
encode_tag(contract)   -> 5;
encode_tag(Other)      -> error({illegal_id_tag_name, Other}).

decode_tag(1) -> account;
decode_tag(2) -> name;
decode_tag(3) -> commitment;
decode_tag(4) -> oracle;
decode_tag(5) -> contract;
decode_tag(X) -> error({illegal_id_tag, X}).
