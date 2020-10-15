%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_connector).

-export([send_tx/4, get_block_by_hash/2, get_top_block/1]).

-export([commitment/2, parent_block/4]).
-export([publish_block/2]).
-export([module/1]).
-export([args/1]).

-export([accept/1]).

-type connector() :: atom().

-type commitment() :: aehc_commitment:commitment().
-type parent_block() :: aehc_parent_block:parent_block().

-callback send_tx(binary(), binary(), binary()) -> ok.
-callback get_top_block() -> parent_block().
-callback get_block_by_hash(binary()) -> parent_block().

-export_type([connector/0]).

%%%===================================================================
%%%  Parent chain simplified proto
%%%===================================================================
%% This API should be used by connector's developer as a wrapper around internal abstract commitments data type;
-spec commitment(Delegate::binary(), KeyblockHash::binary()) ->
    commitment().
commitment(Delegate, KeyblockHash) when
    is_binary(Delegate), is_binary(KeyblockHash) ->
    aehc_commitment:new(aehc_commitment_header:new(Delegate, KeyblockHash)).

%% TODO: Opened for discussion with gorbak25 and radrow;
%%-spec commitment(Delegate::binary(), KeyblockHash::binary(), PoGFHash::binary()) ->
%%    commitment().
%%commitment(Delegate, KeyblockHash, PoGFHash) when
%%    is_binary(Delegate), is_binary(KeyblockHash), is_binary(PoGFHash) ->
%%    aehc_commitment:new(aehc_commitment_header:new(Delegate, KeyblockHash, PoGFHash)).

-spec parent_block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Commitments::[commitment()]) ->
    parent_block().
parent_block(Height, Hash, PrevHash, Commitments) when
    is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Commitments) ->
    Header = aehc_parent_block:new_header(Hash, PrevHash, Height),
    aehc_parent_block:new_block(Header, Commitments).

%%%===================================================================
%%%  Parent chain interface
%%%===================================================================

-spec send_tx(connector(), binary(), binary(), binary()) -> ok | {error, {term(), term()}}.
send_tx(Con, Delegate, Commitment, PoGF) ->
    try
        ok = Con:send_tx(Delegate, Commitment, PoGF)
    catch E:R ->
            {error, {E, R}}
    end.

-spec get_top_block(connector()) -> {ok, parent_block()} | {error, {term(), term()}}.
get_top_block(Con) ->
    try
        Res = Con:get_top_block(), true = aehc_parent_block:is_hc_parent_block(Res),
        {ok, Res}
    catch E:R ->
        {error, {E, R}}
    end.

-spec get_block_by_hash(connector(), binary()) -> {ok, parent_block()} | {error, {term(), term()}}.
get_block_by_hash(Con, Hash) ->
    try
        Res = Con:get_block_by_hash(Hash), true = aehc_parent_block:is_hc_parent_block(Res),
        {ok, Res}
    catch E:R ->
            {error, {E, R}}
    end.

%%%===================================================================
%%%  Connector's management
%%%===================================================================
-spec accept(map()) -> ok | {error, {term(), term()}}.
accept(Conf) ->
    Criteria = [fun accept_top_block/1, fun accept_block_by_hash/1, fun accept_send_tx/1],
    try
        [Fun(Conf) || Fun <- Criteria],
        ok
    catch E:R ->
        {error, {E, R}}
    end.

accept_top_block(Conf) ->
    %% Ability to request the current top block;
    Module = module(Conf),
    {ok, TopBlock} = get_top_block(Module),
    Hash = aehc_parent_block:hash_block(TopBlock),
    Height = aehc_parent_block:height_block(TopBlock),
    Info = "Accept get_top_block procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Module, Hash, Height]).

accept_block_by_hash(Conf) ->
%%     Ability to request genesis block;
    Module = module(Conf),
    Genesis = maps:get(<<"genesis_hash">>, Conf),
    {ok, Block} = get_block_by_hash(Module, Genesis),
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    Info = "Accept get_block_by_hash procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Module, Hash, Height]).

accept_send_tx(Conf) ->
    %% Ability to execute commitment call;
    Module = module(Conf),
    DelegateConf = aeu_env:user_config([<<"hyperchains">>, <<"delegate">>]),
    DelegateConf == undefined orelse
        begin
            {ok, Delegate} = aec_keys:pubkey(),
            KeyblockHash = aec_chain:top_key_block_hash(),
            PoGF = aehc_pogf:hash(no_pogf),
            send_tx(Module, Delegate, KeyblockHash, PoGF),
            Info = "Accept send_tx procedure has passed (connector: ~p, delegate: ~p, hash: ~p, pogf: ~p)",
            lager:info(Info, [Module, Delegate, KeyblockHash, PoGF])
        end.

%%%===================================================================
%%%  Parent chain events
%%%===================================================================

-spec publish_block(term(), parent_block()) -> ok.
publish_block(View, Block) ->
    aehc_parent_mng:publish_block(View, Block).

module(Conf) ->
    ConConf = maps:get(<<"connector">>, Conf),
    binary_to_existing_atom(maps:get(<<"module">>, ConConf), utf8).

args(Conf) ->
    ConConf = maps:get(<<"connector">>, Conf),
    maps:get(<<"args">>, ConConf).
