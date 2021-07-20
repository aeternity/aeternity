%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
-module(aehc_connector).

-export([send_tx/4,
         get_block_by_hash/2,
         get_top_block/1,
         dry_send_tx/4]).

-export([commitment/2,
         parent_block/4,
         publish_block/2,
         module/1,
         args/1,
         accept/1]).

-type connector() :: atom().
-type commitment() :: aehc_commitment:commitment().
-type parent_block() :: aehc_parent_block:parent_block().

-export_type([connector/0]).

-callback get_top_block() -> parent_block().
-callback get_block_by_hash(Hash) -> parent_block() when Hash :: binary().

-callback send_tx(Delegate, Commitment, PoGF) ->
    ok when Delegate :: binary(),
            Commitment :: binary(),
            PoGF :: binary().

-callback dry_send_tx(Delegate, Commitment, PoGF) ->
    ok when Delegate :: binary(),
            Commitment :: binary(),
            PoGF :: binary().

%%%===================================================================
%%%  Parent chain simplified proto
%%%===================================================================
%% This API should be used by connector's developer as a wrapper around
%% internal abstract commitments data type;
-spec commitment(Delegate, KeyblockHash) ->
          commitment() when Delegate :: binary(), KeyblockHash :: binary().
commitment(Delegate, KeyblockHash) when
    is_binary(Delegate), is_binary(KeyblockHash) ->
    aehc_commitment:new(aehc_commitment_header:new(Delegate, KeyblockHash)).

-spec parent_block(Height, Hash, PrevHash, Commitments) ->
          parent_block() when Height :: non_neg_integer(),
                              Hash ::binary(),
                              PrevHash :: binary(),
                              Commitments :: [commitment()].
parent_block(Height, Hash, PrevHash, Commitments) when
    is_integer(Height), is_binary(Hash), is_binary(PrevHash), is_list(Commitments) ->
    Hashes = [aehc_commitment:hash(Commitment) || Commitment <- Commitments],
    Header = aehc_parent_block:new_header(Hash, PrevHash, Height, Hashes),
    aehc_parent_block:new_block(Header, Commitments).

%%%===================================================================
%%%  Parent chain interface
%%%===================================================================

-spec send_tx(connector(), Delegate, Commitment, PoGF) ->
          ok | {error, {term(), term()}}
              when Delegate :: binary(), Commitment :: binary(), PoGF :: binary().
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

-spec get_block_by_hash(connector(), Hash) ->
          {ok, parent_block()} | {error, {term(), term()}} when Hash :: binary().
get_block_by_hash(Con, Hash) ->
    try
        Res = Con:get_block_by_hash(Hash), true = aehc_parent_block:is_hc_parent_block(Res),
        {ok, Res}
    catch E:R ->
            {error, {E, R}}
    end.

-spec dry_send_tx(connector(), Delegate, Commitment, PoGF) ->
          ok | {error, {term(), term()}}
              when Delegate :: binary(), Commitment :: binary(), PoGF :: binary().
dry_send_tx(Con, Delegate, Commitment, PoGF) ->
    try
        ok = Con:dry_send_tx(Delegate, Commitment, PoGF)
    catch E:R ->
        {error, {E, R}}
    end.
%%%===================================================================
%%%  Connector's management
%%%===================================================================
-spec accept(map()) -> ok | {error, {term(), term()}}.
accept(Conf) ->
    Criteria = [fun accept_top_block/1, fun accept_block_by_hash/1, fun accept_dry_send_tx/1],
    try
        [Fun(Conf) || Fun <- Criteria],
        ok
    catch E:R ->
        {error, {E, R}}
    end.

%% Acceptance check (request the current top block);
accept_top_block(Conf) ->
    Module = module(Conf),
    {ok, TopBlock} = get_top_block(Module),
    Hash = aehc_parent_block:hash_block(TopBlock),
    Height = aehc_parent_block:height_block(TopBlock),
    Info = "Accept get_top_block procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Module, Hash, Height]).

%% Acceptance check (request genesis block);
accept_block_by_hash(Conf) ->
    Module = module(Conf),
    Genesis = maps:get(<<"genesis_hash">>, Conf),
    {ok, Block} = get_block_by_hash(Module, Genesis),
    Hash = aehc_parent_block:hash_block(Block),
    Height = aehc_parent_block:height_block(Block),
    Info = "Accept get_block_by_hash procedure has passed (connector: ~p, hash: ~p, height: ~p)",
    lager:info(Info, [Module, Hash, Height]).

%% Acceptance check (commitment call preliminary check);
accept_dry_send_tx(Conf) ->
    Module = module(Conf),
    DelegateConf = aeu_env:user_config([<<"hyperchains">>, <<"delegate">>]),
    DelegateConf == undefined orelse
        begin
            {ok, Delegate} = aec_keys:pubkey(),
            KeyblockHash = aec_chain:top_key_block_hash(),
            PoGF = aehc_pogf:hash(no_pogf),
            dry_send_tx(Module, Delegate, KeyblockHash, PoGF),
            Info = "Accept dry_send_tx procedure has passed (connector: ~p, delegate: ~p, hash: ~p, pogf: ~p)",
            lager:info(Info, [Module, Delegate, KeyblockHash, PoGF])
        end.

%%%===================================================================
%%%  Parent chain events
%%%===================================================================

-spec publish_block(term(), parent_block()) -> ok.
publish_block(View, Block) ->
    aehc_parent_mng:publish_block(View, Block).

%% NOTE: Safely call to existing atom.
%% In the case of non loaded modules related issues should be addressed;
module(Conf) ->
    ConConf = maps:get(<<"connector">>, Conf),
    binary_to_existing_atom(maps:get(<<"module">>, ConConf), utf8).

args(Conf) ->
    ConConf = maps:get(<<"connector">>, Conf),
    maps:get(<<"args">>, ConConf).
