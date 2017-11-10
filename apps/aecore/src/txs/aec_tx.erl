-module(aec_tx).

-export([apply_signed/3,
         is_coinbase/1,
         signers/1]).
-export([serialize/1,
         deserialize/1,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

%% TX body API: getters, helpers...
-export([fee/1,
         nonce/1,
         origin/1]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").

-export_type([tx/0,
              signed_tx/0]).

%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map()) ->
    {ok, Tx :: term()} | {error, Reason :: term()}.

-callback fee(Tx :: term()) ->
    Fee :: integer().

-callback nonce(Tx :: term()) ->
    Nonce :: non_neg_integer() | undefined.

-callback origin(Tx :: term()) ->
    Origin :: pubkey() | undefined.

-callback signers(Tx :: term()) -> [pubkey()].

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()}.


%% Relax type spec for now to have different spec in coinbase/spend
-callback serialize(Tx :: term()) -> term().

%% Relax type spec for now to have different spec in coinbase/spend
-callback deserialize(term()) -> Tx :: term().

-callback type() -> binary().

%%%%=============================================================================
%% API
%%%=============================================================================

fee(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:fee(Tx).

nonce(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:nonce(Tx).

origin(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:origin(Tx).

-spec apply_signed(list(signed_tx()), trees(), non_neg_integer()) ->
                          {ok, trees()}.
apply_signed(SignedTxs, Trees0, Height) ->
    %% The first transaction in valid block is Coinbase
    [_Coinbase|Txs] = verify_and_extract_txs(SignedTxs),
    {ok, Trees1} = apply_txs(Txs, Trees0, Height),
    AwardDelay = aec_governance:block_mine_reward_delay(),
    Trees2 = award_miner(Trees1, Height, AwardDelay),
    {ok, Trees2}.

%% TODO: there should be an easier way to do this...
-spec is_coinbase(signed_tx()) -> boolean().
is_coinbase(Signed) ->
    Tx = aec_tx_sign:data(Signed),
    Mod = tx_dispatcher:handler(Tx),
    Type = Mod:type(),
    <<"coinbase">> =:= Type.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec verify_and_extract_txs(list(signed_tx())) -> list(tx()).
verify_and_extract_txs(SignedTxs) ->
    verify_and_extract_txs(SignedTxs, []).

-spec verify_and_extract_txs(list(signed_tx()), list(tx())) -> list(tx()).
verify_and_extract_txs([], Txs) ->
    lists:reverse(Txs);
verify_and_extract_txs([SignedTx | Rest], Txs) ->
    case aec_tx_sign:verify(SignedTx) of
        ok ->
            Tx = aec_tx_sign:data(SignedTx),
            verify_and_extract_txs(Rest, [Tx | Txs]);
        {error, Reason} ->
            lager:info("Tx ~p verification failed with ~p",
                       [SignedTx, Reason]),
            verify_and_extract_txs(Rest, Txs)
    end.

-spec apply_txs(list(tx()), trees(), height()) -> {ok, trees()}.
apply_txs([], Trees, _Height) ->
    {ok, Trees};
apply_txs([Tx | Rest], Trees0, Height) ->
    case check_single(Tx, Trees0, Height) of
        {ok, Trees1} ->
            {ok, Trees2} = process_single(Tx, Trees1, Height),
            apply_txs(Rest, Trees2, Height);
        {error, Reason} ->
            lager:info("Tx ~p cannot be applied due to an error ~p",
                       [Tx, Reason]),
            apply_txs(Rest, Trees0, Height)
    end.

signers(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:signers(Tx).

serialize(Tx) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:serialize(Tx).

deserialize(Data) ->
    Mod = tx_dispatcher:handler_by_type(type_of(Data)),
    Mod:deserialize(Data).

serialize_to_binary(Tx) ->
    msgpack:pack(serialize(Tx)).

deserialize_from_binary(Bin) ->
    {ok, Unpacked} = msgpack:unpack(Bin),
    deserialize(Unpacked).

type_of([#{}|_] = L) ->
    [Type] = [T || #{<<"type">> := T} <- L],
    Type;
type_of([Type|_]) ->
    Type.

%%------------------------------------------------------------------------------
%% Check transaction. Prepare state tree: e.g., create newly referenced account
%%------------------------------------------------------------------------------
-spec check_single(tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:check(Tx, Trees, Height).

%%------------------------------------------------------------------------------
%% Process the transaction. Accounts must already be present in the state tree
%%------------------------------------------------------------------------------
-spec process_single(tx(), trees(), non_neg_integer()) -> {ok, trees()}.
process_single(Tx, Trees, Height) ->
    Mod = tx_dispatcher:handler(Tx),
    Mod:process(Tx, Trees, Height).

-spec award_miner(trees(), height(), non_neg_integer()) -> trees().
award_miner(Trees, _Height, _AwardDelay) ->
    lager:debug("No transactions in genesis block"),
    Trees;
award_miner(Trees0, Height, AwardDelay) when AwardDelay > Height->
    AwardBlockHeight = Height - AwardDelay,
    {ok, AwardBlock} = aec_chain:get_block_by_height(AwardBlockHeight),
    TotalFee = total_fee(AwardBlock),

    %% Consider creation of aec_accounts_service,
    %% which will take state trees, height and list of (pubkey, operation) pairs
    %% (valuable also during txs processing)
    MinerPubkey = aec_blocks:coinbase_pubkey(AwardBlock),
    Coinbase = case aec_blocks:coinbase(AwardBlock) of
        undefined ->
            lager:error("Fatal error! Missing Coinbase in ~p", AwardBlock);
        Coinbase0 ->
            Coinbase0
    end,
    AccountsTrees0 = aec_trees:accounts(Trees0),

    {ok, Account0} = aec_accounts:get(MinerPubkey, AccountsTrees0),
    {ok, Account} = aec_accounts:earn(Account0, TotalFee, Height),
    {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
    TreesWithFee = aec_trees:set_accounts(Trees0, AccountsTrees),

    {ok, Trees} = apply_txs([Coinbase], TreesWithFee, Height),
    Trees;
award_miner(Trees, _Height, _AwardDelay) ->
    lager:debug("Chain too short to award the miner"),
    Trees.

%% TODO: consider having it stored on side in chain service and computed during
%%       txs application
total_fee(AwardBlock) ->
    Txs = aec_blocks:txs(AwardBlock),
    add_fees(Txs, 0).

add_fees([], Sum) ->
    {ok, Sum};
add_fees([Tx|Txs], Sum) ->
    Fee = fee(Tx),
    add_fees(Txs, Sum + Fee).

