%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_tx).

-export([filter_out_invalid_signatures/1,
         apply_signed/3,
         apply_signed_strict/3,
         is_coinbase/1,
         accounts/1,
         signers/1,
         hash_tx/1]).
-export([serialize/1,
         serialize_for_client/1,
         deserialize/1,
         serialize_to_binary/1,
         deserialize_from_binary/1]).

%% TX body API: getters, helpers...
-export([fee/1,
         nonce/1,
         origin/1]).

-include("common.hrl").
-include("trees.hrl").
-include("core_txs.hrl").

-type tx() :: coinbase_tx() | spend_tx().
-export_type([tx/0]).

%%%%=============================================================================
%% API
%%%=============================================================================

fee(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:fee(Tx).

nonce(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:nonce(Tx).

origin(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:origin(Tx).

-spec filter_out_invalid_signatures(list(aec_tx_sign:signed_tx())) -> list(aec_tx_sign:signed_tx()).
filter_out_invalid_signatures(SignedTxs) ->
    lists:filter(
      fun(SignedTx) ->
              ok =:= aec_tx_sign:verify(SignedTx)
      end, SignedTxs).

-spec apply_signed_strict(list(aec_tx_sign:signed_tx()), trees(), non_neg_integer()) ->
                                 {ok, list(aec_tx_sign:signed_tx()), trees()}
                               | {'error', atom()}.
apply_signed_strict(SignedTxs, Trees, Height) ->
    apply_signed_common(SignedTxs, Trees, Height, true).

-spec apply_signed(list(aec_tx_sign:signed_tx()), trees(), non_neg_integer()) ->
                          {ok, list(aec_tx_sign:signed_tx()), trees()}.
apply_signed(SignedTxs, Trees, Height) ->
    {ok, _, _} = apply_signed_common(SignedTxs, Trees, Height, false).

%% TODO: there should be an easier way to do this...
-spec is_coinbase(aec_tx_sign:signed_tx()) -> boolean().
is_coinbase(Signed) ->
    Tx = aec_tx_sign:data(Signed),
    Mod = aec_tx_dispatcher:handler(Tx),
    Type = Mod:type(),
    <<"coinbase">> =:= Type.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

apply_signed_common(SignedTxs, Trees0, Height, Strict) ->
    Trees1 = aec_trees:perform_pre_transformations(Trees0, Height),
    case apply_on_state_trees(SignedTxs, Trees1, Height, Strict) of
        {ok, SignedTxs1, Trees2} ->
            TotalFee = calculate_total_fee(SignedTxs1),
            Trees3 = grant_fee_to_miner(SignedTxs1, Trees2, TotalFee, Height),
            {ok, SignedTxs1, Trees3};
        {error, _} = E -> E
    end.

apply_on_state_trees(SignedTxs, Trees, Height, Strict) ->
    apply_on_state_trees(SignedTxs, [], Trees, Height, Strict).

apply_on_state_trees([], FilteredSignedTxs, Trees, _Height,_Strict) ->
    {ok, lists:reverse(FilteredSignedTxs), Trees};
apply_on_state_trees([SignedTx | Rest], FilteredSignedTxs, Trees0, Height, Strict) ->
    Tx = aec_tx_sign:data(SignedTx),
    case check_single(Tx, Trees0, Height) of
        {ok, Trees1} ->
            {ok, Trees2} = process_single(Tx, Trees1, Height),
            apply_on_state_trees(Rest, [SignedTx | FilteredSignedTxs], Trees2, Height, Strict);
        {error, Reason} when Strict ->
            lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
            {error, Reason};
        {error, Reason} when not Strict ->
            lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
            apply_on_state_trees(Rest, FilteredSignedTxs, Trees0, Height, Strict)
    end.

calculate_total_fee(SignedTxs) ->
    lists:foldl(
      fun(SignedTx, TotalFee) ->
              Fee = fee(aec_tx_sign:data(SignedTx)),
              TotalFee + Fee
      end, 0, SignedTxs).

-spec hash_tx(tx()) -> tx_hash().
hash_tx(Tx) ->
    aec_sha256:hash(serialize_to_binary(Tx)).

accounts(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:accounts(Tx).

signers(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:signers(Tx).

serialize(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:serialize(Tx).

serialize_for_client(Tx) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:for_client(Tx).

deserialize(Data) ->
    Mod = aec_tx_dispatcher:handler_by_type(type_of(Data)),
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
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:check(Tx, Trees, Height).

%%------------------------------------------------------------------------------
%% Process the transaction. Accounts must already be present in the state tree
%%------------------------------------------------------------------------------
-spec process_single(tx(), trees(), non_neg_integer()) -> {ok, trees()}.
process_single(Tx, Trees, Height) ->
    Mod = aec_tx_dispatcher:handler(Tx),
    Mod:process(Tx, Trees, Height).

-spec grant_fee_to_miner(list(aec_tx_sign:signed_tx()), trees(), non_neg_integer(), height()) ->
                                trees().
grant_fee_to_miner([], Trees, 0, _Height) ->
    lager:info("Empty block -- no fee"),
    Trees;
grant_fee_to_miner(SignedTxs, Trees0, TotalFee, Height) ->
    CoinbaseTxs = lists:filter(fun is_coinbase/1, SignedTxs),
    case CoinbaseTxs of
        [] ->
            lager:info("Invalid coinbase_tx transaction in block -- no fee"),
            Trees0;
        [SignedCoinbaseTx] ->
            #coinbase_tx{account = MinerPubkey} = aec_tx_sign:data(SignedCoinbaseTx),
            AccountsTrees0 = aec_trees:accounts(Trees0),

            {value, Account0} = aec_accounts_trees:lookup(MinerPubkey, AccountsTrees0),
            {ok, Account} = aec_accounts:earn(Account0, TotalFee, Height),

            AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            Trees
    end.
