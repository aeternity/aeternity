-module(aec_tx).

-export([filter_out_invalid_signatures/1,
         apply_signed/3,
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

-export_type([tx/0]).

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

-spec filter_out_invalid_signatures(list(aec_tx_sign:signed_tx())) -> list(aec_tx_sign:signed_tx()).
filter_out_invalid_signatures(SignedTxs) ->
    lists:filter(
      fun(SignedTx) ->
              ok =:= aec_tx_sign:verify(SignedTx)
      end, SignedTxs).

-spec apply_signed(list(aec_tx_sign:signed_tx()), trees(), non_neg_integer()) ->
                          {ok, list(aec_tx_sign:signed_tx()), trees()}.
apply_signed(SignedTxs, Trees0, Height) ->
    {ok, SignedTxs1, Trees1} = apply_on_state_trees(SignedTxs, Trees0, Height),
    TotalFee = calculate_total_fee(SignedTxs1),
    Trees2 = grant_fee_to_miner(SignedTxs1, Trees1, TotalFee, Height),
    {ok, SignedTxs1, Trees2}.

%% TODO: there should be an easier way to do this...
-spec is_coinbase(aec_tx_sign:signed_tx()) -> boolean().
is_coinbase(Signed) ->
    Tx = aec_tx_sign:data(Signed),
    Mod = tx_dispatcher:handler(Tx),
    Type = Mod:type(),
    <<"coinbase">> =:= Type.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

apply_on_state_trees(SignedTxs, Trees, Height) ->
    apply_on_state_trees(SignedTxs, [], Trees, Height).

apply_on_state_trees([], FilteredSignedTxs, Trees, _Height) ->
    {ok, lists:reverse(FilteredSignedTxs), Trees};
apply_on_state_trees([SignedTx | Rest], FilteredSignedTxs, Trees0, Height) ->
    Tx = aec_tx_sign:data(SignedTx),
    case check_single(Tx, Trees0, Height) of
        {ok, Trees1} ->
            {ok, Trees2} = process_single(Tx, Trees1, Height),
            apply_on_state_trees(Rest, [SignedTx | FilteredSignedTxs], Trees2, Height);
        {error, Reason} ->
            lager:debug("Tx ~p cannot be applied due to an error ~p", [Tx, Reason]),
            apply_on_state_trees(Rest, FilteredSignedTxs, Trees0, Height)
    end.

calculate_total_fee(SignedTxs) ->
    lists:foldl(
      fun(SignedTx, TotalFee) ->
              Fee = fee(aec_tx_sign:data(SignedTx)),
              TotalFee + Fee
      end, 0, SignedTxs).

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

            {ok, Account0} = aec_accounts:get(MinerPubkey, AccountsTrees0),
            {ok, Account} = aec_accounts:earn(Account0, TotalFee, Height),

            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            Trees
    end.
