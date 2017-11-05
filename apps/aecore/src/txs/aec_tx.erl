-module(aec_tx).

-export([fee/1,
         apply_signed/3,
         is_coinbase/1]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").


%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map()) ->
    {ok, Tx :: term()} | {error, Reason :: term()}.

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

fee(#coinbase_tx{}) ->
    0;
fee(#spend_tx{fee = F}) ->
    F.

-spec apply_signed(list(signed_tx()), trees(), non_neg_integer()) ->
                          {ok, trees()}.
apply_signed(SignedTxs, Trees0, Height) ->
    Txs = verify_and_extract_txs(SignedTxs),
    {Trees1, TotalFee} = apply_txs_and_calculate_total_fee(Txs, Trees0, Height),
    Trees2 = grant_fee_to_miner(Txs, Trees1, Height, TotalFee),
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

-spec apply_txs_and_calculate_total_fee(list(tx()), trees(), height()) ->
                                               {trees(), non_neg_integer()}.
apply_txs_and_calculate_total_fee(Txs, Trees, Height) ->
    apply_txs_and_calculate_total_fee(Txs, Trees, Height, 0).

-spec apply_txs_and_calculate_total_fee(list(tx()), trees(), height(), non_neg_integer()) ->
                                               {trees(), non_neg_integer()}.
apply_txs_and_calculate_total_fee([], Trees, _Height, TotalFee) ->
    {Trees, TotalFee};
apply_txs_and_calculate_total_fee([Tx | Rest], Trees0, Height, TotalFee) ->
    case check_single(Tx, Trees0, Height) of
        {ok, Trees1} ->
            {ok, Trees2} = process_single(Tx, Trees1, Height),
            TxFee = fee(Tx),
            apply_txs_and_calculate_total_fee(Rest, Trees2, Height, TotalFee + TxFee);
        {error, Reason} ->
            lager:info("Tx ~p cannot be applied due to an error ~p",
                       [Tx, Reason]),
            apply_txs_and_calculate_total_fee(Rest, Trees0, Height, TotalFee)
    end.

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

-spec grant_fee_to_miner(list(tx()), trees(), height(), non_neg_integer()) ->
                                trees().
grant_fee_to_miner([], Trees, _Height, 0) ->
    lager:debug("No transactions in genesis block"),
    Trees;
grant_fee_to_miner(Txs, Trees0, Height, TotalFee) ->
    %% Consider creation of aec_accounts_service,
    %% which will take state trees, height and list of (pubkey, operation) pairs
    %% (valuable also during txs processing)
    MinerPubkey = coinbase_tx_account_pubkey(Txs),
    AccountsTrees0 = aec_trees:accounts(Trees0),

    {ok, Account0} = aec_accounts:get(MinerPubkey, AccountsTrees0),
    {ok, Account} = aec_accounts:earn(Account0, TotalFee, Height),

    {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
    Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
    Trees.

-spec coinbase_tx_account_pubkey(list(tx())) -> pubkey().
coinbase_tx_account_pubkey([#coinbase_tx{account = AccountPubkey} | _Rest]) ->
    AccountPubkey.
