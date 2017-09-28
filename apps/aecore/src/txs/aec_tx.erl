-module(aec_tx).

-optional_callbacks([fee/1]).

-export([initiating_account/1,
         initiating_account_nonce/1,
         fee/1,
         apply_signed/3]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").


%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map(), Trees :: trees()) ->
    {ok, Tx :: term()}.

-callback initiating_account(tx()) -> pubkey().

-callback initiating_account_nonce(tx()) -> account_nonce().

-callback fee(tx()) -> fee().

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

%%%%=============================================================================
%% API
%%%=============================================================================

-spec initiating_account(tx()) -> pubkey().
initiating_account(#coinbase_tx{} = Tx) ->
    aec_coinbase_tx:initiating_account(Tx).

-spec initiating_account_nonce(tx()) -> account_nonce().
initiating_account_nonce(#coinbase_tx{} = Tx) ->
    aec_coinbase_tx:initiating_account_nonce(Tx).

-spec fee(tx()) -> {ok, fee()} | {error, {no_fee_for_tx_type, TxType::atom()}}.
fee(#coinbase_tx{}) ->
    {error, {no_fee_for_tx_type, coinbase}}.

-spec apply_signed(list(signed_tx()), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
apply_signed([], Trees, _Height) ->
    {ok, Trees};
apply_signed([SignedTx | Rest], Trees0, Height) ->
    case aec_tx_sign:verify(SignedTx) of
        ok ->
            Tx = aec_tx_sign:data(SignedTx),
            case check_single(Tx, Trees0, Height) of
                {ok, Trees1} ->
                    case process_single(Tx, Trees1, Height) of
                        {ok, Trees2} ->
                            apply_signed(Rest, Trees2, Height);
                        {error, _Reason} = Error ->
                            Error
                    end;
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Check transaction. Prepare state tree: e.g., create newly referenced account
%%------------------------------------------------------------------------------
-spec check_single(tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check_single(#coinbase_tx{} = Tx, Trees, Height) ->
    aec_coinbase_tx:check(Tx, Trees, Height);
check_single(_Other, _Trees_, _Height) ->
    {error, not_implemented}.

%%------------------------------------------------------------------------------
%% Process the transaction. Accounts must already be present in the state tree
%%------------------------------------------------------------------------------
-spec process_single(tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
process_single(#coinbase_tx{} = Tx, Trees, Height) ->
    aec_coinbase_tx:process(Tx, Trees, Height);
process_single(_Other, _Trees_, _Height) ->
    {error, not_implemented}.
