-module(aec_tx).

-export([apply_signed/3]).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").


%%%=============================================================================
%%% aec_tx behavior callbacks
%%%=============================================================================

-callback new(Args :: map(), Trees :: trees()) ->
    {ok, Tx :: term()}.

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

%%%%=============================================================================
%% API
%%%=============================================================================

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
