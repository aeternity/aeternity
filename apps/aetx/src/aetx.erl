%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aetx).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-callback new(Args :: map()) ->
    {ok, Tx :: term()} | {error, Reason :: term()}.

-callback fee(Tx :: term()) ->
    Fee :: integer().

-callback nonce(Tx :: term()) ->
    Nonce :: non_neg_integer() | undefined.

-callback origin(Tx :: term()) ->
    Origin :: pubkey() | undefined.

-callback signers(Tx :: term()) ->
    [pubkey()].

-callback check(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()} | {error, Reason :: term()}.

-callback process(Tx :: term(), Trees :: trees(), Height :: non_neg_integer()) ->
    {ok, NewTrees :: trees()}.

-callback serialize(Tx :: term()) ->
    term().

-callback deserialize(term()) ->
    Tx :: term().

-callback type() ->
    binary().
