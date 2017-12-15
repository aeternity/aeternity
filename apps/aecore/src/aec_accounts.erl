%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts).

%% API
-export([new/3,
         pubkey/1,
         balance/1,
         nonce/1,
         height/1,
         earn/3,
         spend/4,
         serialize/1,
         deserialize/1]).

-export_type([deterministic_account_binary_with_pubkey/0]).

-include("common.hrl").
-include("trees.hrl").

-type deterministic_account_binary_with_pubkey() :: binary().

new(Pubkey, Balance, Height) ->
    #account{pubkey = Pubkey, balance = Balance, height = Height}.

pubkey(#account{pubkey = Pubkey}) ->
    Pubkey.

balance(#account{balance = Balance}) ->
    Balance.

nonce(#account{nonce = Nonce}) ->
    Nonce.

height(#account{height = Height}) ->
    Height.

earn(#account{balance = Balance0} = Account0, Amount, Height) ->
    {ok, Account0#account{balance = Balance0 + Amount,
                          height = Height}}.

spend(#account{balance = Balance0} = Account0, Amount, Nonce, Height) ->
    {ok, Account0#account{balance = Balance0 - Amount,
                          nonce = Nonce,
                          height = Height}}.

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    term_to_binary(Account).

deserialize(SerializedAccount) ->
    binary_to_term(SerializedAccount).
