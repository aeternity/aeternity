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
         set_nonce/2,
         serialize/1,
         deserialize/1]).


-include("common.hrl").

-record(account, {
          pubkey = <<>>  :: pubkey(),
          balance = 0    :: non_neg_integer(),
          nonce = 0      :: non_neg_integer(),
          height = 0     :: height()}).

-opaque account() :: #account{}.
-export_type([account/0, deterministic_account_binary_with_pubkey/0]).

-type deterministic_account_binary_with_pubkey() :: binary().

-spec new(pubkey(), non_neg_integer(), height()) -> account().
new(Pubkey, Balance, Height) ->
    #account{pubkey = Pubkey, balance = Balance, height = Height}.

-spec pubkey(account()) -> pubkey().
pubkey(#account{pubkey = Pubkey}) ->
    Pubkey.

-spec balance(account()) -> non_neg_integer().
balance(#account{balance = Balance}) ->
    Balance.

-spec nonce(account()) -> non_neg_integer().
nonce(#account{nonce = Nonce}) ->
    Nonce.

-spec height(account()) -> height().
height(#account{height = Height}) ->
    Height.

%% Only used for tests
-spec set_nonce(account(), non_neg_integer()) -> account().
set_nonce(Account, NewNonce) ->
    Account#account{nonce = NewNonce}.

-spec earn(account(), non_neg_integer(), height()) -> {ok, account()}.
earn(#account{balance = Balance0} = Account0, Amount, Height) ->
    {ok, Account0#account{balance = Balance0 + Amount,
                          height = Height}}.

-spec spend(account(), non_neg_integer(), non_neg_integer(), height()) -> {ok, account()}.
spend(#account{balance = Balance0} = Account0, Amount, Nonce, Height) ->
    {ok, Account0#account{balance = Balance0 - Amount,
                          nonce = Nonce,
                          height = Height}}.

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    term_to_binary(Account).

-spec deserialize(binary()) -> account().
deserialize(SerializedAccount) ->
    binary_to_term(SerializedAccount).
