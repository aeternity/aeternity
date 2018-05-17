%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts).

%% API
-export([new/2,
         pubkey/1,
         balance/1,
         nonce/1,
         earn/2,
         spend/3,
         set_nonce/2,
         serialize/1,
         deserialize/1]).


-include("common.hrl").

-define(ACCOUNT_VSN, 1).
-define(ACCOUNT_TYPE, account).

-record(account, {
          pubkey = <<>>  :: pubkey(),
          balance = 0    :: non_neg_integer(),
          nonce = 0      :: non_neg_integer()}).

-opaque account() :: #account{}.
-export_type([account/0, deterministic_account_binary_with_pubkey/0]).

-type deterministic_account_binary_with_pubkey() :: binary().

-spec new(pubkey(), non_neg_integer()) -> account().
new(Pubkey, Balance) ->
    #account{pubkey = Pubkey, balance = Balance}.

-spec pubkey(account()) -> pubkey().
pubkey(#account{pubkey = Pubkey}) ->
    Pubkey.

-spec balance(account()) -> non_neg_integer().
balance(#account{balance = Balance}) ->
    Balance.

-spec nonce(account()) -> non_neg_integer().
nonce(#account{nonce = Nonce}) ->
    Nonce.

%% Only used for tests
-spec set_nonce(account(), non_neg_integer()) -> account().
set_nonce(Account, NewNonce) ->
    Account#account{nonce = NewNonce}.

-spec earn(account(), non_neg_integer()) -> {ok, account()}.
earn(#account{balance = Balance0} = Account0, Amount) ->
    {ok, Account0#account{balance = Balance0 + Amount}}.

-spec spend(account(), non_neg_integer(), non_neg_integer()) -> {ok, account()}.
spend(#account{balance = Balance0} = Account0, Amount, Nonce) ->
    {ok, Account0#account{balance = Balance0 - Amount,
                          nonce = Nonce}}.

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    aec_object_serialization:serialize(
      ?ACCOUNT_TYPE, ?ACCOUNT_VSN,
      serialization_template(?ACCOUNT_VSN),
      [ {pubkey, pubkey(Account)}
      , {nonce, nonce(Account)}
      , {balance, balance(Account)}
      ]).

-spec deserialize(binary()) -> account().
deserialize(SerializedAccount) ->
    [ {pubkey, Pubkey}
    , {nonce, Nonce}
    , {balance, Balance}
    ] = aec_object_serialization:deserialize(
          ?ACCOUNT_TYPE,
          ?ACCOUNT_VSN,
          serialization_template(?ACCOUNT_VSN),
          SerializedAccount),
    #account{ pubkey = Pubkey
            , balance = Balance
            , nonce = Nonce
            }.

serialization_template(?ACCOUNT_VSN) ->
    [ {pubkey, binary}
    , {nonce, int}
    , {balance, int}
    ].
