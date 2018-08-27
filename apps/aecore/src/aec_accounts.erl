%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts).

%% API
-export([new/2,
         id/1,
         pubkey/1,
         balance/1,
         nonce/1,
         earn/2,
         spend/3,
         spend_without_nonce_bump/2,
         set_nonce/2,
         serialize/1,
         deserialize/2,
         serialize_for_client/1]).

-define(ACCOUNT_VSN, 1).
-define(ACCOUNT_TYPE, account).

-record(account, {
          id             :: aec_id:id(),
          balance = 0    :: non_neg_integer(),
          nonce = 0      :: non_neg_integer()}).

-opaque account() :: #account{}.
-export_type([account/0, deterministic_account_binary_with_pubkey/0]).

-type deterministic_account_binary_with_pubkey() :: binary().

-spec new(aec_keys:pubkey(), non_neg_integer()) -> account().
new(Pubkey, Balance) ->
    Id = aec_id:create(account, Pubkey),
    #account{id = Id, balance = Balance}.

-spec id(account()) -> aec_id:id().
id(#account{id = Id}) ->
    Id.

-spec pubkey(account()) -> aec_keys:pubkey().
pubkey(#account{id = Id}) ->
    aec_id:specialize(Id, account).

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

-spec spend_without_nonce_bump(account(), non_neg_integer()) -> {ok, account()}.
%%% NOTE: Only use this if you actually don't want to update the nonce
%%% of the account (e.g., when opening a state channel).
spend_without_nonce_bump(#account{balance = Balance0} = Account0, Amount) ->
    {ok, Account0#account{balance = Balance0 - Amount}}.

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    aec_object_serialization:serialize(
      ?ACCOUNT_TYPE, ?ACCOUNT_VSN,
      serialization_template(?ACCOUNT_VSN),
      [ {nonce, nonce(Account)}
      , {balance, balance(Account)}
      ]).

-spec deserialize(aec_keys:pubkey(), binary()) -> account().
deserialize(Pubkey, SerializedAccount) ->
    [ {nonce, Nonce}
    , {balance, Balance}
    ] = aec_object_serialization:deserialize(
          ?ACCOUNT_TYPE,
          ?ACCOUNT_VSN,
          serialization_template(?ACCOUNT_VSN),
          SerializedAccount),
    #account{ id = aec_id:create(account, Pubkey)
            , balance = Balance
            , nonce = Nonce
            }.

serialization_template(?ACCOUNT_VSN) ->
    [ {nonce, int}
    , {balance, int}
    ].

-spec serialize_for_client(account()) -> map().
serialize_for_client(#account{id      = Id,
                              balance = Balance,
                              nonce   = Nonce}) ->
    #{<<"id">>      => aec_base58c:encode(id_hash, Id),
      <<"balance">> => Balance,
      <<"nonce">>   => Nonce}.

