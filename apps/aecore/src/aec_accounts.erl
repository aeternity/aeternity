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
         flags/1,
         ga_auth_fun/1,
         ga_contract/1,
         nonce/1,

         attach_ga_contract/3,
         earn/2,
         is_legal_at_height/2,
         spend/3,
         spend_without_nonce_bump/2,
         set_nonce/2,
         type/1,

         deserialize/2,
         serialize/1,
         serialize_for_client/1]).

-include_lib("aecontract/include/hard_forks.hrl").

-define(ACCOUNT_VSN_1, 1). %% {Nonce, Balance}
-define(ACCOUNT_VSN_2, 2). %% {Flags, Nonce, Balance, GA_Contract}
-define(ACCOUNT_TYPE, account).

-type fun_hash() :: <<_:256>>.

-record(account, {
          id          :: aeser_id:id(),
          balance = 0 :: non_neg_integer(),
          nonce = 0   :: non_neg_integer(),
          flags = 0   :: non_neg_integer(),  %% NOTE: not yet in use
          ga_contract :: undefined | aeser_id:id(),
          ga_auth_fun :: undefined | fun_hash() }).

-opaque account() :: #account{}.
-export_type([account/0, deterministic_account_binary_with_pubkey/0]).

-type deterministic_account_binary_with_pubkey() :: binary().

-spec new(aec_keys:pubkey(), non_neg_integer()) -> account().
new(Pubkey, Balance) ->
    Id = aeser_id:create(account, Pubkey),
    #account{id = Id, balance = Balance}.

-spec id(account()) -> aeser_id:id().
id(#account{id = Id}) ->
    Id.

-spec pubkey(account()) -> aec_keys:pubkey().
pubkey(#account{id = Id}) ->
    aeser_id:specialize(Id, account).

-spec balance(account()) -> non_neg_integer().
balance(#account{balance = Balance}) ->
    Balance.

-spec nonce(account()) -> non_neg_integer().
nonce(#account{nonce = Nonce}) ->
    Nonce.

-spec flags(account()) -> non_neg_integer().
flags(#account{flags = Flags}) ->
    Flags.

-spec ga_contract(account()) -> undefined | aeser_id:id().
ga_contract(#account{ga_contract = GAContract}) ->
    GAContract.

-spec ga_auth_fun(account()) -> undefined | fun_hash().
ga_auth_fun(#account{ga_auth_fun = GAAuthFun}) ->
    GAAuthFun.

%% Only used for tests
-spec set_nonce(account(), non_neg_integer()) -> account().
set_nonce(Account, NewNonce) ->
    Account#account{nonce = NewNonce}.

-spec attach_ga_contract(account(), aeser_id:id(), fun_hash()) -> {ok, account()}.
attach_ga_contract(Account, GAContract, <<_:256>> = GAAuthFun) ->
    contract = aeser_id:specialize_type(GAContract),
    {ok, Account#account{ga_contract = GAContract,
                         ga_auth_fun = GAAuthFun }}.

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

-spec type(account()) -> basic | generalized.
type(#account{ ga_contract = undefined }) -> basic;
type(#account{})                          -> generalized.

is_legal_at_height(Account, Height) ->
    case type(Account) of
        basic       -> true;
        generalized -> aec_hard_forks:protocol_effective_at_height(Height) >= ?FORTUNA_PROTOCOL_VSN
    end.

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(Account) ->
    case type(Account) of
        basic -> serialize_basic(Account);
        generalized -> serialize_generalized(Account)
    end.

serialize_basic(Account) ->
    aeser_chain_objects:serialize(
      ?ACCOUNT_TYPE, ?ACCOUNT_VSN_1,
      serialization_template(?ACCOUNT_VSN_1),
      [ {nonce, nonce(Account)}
      , {balance, balance(Account)}
      ]).

serialize_generalized(Account) ->
    aeser_chain_objects:serialize(
      ?ACCOUNT_TYPE, ?ACCOUNT_VSN_2,
      serialization_template(?ACCOUNT_VSN_2),
      [ {flags, flags(Account)}
      , {balance, balance(Account)}
      , {ga_contract, ga_contract(Account)}
      , {ga_auth_fun, ga_auth_fun(Account)}
      ]).

-spec deserialize(aec_keys:pubkey(), binary()) -> account().
deserialize(Pubkey, SerializedAccount) ->
    case aeser_chain_objects:deserialize_type_and_vsn(SerializedAccount) of
        {?ACCOUNT_TYPE = Type, ?ACCOUNT_VSN_1 = Vsn, _Rest} ->
            [ {nonce, Nonce}
            , {balance, Balance}
            ] = aeser_chain_objects:deserialize(
                    Type, Vsn, serialization_template(Vsn), SerializedAccount),
            #account{ id = aeser_id:create(account, Pubkey)
                    , balance = Balance
                    , nonce = Nonce
                    };
        {?ACCOUNT_TYPE = Type, ?ACCOUNT_VSN_2 = Vsn, _Rest} ->
            [ {flags, Flags}
            , {balance, Balance}
            , {ga_contract, GAContract}
            , {ga_auth_fun, GAAuthFun}
            ] = aeser_chain_objects:deserialize(
                    Type, Vsn, serialization_template(Vsn), SerializedAccount),
            #account{ id = aeser_id:create(account, Pubkey)
                    , balance = Balance
                    , flags = Flags
                    , ga_contract = GAContract
                    , ga_auth_fun = GAAuthFun
                    }
    end.

serialization_template(?ACCOUNT_VSN_1) ->
    [ {nonce, int}
    , {balance, int}
    ];
serialization_template(?ACCOUNT_VSN_2) ->
    [ {flags, int}
    , {balance, int}
    , {ga_contract, id}
    , {ga_auth_fun, binary}
    ].

-spec serialize_for_client(account()) -> map().
serialize_for_client(#account{id      = Id,
                              balance = Balance,
                              nonce   = Nonce}) ->
    #{<<"id">>      => aeser_api_encoder:encode(id_hash, Id),
      <<"balance">> => Balance,
      <<"nonce">>   => Nonce}.

