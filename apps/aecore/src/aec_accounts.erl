%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Accounts.
%%% @end
%%%-------------------------------------------------------------------
-module(aec_accounts).

%% API
-export([new/2,
         new/3,

         id/1,
         pubkey/1,
         balance/1,
         flags/1,
         ga_auth_fun/1,
         ga_contract/1,
         nonce/1,

         attach_ga_contract/3,
         earn/2,
         spend/3,
         spend_without_nonce_bump/2,
         set_nonce/2,
         type/1,

         is_payable/1,

         deserialize/2,
         serialize/1,
         serialize_for_client/1]).

-export([ record_fields/1 ]).

-include_lib("aecontract/include/hard_forks.hrl").
-include("../../aecontract/include/aecontract.hrl").

-define(ACCOUNT_VSN_1, 1). %% {Nonce, Balance}
-define(ACCOUNT_VSN_2, 2). %% {Flags, Nonce, Balance, GA_Contract, GA_AuthFun}
-define(ACCOUNT_VSN_3, 3). %% {Flags, Nonce, Balance}
-define(ACCOUNT_TYPE, account).

-type fun_hash() :: <<_:256>>.
-type flag()  :: non_payable.
-type flags() :: [flag()].

-record(account, {
          id          :: aeser_id:id(),
          balance = 0 :: non_neg_integer(),
          nonce = 0   :: non_neg_integer(),
          flags = 0   :: non_neg_integer(),  %% non_payable | ...
          ga_contract :: undefined | aeser_id:id(),
          ga_auth_fun :: undefined | fun_hash() }).

-opaque account() :: #account{}.
-export_type([account/0, deterministic_account_binary_with_pubkey/0]).

-type deterministic_account_binary_with_pubkey() :: binary().

%% ==================================================================
%% Tracing support
record_fields(account) -> record_info(fields, account);
record_fields(_) -> no.

%% ==================================================================

-spec new(aec_keys:pubkey(), non_neg_integer()) -> account().
new(Pubkey, Balance) ->
    new(Pubkey, Balance, []).

-spec new(aec_keys:pubkey(), non_neg_integer(), flags()) -> account().
new(Pubkey, Balance, []) ->
    Id = aeser_id:create(account, Pubkey),
    #account{id = Id, balance = Balance};
new(Pubkey, Balance, Flags) ->
    Id = aeser_id:create(account, Pubkey),
    #account{id = Id, balance = Balance, flags = set_flags(Flags)}.

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
type(#account{ ga_contract = C })         -> contract = aeser_id:specialize_type(C), generalized.

-spec is_payable(account()) -> boolean().
is_payable(#account{ flags = 0 }) -> true;
is_payable(#account{ flags = N }) -> not get_flag(non_payable, N).

-spec serialize(account()) -> deterministic_account_binary_with_pubkey().
serialize(#account{ flags = 0, ga_contract = undefined } = Account) ->
    serialize(?ACCOUNT_VSN_1, Account);
serialize(#account{ ga_contract = GA } = Account) when GA /= undefined ->
    serialize(?ACCOUNT_VSN_2, Account);
serialize(Account) ->
    serialize(?ACCOUNT_VSN_3, Account).

serialize(Vsn = ?ACCOUNT_VSN_1, Account) ->
    aeser_chain_objects:serialize(?ACCOUNT_TYPE, Vsn, serialization_template(Vsn),
      [ {nonce, nonce(Account)}
      , {balance, balance(Account)}
      ]);
serialize(Vsn = ?ACCOUNT_VSN_2, Account) ->
    aeser_chain_objects:serialize(?ACCOUNT_TYPE, Vsn, serialization_template(Vsn),
      [ {flags, flags(Account)}
      , {nonce, nonce(Account)}
      , {balance, balance(Account)}
      , {ga_contract, ga_contract(Account)}
      , {ga_auth_fun, ga_auth_fun(Account)}
      ]);
serialize(Vsn = ?ACCOUNT_VSN_3, Account) ->
    aeser_chain_objects:serialize(?ACCOUNT_TYPE, Vsn, serialization_template(Vsn),
      [ {flags, flags(Account)}
      , {nonce, nonce(Account)}
      , {balance, balance(Account)}
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
            , {nonce, Nonce}
            , {balance, Balance}
            , {ga_contract, GAContract}
            , {ga_auth_fun, GAAuthFun}
            ] = aeser_chain_objects:deserialize(
                    Type, Vsn, serialization_template(Vsn), SerializedAccount),
            #account{ id = aeser_id:create(account, Pubkey)
                    , nonce = Nonce
                    , balance = Balance
                    , flags = Flags
                    , ga_contract = GAContract
                    , ga_auth_fun = GAAuthFun
                    };
        {?ACCOUNT_TYPE = Type, ?ACCOUNT_VSN_3 = Vsn, _Rest} ->
            [ {flags, Flags}
            , {nonce, Nonce}
            , {balance, Balance}
            ] = aeser_chain_objects:deserialize(
                    Type, Vsn, serialization_template(Vsn), SerializedAccount),
            #account{ id = aeser_id:create(account, Pubkey)
                    , nonce = Nonce
                    , balance = Balance
                    , flags = Flags
                    }
    end.

serialization_template(?ACCOUNT_VSN_1) ->
    [ {nonce, int}
    , {balance, int}
    ];
serialization_template(?ACCOUNT_VSN_2) ->
    [ {flags, int}
    , {nonce, int}
    , {balance, int}
    , {ga_contract, id}
    , {ga_auth_fun, binary}
    ];
serialization_template(?ACCOUNT_VSN_3) ->
    [ {flags, int}
    , {nonce, int}
    , {balance, int}
    ].

-spec serialize_for_client(account()) -> map().
serialize_for_client(#account{id      = Id,
                              balance = Balance,
                              nonce   = Nonce} = Account) ->
    ExtraInfo =
        case type(Account) of
            generalized ->
                %% This code is not defensive, we are guaranteed that contract and function hash exist
                %% If not, just crash
                {contract, ContractPK} = aeser_id:specialize(Account#account.ga_contract),
                {ok, Contract} = aec_chain:get_contract(ContractPK),
                AuthFunName =
                    case aect_contracts:abi_version(Contract) of
                        ?ABI_AEVM_SOPHIA_1 ->
                            #{type_info := TypeInfo} = aect_sophia:deserialize(
                                                         aect_contracts:code(Contract)),
                            {ok, AuthFunName0} =
                                aeb_aevm_abi:function_name_from_type_hash(
                                  Account#account.ga_auth_fun, TypeInfo),
                            AuthFunName0;
                        ?ABI_FATE_SOPHIA_1 ->
                            #{byte_code := ByteCode} =
                                aect_sophia:deserialize(aect_contracts:code(Contract)),
                            {ok, AuthFunName0} =
                                aeb_fate_abi:get_function_name_from_function_hash(
                                    Account#account.ga_auth_fun, aeb_fate_code:deserialize(ByteCode)),
                            AuthFunName0
                    end,
                #{<<"kind">>        => <<"generalized">>,
                  <<"auth_fun">>    => AuthFunName,
                  <<"contract_id">> => aeser_api_encoder:encode(contract_pubkey, ContractPK)};
            basic ->
                #{<<"kind">> => <<"basic">>}
        end,
    maps:merge(ExtraInfo, #{<<"id">>      => aeser_api_encoder:encode(id_hash, Id),
                            <<"balance">> => Balance,
                            <<"payable">> => is_payable(Account),
                            <<"nonce">>   => Nonce}).

%% Flags
-define(FLAG_NON_PAYABLE_VALUE, 1).

get_flag(non_payable, N) -> ((N div ?FLAG_NON_PAYABLE_VALUE) rem 2) == 1.

set_flags([]) -> 0;
set_flags([Flag | Flags]) -> flag_to_val(Flag) + set_flags(Flags).

flag_to_val(non_payable) -> ?FLAG_NON_PAYABLE_VALUE;
flag_to_val(_)           -> 0.
