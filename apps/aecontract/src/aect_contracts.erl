%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract objects
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts).

%% API
-export([ deserialize/2
        , store_id/1
        , is_legal_call/2
        , is_legal_version_at_height/3
        , new/1
        , new/5 %% For use without transaction
        , serialize/1
        , serialize_for_client/1
        , compute_contract_pubkey/2
        , compute_contract_store_id/1
        , is_legal_state_fun/1
          %% Getters
        , id/1
        , pubkey/1
        , owner_id/1
        , owner_pubkey/1
        , abi_version/1
        , ct_version/1
        , vm_version/1
        , code/1
        , state/1
        , log/1
        , active/1
        , referrer_ids/1
        , deposit/1
          %% Setters
        , set_pubkey/2
        , set_owner/2
        , set_code/2
        , set_state/2
        , set_log/2
        , set_active/2
        , set_referrers/2
        , set_deposit/2
        , is_legal_serialization_at_height/3
        ]).

-export([pack_vm_abi/1, split_vm_abi/1]).

-include("aecontract.hrl").
-include_lib("aecontract/include/hard_forks.hrl").

-ifdef(TEST).
-export([internal_set_state/2]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type amount() :: non_neg_integer().
-type store() :: aect_contracts_store:store().

-record(contract, {
        %% Normal account fields
        id           :: aec_id:id(),
        owner_id     :: aec_id:id(),
        ct_version   :: version(),
        code         :: binary(),     %% The byte code
        store        :: store(),      %% The current state/store (stored in a subtree in mpt)
        log          :: binary(),     %% The current event log
        active       :: boolean(),    %% false when disabled, but don't remove unless referrer_ids == []
        referrer_ids :: list(aec_id:id()), %% List of contracts depending on this contract
        deposit      :: amount()
    }).

-opaque contract() :: #contract{}.

-type id() :: aec_id:id().
-type pubkey() :: aec_keys:pubkey().
-type store_id() :: binary().
-type serialized() :: binary().
-type vm_version() :: 0..16#FFFF.
-type abi_version() :: 0..16#FFFF.
-type version() :: #{ vm := vm_version(), abi := abi_version() }.
-type serial_version() :: 0..16#FFFFFFFF.
-type height() :: non_neg_integer().
-type vm_usage_type() ::  'call' | 'create' | 'oracle_register'.

-export_type([ contract/0
             , amount/0
             , id/0
             , pubkey/0
             , serialized/0
             , version/0
             , abi_version/0
             , vm_version/0
             , store/0
             ]).

-define(PUB_SIZE, 32).
-define(HASH_SIZE, 32).
-define(CONTRACT_TYPE, contract).
-define(CONTRACT_VSN, 1).
-define(STORE_PREFIX, <<16:8/integer-unsigned-unit:1>>). %% To collect storage trees in one subtree.

%%%===================================================================
%%% API
%%%===================================================================

-spec is_legal_call(FromVersion :: version(), ToVersion :: version()) ->
                          boolean().
%% NOTE: Keep this up to date if the different sophia vms gets incompatible.
is_legal_call(X, X) -> true;
is_legal_call(#{vm := ?VM_AEVM_SOPHIA_2, abi := X},
              #{vm := ?VM_AEVM_SOPHIA_1, abi := X}) -> true;
is_legal_call(_, _) -> false.

-spec is_legal_version_at_height(vm_usage_type(), version(), height()) -> boolean().
is_legal_version_at_height(Operation, Version, Height) ->
    ProtocolVSN = aec_hard_forks:protocol_effective_at_height(Height),
    is_legal_version_in_protocol(Operation, Version, ProtocolVSN).

is_legal_serialization_at_height(?ABI_SOLIDITY_1, _, _Height) ->
    true;
is_legal_serialization_at_height(?ABI_SOPHIA_1, Vsn, Height) ->
    aect_sophia:is_legal_serialization_at_height(Vsn, Height).

is_legal_version_in_protocol(create, #{vm := ?VM_AEVM_SOPHIA_1, abi := ?ABI_SOPHIA_1}, ProtocolVersion) ->
    case ProtocolVersion of
        ?ROMA_PROTOCOL_VSN    -> true;
        ?MINERVA_PROTOCOL_VSN -> false
    end;
is_legal_version_in_protocol(create, #{vm := ?VM_AEVM_SOPHIA_2, abi := ?ABI_SOPHIA_1}, ProtocolVersion) ->
    case ProtocolVersion of
        ?ROMA_PROTOCOL_VSN    -> false;
        ?MINERVA_PROTOCOL_VSN -> true
    end;
is_legal_version_in_protocol(call, #{vm := VMVersion}, ProtocolVersion) ->
    case ProtocolVersion of
        ?ROMA_PROTOCOL_VSN    when VMVersion =:= ?VM_AEVM_SOPHIA_1 -> true;
        ?MINERVA_PROTOCOL_VSN when VMVersion =:= ?VM_AEVM_SOPHIA_1;
                                   VMVersion =:= ?VM_AEVM_SOPHIA_2 -> true;
        _                     when VMVersion =:= ?VM_AEVM_SOLIDITY_1 -> ?VM_AEVM_SOLIDITY_1_enabled;
        _ -> false
    end;
is_legal_version_in_protocol(oracle_register, #{abi := ?ABI_NO_VM}, _ProtocolVersion) ->
    true;
is_legal_version_in_protocol(oracle_register, #{abi := ?ABI_SOPHIA_1}, _ProtocolVersion) ->
    true;
is_legal_version_in_protocol(_, _, _) ->
    false.

-spec store_id(contract()) -> store_id().
store_id(C) ->
    compute_contract_store_id(pubkey(C)).

-spec new(aect_create_tx:tx()) -> contract().
new(RTx) ->
    new(aect_create_tx:owner_pubkey(RTx),
        aect_create_tx:nonce(RTx),
        aect_create_tx:ct_version(RTx),
        aect_create_tx:code(RTx),
        aect_create_tx:deposit(RTx)).

-spec new(aec_keys:pubkey(), integer(), version(), binary(), amount()) -> contract().
%% NOTE: Should only be used for contract execution without transaction
new(Owner, Nonce, CTVersion, Code, Deposit) ->
    Pubkey = compute_contract_pubkey(Owner, Nonce),
    C = #contract{ id           = aec_id:create(contract, Pubkey),
                   owner_id     = aec_id:create(account, Owner),
                   ct_version   = CTVersion,
                   code         = Code,
                   store        = aect_contracts_store:new(),
                   log          = <<>>,
                   active       = true,
                   referrer_ids = [],
                   deposit      = Deposit
                 },
    C = assert_fields(C),
    C.

-spec serialize(contract()) -> serialized().
serialize(#contract{owner_id     = OwnerId,
                    ct_version   = CTVersion,
                    code         = Code,
                    log          = Log,
                    active       = Active,
                    referrer_ids = ReferrerIds,
                    deposit      = Deposit}) ->
    aec_object_serialization:serialize(
      ?CONTRACT_TYPE,
      ?CONTRACT_VSN,
      serialization_template(?CONTRACT_VSN),
      [ {owner_id, OwnerId}
      , {ct_version, pack_vm_abi(CTVersion)}
      , {code, Code}
      , {log, Log}
      , {active, Active}
      , {referrer_ids, ReferrerIds}
      , {deposit, Deposit}
      ]).

-spec serialize_for_client(contract()) -> map().
serialize_for_client(#contract{id           = Id,
                               owner_id     = OwnerId,
                               ct_version   = CTVersion,
                               log          = Log,
                               active       = Active,
                               referrer_ids = ReferrerIds,
                               deposit      = Deposit}) ->
    #{ <<"id">>           => aehttp_api_encoder:encode(id_hash, Id)
     , <<"owner_id">>     => aehttp_api_encoder:encode(id_hash, OwnerId)
     , <<"vm_version">>   => maps:get(vm, CTVersion)
     , <<"abi_version">>  => maps:get(abi, CTVersion)
     , <<"log">>          => Log
     , <<"active">>       => Active
     , <<"referrer_ids">> => [aehttp_api_encoder:encode(id_hash, RId) || RId <- ReferrerIds]
     , <<"deposit">>      => Deposit
     }.

-spec deserialize(pubkey(), serialized()) -> contract().
deserialize(Pubkey, Bin) ->
    [ {owner_id, OwnerId}
    , {ct_version, CTVersion}
    , {code, Code}
    , {log, Log}
    , {active, Active}
    , {referrer_ids, ReferrerIds}
    , {deposit, Deposit}
    ] = aec_object_serialization:deserialize(
          ?CONTRACT_TYPE,
          ?CONTRACT_VSN,
          serialization_template(?CONTRACT_VSN),
          Bin
          ),
    [contract = aec_id:specialize_type(R) || R <- ReferrerIds],
    account = aec_id:specialize_type(OwnerId),
    #contract{ id           = aec_id:create(contract, Pubkey)
             , owner_id     = OwnerId
             , ct_version   = split_vm_abi(CTVersion)
             , code         = Code
             , store        = aect_contracts_store:new()
             , log          = Log
             , active       = Active
             , referrer_ids = ReferrerIds
             , deposit      = Deposit
             }.

serialization_template(?CONTRACT_VSN) ->
    [ {owner_id, id}
    , {ct_version, int}
    , {code, binary}
    , {log, binary}
    , {active, bool}
    , {referrer_ids, [id]}
    , {deposit, int}
    ].

-spec compute_contract_pubkey(aec_keys:pubkey(), non_neg_integer()) -> aec_keys:pubkey().
compute_contract_pubkey(<<_:?PUB_SIZE/binary>> = Owner, Nonce) when Nonce >= 0  ->
    NonceBin = binary:encode_unsigned(Nonce),
    aec_hash:hash(pubkey, <<Owner/binary, NonceBin/binary>>).

-spec compute_contract_store_id(aec_keys:pubkey()) -> store_id().
compute_contract_store_id(CId) ->
    %% The STORE_PREFIX is used to name the storage tree and keep
    %% all storage nodes in one subtree under the contract tree.
    << CId/binary, ?STORE_PREFIX/binary>>.

-spec is_legal_state_fun(contract()) -> fun((store()) -> boolean()).
is_legal_state_fun(Contract) ->
    fun(Store) ->
            try set_state(Store, Contract) of
                _ -> true
            catch
                _:_ -> false
            end
    end.

%%%===================================================================
%%% Getters

%% The Id if the contract.
-spec id(contract()) -> id().
id(#contract{id = Id}) ->
   Id.

%% The address of the contract account.
-spec pubkey(contract()) -> pubkey().
pubkey(#contract{id = Id}) ->
    aec_id:specialize(Id, contract).

%% The id of the owner.
-spec owner_id(contract()) -> aec_id:id().
owner_id(#contract{owner_id = OwnerId}) ->
    OwnerId.

%% The owner of the contract is (initially) the account that created it.
-spec owner_pubkey(contract()) -> aec_keys:pubkey().
owner_pubkey(#contract{owner_id = OwnerId}) ->
    aec_id:specialize(OwnerId, account).

%% The ABI version used by the contract.
-spec abi_version(contract()) -> abi_version().
abi_version(#contract{ct_version = #{abi := ABIVersion}}) ->
    ABIVersion.

%% The version used by the contract.
-spec ct_version(contract()) -> version().
ct_version(#contract{ct_version = CTVersion}) ->
    CTVersion.

%% The VM version used by the contract.
-spec vm_version(contract()) -> vm_version().
vm_version(#contract{ct_version = #{vm := VmVersion}}) ->
    VmVersion.

%% The contract byte code.
-spec code(contract()) -> binary().
code(#contract{code = Code}) ->
    Code.

%% The representation of the contract state data.
-spec state(contract()) -> store().
state(#contract{store = Store}) ->
    Store.

%% The log of the contract.
-spec log(contract()) -> binary().
log(#contract{log = Log}) ->
    Log.

%% Is the contract active or has it been deactivated?
-spec active(contract()) -> boolean().
active(#contract{active = Active}) ->
    Active.

%% A list of other contracts referring to this contract.
-spec referrer_ids(contract()) -> list(aec_id:id()).
referrer_ids(#contract{referrer_ids = ReferrerIds}) ->
    ReferrerIds.

%% The amount deposited at contract creation.
-spec deposit(contract()) -> amount().
deposit(#contract{deposit = Deposit}) ->
    Deposit.

%%%===================================================================
%%% Setters

-spec set_pubkey(pubkey(), contract()) -> contract().
set_pubkey(X, C) ->
    C#contract{id = aec_id:create(contract, assert_field(pubkey, X))}.

-spec set_owner(aec_keys:pubkey(), contract()) -> contract().
set_owner(X, C) ->
    C#contract{owner_id = aec_id:create(account, assert_field(pubkey, X))}.

-spec set_code(binary(), contract()) -> contract().
set_code(X, C) ->
    C#contract{code = assert_field(code, X)}.

-spec set_state(store(), contract()) -> contract().
set_state(X, C) ->
    internal_set_state(assert_field(store, X, C), C).

internal_set_state(X, C) ->
    C#contract{store = X}.

-spec set_log(binary(), contract()) -> contract().
set_log(X, C) ->
    C#contract{log = assert_field(log, X)}.

-spec set_active(boolean(), contract()) -> contract().
set_active(X, C) ->
    C#contract{active = assert_field(active, X)}.

-spec set_referrers([id()], contract()) -> contract().
set_referrers(X, C) ->
    C#contract{referrer_ids = [aec_id:create(contract, Y)
                               || Y <- assert_field(referrers, X)]}.

-spec set_deposit(amount(), contract()) -> contract().
set_deposit(X, C) ->
    C#contract{deposit = assert_field(deposit, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(C) ->
    List = [ {pubkey,     pubkey(C)}
           , {owner,      owner_pubkey(C)}
           , {ct_version, C#contract.ct_version}
           , {code,       C#contract.code}
           , {store,      C#contract.store}
           , {log,        C#contract.log}
           , {active,     C#contract.active}
           , {referrers,  referrer_ids(C)}
           , {deposit,    C#contract.deposit}
           ],
    List1 = [try assert_field(X, Y, C), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> C;
        Other -> error({missing, Other})
    end.

assert_field(store = FieldKey, FieldValue, C) ->
    %% We can't afford to validate the mptree part of the store
    assert_field_store(FieldKey, aect_contracts_store:write_cache(FieldValue),
                       C#contract.ct_version),
    FieldValue;
assert_field(FieldKey, FieldValue, _) ->
    assert_field(FieldKey, FieldValue).

assert_field(pubkey, <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(owner,  <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(ct_version, X) ->
    case is_legal_version(X) of
        true  -> X;
        false -> error({illegal, ct_version, X})
    end;
assert_field(code, X)       when is_binary(X)            -> X;
assert_field(log, X)        when is_binary(X)            -> X;
assert_field(active, X)     when X =:= true; X =:= false -> X;
assert_field(referrers = Field, X) ->
    try [ assert_field(referrer, Y) || Y <- X ]
    catch _:_ -> error({illegal, Field, X}) end;
assert_field(referrer, <<_:?PUB_SIZE/binary>> = X)       -> X;
assert_field(deposit, X)    when is_integer(X), X >= 0   -> X;
assert_field(Field, X) -> error({illegal, Field, X}).

assert_field_store(store = Field, X, Version) when is_map(X) ->
    try
        F = fun(K, V, unused) ->
                    assert_field_store(store_k, K, Version),
                    assert_field_store(store_v, V, Version),
                    unused
            end,
        %% map iterator would limit memory usage though it is available from OTP 21.
        maps:fold(F, unused, X),
        X
    catch _:_ -> error({illegal, Field, X}) end;
assert_field_store(store_k = Field, X, Version) when is_binary(X),
                                               byte_size(X) > 0 ->
    try true = aevm_eeevm_store:is_valid_key(Version, X)
    catch _:_ -> error({illegal, Field, X}) end;
assert_field_store(store_v, X,_Version) when is_binary(X) -> X.

is_legal_version(#{vm := VM, abi := ABI}) ->
    case {VM, ABI} of
        {?VM_AEVM_SOPHIA_1,   ?ABI_SOPHIA_1}   -> true;
        {?VM_AEVM_SOPHIA_2,   ?ABI_SOPHIA_1}   -> true;
        {?VM_AEVM_SOLIDITY_1, ?ABI_SOLIDITY_1} -> ?VM_AEVM_SOLIDITY_1_enabled;
        _                                      -> false
    end.

-spec split_vm_abi(serial_version()) -> version().
split_vm_abi(VMABI) ->
    <<VM:16, ABI:16>> = <<VMABI:32>>,
    case VM of
        0 -> #{vm => ABI, abi => ABI};
        _ -> #{vm => VM,  abi => ABI}
    end.

-spec pack_vm_abi(version()) -> serial_version().
pack_vm_abi(#{vm := ABI, abi := ABI}) when ABI =< ?ABI_SOLIDITY_1 ->
    ABI;
pack_vm_abi(#{vm := VM, abi := ABI}) ->
    <<VMABI:32>> = <<VM:16, ABI:16>>,
    VMABI.

