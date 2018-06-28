%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% ADT for contract objects
%%% @end
%%%-------------------------------------------------------------------

-module(aect_contracts).

%% API
-export([ deserialize/2
        , id/1
        , store_id/1
        , new/1
        , new/5 %% For use without transaction
        , serialize/1
        , compute_contract_pubkey/2
        , compute_contract_store_id/1
        , is_legal_state_fun/1
          %% Getters
        , pubkey/1
        , owner/1
        , vm_version/1
        , code/1
        , state/1
        , log/1
        , active/1
        , referers/1
        , deposit/1
          %% Setters
        , set_pubkey/2
        , set_owner/2
        , set_vm_version/2
        , set_code/2
        , set_state/2
        , set_log/2
        , set_active/2
        , set_referers/2
        , set_deposit/2
        ]).

-ifdef(TEST).
-export([internal_set_state/2]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type amount() :: non_neg_integer().
-type store() :: #{binary() => binary()}.

-record(contract, {
        %% Normal account fields
        id         :: aec_id:id(),
        owner      :: aec_id:id(),
        vm_version :: vm_version(),
        code       :: binary(),     %% The byte code
        store      :: store(),      %% The current state/store (stored in a subtree in mpt)
        log        :: binary(),     %% The current event log
        active     :: boolean(),    %% false when disabled, but don't remove unless referers == []
        referers   :: [aec_id:id()],%% List of contracts depending on this contract
        deposit    :: amount()
    }).

-opaque contract() :: #contract{}.

-type id() :: aec_keys:pubkey().
-type store_id() :: binary().
-type serialized() :: binary().
-type vm_version() :: byte().

-export_type([ contract/0
             , amount/0
             , id/0
             , serialized/0
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

-spec id(contract()) -> id().
id(C) ->
  pubkey(C).

-spec store_id(contract()) -> store_id().
store_id(C) ->
    compute_contract_store_id(pubkey(C)).

-spec new(aect_create_tx:tx()) -> contract().
new(RTx) ->
    new(aect_create_tx:owner(RTx),
        aect_create_tx:nonce(RTx),
        aect_create_tx:vm_version(RTx),
        aect_create_tx:code(RTx),
        aect_create_tx:deposit(RTx)).

-spec new(aec_keys:pubkey(), integer(), integer(), binary(), amount()) -> contract().
%% NOTE: Should only be used for contract execution without transaction
new(Owner, Nonce, VmVersion, Code, Deposit) ->
    Pubkey = compute_contract_pubkey(Owner, Nonce),
    C = #contract{ id         = aec_id:create(contract, Pubkey),
                   owner      = aec_id:create(account, Owner),
                   vm_version = VmVersion,
                   code       = Code,
                   store      = #{},
                   log        = <<>>,
                   active     = true,
                   referers   = [],
                   deposit    = Deposit
                 },
    C = assert_fields(C),
    C.

-spec serialize(contract()) -> serialized().
serialize(#contract{owner = OwnerId, referers = RefererIds} = C) ->
    aec_object_serialization:serialize(
      ?CONTRACT_TYPE,
      ?CONTRACT_VSN,
      serialization_template(?CONTRACT_VSN),
      [ {owner, OwnerId}
      , {vm_version, vm_version(C)}
      , {code, code(C)}
      , {log, log(C)}
      , {active, active(C)}
      , {referers, RefererIds}
      , {deposit, deposit(C)}
      ]).

-spec deserialize(id(), serialized()) -> contract().
deserialize(Pubkey, Bin) ->
    [ {owner, OwnerId}
    , {vm_version, VmVersion}
    , {code, Code}
    , {log, Log}
    , {active, Active}
    , {referers, RefererIds}
    , {deposit, Deposit}
    ] = aec_object_serialization:deserialize(
          ?CONTRACT_TYPE,
          ?CONTRACT_VSN,
          serialization_template(?CONTRACT_VSN),
          Bin
          ),
    [contract = aec_id:specialize_type(R) || R <- RefererIds],
    account = aec_id:specialize_type(OwnerId),
    #contract{ id         = aec_id:create(contract, Pubkey)
             , owner      = OwnerId
             , vm_version = VmVersion
             , code       = Code
             , store      = #{}
             , log        = Log
             , active     = Active
             , referers   = RefererIds
             , deposit    = Deposit
             }.

serialization_template(?CONTRACT_VSN) ->
    [ {owner, id}
    , {vm_version, int}
    , {code, binary}
    , {log, binary}
    , {active, bool}
    , {referers, [id]}
    , {deposit, int}
    ].

-spec compute_contract_pubkey(aec_keys:pubkey(), non_neg_integer()) -> id().
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

%% The address of the contract account.
-spec pubkey(contract()) -> id().
pubkey(C) -> aec_id:specialize(C#contract.id, contract).

%% The owner of the contract is (initially) the account that created it.
-spec owner(contract()) -> aec_keys:pubkey().
owner(C) -> aec_id:specialize(C#contract.owner, account).

%% The VM version used by the contract.
-spec vm_version(contract()) -> vm_version().
vm_version(C) -> C#contract.vm_version.

%% The contract byte code.
-spec code(contract()) -> binary().
code(C) -> C#contract.code.

%% The representation of the contract state data.
-spec state(contract()) -> store().
state(C) -> C#contract.store.

%% The log of the contract.
-spec log(contract()) -> binary().
log(C) -> C#contract.log.

%% Is the contract active or has it been deactivated?
-spec active(contract()) -> boolean().
active(C) -> C#contract.active.

%% A list of other contracts referring to this contract.
-spec referers(contract()) -> [id()].
referers(C) -> [aec_id:specialize(X, contract) || X <- C#contract.referers].

%% The amount deposited at contract creation.
-spec deposit(contract()) -> amount().
deposit(C) -> C#contract.deposit.

%%%===================================================================
%%% Setters

-spec set_pubkey(id(), contract()) -> contract().
set_pubkey(X, C) ->
    C#contract{id = aec_id:create(contract, assert_field(pubkey, X))}.

-spec set_owner(aec_keys:pubkey(), contract()) -> contract().
set_owner(X, C) ->
    C#contract{owner = aec_id:create(account, assert_field(pubkey, X))}.

-spec set_vm_version(vm_version(), contract()) -> contract().
set_vm_version(X, C) ->
    C#contract{vm_version = assert_field(vm_version, X)}.

-spec set_code(binary(), contract()) -> contract().
set_code(X, C) ->
    C#contract{code = assert_field(code, X)}.

-spec set_state(store(), contract()) -> contract().
set_state(X, C) ->
    internal_set_state(assert_field(store, X), C).

internal_set_state(X, C) ->
    C#contract{store = X}.

-spec set_log(binary(), contract()) -> contract().
set_log(X, C) ->
    C#contract{log = assert_field(log, X)}.

-spec set_active(boolean(), contract()) -> contract().
set_active(X, C) ->
    C#contract{active = assert_field(active, X)}.

-spec set_referers([id()], contract()) -> contract().
set_referers(X, C) ->
    C#contract{referers = [aec_id:create(contract, Y)
                           || Y <- assert_field(referers, X)]}.

-spec set_deposit(amount(), contract()) -> contract().
set_deposit(X, C) ->
    C#contract{deposit = assert_field(deposit, X)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_fields(C) ->
    List = [ {pubkey,     pubkey(C)}
           , {owner,      owner(C)}
           , {vm_version, C#contract.vm_version}
           , {code,       C#contract.code}
           , {store,      C#contract.store}
           , {log,        C#contract.log}
           , {active,     C#contract.active}
           , {referers,   referers(C)}
           , {deposit,    C#contract.deposit}
           ],
    List1 = [try assert_field(X, Y), [] catch _:X -> X end
             || {X, Y} <- List],
    case lists:flatten(List1) of
        [] -> C;
        Other -> error({missing, Other})
    end.

assert_field(pubkey, <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(owner,  <<_:?PUB_SIZE/binary>> = X)         -> X;
assert_field(vm_version, X) when is_integer(X), X > 0,
                                                X < 6    -> X;
assert_field(code, X)       when is_binary(X)            -> X;
assert_field(store = Field, X) when is_map(X) ->
    try
        F = fun(K, V, unused) ->
                    assert_field(store_k, K),
                    assert_field(store_v, V),
                    unused
            end,
        %% map iterator would limit memory usage though it is available from OTP 21.
        maps:fold(F, unused, X),
        X
    catch _:_ -> error({illegal, Field, X}) end;
assert_field(store_k, X) when is_binary(X), byte_size(X) > 0 -> X;
assert_field(store_v, X)    when is_binary(X)            -> X;
assert_field(log, X)        when is_binary(X)            -> X;
assert_field(active, X)     when X =:= true; X =:= false -> X;
assert_field(referers = Field, X) ->
    try [ assert_field(referer, Y) || Y <- X ]
    catch _:_ -> error({illegal, Field, X}) end;
assert_field(referer, <<_:?PUB_SIZE/binary>> = X)        -> X;
assert_field(deposit, X)    when is_integer(X), X >= 0   -> X;
assert_field(Field, X) -> error({illegal, Field, X}).
