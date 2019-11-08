-module(aec_block_fork).

-export([apply_minerva/1,
         apply_fortuna/1,
         apply_lima/2
        ]).

-spec apply_minerva(aec_trees:trees()) -> aec_trees:trees().
apply_minerva(Trees) ->
    apply_accounts_file(Trees, aec_fork_block_settings:minerva_accounts()).

-spec apply_fortuna(aec_trees:trees()) -> aec_trees:trees().
apply_fortuna(Trees) ->
    apply_accounts_file(Trees, aec_fork_block_settings:fortuna_accounts()).

-spec apply_lima(aec_trees:trees(), aetx_env:env()) -> aec_trees:trees().
apply_lima(Trees, TxEnv) ->
    Trees1 = apply_accounts_file(Trees, aec_fork_block_settings:lima_accounts()),
    Trees2 = apply_accounts_file(Trees1, aec_fork_block_settings:lima_extra_accounts()),
    apply_hard_fork_contracts_file(aec_fork_block_settings:lima_contracts(), Trees2, TxEnv).

apply_accounts_file(Trees, Accounts) ->
    AccTrees =
        lists:foldl(
            fun({Pubkey, Balance}, AccumAccTrees) when
                  is_integer(Balance) andalso Balance >= 0
                  andalso byte_size(Pubkey) =:= 32 ->
                Account =
                    case aec_accounts_trees:lookup(Pubkey, AccumAccTrees) of
                        none -> aec_accounts:new(Pubkey, Balance);
                        {value, OldAcc} ->
                            {ok, NewAcc} = aec_accounts:earn(OldAcc, Balance),
                            NewAcc
                    end,
                aec_accounts_trees:enter(Account, AccumAccTrees)
            end,
            aec_trees:accounts(Trees),
            Accounts),
    aec_trees:set_accounts(Trees, AccTrees).

%%% ===========================================
%%% Inserting a contract at hard fork height
%%% ===========================================
%%%
%%%  This is introduced at Lima hard fork to enable distributing funds
%%%  to accounts that missed the final migration deadline.
%%%
%%%  The contracts are created in the normal way, using a
%%%  contract_create_tx, but to avoid side effects from the init call,
%%%  the transactions are executed in isolation on the same state
%%%  trees (i.e., the state trees coming in to the key block).
%%%
%%%  The new contracts objects and their accounts are then inserted
%%%  into the original state trees, one by one.
%%%
%%%  The owner of the contracts (i.e., the originator of the fictive
%%%  create transactions) is the lock account from governance.  Since
%%%  the contract pubkeys depends on the nonce of this account, the
%%%  nonce must be bumped, but the balance of this account is unchanged.
%%%
%%%  The end result is that the state trees is transformed as:
%%%    - The new contracts are inserted
%%%    - The new contracts have balances from freshly created tokens
%%%    - The lock account will have a new nonce.
%%%
%%%  Note that there are no call objects, and no transactions recorded on
%%%  the chain, and no fees or gas compensation, is given for this execution.
%%%
%%%  Also note that any side effects of executing the init function
%%%  will only affect the account itself. It is therefore highly
%%%  discouraged to have other side effects in the init call.


apply_hard_fork_contracts_file([], Trees,_TxEnv) ->
    %% Nothing to do
    Trees;
apply_hard_fork_contracts_file(Specs0, Trees, TxEnv) ->
    Specs         = sort_by_nonce(Specs0),
    Static        = fork_contracts_static_specs(TxEnv),
    OwnerPubkey   = maps:get(owner_pubkey, Static),
    CreateTxs     = fun(Spec) -> contract_create_tx(Spec, Static, TxEnv) end,
    Txs           = lists:map(CreateTxs, Specs),
    Trees1        = fork_contracts_prepare_owner(Txs, Specs, Static, Trees),
    ApplyTxs      = fun(Tx, T) -> apply_hard_fork_contract_tx(Tx, T, TxEnv) end,
    {Contracts, Trees2} = lists:mapfoldl(ApplyTxs, Trees1, Txs),
    Nonce         = get_nonce(OwnerPubkey, Trees2),
    hard_fork_contracts_post_processing(Contracts, OwnerPubkey, Nonce, Trees).

fork_contracts_static_specs(TxEnv) ->
    OwnerPubkey   = aec_governance:locked_coins_holder_account(),
    GasLimit      = aec_governance:block_gas_limit(),
    GasPrice      = aec_governance:minimum_gas_price(aetx_env:consensus_version(TxEnv)),
    #{ owner_pubkey => OwnerPubkey
     , gas_limit    => GasLimit
     , gas_price    => GasPrice
     }.

fork_contracts_prepare_owner(Txs, Specs, Static, Trees) ->
    #{gas_limit := GL, gas_price := GP, owner_pubkey := OwnerPubkey} = Static,
    TotalFee       = lists:sum([aetx:fee(Tx) || Tx <- Txs]),
    TotalAmount    = lists:sum([Amount || #{amount := Amount} <- Specs]),
    NeededFunds    = GP * GL * length(Specs) + TotalAmount + TotalFee,
    AccountsTrees  = aec_trees:accounts(Trees),
    {ok, Account}  = case aec_accounts_trees:lookup(OwnerPubkey, AccountsTrees) of
                         none -> {ok, aec_accounts:new(OwnerPubkey, NeededFunds)};
                         {value, A} -> aec_accounts:earn(A, NeededFunds)
                     end,
    AccountsTrees1 = aec_accounts_trees:enter(Account, AccountsTrees),
    aec_trees:set_accounts(Trees, AccountsTrees1).

get_nonce(OwnerPubkey, Trees) ->
    ATrees = aec_trees:accounts(Trees),
    aec_accounts:nonce(aec_accounts_trees:get(OwnerPubkey, ATrees)).

sort_by_nonce(Specs) ->
    lists:sort(fun(#{nonce := N1}, #{nonce := N2}) -> N1 =< N2 end, Specs).

hard_fork_contracts_post_processing(Cs, OwnerPubkey, Nonce, OriginalTrees) ->
    InitAcc = {aec_trees:contracts(OriginalTrees),
               aec_trees:accounts(OriginalTrees)},
    UpdateFun = fun({Account, Contract}, {CtTree, AccTree}) ->
                        {aect_state_tree:insert_contract(Contract, CtTree),
                         aec_accounts_trees:enter(Account, AccTree)}
                end,
    {Contracts1, Accounts1} = lists:foldl(UpdateFun, InitAcc, Cs),
    %% Make sure the owner exists, has the right nonce, but only the original balance
    OwnerAccount = case aec_accounts_trees:lookup(OwnerPubkey, Accounts1) of
                       none -> aec_accounts:new(OwnerPubkey, 0);
                       {value, A} -> A
                   end,
    OwnerAccount1 = aec_accounts:set_nonce(OwnerAccount, Nonce),
    Accounts2     = aec_accounts_trees:enter(OwnerAccount1, Accounts1),
    aec_trees:set_accounts(aec_trees:set_contracts(OriginalTrees, Contracts1), Accounts2).

contract_create_tx(#{ amount       := Amount
                    , vm_version   := VMVersion
                    , abi_version  := ABIVersion
                    , nonce        := Nonce
                    , code         := Code
                    , call_data    := CallData
                    , pubkey       := ExpectedPubkey},
                   #{ owner_pubkey  := OwnerPubkey
                    , gas_limit     := GasLimit
                    , gas_price     := GasPrice
                    }, TxEnv) ->
    TxSpec = #{owner_id    => aeser_id:create(account, OwnerPubkey),
               nonce       => Nonce,
               code        => Code,
               vm_version  => VMVersion,
               abi_version => ABIVersion,
               deposit     => 0,
               amount      => Amount,
               gas         => GasLimit,
               gas_price   => GasPrice,
               call_data   => CallData,
               fee         => 1000000000000000}, %% Overshoot the size of the actual fee
    {ok, DummyTx} = aect_create_tx:new(TxSpec),
    Height   = aetx_env:height(TxEnv),
    Protocol = aetx_env:consensus_version(TxEnv),
    MinFee   = aetx:min_fee(DummyTx, Height, Protocol),
    {ok, Tx} = aect_create_tx:new(TxSpec#{fee => MinFee}),
    %% Make sure the transaction will give the expected pubkey.
    case aect_contracts:compute_contract_pubkey(OwnerPubkey, Nonce) of
        ExpectedPubkey -> Tx;
        Other          -> error({unexpected_pubkey, Other, ExpectedPubkey})
    end.

apply_hard_fork_contract_tx(Tx, Trees, TxEnv) ->
    case aetx:process(Tx, Trees, TxEnv) of
        {ok, Trees1, _} ->
            OwnerPubkey    = aetx:origin(Tx),
            Nonce          = aetx:nonce(Tx),
            ContractPubkey = aect_contracts:compute_contract_pubkey(OwnerPubkey, Nonce),
            CallPubkey     = aect_call:id(OwnerPubkey, Nonce, ContractPubkey),
            CallTree       = aec_trees:calls(Trees1),
            {value, Call}  = aect_call_state_tree:lookup_call(ContractPubkey, CallPubkey, CallTree),
            case aect_call:return_type(Call) of
                ok ->
                    CtTrees  = aec_trees:contracts(Trees1),
                    AccTrees = aec_trees:accounts(Trees1),
                    Contract = aect_state_tree:get_contract(ContractPubkey, CtTrees, [full_store_cache]),
                    Account  = aec_accounts_trees:get(ContractPubkey, AccTrees),
                    {{Account, Contract}, Trees1};
                What ->
                    Value = aect_call:return_value(Call),
                    error({failed_apply_hard_fork_contracts,{call_error, What, Value}})
            end;
        {error, What} ->
            error({failed_apply_hard_fork_contracts, What, Tx})
    end.
