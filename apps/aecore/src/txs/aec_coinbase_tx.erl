-module(aec_coinbase_tx).

%% API
-export([new/2,
         check/3,
         process/3,
         serialize/1,
         deserialize/1,
         type/0]).

-behavior(aec_tx).

-include("common.hrl").
-include("trees.hrl").
-include("txs.hrl").


-spec new(map(), trees()) -> {ok, coinbase_tx()}.
new(#{account := AccountPubkey}, _Trees) ->
    {ok, #coinbase_tx{account = AccountPubkey}}.

-spec check(coinbase_tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
check(#coinbase_tx{account = AccountPubkey,
                   nonce = TxNonce}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, Account} ->
            case aec_accounts:nonce(Account) of
                Nonce when Nonce < TxNonce ->
                    lager:debug("Tx nonce (~p) < Acc nonce (~p) - check!",
                                [TxNonce, Nonce]),
                    {ok, Trees0};
                _ ->
                    {error, nonce_too_small}
            end;
        {error, notfound} ->
            lager:debug("Account (~p) not found", [AccountPubkey]),
            %% Add newly referenced account (w/0 amount) to the state
            Account = aec_accounts:new(AccountPubkey, 0, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees}
    end.

-spec process(coinbase_tx(), trees(), non_neg_integer()) -> {ok, trees()} | {error, term()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    case aec_accounts:get(AccountPubkey, AccountsTrees0) of
        {ok, Account0} ->
            Reward = aec_governance:block_mine_reward(),
            {ok, Account} = aec_accounts:earn(Account0, Reward, Height),
            {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
            Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
            {ok, Trees};
        {error, notfound} ->
            {error, account_not_found}
    end.

-define(CB_TX_TYPE, <<"coinbase">>).
-define(CB_TX_VSN, 1).

serialize(#coinbase_tx{account = Account, nonce = Nonce}) ->
    [type(), version(), Account, Nonce].

deserialize([?CB_TX_TYPE, ?CB_TX_VSN, Account, Nonce]) ->
    #coinbase_tx{account = Account, nonce = Nonce}.

type() ->
    ?CB_TX_TYPE.

version() ->
    ?CB_TX_VSN.
