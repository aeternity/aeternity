%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_coinbase_tx).

%% API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0]).

-behavior(aetx).

-include("common.hrl").
-include("trees.hrl").
-include("core_txs.hrl").


-spec new(map()) -> {ok, coinbase_tx()}.
new(#{account := AccountPubkey}) ->
    {ok, #coinbase_tx{account = AccountPubkey}}.

-spec fee(coinbase_tx()) -> integer().
fee(#coinbase_tx{}) ->
    0.

-spec nonce(coinbase_tx()) -> undefined.
nonce(#coinbase_tx{}) ->
    undefined.

-spec origin(coinbase_tx()) -> undefined.
origin(#coinbase_tx{}) ->
    undefined.

-spec check(coinbase_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    case aec_tx_common:ensure_account_at_height(AccountPubkey, Trees0, Height) of
        {ok, Trees} ->
            {ok, Trees};
        {error, account_height_too_big} = Error ->
            Error
    end.

%% Only aec_governance:block_mine_reward() is granted to miner's account here.
%% Amount from all the fees of transactions included in the block
%% is added to miner's account in aec_tx:apply_signed/3.
-spec process(coinbase_tx(), trees(), height()) -> {ok, trees()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    {value, Account0} = aec_accounts_trees:lookup(AccountPubkey, AccountsTrees0),

    Reward = aec_governance:block_mine_reward(),
    {ok, Account} = aec_accounts:earn(Account0, Reward, Height),

    AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees0),
    Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
    {ok, Trees}.

-spec signers(coinbase_tx()) -> [pubkey()].
signers(#coinbase_tx{account = AccountPubkey}) -> [AccountPubkey].


-define(CB_TX_TYPE, <<"coinbase">>).
-define(CB_TX_VSN, 1).

-spec serialize(coinbase_tx()) -> [map()].
serialize(#coinbase_tx{account = Account}) ->
    [#{<<"type">> => type()},
     #{<<"vsn">> => version()},
     #{<<"acct">> => Account}].

-spec deserialize([map()]) -> coinbase_tx().
deserialize([#{<<"type">> := ?CB_TX_TYPE},
             #{<<"vsn">>  := ?CB_TX_VSN},
             #{<<"acct">> := Account}]) ->
    #coinbase_tx{account = Account}.

-spec type() -> binary().
type() ->
    ?CB_TX_TYPE.

version() ->
    ?CB_TX_VSN.
