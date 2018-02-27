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
         accounts/1,
         signers/1,
         serialize/1,
         deserialize/1,
         for_client/1]).

-behavior(aetx).

-include("common.hrl").

-record(coinbase_tx, {account       = <<>> :: pubkey(),
                      block_height  :: non_neg_integer()}).

-opaque tx() :: #coinbase_tx{}.

-export_type([tx/0]).

-spec new(map()) -> {ok, aetx:tx()}.
new(#{account := AccountPubkey, block_height := Height}) ->
    {ok, aetx:new(?MODULE, #coinbase_tx{account = AccountPubkey,
                                        block_height = Height })}.

-spec fee(tx()) -> integer().
fee(#coinbase_tx{}) ->
    0.

-spec nonce(tx()) -> undefined.
nonce(#coinbase_tx{}) ->
    undefined.

-spec origin(tx()) -> undefined.
origin(#coinbase_tx{}) ->
    undefined.

-spec check(tx(), aec_trees:trees(), height()) ->
                    {ok, aec_trees:trees()} | {error, term()}.
check(#coinbase_tx{block_height = CBHeight}, _Trees, Height)
    when CBHeight =/= Height ->
    {error, wrong_height};
check(#coinbase_tx{account = AccountPubkey}, Trees, Height) ->
    aec_trees:ensure_account_at_height(AccountPubkey, Trees, Height).

%% Only aec_governance:block_mine_reward() is granted to miner's account here.
%% Amount from all the fees of transactions included in the block
%% is added to miner's account in aec_trees:apply_signed_txs/3.
-spec process(tx(), aec_trees:trees(), height()) -> {ok, aec_trees:trees()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    {value, Account0} = aec_accounts_trees:lookup(AccountPubkey, AccountsTrees0),

    Reward = aec_governance:block_mine_reward(),
    {ok, Account} = aec_accounts:earn(Account0, Reward, Height),

    AccountsTrees = aec_accounts_trees:enter(Account, AccountsTrees0),
    Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
    {ok, Trees}.

-spec accounts(tx()) -> [pubkey()].
accounts(#coinbase_tx{account = AccountPubkey}) -> [AccountPubkey].

-spec signers(tx()) -> [pubkey()].
signers(#coinbase_tx{account = AccountPubkey}) -> [AccountPubkey].

-define(CB_TX_VSN, 1).

-spec serialize(tx()) -> [map()].
serialize(#coinbase_tx{account = Account, block_height = Height}) ->
    [#{<<"vsn">> => version()},
     #{<<"acct">> => Account},
     #{<<"h">> => Height}].

-spec deserialize([map()]) -> tx().
deserialize([#{<<"vsn">>  := ?CB_TX_VSN},
             #{<<"acct">> := Account},
             #{<<"h">> := Height}]) ->
    #coinbase_tx{account = Account, block_height = Height}.

for_client(#coinbase_tx{account = Account, block_height = Height}) ->
    #{<<"account">> => aec_base58c:encode(account_pubkey,Account),
      <<"data_schema">> => <<"CoinbaseTxJSON">>, % swagger schema name
      <<"block_height">> => Height,
      <<"vsn">> => ?CB_TX_VSN}.

version() ->
    ?CB_TX_VSN.
