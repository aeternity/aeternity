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

-spec check(coinbase_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    case aec_tx_common:ensure_account_at_height(AccountPubkey, Trees0, Height) of
        {ok, Trees} ->
            {ok, Trees};
        {error, account_height_too_big} = Error ->
            Error
    end.

-spec process(coinbase_tx(), trees(), height()) -> {ok, trees()}.
process(#coinbase_tx{account = AccountPubkey}, Trees0, Height) ->
    AccountsTrees0 = aec_trees:accounts(Trees0),
    {ok, Account0} = aec_accounts:get(AccountPubkey, AccountsTrees0),

    Reward = aec_governance:block_mine_reward(),
    {ok, Account} = aec_accounts:earn(Account0, Reward, Height),

    {ok, AccountsTrees} = aec_accounts:put(Account, AccountsTrees0),
    Trees = aec_trees:set_accounts(Trees0, AccountsTrees),
    {ok, Trees}.

-define(CB_TX_TYPE, <<"coinbase">>).
-define(CB_TX_VSN, 1).

-spec serialize(coinbase_tx()) -> [map()].
serialize(#coinbase_tx{account = Account}) ->
    [#{<<"type">> => type()}, #{<<"vsn">> => version()},
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
