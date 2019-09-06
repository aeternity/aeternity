%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Utilities for checking the block reward (see module aec_dev_reward).
%%%
%%%     Intended for Developer usage from the console of a node.
%%% @end
%%%-------------------------------------------------------------------

-module(aec_split_check).

-export([ run_for_mainnet/1
        , run_for_testnet/1
        , balance_mainnet/1
        , balance_testnet/1
        , balance/2
        , rewards/1
        ]).

-include_lib("aecontract/include/hard_forks.hrl").

-define(TESTNET_ACC, <<"ak_2A3PZPfMC2X7ZVy4qGXz2xh2Lbh79Q4UvZ5fdH7QVFocEgcKzU">>).
-define(MAINNET_ACC, <<"ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8">>).

run_for_mainnet(H) ->
    balance_mainnet(H) =:= rewards(H).

run_for_testnet(H) ->
    balance_testnet(H) =:= rewards(H).

balance_mainnet(H) ->
    balance(H, ?MAINNET_ACC).

balance_testnet(H) ->
    balance(H, ?TESTNET_ACC).

%% Return balance of specified account at key block at specified height.
balance(H, Account) ->
    {ok, HashNext} = aec_chain:get_key_header_by_height(H+1),
    Hash = aec_headers:prev_key_hash(HashNext),
    {ok, Trees} = aec_chain:get_block_state(Hash),
    AccountTrees = aec_trees:accounts(Trees),
    {_, Kbin} = aeser_api_encoder:decode(Account),
    RewardAccount = aec_accounts:balance(aec_accounts_trees:get(Kbin, AccountTrees)),
    RewardAccount.

rewards(H) ->
    #{?FORTUNA_PROTOCOL_VSN := FortunaHeight} = aec_hard_forks:protocols(),
    LastHeight = H - aec_governance:beneficiary_reward_delay(),
    lists:sum([ single(X) || X <- lists:seq(FortunaHeight+1, LastHeight) ]).

single(H) ->
    {ok, HeaderNext} = aec_chain:get_key_header_by_height(H+1),
    Hash = aec_headers:prev_key_hash(HeaderNext),
    {value, Fees} = aec_db:find_block_fees(Hash),
    Ben1 = Fees * 4 div 10,
    Ben2 = Fees - Ben1,
    Ben1Split = Ben1 * 109 div 1000,
    Ben2Split = Ben2 * 109 div 1000,
    Coin = aec_coinbase:coinbase_at_height(H),
    CoinSplit = Coin * 109 div 1000,
    %% Fraud check for current block, as it may reject reward
    {value, false} = aec_db:find_block_fraud_status(Hash),
    %% Fraud check for previous block, as it gives additional reward
    {value, false} = aec_db:find_block_fraud_status(
                       begin {ok, Header} = aec_chain:get_header(Hash),
                             aec_headers:prev_key_hash(Header)
                       end),
    DevReward = Ben1Split + Ben2Split + CoinSplit,
    DevReward.
