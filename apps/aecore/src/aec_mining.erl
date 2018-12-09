%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

-export([mine/4]).

-ifdef(TEST).
-export([get_miner_account_balance/0]).
-endif.

-spec mine(binary(), aec_pow:sci_int(), aec_pow:nonce(), non_neg_integer()) ->  aec_pow:pow_result().
mine(HeaderBin, Target, Nonce, MinerInstance) ->
    aec_pow_cuckoo:generate(HeaderBin, Target, Nonce, MinerInstance).

-ifdef(TEST).
-spec get_miner_account_balance() -> {ok, non_neg_integer()} |
                                     {error, account_not_found}.
get_miner_account_balance() ->
    {ok, Pubkey} = aec_keys:pubkey(),
    case aec_chain:get_account(Pubkey) of
        {value, A} ->
            {ok, aec_accounts:balance(A)};
        none ->
            {error, account_not_found}
    end.
-endif.
