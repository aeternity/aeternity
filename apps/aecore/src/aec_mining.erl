%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

-export([mine/5]).

-spec mine(binary(), aec_pow:sci_int(), aec_pow:nonce(),
           aec_pow:miner_config(), aec_pow:miner_instance() | 'undefined') ->  aec_pow:pow_result().
mine(HeaderBin, Target, Nonce, MinerConfig, MinerInstance) ->
    aec_pow_cuckoo:generate(HeaderBin, Target, Nonce, MinerConfig, MinerInstance).

