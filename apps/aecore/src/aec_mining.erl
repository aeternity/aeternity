%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

-export([check_env/0,
         mine/5]).

%%------------------------------------------------------------------------------
%% Assert that configuration options 'mining > cuckoo > miners' and
%% 'mining > cuckoo > edge_bits' are not used together with deprecated
%% configuration property 'mining > cuckoo > miner'.
%%------------------------------------------------------------------------------
check_env() ->
    case {aeu_env:user_map([<<"mining">>, <<"cuckoo">>, <<"miners">>]),
          aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"edge_bits">>])} of
        {undefined, undefined} -> ok;
        {_, _} ->
            case aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"miner">>]) of
                undefined -> ok;
                _ ->
                    lager:error("Config error: deprecated property 'mining > cuckoo > miner' cannot be used "
                                "together with 'mining > cuckoo > miners' or 'mining > cuckoo > edge_bits'"),
                    exit(cuckoo_config_validation_failed)
            end
    end.

-spec mine(binary(), aec_pow:sci_int(), aec_pow:nonce(),
           aec_pow:miner_config(), aec_pow:miner_instance() | 'undefined') ->  aec_pow:pow_result().
mine(HeaderBin, Target, Nonce, MinerConfig, MinerInstance) ->
    aec_pow_cuckoo:generate(HeaderBin, Target, Nonce, MinerConfig, MinerInstance).

