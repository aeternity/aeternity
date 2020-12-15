%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(aec_mining).

-export([check_env/0,
         get_miner_configs/0,
         generate/5,
         verify/4
        ]).

-define(DEFAULT_EXECUTABLE_GROUP   , <<"aecuckoo">>).
-define(DEFAULT_EXTRA_ARGS         , <<>>).
-define(DEFAULT_HEX_ENCODED_HEADER , false).
-define(DEFAULT_REPEATS            , 1).
-define(DEFAULT_EDGE_BITS          , 29).

-define(DEFAULT_CUCKOO_ENV,
        {?DEFAULT_EDGE_BITS,
         [{<<"mean29-generic">>, ?DEFAULT_EXTRA_ARGS, ?DEFAULT_HEX_ENCODED_HEADER,
           ?DEFAULT_REPEATS, undefined, ?DEFAULT_EXECUTABLE_GROUP}]}).

%% API.

%%------------------------------------------------------------------------------
%% Assert that configuration options 'mining > cuckoo > miners' and
%% 'mining > cuckoo > edge_bits' are not used together with deprecated
%% configuration property 'mining > cuckoo > miner'.
%%------------------------------------------------------------------------------
-spec check_env() -> ok.
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

%%------------------------------------------------------------------------------
%% Read and parse miner configs.
%%
%% Miners defined in epoch.{json,yaml} user config file take precedence.
%% If there are no miners defined in the user config, sys.config cuckoo
%% miners are read. If there are neither user config nor sys.config miners
%% ?DEFAULT_CUCKOO_ENV is used as the last resort option (i.e. mean29-generic
%% without any extra args).
%%------------------------------------------------------------------------------
-spec get_miner_configs() -> [aeminer_pow_cuckoo:config()].
get_miner_configs() ->
    case get_miners_from_user_config() of
        {ok, MinerConfigs} -> MinerConfigs;
        undefined ->
            case get_miners_from_deprecated_user_config() of
                {ok, MinerConfigs} -> MinerConfigs;
                undefined          -> get_miners_from_sys_config()
            end
    end.

-spec generate(aeminer_pow_cuckoo:hashable(), aeminer_pow:sci_target(),
               aeminer_pow:nonce(), aeminer_pow_cuckoo:config(),
               aeminer_pow:instance() | undefined) ->
    {ok, {aeminer_pow:nonce(), aeminer_pow_cuckoo:solution()}} | {error, term()}.
generate(Data, Target, Nonce, Config, MinerInstance) ->
    aeminer_pow_cuckoo:generate(Data, Target, Nonce, Config, MinerInstance).

-spec verify(aeminer_pow_cuckoo:hashable(), aeminer_pow:nonce(),
             aeminer_pow_cuckoo:solution(), aeminer_pow:sci_target()) ->
    boolean().
verify(Data, Nonce, Soln, Target) ->
    EdgeBits = get_edge_bits(),
    aeminer_pow_cuckoo:verify(Data, Nonce, Soln, Target, EdgeBits).

%% Internal functions.

%%------------------------------------------------------------------------------
%% Config handling
%%------------------------------------------------------------------------------

get_miners_from_user_config() ->
    case aeu_env:user_map([<<"mining">>, <<"cuckoo">>, <<"miners">>]) of
        {ok, MinerConfigMaps} ->
            MinerConfigs =
                lists:foldl(
                  fun(ConfigMap, Configs) ->
                          [build_miner_config(ConfigMap) | Configs]
                  end, [], MinerConfigMaps),
            {ok, MinerConfigs};
        undefined -> undefined
    end.

get_miners_from_deprecated_user_config() ->
    case aeu_env:user_map([<<"mining">>, <<"cuckoo">>, <<"miner">>]) of
        {ok, MinerConfigMap} ->
            %% In the deprecated config 'mining > cuckoo > miner'
            %% 'instances' is the property indicating the number of instances to be addressed.
            %% Addressed instances list has to be generated accordingly (indexed from 0).
            case maps:get(<<"instances">>, MinerConfigMap, undefined) of
                undefined ->
                    MinerConfigs = [build_miner_config(MinerConfigMap)],
                    {ok, MinerConfigs};
                InstancesCount ->
                    AddressedInstances = lists:seq(0, InstancesCount - 1),
                    MinerConfigMap1    = MinerConfigMap#{<<"addressed_instances">> => AddressedInstances},
                    MinerConfigs       = [build_miner_config(MinerConfigMap1)],
                    {ok, MinerConfigs}
            end;
        undefined -> undefined
    end.

get_miners_from_sys_config() ->
    {_, MinerConfigLists} = get_options(),
    lists:foldl(
      fun({_, _, _, _, _, _} = Config, Configs) ->
              [build_miner_config(Config) | Configs]
      end, [], MinerConfigLists).

get_options() ->
    {_, _} = aeu_env:get_env(aecore, aec_mining, ?DEFAULT_CUCKOO_ENV).

build_miner_config(Config) when is_map(Config) ->
    Exec      = maps:get(<<"executable">>, Config),
    ExecGroup = maps:get(<<"executable_group">>, Config, ?DEFAULT_EXECUTABLE_GROUP),
    ExtraArgs = maps:get(<<"extra_args">>, Config, ?DEFAULT_EXTRA_ARGS),
    HexEncHdr = maps:get(<<"hex_encoded_header">>, Config, ?DEFAULT_HEX_ENCODED_HEADER),
    Repeats   = maps:get(<<"repeats">>, Config, ?DEFAULT_REPEATS),
    Instances = maps:get(<<"addressed_instances">>, Config, undefined),
    EdgeBits  = get_edge_bits(),
    aeminer_pow_cuckoo:config(Exec, ExecGroup, ExtraArgs, HexEncHdr, Repeats, EdgeBits, Instances);
build_miner_config({Exec, ExtraArgs, HexEncHdr, Repeats, Instances, ExecGroup}) ->
    EdgeBits  = get_edge_bits(),
    aeminer_pow_cuckoo:config(Exec, ExecGroup, ExtraArgs, HexEncHdr, Repeats, EdgeBits, Instances).

get_edge_bits() ->
    {EdgeBits, _} = get_options(),
    aeu_env:user_config([<<"mining">>, <<"cuckoo">>, <<"edge_bits">>], EdgeBits).
