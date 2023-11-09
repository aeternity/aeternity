-module(aec_fork_block_settings).

-include_lib("aecontract/include/hard_forks.hrl").

-export([dir/1,
         hardcoded_dir/1,
         genesis_accounts/0,
         minerva_accounts/0,
         fortuna_accounts/0,
         lima_accounts/0,
         lima_extra_accounts/0,
         lima_contracts/0,
         block_whitelist/0,
         pre_iris_map_ordering/0,
         hc_seed_contracts/2,
         is_custom_fork/0,
         accounts/1,
         extra_accounts/1,
         contracts/1
        ]).

-export([ accounts_file_name/1
        , extra_accounts_file_name/1
        , contracts_file_name/1
        , whitelist_filename/0
        ]).

-define(GENESIS_DIR, ".genesis").
-define(MINERVA_DIR, ".minerva").
-define(FORTUNA_DIR, ".fortuna").
-define(LIMA_DIR,    ".lima").
-define(IRIS_DIR,    ".iris").
-define(CERES_DIR,   ".ceres").

-spec dir(aec_hard_forks:protocol_vsn()) -> string().
dir(ProtocolVsn) ->
    ProtocolBin = integer_to_binary(ProtocolVsn),
    case aeu_env:config_value([<<"chain">>, <<"hard_forks">>, ProtocolBin, <<"directory">>],
                              aecore, [hard_forks, ProtocolVsn, directory],
                              undefined) of
        undefined ->
            case not is_custom_fork() orelse
                  aeu_env:config_value([<<"chain">>, <<"hard_forks">>, ProtocolBin, <<"use_hardcoded_directory">>],
                                       aecore, [hard_forks, ProtocolVsn, use_hardcoded_directory],
                                       false) of
                true ->
                    hardcoded_dir(ProtocolVsn);
                _ ->
                    BaseName = lists:concat([".", integer_to_list(ProtocolVsn)]),
                    filename:join(aeu_env:data_dir(aecore), BaseName)
            end;
        UserDir ->
            UserDir
    end.

-spec genesis_accounts() -> list().
genesis_accounts() ->
    ConsensusModule = aec_consensus:get_genesis_consensus_module(),
    Header = ConsensusModule:genesis_raw_header(),
    Protocol = aec_headers:version(Header),
    preset_accounts(accounts, Protocol,
                     genesis_accounts_file_missing).

-spec minerva_accounts() -> list().
minerva_accounts() -> preset_accounts(accounts, ?MINERVA_PROTOCOL_VSN,
                                      minerva_accounts_file_missing).

-spec fortuna_accounts() -> list().
fortuna_accounts() -> preset_accounts(accounts, ?FORTUNA_PROTOCOL_VSN,
                                      fortuna_accounts_file_missing).

-spec lima_accounts() -> list().
lima_accounts() -> preset_accounts(accounts, ?LIMA_PROTOCOL_VSN,
                                   lima_accounts_file_missing).

-spec lima_extra_accounts() -> list().
lima_extra_accounts() -> preset_accounts(extra_accounts, ?LIMA_PROTOCOL_VSN,
                                         lima_extra_accounts_file_missing).

-spec lima_contracts() -> list().
lima_contracts() -> preset_contracts(?LIMA_PROTOCOL_VSN,
                                     lima_contracts_file_missing).

-spec accounts(aec_hard_forks:protocol_vsn()) -> list().
accounts(ProtocolVsn) -> preset_accounts(accounts, ProtocolVsn, undefined).

-spec extra_accounts(aec_hard_forks:protocol_vsn()) -> list().
extra_accounts(ProtocolVsn) -> preset_accounts(extra_accounts, ProtocolVsn, undefined).

-spec contracts(aec_hard_forks:protocol_vsn()) -> list().
contracts(ProtocolVsn) -> preset_contracts(ProtocolVsn, undefined).

lookup_fork_config(Protocol, Key, Default) when is_integer(Protocol) ->
    case aeu_env:config_value([<<"chain">>, <<"hard_forks">>], aecore, hard_forks, undefined) of
        undefined ->
            maybe_hardcoded_config(Protocol, Key, Default);
        Map when is_map(Map) ->
            case maps:get(integer_to_binary(Protocol), Map, undefined) of
                undefined ->
                    Default;
                Height when is_integer(Height) ->
                    if Key == <<"height">> ->
                            Height;
                       true ->
                            maybe_hardcoded_config(Protocol, Key, Default)
                    end;
                Map1 when is_map(Map1) ->
                    maps:get(integer_to_binary(Key), Map, Default)
            end
    end.

maybe_hardcoded_config(Protocol, Key, Default) when is_binary(Protocol),
                                                    Protocol =< ?CERES_PROTOCOL_VSN ->
    case Key of
        <<"use_hardcoded_directory">> -> true;
        <<"directory">>               -> hardcoded_dir(Protocol)
    end;
maybe_hardcoded_config(Protocol, Key, Default) ->
    case Key of
        <<"use_hardcoded_dir">> -> false;
        _ ->
            Default
    end.

-spec is_custom_fork() -> boolean().
is_custom_fork() ->
    aeu_env:user_map_or_env([<<"chain">>, <<"hard_forks">>], aecore, hard_forks, undefined) =/= undefined.

-spec hardcoded_dir(aec_hard_forks:protocol_vsn()) -> string().
hardcoded_dir(ProtocolVsn) ->
    Dir = hardcoded_basename(ProtocolVsn),
    filename:join(aeu_env:data_dir(aecore), Dir).

block_whitelist() ->
    P = whitelist_filename(),
    case filelib:is_file(P) of
        false ->
            lager:debug("Block whitelist not present"),
            #{};
        true ->
            lager:debug("Loading block whitelist"),
            {ok, Data} = file:read_file(P),
            Whitelist = maps:from_list([
                begin
                    {key_block_hash, V1} = aeser_api_encoder:decode(V),
                    {binary_to_integer(K), V1} end || {K,V} <- maps:to_list(jsx:decode(Data, [return_maps]))]),
            lager:debug("Loaded ~p whitelisted blocks", [maps:size(Whitelist)]),
            Whitelist
    end.

whitelist_filename() ->
    DefaultPath = filename:join([aeu_env:data_dir(aecore), whitelist_json_file()]),
    {ok, F} = aeu_env:find_config([<<"sync">>, <<"whitelist_file">>],
                                  [user_config,
                                   {value, DefaultPath}]),
    filename:absname(setup:expand_value(aecore, F)).

pre_iris_map_ordering() ->
    P = filename:join([aeu_env:data_dir(aecore), pre_iris_map_ordering_file()]),
    case filelib:is_file(P) of
        false ->
            lager:debug("pre IRIS map ordering file not present"),
            #{};
        true ->
            lager:debug("Loading pre IRIS map ordering"),
            {ok, Data} = file:read_file(P),
            MapOrdering = maps:from_list([
                begin
                    {contract_bytearray, K2} = aeser_api_encoder:decode(K),
                    {contract_bytearray, V2} = aeser_api_encoder:decode(V),
                    {aeb_fate_encoding:deserialize(K2), aeb_fate_encoding:deserialize(V2)}
                end || {K,V} <- maps:to_list(jsx:decode(Data, [return_maps]))]),
            lager:debug("Loaded ~p pre iris map orderings", [maps:size(MapOrdering)]),
            MapOrdering
    end.

-spec preset_accounts(accounts | extra_accounts, aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_accounts(Type, Release, ErrorMsg) ->
    case read_preset(Type, Release) of
        {error, {_Err, PresetAccountsFile}} ->
            % no setup, no preset accounts
            case ErrorMsg of
                undefined ->
                    [];
                _ ->
                    erlang:error({ErrorMsg, PresetAccountsFile})
            end;
        {ok, JSONData} ->
            DecodedData =
                try jsx:decode(JSONData) of
                    %% JSX decodes an empty json object as [{}]:
                    %% [{}] = jsx:decode(<<"{}">>).
                    [{}] -> [];
                    L when is_list(L) -> L
                catch
                  error:_ ->
                    erlang:error(invalid_accounts_json)
                end,
            Accounts =
                lists:map(
                    fun({EncodedPubKey, Amt}) ->
                        {ok, PubKey} = aeser_api_encoder:safe_decode(account_pubkey, EncodedPubKey),
                        {PubKey, Amt}
                    end,
                    DecodedData),
            % ensure deterministic ordering of accounts
            lists:keysort(1, Accounts)
    end.

-spec read_preset(accounts | extra_accounts, aec_hard_forks:protocol_vsn()) ->
        {ok, binary()}| {error, {atom(), string()}}.
read_preset(accounts, Release) ->
    PresetAccountsFile = aec_fork_block_settings:accounts_file_name(Release),
    case file:read_file(PresetAccountsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetAccountsFile}}
    end;
read_preset(extra_accounts, Release) ->
    PresetExtraFile = aec_fork_block_settings:extra_accounts_file_name(Release),
    case file:read_file(PresetExtraFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetExtraFile}}
    end.

-spec preset_contracts(aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_contracts(Release, ErrorMsg) ->
    case read_preset_contracts(Release) of
        {error, {_Err, Msg}} ->
            % no setup, no preset contracts
            case ErrorMsg of
                undefined ->
                    [];
                _ ->
                    erlang:error({ErrorMsg, Msg})
            end;
        {ok, JSONData} ->
            DecodedData =
                try jsx:decode(JSONData) of
                    %% JSX decodes an empty json object as [{}]:
                    %% [{}] = jsx:decode(<<"{}">>).
                    [{}] -> [];
                    L when is_list(L) -> L
                catch
                  error:_ ->
                    erlang:error(invalid_contracts_json)
                end,
            lists:map(
              fun({EncodedPubKey, Spec}) ->
                      Map = maps:from_list([{<<"pubkey">>, EncodedPubKey}|Spec]),
                      decode_contract_spec(Map)
              end,
              DecodedData)
    end.

%%% ===========================================
%%% Spec format for the hard fork contract file
%%% ===========================================
%%%
%%% { <API encoded pubkey for contract> : { "amount" : <integer>,
%%%                                         "vm_version" : <integer>,
%%%                                         "abi_version" : <integer>,
%%%                                         "nonce" : <integer>
%%%                                         "code" : <API encoded contract byte array>
%%%                                         "call_data" : <API encoded contract byte array>
%%%                                       }
%%% ...
%%% }
%%%
%%% The locked token account will be the owner of the contracts.
%%% The nonces must correspond to the nonces of the owner account.
%%% The nonces must be consecutive (but not necessarity ordered)
%%% The pubkey of the contract must correspond to the computed pubkey
%%%    (based on owner account and nonce). This is mostly a fail safe to ensure
%%%    that the contract pubkey is visible in the file.
%%%
%%% Partial example spec:
%%% { "ct_asdf...."    : { "amount"      : 42,
%%%                        "vm_version"  : 5,
%%%                        "abi_version" : 3,
%%%                        "nonce"       : 1,
%%%                        "code"        : "cb_asdf....",
%%%                        "call_data"   : "cb_asdf...."
%%%                      }
%%% }

decode_contract_spec(#{ <<"amount">>       := Amount
                      , <<"vm_version">>   := VMVersion
                      , <<"abi_version">>  := ABIVersion
                      , <<"nonce">>        := Nonce
                      , <<"code">>         := EncodedCode
                      , <<"call_data">>    := EncodedCallData
                      , <<"pubkey">>       := EncodedPubKey} = SpecIn
                    ) when is_integer(Amount),
                           is_integer(VMVersion),
                           is_integer(ABIVersion),
                           is_integer(Nonce),
                           is_binary(EncodedCode),
                           is_binary(EncodedCallData),
                           is_binary(EncodedPubKey) ->
    try
        7 = maps:size(SpecIn), %% Only the fields above
        {ok, PubKey}   = aeser_api_encoder:safe_decode(contract_pubkey, EncodedPubKey),
        {ok, Code}     = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCode),
        {ok, CallData} = aeser_api_encoder:safe_decode(contract_bytearray, EncodedCallData),
        #{ amount       => Amount
         , vm_version   => VMVersion
         , abi_version  => ABIVersion
         , nonce        => Nonce
         , code         => Code
         , call_data    => CallData
         , pubkey       => PubKey}
    catch _:_ ->
        error({invalid_spec, SpecIn})
    end;
decode_contract_spec(SpecIn) ->
    error({invalid_spec, SpecIn}).



-spec read_preset_contracts(aec_hard_forks:protocol_vsn()) -> {ok, binary()}| {error, {atom(), string()}}.
read_preset_contracts(Release) ->
    PresetContractsFile = aec_fork_block_settings:contracts_file_name(Release),
    case file:read_file(PresetContractsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetContractsFile}}
    end.

accounts_file_name(Release) ->
    case aeu_env:find_config([<<"system">>, <<"custom_prefunded_accs_file">>], [user_config]) of
        undefined ->
            filename:join([dir(Release), accounts_json_file()]);
        {ok, CustomAccsFilePath} ->
            case filelib:is_file(CustomAccsFilePath) of 
                true ->
                    lager:info("Custom file for prefunded accounts provided: ~p ~n", [CustomAccsFilePath]),
                    CustomAccsFilePath;
                false ->
                    lager:info("Invalid path to file with prefunded accounts provided: File not found.: ~p ~n", [CustomAccsFilePath]),
                    erlang:error({provided_accounts_file_not_found, CustomAccsFilePath})
            end
    end.

extra_accounts_file_name(Release) ->
    filename:join([dir(Release), extra_accounts_json_file()]).

contracts_file_name(Release) ->
    filename:join([dir(Release), contracts_json_file()]).

seed_contracts_file_name(Release, NetworkId) ->
    filename:join([dir(Release), <<NetworkId/binary, "_contracts.json">>]).

-ifdef(TEST).
accounts_json_file() ->
    NetworkId = aec_governance:get_network_id(),
    ConsensusModule = aec_consensus:get_consensus_module_at_height(0),
    case NetworkId of
        <<"ae_mainnet">>                  -> "accounts.json";
        <<"ae_uat">>                      -> "accounts_uat.json";
        _ ->
            case ConsensusModule:get_type() of
                pos ->
                    NetworkId = aec_governance:get_network_id(),
                    NetworkIdStr = binary_to_list(NetworkId),
                    NetworkIdStr ++ "_accounts.json";
                pow ->
                    "accounts_test.json"
            end
    end.

extra_accounts_json_file() ->
    "extra_accounts_test.json".

contracts_json_file() ->
    "contracts_test.json".

whitelist_json_file() ->
    ".block_whitelist.json".

pre_iris_map_ordering_file() ->
    ".pre_iris_map_ordering.json".

-else.
accounts_json_file() ->
    ConsensusModule = aec_consensus:get_consensus_module_at_height(0),
    NetworkId = aec_governance:get_network_id(),
    case ConsensusModule:get_type() of
        pos ->
            NetworkIdStr = binary_to_list(NetworkId),
            NetworkIdStr ++ "_accounts.json";
        pow ->
            case NetworkId of
                <<"ae_mainnet">>                  -> "accounts.json";
                <<"ae_uat">>                      -> "accounts_uat.json";
                _                                 -> "accounts_test.json"
            end
    end.

extra_accounts_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "extra_accounts.json";
        <<"ae_uat">>     -> "extra_accounts_uat.json";
        _                -> "extra_accounts_test.json"
    end.

contracts_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "contracts.json";
        <<"ae_uat">>     -> "contracts_uat.json";
        _                -> "contracts_test.json"
    end.

whitelist_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> ".block_whitelist.json";
        <<"ae_uat">>     -> ".block_whitelist_uat.json";
        _                -> ".block_whitelist_test.json"
    end.

pre_iris_map_ordering_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> ".pre_iris_map_ordering.json";
        <<"ae_uat">>     -> ".pre_iris_map_ordering_uat.json";
        _                -> ".pre_iris_map_ordering_test.json"
    end.
-endif.

hc_seed_contracts(Protocol, NetworkId) ->
    ContractsFile = seed_contracts_file_name(Protocol, NetworkId),
    case file:read_file(ContractsFile) of
        {ok, Data} ->
            {ok, jsx:decode(Data, [return_maps])};
        {error, Err} -> {error, {Err, ContractsFile}}
    end.

hardcoded_basename(ProtocolVsn) ->
    case ProtocolVsn of
        ?ROMA_PROTOCOL_VSN    -> ?GENESIS_DIR;
        ?MINERVA_PROTOCOL_VSN -> ?MINERVA_DIR;
        ?FORTUNA_PROTOCOL_VSN -> ?FORTUNA_DIR;
        ?LIMA_PROTOCOL_VSN    -> ?LIMA_DIR;
        ?IRIS_PROTOCOL_VSN    -> ?IRIS_DIR;
        ?CERES_PROTOCOL_VSN   -> ?CERES_DIR
    end.
