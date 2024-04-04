-module(aec_fork_block_settings).

-include_lib("aecontract/include/hard_forks.hrl").

-export([hardcoded_dir/1,
         genesis_accounts/0,
         minerva_accounts/0,
         fortuna_accounts/0,
         lima_accounts/0,
         lima_extra_accounts/0,
         lima_contracts/0,
         block_whitelist/0,
         pre_iris_map_ordering/0,
         is_custom_fork/1,
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


-spec genesis_accounts() -> list().
genesis_accounts() ->
    ConsensusModule = aec_consensus:get_genesis_consensus_module(),
    Header = ConsensusModule:genesis_raw_header(),
    Protocol = aec_headers:version(Header),

    PresetAs = preset_accounts(accounts, Protocol, genesis_accounts_file_missing),
    ConfigAs = configured_genesis_accounts(),
    merge_accounts(PresetAs, ConfigAs).

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
lima_contracts() -> preset_hardcoded_contracts(?LIMA_PROTOCOL_VSN,
                                     lima_contracts_file_missing).

-spec accounts(aec_hard_forks:protocol_vsn()) -> list().
accounts(ProtocolVsn) -> preset_accounts(accounts, ProtocolVsn, accounts_file_missing).

-spec extra_accounts(aec_hard_forks:protocol_vsn()) -> list().
extra_accounts(ProtocolVsn) -> preset_accounts(extra_accounts, ProtocolVsn, extra_accounts_file_missing).

-spec contracts(aec_hard_forks:protocol_vsn()) -> map().
contracts(ProtocolVsn) -> preset_contracts(ProtocolVsn, contracts_file_missing).

-spec is_custom_fork(aec_hard_forks:protocol_vsn()) -> boolean().
is_custom_fork(ProtocolVsn) ->
    case aeu_env:config_value([<<"chain">>, <<"hard_forks">>], aecore, hard_forks, undefined) of
        undefined ->
            false;
        Map ->
            case maps:get(integer_to_binary(ProtocolVsn), Map, undefined) of
                Height when is_integer(Height) ->
                    false;
                _ ->
                    case application:get_env(aecore, fork) of
                        {ok, #{version := ProtocolVsn}} -> false;
                        _ -> true
                    end
            end
    end.

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
            erlang:error({ErrorMsg, PresetAccountsFile});
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
            Accounts = decode_accounts(DecodedData),
            % ensure deterministic ordering of accounts
            lists:keysort(1, Accounts)
    end.

-spec read_preset(accounts | extra_accounts, aec_hard_forks:protocol_vsn()) ->
        {ok, binary()} | {error, {atom(), string()}}.
read_preset(accounts, Release) ->
    read_hard_fork_file(Release, <<"accounts_file">>, fun(Protocol) -> read_hard_fork_file(aec_fork_block_settings:accounts_file_name(Protocol)) end);
read_preset(extra_accounts, Release) ->
    read_hard_fork_file(Release, <<"extra_accounts_file">>, fun(Protocol) -> read_hard_fork_file(aec_fork_block_settings:extra_accounts_file_name(Protocol)) end).

read_hard_fork_file(Protocol, Key, DefaultFun) when is_integer(Protocol) ->
    case aeu_env:config_value([<<"chain">>, <<"hard_forks">>], aecore, hard_forks, undefined) of
        undefined ->
            DefaultFun(Protocol);
        Map when is_map(Map) ->
            case maps:get(integer_to_binary(Protocol), Map, undefined) of
                undefined ->
                    DefaultFun(Protocol);
                Height when is_integer(Height) ->
                    DefaultFun(Protocol);
                Map1 when is_map(Map1) ->
                    case maps:get(Key, Map1, undefined) of
                        %% Setting files for a height are not mandatory so return empty object, an error will be return if the file name is set but not found
                        undefined ->
                            {ok, <<"{}">>};
                        FileName ->
                            AbsoluteFileName = case filename:pathtype(FileName) of
                                                absolute ->
                                                    FileName;
                                                _ ->
                                                    filename:join([aeu_env:data_dir(aecore), FileName])
                                                end,
                            read_hard_fork_file(AbsoluteFileName)
                    end
            end
    end.

read_hard_fork_file(FileName) ->
    case file:read_file(FileName) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, FileName}}
    end.

-spec preset_contracts(aec_hard_forks:protocol_vsn(), atom()) -> map().
preset_contracts(Release, ErrorMsg) ->
    case read_preset_contracts(Release) of
        {error, {_Err, Msg}} ->
            erlang:error({ErrorMsg, Msg});
        {ok, JSONData} ->
            jsx:decode(JSONData, [return_maps])
    end.

-spec preset_hardcoded_contracts(aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_hardcoded_contracts(Release, ErrorMsg) ->
    case read_hard_fork_file(aec_fork_block_settings:contracts_file_name(Release)) of
        {error, {_Err, Msg}} ->
            erlang:error({ErrorMsg, Msg});
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
    read_hard_fork_file(Release, <<"contracts_file">>, fun(Protocol) -> read_hard_fork_file(aec_fork_block_settings:contracts_file_name(Protocol)) end).

accounts_file_name(Release) ->
    case aeu_env:find_config([<<"system">>, <<"custom_prefunded_accs_file">>], [user_config]) of
        undefined ->
            filename:join([hardcoded_dir(Release), accounts_json_file()]);
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
    filename:join([hardcoded_dir(Release), extra_accounts_json_file()]).

contracts_file_name(Release) ->
    filename:join([hardcoded_dir(Release), contracts_json_file()]).


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

hardcoded_basename(ProtocolVsn) ->
    case ProtocolVsn of
        ?ROMA_PROTOCOL_VSN    -> ?GENESIS_DIR;
        ?MINERVA_PROTOCOL_VSN -> ?MINERVA_DIR;
        ?FORTUNA_PROTOCOL_VSN -> ?FORTUNA_DIR;
        ?LIMA_PROTOCOL_VSN    -> ?LIMA_DIR;
        ?IRIS_PROTOCOL_VSN    -> ?IRIS_DIR;
        ?CERES_PROTOCOL_VSN   -> ?CERES_DIR
    end.

configured_genesis_accounts() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> [];
        <<"ae_uat">> -> [];
        _ -> decode_accounts(aeu_env:user_config([<<"chain">>, <<"genesis_accounts">>], #{}))
    end.

decode_accounts(EncodedAccounts) when is_map(EncodedAccounts) ->
    decode_accounts(maps:to_list(EncodedAccounts));
decode_accounts(EncodedAccounts) when is_list(EncodedAccounts) ->
    F = fun({EncodedPubKey, Amount}) ->
            {ok, PubKey} = aeser_api_encoder:safe_decode(account_pubkey, EncodedPubKey),
            {PubKey, Amount}
        end,
    lists:map(F, EncodedAccounts).

%% Merge accounts from file and accounts from config - config takes precedence.
merge_accounts(PresetAs, ConfigAs) ->
    lists:keysort(1, maps:to_list(maps:merge(maps:from_list(PresetAs), maps:from_list(ConfigAs)))).
