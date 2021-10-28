-module(aec_fork_block_settings).

-include_lib("aecontract/include/hard_forks.hrl").

-export([dir/1,
         genesis_accounts/0,
         minerva_accounts/0,
         fortuna_accounts/0,
         lima_accounts/0,
         lima_extra_accounts/0,
         lima_contracts/0,
         block_whitelist/0,
         pre_iris_map_ordering/0
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

-spec dir(aec_hard_forks:protocol_vsn()) -> string().
dir(ProtocolVsn) ->
    Dir =
        case ProtocolVsn of
            ?ROMA_PROTOCOL_VSN    -> ?GENESIS_DIR;
            ?MINERVA_PROTOCOL_VSN -> ?MINERVA_DIR;
            ?FORTUNA_PROTOCOL_VSN -> ?FORTUNA_DIR;
            ?LIMA_PROTOCOL_VSN    -> ?LIMA_DIR
        end,
    filename:join(aeu_env:data_dir(aecore), Dir).

-spec genesis_accounts() -> list().
genesis_accounts() -> preset_accounts(accounts, ?ROMA_PROTOCOL_VSN,
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
read_preset_contracts(?LIMA_PROTOCOL_VSN = Release) ->
    PresetContractsFile = aec_fork_block_settings:contracts_file_name(Release),
    case file:read_file(PresetContractsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetContractsFile}}
    end.

accounts_file_name(Release) ->
    filename:join([dir(Release), accounts_json_file()]).

extra_accounts_file_name(Release) ->
    filename:join([dir(Release), extra_accounts_json_file()]).

contracts_file_name(Release) ->
    filename:join([dir(Release), contracts_json_file()]).

-ifdef(TEST).
accounts_json_file() ->
    "accounts_test.json".

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
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "accounts.json";
        <<"ae_uat">>     -> "accounts_uat.json";
        _                -> "accounts_test.json"
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
