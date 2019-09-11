-module(aec_fork_block_settings).

-include_lib("aecontract/include/hard_forks.hrl").

-export([dir/1,
         genesis_accounts/0,
         minerva_accounts/0,
         fortuna_accounts/0,
         lima_accounts/0,
         lima_contracts/0
        ]).

-export([ accounts_file_name/1
        , contracts_file_name/1
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
genesis_accounts() -> preset_accounts(?ROMA_PROTOCOL_VSN,
                                      genesis_accounts_file_missing).

-spec minerva_accounts() -> list().
minerva_accounts() -> preset_accounts(?MINERVA_PROTOCOL_VSN,
                                      minerva_accounts_file_missing).

-spec fortuna_accounts() -> list().
fortuna_accounts() -> preset_accounts(?FORTUNA_PROTOCOL_VSN,
                                      fortuna_accounts_file_missing).

-spec lima_accounts() -> list().
lima_accounts() -> preset_accounts(?LIMA_PROTOCOL_VSN,
                                   lima_accounts_file_missing).

-spec lima_contracts() -> list().
lima_contracts() -> preset_contracts(?LIMA_PROTOCOL_VSN,
                                     lima_contracts_file_failed).


-spec preset_accounts(aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_accounts(Release, ErrorMsg) ->
    case read_preset_accounts(Release) of
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

-spec read_preset_accounts(aec_hard_forks:protocol_vsn()) -> {ok, binary()}| {error, {atom(), string()}}.
read_preset_accounts(Release) ->
    PresetAccountsFile = aec_fork_block_settings:accounts_file_name(Release),
    case file:read_file(PresetAccountsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetAccountsFile}}
    end.


-spec preset_contracts(aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_contracts(Release, ErrorMsg) ->
    case read_preset_contracts(Release) of
        {error, {_Err, Msg}} ->
            % no setup, no preset accounts
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
                      {ok, PubKey} = aeser_api_encoder:safe_decode(contract_pubkey, EncodedPubKey),
                      Spec#{pubkey => PubKey}
              end,
              DecodedData)
    end.

-spec read_preset_contracts(aec_hard_forks:protocol_vsn()) -> {ok, binary()}| {error, {atom(), string()}}.
read_preset_contracts(?LIMA_PROTOCOL_VSN = Release) ->
    PresetContractsFile = aec_fork_block_settings:contracts_file_name(Release),
    case file:read_file(PresetContractsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetContractsFile}}
    end;
read_preset_contracts(Release) ->
    {error, {no_contracts_file_for_release, release_to_string(Release)}}.

release_to_string(?ROMA_PROTOCOL_VSN) -> "Roma";
release_to_string(?MINERVA_PROTOCOL_VSN) -> "Minerva";
release_to_string(?FORTUNA_PROTOCOL_VSN) -> "Fortuna";
release_to_string(?LIMA_PROTOCOL_VSN) -> "Lima".

accounts_file_name(Release) ->
    filename:join([dir(Release), accounts_json_file()]).

contracts_file_name(Release) ->
    filename:join([dir(Release), contracts_json_file()]).

-ifdef(TEST).
accounts_json_file() ->
    "accounts_test.json".

contracts_json_file() ->
    "accounts_test.json".

-else.
accounts_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "accounts.json";
        <<"ae_uat">>     -> "accounts_uat.json";
        _                -> "accounts_test.json"
    end.

contracts_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "contracts.json";
        <<"ae_uat">>     -> "contracts_uat.json";
        _                -> "contracts_test.json"
    end.
-endif.
