-module(aec_fork_block_settings).

-include_lib("aecontract/include/hard_forks.hrl").

-export([dir/1,
         genesis_accounts/0,
         minerva_accounts/0]).

-define(GENESIS_DIR, ".genesis").
-define(MINERVA_DIR, ".minerva").


-spec dir(aec_hard_forks:protocol_vsn()) -> string().
dir(ProtocolVsn) ->
    Dir =
        case ProtocolVsn of
            ?ROMA_PROTOCOL_VSN    -> ?GENESIS_DIR;
            ?MINERVA_PROTOCOL_VSN -> ?MINERVA_DIR
        end,
    filename:join(aeu_env:data_dir(aecore), Dir).

-spec genesis_accounts() -> list().
genesis_accounts() -> preset_accounts(?ROMA_PROTOCOL_VSN,
                                      genesis_accounts_file_missing).

-spec minerva_accounts() -> list().
minerva_accounts() -> preset_accounts(?MINERVA_PROTOCOL_VSN,
                                      minerva_accounts_file_missing).

-spec preset_accounts(aec_hard_forks:protocol_vsn(), atom()) -> list().
preset_accounts(Release, ErrorMsg) ->
    case read_presets(Release) of
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

-spec read_presets(aec_hard_forks:protocol_vsn()) -> {ok, binary()}| {error, {atom(), string()}}.
read_presets(Release) ->
    PresetAccountsFile = filename:join([dir(Release), accounts_json_file()]),
    case file:read_file(PresetAccountsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetAccountsFile}}
    end.


-ifdef(TEST).
accounts_json_file() ->
    "accounts_test.json".
-else.
accounts_json_file() ->
    case aec_governance:get_network_id() of
        <<"ae_mainnet">> -> "accounts.json";
        <<"ae_uat">>     -> "accounts_uat.json";
        _                -> "accounts_test.json"
    end.
-endif.
