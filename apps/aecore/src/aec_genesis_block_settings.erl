-module(aec_genesis_block_settings).

-export([dir/0,
         preset_accounts/0,
         read_presets/0]).

dir() ->
    filename:join(aeu_env:data_dir(aecore), ".genesis").

-spec preset_accounts() -> list().
preset_accounts() ->
    case read_presets() of
        {error, {_Err, PresetAccountsFile}} ->
            % no setup, no preset accounts
            erlang:error({genesis_accounts_file_missing, PresetAccountsFile});
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
                        {ok, PubKey} = aec_base58c:safe_decode(account_pubkey, EncodedPubKey),
                        {PubKey, Amt}
                    end,
                    DecodedData),
            % ensure deterministic ordering of accounts
            lists:keysort(1, Accounts)
    end.

-spec read_presets() -> {ok, binary()}| {error, atom(), string()}.
read_presets() ->
    PresetAccountsFile = filename:join([dir(), "accounts.json"]),
    case file:read_file(PresetAccountsFile) of
        {ok, _} = OK -> OK;
        {error, Err} -> {error, {Err, PresetAccountsFile}}
    end.
