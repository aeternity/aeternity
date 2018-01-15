-module(aec_genesis_block_settings).

-export([dir/0,
         preset_accounts/0]).

dir() ->
    filename:join(aeu_env:data_dir(aecore), ".genesis").

preset_accounts() ->
    PresetAccountsFile = filename:join([dir(), "accounts.json"]),
    case file:read_file(PresetAccountsFile) of
        {error, _Err} ->
            % no setup, no preset accounts
            erlang:error({genesis_accounts_file_missing, PresetAccountsFile});
        {ok, JSONData} ->
            Accounts =
                lists:map(
                    fun({EncodedPubKey, Amt}) ->
                        {ok, PubKey} = aec_base58c:safe_decode(account_pubkey, EncodedPubKey),
                        %% PubKey = base64:decode(EncodedPubKey),
                        {PubKey, Amt}
                    end,
                    jsx:decode(JSONData)),
            % ensure deterministic ordering of accounts
            lists:keysort(1, Accounts)
    end.
