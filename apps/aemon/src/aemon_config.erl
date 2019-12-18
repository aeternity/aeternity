-module(aemon_config).

%% API
-export([ privkey/0
        , pubkey/0
        , is_active/0
        , amount/0
        , autostart/0
        , interval/0
        , ttl/0
        ]).

%% ==================================================================
%% API

pubkey() ->
    raw_key(<<"pubkey">>, publisher_pubkey, <<>>).

privkey() ->
    raw_key(<<"privkey">>, publisher_privkey, <<>>).

is_active() ->
    case pubkey() of
        <<>> ->
            false;
        _ ->
            env([<<"active">>], active, false)
    end.

autostart() ->
    case privkey() of
        <<>> ->
            false;
        _ ->
            env(<<"autostart">>, publisher_autostart, false)
    end.

amount() ->
    env(<<"amount">>, publisher_spend_amount, 20000).

interval() ->
    env(<<"interval">>, publisher_interval, 10000).

ttl() ->
    env(<<"ttl">>, publisher_spend_ttl, 10).

%% ==================================================================
%% internal functions

raw_key(YKey, SKey, Default) ->
    Value = env(YKey, SKey, Default),
    raw_key(Value).

raw_key(<<>>) ->
    <<>>;
raw_key(Raw) ->
    {_, Key} = aeser_api_encoder:decode(Raw),
    Key.

env(YamlKey, Key, Default) when is_binary(YamlKey) ->
    env([<<"publisher">>, YamlKey], Key, Default);
env(YamlKey, Key, Default) ->
    aeu_env:user_config_or_env([<<"monitoring">> | YamlKey], aemon, Key, Default).
