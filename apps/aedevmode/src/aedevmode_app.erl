-module(aedevmode_app).
-behavior(application).

-export([ start/2
        , stop/1 ]).

-export([post_check/0]).

start(_StartType, _StartArgs) ->
    aedevmode_sup:start_link().

stop(_State) ->
    lager:info("Stopping aedevmode_app", []),
    ok.

%% Run at 300, i.e. after normal env checks AND the db start
post_check() ->
    case aecore_env:is_dev_mode() of
        true ->
            Pub = patron_pubkey_for_testing(),
            aeu_env:suggest_config([<<"mining">>, <<"beneficiary">>], Pub);
        false ->
            ok
    end.

patron_pubkey_for_testing() ->
    #{pubkey := Pub} = aecore_env:patron_keypair_for_testing(),
    aeser_api_encoder:encode(account_pubkey, Pub).
