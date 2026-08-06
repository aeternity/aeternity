%%%-------------------------------------------------------------------
%%% @doc
%%%   Regression guard for the Rosetta balance-reconstruction path.
%%%
%%%   aeapi:format_txs/2 re-executes historical block txs via dry-run to
%%%   recover their balance-change events. It MUST pass the 'replay'
%%%   dry-run profile so the always-on dry-run Arcus gas metering does NOT
%%%   re-meter those historical calls at the forward (higher) cost -- which
%%%   could flip a gas-tight call ok->out_of_gas and drop its balance events,
%%%   silently corrupting reconstructed balances. This makes that invariant
%%%   test-guarded rather than convention-dependent.
%%% @end
%%%-------------------------------------------------------------------
-module(aeapi_tests).

-include_lib("eunit/include/eunit.hrl").

reconstruction_pins_replay_profile_test() ->
    meck:new(aec_dry_run, [passthrough]),
    Self = self(),
    try
        meck:expect(aec_dry_run, dry_run,
                    fun(_Top, _Accounts, _DryTxs, Opts) ->
                        Self ! {dry_run_opts, Opts},
                        {ok, {[ok], []}}  %% one result to zip with the one tx
                    end),
        _ = aeapi:format_txs([dummy_signed_spend()], undefined),
        Opts = receive {dry_run_opts, O} -> O after 2000 -> error(dry_run_not_called) end,
        ?assertEqual(replay, proplists:get_value(dry_run_profile, Opts)),
        ?assert(lists:member(tx_events, Opts))
    after
        meck:unload(aec_dry_run)
    end.

dummy_signed_spend() ->
    {ok, Tx} = aec_spend_tx:new(#{ sender_id    => aeser_id:create(account, <<1:256>>)
                                 , recipient_id => aeser_id:create(account, <<2:256>>)
                                 , amount       => 1
                                 , fee          => 20000
                                 , nonce        => 1
                                 , ttl          => 0
                                 , payload      => <<>> }),
    aetx_sign:new(Tx, [<<0:(64*8)>>]).
