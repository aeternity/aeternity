-module(aec_dev_reward).

-export([ensure_env/0]). % setup hook

-export([enabled/0,
         activated/1,
         beneficiaries/0,
         allocated_shares/0,
         total_shares/0,
         split/3]).

-include_lib("aecontract/include/hard_forks.hrl").


-define(ENABLED, false).
-define(TOTAL_SHARES, 1000). % 100 shares == 10% of the reward
%%% for: "ak_2A3PZPfMC2X7ZVy4qGXz2xh2Lbh79Q4UvZ5fdH7QVFocEgcKzU:100" TODO - use actual pubkey for mainnet account
-define(BENEFICIARIES, [{<<152,57,168,5,218,153,177,254,226,207,243,133,11,50,143,68,121,242,94,41,187,198,158,67,133,88,6,71,55,26,85,54>>, 100}]).

ensure_env() ->
    Enabled = cfg(<<"protocol_beneficiaries_enabled">>, ?ENABLED),
    Benefs0 = cfg(<<"protocol_beneficiaries">>, lists:map(fun unparse_beneficiary/1, ?BENEFICIARIES)),
    case Enabled andalso parse_beneficiaries(Benefs0) of
        false ->
            application:set_env(aecore, dev_reward_enabled, false);
        {ok, Beneficiaries} ->
            {_, Shares} = lists:unzip(Beneficiaries),
            AllocShares = lists:sum(Shares),
            if AllocShares == 0 ->
                    exit({invalid_protocol_beneficiaries, sum_shares_is_zero});
               AllocShares > ?TOTAL_SHARES ->
                    exit({invalid_protocol_beneficiaries, sum_shares_too_large});
               AllocShares =< ?TOTAL_SHARES ->
                    application:set_env(aecore, dev_reward_enabled, true),
                    application:set_env(aecore, dev_reward_allocated_shares, AllocShares),
                    application:set_env(aecore, dev_reward_beneficiaries, Beneficiaries)
            end;
        {error, Reason} ->
            exit({invalid_protocol_beneficiaries, Reason})
    end.

cfg(Key, Default) ->
    aeu_env:user_config([<<"chain">>, Key], Default).

env(Key, Default) ->
    aeu_env:get_env(aecore, Key, Default).

enabled() ->
    env(dev_reward_enabled, ?ENABLED).
beneficiaries() ->
    env(dev_reward_beneficiaries, ?BENEFICIARIES).
total_shares() ->
    ?TOTAL_SHARES.
allocated_shares() ->
    {ok, V} = aeu_env:get_env(aecore, dev_reward_allocated_shares),
    V.

activated(Height) ->
    case env(dev_reward_activated, undefined) of %% for eunit to avoid mocking
        undefined ->
            aec_hard_forks:protocol_effective_at_height(Height) >= ?FORTUNA_PROTOCOL_VSN;
        Val when is_boolean(Val) ->
            Val
    end.


unparse_beneficiary({PK, Share}) ->
    <<(aeser_api_encoder:encode(account_pubkey, PK))/binary, ":", (integer_to_binary(Share))/binary>>.

parse_beneficiary(BeneficiaryShareStr) ->
    Regex = "^(?'account'ak_[1-9A-HJ-NP-Za-km-z]*):(?'share'[0-9]+)$",
    case re:split(BeneficiaryShareStr, Regex, [trim, group, {return, binary}]) of
        [[<<>>, Account, Share]] ->
            case aeser_api_encoder:safe_decode(account_pubkey, Account) of
                {ok, PubKey} ->
                    {PubKey, binary_to_integer(Share)};
                {error, _} ->
                    error({invalid_protocol_beneficiary, Account})
            end;
        _ ->
            error({invalid_protocol_beneficiary_share, BeneficiaryShareStr})
    end.

parse_beneficiaries([_|_] = BeneficiarySharesStrs) ->
    try lists:foldl(fun (Str, Acc) -> [parse_beneficiary(Str) | Acc] end,
                    [], BeneficiarySharesStrs) of
        Parsed ->
            {ok, Parsed}
    catch
        error:Reason ->
            {error, Reason}
    end.


split(BeneficiaryReward1, BeneficiaryReward2, NewestNodeHeight) ->
    case enabled() andalso activated(NewestNodeHeight) of
        true ->
            AllocShares = allocated_shares(),
            TotalShares = total_shares(),
            AbsContrib1 = (BeneficiaryReward1 * AllocShares) div TotalShares,
            AbsContrib2 = (BeneficiaryReward2 * AllocShares) div TotalShares,
            DevContrib  = AbsContrib1 + AbsContrib2,
            {Leftover, [{PK0, Amount0} | RemDevRewards]} =
                lists:foldl(
                  fun ({PK, PKShares}, {Remaining, Acc}) ->
                          Reward = DevContrib * PKShares div AllocShares,
                          {Remaining - Reward, [{PK, Reward} | Acc]}
                  end, {DevContrib, []}, beneficiaries()),
            {{BeneficiaryReward1 - AbsContrib1, BeneficiaryReward2 - AbsContrib2},
             [{PK0, Amount0 + Leftover} | RemDevRewards]};
        false ->
            {{BeneficiaryReward1, BeneficiaryReward2}, []}
    end.
