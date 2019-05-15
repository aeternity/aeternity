-module(aec_dev_reward).

-export([ensure_env/0]). % setup hook

-export([enabled/0,
         activated/1,
         beneficiaries/0,
         allocated_shares/0,
         total_shares/0,
         split/3]).

-ifdef(TEST).
-export([split_int/5]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").


-define(ENABLED, false).
-define(TOTAL_SHARES, 1000). % 100 shares == 10% of the reward
%%% for: "ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8:109"
%%% weighted avg BRI voting result of 1% to 20% (yes) votes is 10.89869526640124746202%, 109 of 1000 shares will be the protocol reward
-define(BENEFICIARIES, [{<<172,241,128,85,116,104,119,143,197,105,4,192,224,207,200,138,230,84,111,38,89,33,239,21,201,183,185,209,19,60,109,136>>, 109}]).

ensure_env() ->
    Enabled = cfg(<<"protocol_beneficiaries_enabled">>, ?ENABLED),
    Benefs0 = cfg(<<"protocol_beneficiaries">>, lists:map(fun unparse_beneficiary/1, ?BENEFICIARIES)),
    case Enabled andalso parse_beneficiaries(Benefs0) of
        false ->
            application:set_env(aecore, dev_reward_enabled, false);
        {ok, BenefShares0} ->
            BenefShares = lists:sort(BenefShares0),
            {_, Shares} = lists:unzip(BenefShares),
            AllocShares = lists:sum(Shares),
            if AllocShares == 0 ->
                    exit({invalid_protocol_beneficiaries, sum_shares_is_zero});
               AllocShares > ?TOTAL_SHARES ->
                    exit({invalid_protocol_beneficiaries, sum_shares_too_large});
               AllocShares =< ?TOTAL_SHARES ->
                    application:set_env(aecore, dev_reward_enabled, true),
                    application:set_env(aecore, dev_reward_allocated_shares, AllocShares),
                    application:set_env(aecore, dev_reward_beneficiaries, BenefShares)
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
        [[<<>>, AccountBin, ShareBin]] ->
            case aeser_api_encoder:safe_decode(account_pubkey, AccountBin) of
                {ok, PubKey} ->
                    Share = binary_to_integer(ShareBin),
                    Share > 0 orelse error({invalid_share, Share}),
                    {PubKey, Share};
                {error, _} ->
                    error({invalid_account, AccountBin})
            end;
        _ ->
            error({invalid_format, BeneficiaryShareStr})
    end.

parse_beneficiaries([]) ->
    {error, no_beneficiaries};
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
            split_int(BeneficiaryReward1, BeneficiaryReward2,
                      allocated_shares(), total_shares(),
                      beneficiaries());
        false ->
            {{BeneficiaryReward1, BeneficiaryReward2}, []}
    end.

split_int(BeneficiaryReward1, BeneficiaryReward2,
          AllocShares, TotalShares,
          Beneficiaries = [_|_]) when
      is_integer(BeneficiaryReward1), BeneficiaryReward1 >= 0,
      is_integer(BeneficiaryReward2), BeneficiaryReward2 >= 0,
      is_integer(AllocShares), AllocShares > 0,
      is_integer(TotalShares), TotalShares > 0,
      AllocShares =< TotalShares ->
    %% Assumption: Sum of shares of specified beneficiaries
    %% (`Beneficiaries`) is equal to specified allocated shares
    %% (`AllocShares`).
    AbsContrib1 = (BeneficiaryReward1 * AllocShares) div TotalShares,
    AbsContrib2 = (BeneficiaryReward2 * AllocShares) div TotalShares,
    DevContrib  = AbsContrib1 + AbsContrib2,
    {Leftover, [{PK0, Amount0} | RemDevRewards]} =
        lists:foldl(
          fun ({PK, PKShares}, {Remaining, Acc}) ->
                  Reward = DevContrib * PKShares div AllocShares,
                  {Remaining - Reward, [{PK, Reward} | Acc]}
          end, {DevContrib, []}, Beneficiaries),
    {{BeneficiaryReward1 - AbsContrib1, BeneficiaryReward2 - AbsContrib2},
     [{PK0, Amount0 + Leftover} | RemDevRewards]}.
