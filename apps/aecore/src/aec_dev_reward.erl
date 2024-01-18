-module(aec_dev_reward).

-export([ensure_env/0]). % setup hook

-export([enabled/0,
         activated/1,
         beneficiaries/1,
         allocated_shares/1,
         total_shares/0,
         split/3]).

-ifdef(TEST).
-export([split_int/5,
         set_beneficiaries/1]).
-endif.

-include_lib("aecontract/include/hard_forks.hrl").


-define(ENABLED, true).
-define(TOTAL_SHARES, 1000). % 100 shares == 10% of the reward
%%% weighted avg BRI voting result of 1% to 20% (yes) votes is 10.89869526640124746202%, 109 of 1000 shares will be the protocol reward
%%% for: "ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8:109"
%%%%%%%%% TODO update with real values
-define(MAINNET_BENEFICIARIES, [{<<172,241,128,85,116,104,119,143,197,105,4,192,224,207,200,138,230,84,111,38,89,33,239,21,201,183,185,209,19,60,109,136>>, 109, undefined, ?LIMA_PROTOCOL_VSN},
                                {<<152,57,168,5,218,153,177,254,226,207,243,133,11,50,143,68,121,242,94,41,187,198,158,67,133,88,6,71,55,26,85,54>>, 109, ?IRIS_PROTOCOL_VSN, undefined}]).
%%% for: "ak_2A3PZPfMC2X7ZVy4qGXz2xh2Lbh79Q4UvZ5fdH7QVFocEgcKzU:109"
-define(TESTNET_BENEFICIARIES, [{<<152,57,168,5,218,153,177,254,226,207,243,133,11,50,143,68,121,242,94,41,187,198,158,67,133,88,6,71,55,26,85,54>>, 109, undefined, ?IRIS_PROTOCOL_VSN}]).

ensure_env() ->
    Enabled = cfg(<<"protocol_beneficiaries_enabled">>, ?ENABLED),
    DefaultBenefs = default_beneficiaries(),
    Benefs0 = cfg(<<"protocol_beneficiaries">>, lists:map(fun unparse_beneficiary/1, DefaultBenefs)),
    case Enabled andalso parse_beneficiaries(Benefs0) of
        false ->
            application:set_env(aecore, dev_reward_enabled, false);
        {ok, ProtocolBenefShares0} ->
            {ProtocolAllocShares, ProtocolBenefShares} = all_allocated_shares_and_beneficiaries(ProtocolBenefShares0),
            application:set_env(aecore, dev_reward_enabled, true),
            application:set_env(aecore, dev_reward_allocated_shares, ProtocolAllocShares),
            application:set_env(aecore, dev_reward_beneficiaries, ProtocolBenefShares);
        {error, Reason} ->
            exit({invalid_protocol_beneficiaries, Reason})
    end.

all_allocated_shares_and_beneficiaries(ProtocolBenefShares) ->
    lists:foldl(fun({ProtocolVsn, BenefShares0}, {ProtocolAllocSharesAccum, ProtocolBenefSharesAccum}) ->
                        {AllocShares, BenefShares} = allocated_shares_and_beneficiaries(BenefShares0),
                        {maps:put(ProtocolVsn, AllocShares, ProtocolAllocSharesAccum),
                            maps:put(ProtocolVsn, BenefShares, ProtocolBenefSharesAccum)} end,
                            {#{}, #{}}, ProtocolBenefShares).

allocated_shares_and_beneficiaries(BenefShares0) ->
    BenefShares = lists:sort(BenefShares0),
    {_, Shares} = lists:unzip(BenefShares),
    AllocShares = lists:sum(Shares),
    if AllocShares == 0 ->
            exit({invalid_protocol_beneficiaries, sum_shares_is_zero});
       AllocShares > ?TOTAL_SHARES ->
            exit({invalid_protocol_beneficiaries, sum_shares_too_large});
       AllocShares =< ?TOTAL_SHARES ->
            {AllocShares, BenefShares}
    end.

cfg(Key, Default) ->
    aeu_env:user_config([<<"chain">>, Key], Default).

env(Key, Default) ->
    aeu_env:get_env(aecore, Key, Default).

default_beneficiaries() ->
    case aec_governance:get_network_id() of
        <<"ae_uat">> -> ?TESTNET_BENEFICIARIES;
        _            -> ?MAINNET_BENEFICIARIES
    end.

enabled() ->
    env(dev_reward_enabled, ?ENABLED).

beneficiaries(ProtocolVsn) ->
    case env(dev_reward_beneficiaries, undefined) of
        undefined ->
            beneficiaries_at_protocol(ProtocolVsn, ?MAINNET_BENEFICIARIES);
        V ->
            maps:get(ProtocolVsn, V)
    end.

total_shares() ->
    ?TOTAL_SHARES.

allocated_shares(ProtocolVsn) ->
    {ok, V} = aeu_env:get_env(aecore, dev_reward_allocated_shares),
    maps:get(ProtocolVsn, V).

activated(Protocol) ->
    case env(dev_reward_activated, undefined) of %% for eunit to avoid mocking
        undefined ->
            Protocol >= ?FORTUNA_PROTOCOL_VSN;
        Val when is_boolean(Val) ->
            Val
    end.


unparse_beneficiary({PK, Share, FromProtocol, ToProtocol}) ->
    FromProtocolBin = unparse_protocol(FromProtocol),
    ToProtocolBin = unparse_protocol(ToProtocol),
    <<(aeser_api_encoder:encode(account_pubkey, PK))/binary, ":", (integer_to_binary(Share))/binary,
        ":", FromProtocolBin/binary, ":", ToProtocolBin/binary>>.

unparse_protocol(undefined) ->
    <<>>;
unparse_protocol(ProtocolVsn) ->
    integer_to_binary(ProtocolVsn).

parse_beneficiary(BeneficiaryShareStr) ->
    Regex = "^(?'account'ak_[1-9A-HJ-NP-Za-km-z]*):(?'share'[0-9]+)(:(?'from'[0-9]*):(?'to'[0-9]*)){0,1}$",
    case re:split(BeneficiaryShareStr, Regex, [trim, group, {return, binary}]) of
        [[<<>>, AccountBin, ShareBin, _, FromProtocolBin, ToProtocolBin]] ->
            case aeser_api_encoder:safe_decode(account_pubkey, AccountBin) of
                {ok, PubKey} ->
                    Share = binary_to_integer(ShareBin),
                    Share > 0 orelse error({invalid_share, Share}),
                    FromProtocol = case FromProtocolBin of
                                            <<>> ->
                                                min_protocol();
                                            FromBin ->
                                                binary_to_integer(FromBin)
                                            end,
                    FromProtocol > 0 orelse error({invalid_from_protocol, FromProtocol}),
                    ToProtocol = case ToProtocolBin of
                                            <<>> ->
                                                max_protocol();
                                            ToBin ->
                                                binary_to_integer(ToBin)
                                            end,
                    ToProtocol >= FromProtocol orelse error({invalid_to_protocol, ToProtocol}),
                    {PubKey, Share, FromProtocol, ToProtocol};
                {error, _} ->
                    error({invalid_account, AccountBin})
            end;
        _ ->
            error({invalid_format, BeneficiaryShareStr})
    end.

parse_beneficiaries([]) ->
    {error, no_beneficiaries};
parse_beneficiaries([_|_] = BeneficiarySharesProtocolsStrs) ->
    try lists:foldl(fun (Str, Acc) -> [parse_beneficiary(Str) | Acc] end,
                    [], BeneficiarySharesProtocolsStrs) of
        BeneficiarySharesProtocols ->
            {ok, [{ProtocolVsn, beneficiaries_at_protocol(ProtocolVsn, BeneficiarySharesProtocols)} ||
                    ProtocolVsn <- maps:keys(aec_hard_forks:protocols())]}
    catch
        error:Reason ->
            {error, Reason}
    end.

beneficiaries_at_protocol(ProtocolVsn, Beneficiaries) ->
    lists:filtermap(fun ({PubKey, Share, FromProtocol0, ToProtocol0}) ->
        ToProtocol = case ToProtocol0 of
                        undefined ->
                            max_protocol();
                        _ ->
                            ToProtocol0
                        end,
        FromProtocol = case FromProtocol0 of
                        undefined ->
                            min_protocol();
                        _ ->
                            FromProtocol0
                        end,
        case FromProtocol =< ProtocolVsn andalso ToProtocol >= ProtocolVsn of
            true ->
                {true, {PubKey, Share}};
            _ ->
                false
        end
    end, Beneficiaries).

split(BeneficiaryReward1, BeneficiaryReward2, NewestNodeVersion) ->
    case enabled() andalso activated(NewestNodeVersion) of
        true ->
            split_int(BeneficiaryReward1, BeneficiaryReward2,
                      allocated_shares(NewestNodeVersion), total_shares(),
                      beneficiaries(NewestNodeVersion));
        false ->
            {{BeneficiaryReward1, BeneficiaryReward2}, []}
    end.

max_protocol() ->
    lists:max(maps:keys(aec_hard_forks:protocols())).

min_protocol() ->
    lists:min(maps:keys(aec_hard_forks:protocols())).

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


-ifdef(TEST).

set_beneficiaries(BeneficiaryShares) ->
    ProtocolBenefShares0 = [{ProtocolVsn, beneficiaries_at_protocol(ProtocolVsn, BeneficiaryShares)} ||
                                ProtocolVsn <- maps:keys(aec_hard_forks:protocols())],
    {ProtocolAllocShares, ProtocolBenefShares} = all_allocated_shares_and_beneficiaries(ProtocolBenefShares0),
    application:set_env(aecore, dev_reward_allocated_shares, ProtocolAllocShares),
    application:set_env(aecore, dev_reward_beneficiaries, ProtocolBenefShares).

-endif.