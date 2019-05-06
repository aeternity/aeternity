%%%-------------------------------------------------------------------
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Aeternity stratum DB tables
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aestratum_db).

-export([check_tables/1,
         create_tables/1,
         delete_tables/0]).

-export([sort_key/0,
         is_empty/1,
         get_hash/1,
         store_share/3,
         store_round/0,
         store_payment/1,
         update_payment/7,
         get_candidate/1,
         store_candidate/3,
         delete_candidates_older_than/1,
         shares_range/1,
         shares_selector/2,
         shares_slices/1,
         delete_reward_records/1,
         delete_payment_records/1]).

-include("aestratum.hrl").
-include("aestratum_log.hrl").
-include_lib("aecore/include/blocks.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SHARES_BATCH_LENGTH, 1000).

-type sort_key() :: non_neg_integer().
-type amount() :: non_neg_integer().

-export_type([sort_key/0,
              amount/0]).

%%%===================================================================

-define(TAB_DEF(Name, Type, Mode),
        {Name, [aec_db:tab_copies(Mode),
                {type, Type},
                {record_name, Name},
                {attributes, record_info(fields, Name)},
                {user_properties, [{vsn, table_vsn(Name)}]}]}).

create_tables(Mode) ->
    Specs = [table_specs(Tab, Mode) || {missing_table, Tab} <- check_tables()],
    [{atomic, ok} = mnesia:create_table(Tab, Spec) || {Tab, Spec} <- Specs].

delete_tables() ->
    [mnesia:delete_table(Tab) || Tab <- ?TABS].


check_tables(Acc) ->
    check_tables() ++ Acc.

check_tables() ->
    lists:foldl(fun ({T, S}, Acc) -> aec_db:check_table(T, S, Acc) end, [], tables_specs(disc)).

table_specs(?HASHES_TAB, Mode) -> ?TAB_DEF(?HASHES_TAB, set, Mode);
table_specs(?SHARES_TAB, Mode) -> ?TAB_DEF(?SHARES_TAB, ordered_set, Mode);
table_specs(?ROUNDS_TAB, Mode) -> ?TAB_DEF(?ROUNDS_TAB, ordered_set, Mode);
table_specs(?REWARDS_TAB, Mode) -> ?TAB_DEF(?REWARDS_TAB, ordered_set, Mode);
table_specs(?PAYMENTS_TAB, Mode) -> ?TAB_DEF(?PAYMENTS_TAB, ordered_set, Mode);
table_specs(?CANDIDATES_TAB, Mode) -> ?TAB_DEF(?CANDIDATES_TAB, set, Mode).

tables_specs(Mode) -> [table_specs(Tab, Mode) || Tab <- ?TABS].

table_vsn(_) -> 1.

%%%===================================================================

-spec sort_key() -> sort_key().
sort_key() ->
    abs(erlang:unique_integer([monotonic])).

-spec store_share(binary(), pos_integer(), binary()) ->
                         {ok, #aestratum_share{}, #aestratum_hash{}}.
store_share(Miner, MinerTarget, Hash) ->
    SortKey = sort_key(),
    Share = #aestratum_share{key = SortKey,
                             hash = Hash,
                             target = MinerTarget,
                             miner = Miner},
    HashR = #aestratum_hash{hash = Hash,
                            key = SortKey},
    ok = mnesia:write(Share),
    ok = mnesia:write(HashR),
    {ok, Share, HashR}.

-spec store_round() -> {ok, #aestratum_round{}}.
store_round() ->
    Round = #aestratum_round{key = sort_key()},
    ok = mnesia:write(Round),
    {ok, Round}.


store_payment(#aestratum_payment{} = P) ->
    ok = mnesia:write(P),
    {ok, P}.

update_payment(#aestratum_payment{} = P, AbsMap, Fee, Gas, TxHash, Nonce, Date) ->
    P1 = P#aestratum_payment{fee = Fee,
                             gas = Gas,
                             absmap = AbsMap,
                             tx_hash = TxHash,
                             nonce = Nonce,
                             date = Date},
    ok = mnesia:write(P1),
    {ok, P1}.


get_candidate(BlockHash) ->
    case mnesia:read(aestratum_candidate, BlockHash) of
        [#aestratum_candidate{} = Rec] -> {ok, Rec};
        [] -> {error, not_found}
    end.


store_candidate(BlockHash, HeaderBin, CandidateRecord) ->
    C  = #aestratum_candidate{block_hash = BlockHash,
                              header = HeaderBin,
                              record = CandidateRecord,
                              date   = erlang:universaltime()},
    ok = mnesia:write(C),
    {ok, C}.


delete_candidates_older_than(Date) ->
    Hashes = mnesia:select(?CANDIDATES_TAB,
                           ets:fun2ms(fun (#aestratum_candidate{date = D} = C)
                                            when D < Date -> C#aestratum_candidate.block_hash
                                      end)),
    [mnesia:delete(?CANDIDATES_TAB, H, write) || H <- Hashes],
    ok.


get_hash(Hash) ->
    mnesia:dirty_read(?HASHES_TAB, Hash).


-spec shares_range(sort_key()) -> {ok, sort_key(), sort_key()} | {error, no_range}.
shares_range(RewardShareKey) ->
    First = case mnesia:prev(?ROUNDS_TAB, RewardShareKey) of
                '$end_of_table' -> RewardShareKey;
                Val -> Val
            end,
    Selector = ets:fun2ms(fun (#aestratum_round{key = K} = R) when K >= First -> R end),
    case mnesia:select(?ROUNDS_TAB, Selector, ?LAST_N + 1, read) of
        {[_ | _] = Rounds, _Cont} ->
            #aestratum_round{key = Last} = lists:last(Rounds),
            {ok, First, Last};
        _ ->
            {error, no_range}
    end.

-spec shares_selector(sort_key(), sort_key()) -> ets:match_spec().
shares_selector(FirstKey, LastKey) ->
    ets:fun2ms(fun (#aestratum_share{key = SK} = Share)
                     when SK >= FirstKey, SK =< LastKey -> Share
               end).

shares_slices(Selector) ->
    mnesia:select(?SHARES_TAB, Selector, ?SHARES_BATCH_LENGTH, read).


delete_reward_records(Height) ->
    case mnesia:read(aestratum_reward, Height) of
        [#aestratum_reward{round_key = RoundKey}] ->
            RoundSpec = ets:fun2ms(fun (#aestratum_round{key = K}) when K > RoundKey -> K end),
            Rounds    = mnesia:select(?ROUNDS_TAB, RoundSpec),
            ShareSpec = ets:fun2ms(fun (#aestratum_share{key = K, hash = H}) when K > RoundKey ->
                                           {K, H}
                                   end),
            {Shares, Hashes} = lists:unzip(mnesia:select(?SHARES_TAB, ShareSpec)),
            [mnesia:delete(?SHARES_TAB, S, write) || S <- Shares],
            [mnesia:delete(?HASHES_TAB, H, write) || H <- Hashes],
            [mnesia:delete(?ROUNDS_TAB, R, write) || R <- Rounds],
            mnesia:delete(?REWARDS_TAB, Height, write),
            ?INFO("deleted records for height ~p", [Height]),
            ok;
        [] ->
            %% this shouldn't happen, since rewards are deleted one by one
            ok
    end.

delete_payment_records(TxHashes) ->
    [mnesia:delete(?PAYMENTS_TAB, Tx, write) || Tx <- TxHashes].

is_empty(Tab) ->
    mnesia:table_info(Tab, size) == 0.
