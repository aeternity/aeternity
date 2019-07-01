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
         delete_tables/0,
         clear_tables/0]).

-export([keys/1,
         sort_key/0,
         is_empty/1,
         get_hash/1,
         store_share/3,
         mark_share_as_solution/2,
         store_round/0,
         get_reward/1,
         store_reward/1,
         has_payments/1,
         oldest_unpaid_payment/0,
         select_payments/1,
         select_payments/2,
         store_payment/1,
         store_payments/1,
         update_payment/4,
         delete_payment/1,
         get_candidate/1,
         store_candidate/3,
         delete_candidates_older_than/1,
         shares_range/1,
         shares_selector/2,
         shares_slices/1,
         payments/1,
         delete_records/1]).


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

clear_tables() ->
    [mnesia:clear_table(Tab) || Tab <- ?TABS].

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
    Share = #aestratum_share{key    = SortKey,
                             hash   = Hash,
                             target = MinerTarget,
                             miner  = Miner},
    HashR = #aestratum_hash{hash = Hash,
                            key  = SortKey},
    ok = mnesia:write(Share),
    ok = mnesia:write(HashR),
    {ok, Share, HashR}.

-spec mark_share_as_solution(binary(), binary()) -> ok.
mark_share_as_solution(ShareHash, BlockHash) ->
    [#aestratum_hash{key = Key}] = mnesia:read(?HASHES_TAB, ShareHash),
    [#aestratum_share{} = Share] = mnesia:read(?SHARES_TAB, Key),
    ok = mnesia:delete({?HASHES_TAB, ShareHash}),
    ok = mnesia:write(#aestratum_hash{hash = BlockHash, key = Key}),
    ok = mnesia:write(Share#aestratum_share{hash = BlockHash}).


-spec store_round() -> {ok, #aestratum_round{}}.
store_round() ->
    Round = #aestratum_round{key = sort_key()},
    ok = mnesia:write(Round),
    {ok, Round}.

get_reward(Height) ->
    case mnesia:read(?REWARDS_TAB, Height) of
        [Reward] -> {ok, Reward};
        [] -> {error, not_found}
    end.

store_reward(#aestratum_reward{} = R) ->
    ok = mnesia:write(R),
    {ok, R}.


store_payments(#aestratum_reward{} = Reward) ->
    store_payments(aestratum_reward:relative_payments(Reward));
store_payments(Ps) when is_list(Ps) ->
    [{ok, _} = store_payment(P) || P <- Ps].


store_payment(#aestratum_payment{} = P) ->
    ok = mnesia:write(P),
    {ok, P}.

update_payment(#aestratum_payment{} = P, AbsMap, TxHash, Date) ->
    P1 = P#aestratum_payment{absmap  = AbsMap,
                             tx_hash = TxHash,
                             date    = Date},
    ok = mnesia:write(P1),
    {ok, P1}.

delete_payment({_, _} = Id) ->
    ok = mnesia:delete({?PAYMENTS_TAB, Id}).

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


payments(Height) ->
    Spec = ets:fun2ms(fun (#aestratum_payment{id = {H, _}} = P) when H == Height -> P end),
    mnesia:select(?PAYMENTS_TAB, Spec).


payment_spec(_WasPaid = true) ->
    ets:fun2ms(fun (#aestratum_payment{tx_hash = TH} = P) when is_binary(TH) -> P end);
payment_spec(_WasPaid = false) ->
    ets:fun2ms(fun (#aestratum_payment{tx_hash = undefined} = P) -> P end).

select_payments(WasPaid) ->
    mnesia:select(aestratum_payment, payment_spec(WasPaid), read).
select_payments(WasPaid, N) ->
    case mnesia:select(aestratum_payment, payment_spec(WasPaid), N, read) of
        '$end_of_table' -> [];
        {Res, _} -> Res
    end.

has_payments(WasPaid) ->
    length(select_payments(WasPaid, 1)) > 0.

oldest_unpaid_payment() ->
    case select_payments(false, 1) of
        [P | _] -> {ok, P};
        [] -> none
    end.


keys(Height) ->
    case mnesia:read(aestratum_reward, Height) of
        [#aestratum_reward{round_key = RoundKey}] ->
            ShareMS = ets:fun2ms(fun (#aestratum_share{key = K, hash = H}) when K >= RoundKey ->
                                         {K, H}
                                 end),
            RoundMS = ets:fun2ms(fun (#aestratum_round{key = K}) when K >= RoundKey -> K end),

            {Shares, Hashes} = lists:unzip(mnesia:select(?SHARES_TAB, ShareMS)),
            Rounds   = mnesia:select(?ROUNDS_TAB, RoundMS),
            Payments = [P#aestratum_payment.id ||
                           P <- mnesia:match_object(
                                  #aestratum_payment{id = {Height, '_'}, _ = '_'})],

            #{?HASHES_TAB   => Hashes,
              ?SHARES_TAB   => Shares,
              ?ROUNDS_TAB   => Rounds,
              ?REWARDS_TAB  => [Height],
              ?PAYMENTS_TAB => Payments};
        [] ->
            #{}
    end.


delete_records(Height) ->
    TabsKeys = keys(Height),
    maps:map(fun delete_keys/2, TabsKeys),
    ?INFO("removed keys for reward at height ~p: ~p", [Height, TabsKeys]),
    ok.

delete_keys(Tab, Keys) ->
    [mnesia:delete(Tab, K, write) || K <- Keys],
    ok.

is_empty(Tab) ->
    mnesia:table_info(Tab, size) == 0.
