%%%-------------------------------------------------------------------
%%% @doc ETS-backed log index store.
%%%
%%% Holds an `ordered_set' keyed by `{Address, Height, TxIdx, LogIdx}'.
%%% Address-prefix scans are efficient via `ets:select/2' on key
%%% patterns; height-only scans walk a height-bounded slice. No
%%% persistence: the table is recreated at every node start and the
%%% indexer rebuilds forward from there. Heights below the indexer's
%%% floor still resolve via the inline `aerpc_logs' walker.
%%%
%%% The two ETS tables are owned by the indexer process so they
%%% disappear when the indexer terminates; this is intentional so that
%%% an indexer restart cannot serve stale data.
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_log_store).

-export([init/0,
         make_entry/9,
         insert/1,
         insert_many/1,
         select_range/3,
         floor_height/0,
         watermark/0,
         set_floor/1,
         set_watermark/1,
         indexed/1]).

-define(IDX,  aerpc_log_idx).
-define(META, aerpc_log_meta).

-include("aerpc_log_store.hrl").

-export_type([log_entry/0]).
-type log_entry() :: #log_entry{}.

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Create both ETS tables. Must be called by the owning process
%% (which is then responsible for their lifetime).
-spec init() -> ok.
init() ->
    case ets:info(?IDX) of
        undefined ->
            ets:new(?IDX,
                    [ordered_set, named_table, public,
                     {keypos, 1}, {read_concurrency, true}]),
            ok;
        _ -> ok
    end,
    case ets:info(?META) of
        undefined ->
            ets:new(?META,
                    [set, named_table, public,
                     {keypos, 1}, {read_concurrency, true}]),
            ok;
        _ -> ok
    end,
    ok.

-spec make_entry(binary(), non_neg_integer(), non_neg_integer(),
                 non_neg_integer(), [binary()], binary(),
                 binary(), binary(), binary()) -> log_entry().
make_entry(Address, Height, TxIdx, LogIdx, Topics, Data,
           BlockHash, MicroBlockHash, TxHash) ->
    #log_entry{address          = Address,
               height           = Height,
               tx_idx           = TxIdx,
               log_idx          = LogIdx,
               topics           = Topics,
               data             = Data,
               block_hash       = BlockHash,
               micro_block_hash = MicroBlockHash,
               tx_hash          = TxHash}.

-spec insert(log_entry()) -> true.
insert(#log_entry{address = A, height = H, tx_idx = T, log_idx = L} = Entry) ->
    ets:insert(?IDX, {{A, H, T, L}, Entry}).

-spec insert_many([log_entry()]) -> ok.
insert_many(Entries) ->
    [insert(E) || E <- Entries],
    ok.

%% @doc Return every indexed log entry with `From =< Height =< To',
%% optionally constrained to one of the supplied addresses. An empty
%% address list means "any". Output order is canonical (address asc,
%% then height/tx-idx/log-idx asc) which matches the eth-shape
%% indexer's expectations.
-spec select_range(any | [binary()], non_neg_integer(),
                   non_neg_integer()) -> [log_entry()].
select_range(any, From, To) ->
    %% Whole-table scan bounded by height. Key tuple is
    %% {Address, Height, TxIdx, LogIdx} -- read Height from element 2
    %% of the key for the range guard.
    Spec = [{{'$1', '$2'},
             [{'andalso',
                  {'>=', {element, 2, '$1'}, From},
                  {'=<', {element, 2, '$1'}, To}}],
             ['$2']}],
    ets:select(?IDX, Spec);
select_range(Addresses, From, To) when is_list(Addresses) ->
    %% For each address run a bounded prefix scan. Cheaper than the
    %% any-address path when the indexer has many contracts.
    lists:flatten(
      [select_one_address(A, From, To) || A <- Addresses]).

select_one_address(Addr, From, To) ->
    %% ordered_set: keys are {Address, Height, _, _}. Use a guard on
    %% the first element + height range.
    Spec = [{{{Addr, '$1', '_', '_'}, '$2'},
             [{'andalso',
                  {'>=', '$1', From},
                  {'=<', '$1', To}}],
             ['$2']}],
    ets:select(?IDX, Spec).

-spec floor_height() -> non_neg_integer() | undefined.
floor_height() ->
    case ets:lookup(?META, floor_height) of
        [{floor_height, H}] -> H;
        [] -> undefined
    end.

-spec watermark() -> non_neg_integer() | undefined.
watermark() ->
    case ets:lookup(?META, watermark) of
        [{watermark, H}] -> H;
        [] -> undefined
    end.

-spec set_floor(non_neg_integer()) -> true.
set_floor(H) when is_integer(H), H >= 0 ->
    ets:insert(?META, {floor_height, H}).

-spec set_watermark(non_neg_integer()) -> true.
set_watermark(H) when is_integer(H), H >= 0 ->
    ets:insert(?META, {watermark, H}).

%% @doc Return `true' if the requested height range is fully covered by
%% the index. Callers should fall back to the inline walker otherwise.
-spec indexed({non_neg_integer(), non_neg_integer()}) -> boolean().
indexed({From, To}) when From =< To ->
    case {floor_height(), watermark()} of
        {Floor, Watermark}
          when is_integer(Floor), is_integer(Watermark),
               Floor =< From, To =< Watermark ->
            true;
        _Other ->
            false
    end.
