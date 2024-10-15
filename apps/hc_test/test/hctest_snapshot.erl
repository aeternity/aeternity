%%% @doc Features for snapshotting and printing the chain state
%%% @end
-module(hctest_snapshot).

%% API
%%-export([write/1]).

-export([cc_snap/1]).

-type snapshot() :: #{
    time_micro => integer(),
    height => integer(),
    top_header => binary(),
    chain => list(),
    txpool => aec_blocks:tx_list()
}.

-spec cc_snap(#{node => node(), history_size => pos_integer()}) -> snapshot().
cc_snap(#{node := Node} = Options) ->
    HistorySize = maps:get(history_size, Options, 10),
    TopBlock = aecore_suite_utils:rpc(Node, aec_chain, top_block, []),
    TopHeight = aecore_suite_utils:rpc(Node, aec_chain, top_height, []),
    TopHeader = aecore_suite_utils:rpc(Node, aec_chain, top_header, []),
    TopBlockHash = aecore_suite_utils:rpc(Node, aec_chain, top_block_hash, []),
    {ok, TxPool} = aecore_suite_utils:rpc(Node, aec_tx_pool, peek, [infinity]),
    #{
        time_micro => erlang:system_time(microsecond),
        height => printable(TopHeight),
        top_header => printable(TopHeader),
        top_hash => printable(TopBlockHash),
        chain => printable(read_chain_top(HistorySize - 1, Node, [TopBlock])),
        txpool => printable(TxPool)
    }.

%% @private
read_chain_top(N, _Node, Result) when N =< 0 ->
    Result;
read_chain_top(N, Node, Result) ->
    PrevHash = aec_blocks:prev_hash(hd(Result)),
    ReadBlock = aecore_suite_utils:rpc(Node, aec_chain, get_block, [PrevHash]),
    case ReadBlock of
        {ok, PrevBlock} -> read_chain_top(N - 1, Node, [PrevBlock | Result]);
        _ -> [{block_read_failed, PrevHash} | Result]
    end.

%% @private
-spec binary_to_hex(binary()) -> string().
binary_to_hex(Bin) when is_binary(Bin) ->
    iolist_to_binary([
        begin
            if
                N < 10 -> 48 + N;
                true -> 87 + N
            end
        end
     || <<N:4>> <= Bin
    ]).

%% @private
printable(Bin) when is_binary(Bin) ->
    binary_to_hex(Bin);
printable(L) when is_list(L) ->
    [printable(X) || X <- L];
printable(T) when is_tuple(T) ->
    list_to_tuple(printable(tuple_to_list(T)));
printable(X) ->
    X.
