%%%-------------------------------------------------------------------
%%% @doc Log enumeration for the AE JSON-RPC layer.
%%%
%%% Walks the requested generation range, pulls the `aect_call' record
%%% for every contract-call / contract-create tx, and emits the matching
%%% log entries in Eth-shaped JSON. Address and topic filtering follows
%%% the standard eth_getLogs semantics:
%%%
%%%   * `address': single value or list -- exact-match on the emitter.
%%%   * `topics':  positional slots; each slot is `null' (any), a
%%%     single value (exact), or a list (OR-of).
%%% @end
%%%-------------------------------------------------------------------
-module(aerpc_logs).

-include("aerpc_log_store.hrl").

-export([get_logs/1, raw_logs_for_block/1]).

%% Maximum span of generations a single one-shot scan may cover. Without
%% a bound a "fromBlock: 0, toBlock: latest" query would walk the entire
%% chain in one request. The value is intentionally low for v1; it can be
%% lifted once a filter registry exists to amortize.
-define(MAX_RANGE, 1000).

%% ===================================================================
%% Public API
%% ===================================================================

-spec get_logs(map()) ->
    {ok, [map()]} | {error, integer(), binary()}.
get_logs(Filter) when is_map(Filter) ->
    case parse_filter(Filter) of
        {ok, Criteria}      -> dispatch_collect(Criteria);
        {error, _, _} = Err -> Err
    end;
get_logs(_) ->
    {error, -32602, <<"Invalid params">>}.

%% @doc Pick the cheapest collection path for the requested criteria.
%% Falls back to the inline `aec_chain' walker when the requested range
%% is not (yet) covered by the in-memory index. Single-block lookups
%% take the inline walker too -- the cost is identical and avoids a
%% special case while still serving correctly under any index state.
dispatch_collect(#{block_scope := {single, _}} = Criteria) ->
    collect(Criteria);
dispatch_collect(#{block_scope := {range, From, To}} = Criteria) ->
    case aerpc_log_store:indexed({From, To}) of
        true  -> {ok, lookup_from_index(Criteria, From, To)};
        false -> collect(Criteria)
    end.

%% @doc Pull every raw log entry emitted in a single key-block.
%% Returns `[{Address, Topics, Data}]' (the AE-native `aect_call:log/1'
%% triples) without filtering or shaping. Used by the bloom builder
%% and the subscription log fan-out path; both want the unshaped
%% bytes for either hashing or downstream serialization.
-spec raw_logs_for_block(binary()) ->
    [{binary(), [binary()], binary()}].
raw_logs_for_block(KeyBlockHash) when is_binary(KeyBlockHash) ->
    case aec_chain:get_generation_by_hash(KeyBlockHash, forward) of
        {ok, #{micro_blocks := MBs}} ->
            lists:flatten(
                [logs_for_tx(STx, KeyBlockHash)
                 || MB <- MBs, STx <- aec_blocks:txs(MB)]);
        error ->
            []
    end.

%% ===================================================================
%% Filter parsing
%% ===================================================================

parse_filter(F) ->
    case maps:get(<<"blockHash">>, F, undefined) of
        undefined ->
            parse_range_filter(F);
        Hash when is_binary(Hash) ->
            case aerpc_block:decode_block_hash(Hash) of
                {ok, BlockHash} ->
                    parse_rest(F, {single, BlockHash});
                {error, _, _} = Err ->
                    Err
            end;
        _ ->
            {error, -32602, <<"Invalid params">>}
    end.

parse_range_filter(F) ->
    From = maps:get(<<"fromBlock">>, F, <<"latest">>),
    To   = maps:get(<<"toBlock">>,   F, <<"latest">>),
    case {aerpc_block:resolve_tag(From), aerpc_block:resolve_tag(To)} of
        {{ok, FromH}, {ok, ToH}} when ToH >= FromH ->
            case ToH - FromH < ?MAX_RANGE of
                true  -> parse_rest(F, {range, FromH, ToH});
                false -> {error, -32005, <<"Range too wide">>}
            end;
        _ ->
            {error, -32602, <<"Invalid params">>}
    end.

parse_rest(F, BlockScope) ->
    case parse_addresses(maps:get(<<"address">>, F, undefined)) of
        {ok, Addresses} ->
            case parse_topics(maps:get(<<"topics">>, F, [])) of
                {ok, Topics} ->
                    {ok, #{block_scope => BlockScope,
                           addresses   => Addresses,
                           topics      => Topics}};
                {error, _, _} = Err ->
                    Err
            end;
        {error, _, _} = Err ->
            Err
    end.

parse_addresses(undefined) -> {ok, any};
parse_addresses(Addr) when is_binary(Addr) ->
    case decode_one_address(Addr) of
        {ok, Bin}           -> {ok, [Bin]};
        {error, _, _} = Err -> Err
    end;
parse_addresses(List) when is_list(List) ->
    decode_many_addresses(List, []);
parse_addresses(_) ->
    {error, -32602, <<"Invalid params">>}.

decode_many_addresses([], Acc) -> {ok, lists:reverse(Acc)};
decode_many_addresses([H | T], Acc) when is_binary(H) ->
    case decode_one_address(H) of
        {ok, Bin}           -> decode_many_addresses(T, [Bin | Acc]);
        {error, _, _} = Err -> Err
    end;
decode_many_addresses(_, _) ->
    {error, -32602, <<"Invalid params">>}.

decode_one_address(Bin) ->
    case Bin of
        <<"ct_", _/binary>> ->
            case aeapi:decode_contract_pubkey(Bin) of
                {ok, B} -> {ok, B};
                _       -> {error, -32602, <<"Invalid address">>}
            end;
        <<"ak_", _/binary>> ->
            case aeapi:decode_account_pubkey(Bin) of
                {ok, B} -> {ok, B};
                _       -> {error, -32602, <<"Invalid address">>}
            end;
        <<"0x", _/binary>> ->
            try
                B = aerpc_encoding:from_hex_data(Bin),
                case byte_size(B) of
                    32 -> {ok, B};
                    _  -> {error, -32602, <<"Invalid address">>}
                end
            catch _:_ -> {error, -32602, <<"Invalid address">>}
            end;
        _ ->
            {error, -32602, <<"Invalid address">>}
    end.

parse_topics(List) when is_list(List), length(List) =< 4 ->
    parse_topics_(List, []);
parse_topics(_) ->
    {error, -32602, <<"Invalid params">>}.

parse_topics_([], Acc) -> {ok, lists:reverse(Acc)};
parse_topics_([null | T], Acc) -> parse_topics_(T, [any | Acc]);
parse_topics_([H | T], Acc) when is_binary(H) ->
    case decode_topic(H) of
        {ok, Bin}           -> parse_topics_(T, [{eq, Bin} | Acc]);
        {error, _, _} = Err -> Err
    end;
parse_topics_([L | T], Acc) when is_list(L) ->
    case decode_topic_list(L, []) of
        {ok, Bins}          -> parse_topics_(T, [{any_of, Bins} | Acc]);
        {error, _, _} = Err -> Err
    end;
parse_topics_(_, _) ->
    {error, -32602, <<"Invalid params">>}.

decode_topic_list([], Acc) -> {ok, lists:reverse(Acc)};
decode_topic_list([H | T], Acc) when is_binary(H) ->
    case decode_topic(H) of
        {ok, B}             -> decode_topic_list(T, [B | Acc]);
        {error, _, _} = Err -> Err
    end;
decode_topic_list(_, _) ->
    {error, -32602, <<"Invalid params">>}.

decode_topic(<<"0x", _/binary>> = Hex) ->
    try
        B = aerpc_encoding:from_hex_data(Hex),
        case byte_size(B) of
            32 -> {ok, B};
            _  -> {error, -32602, <<"Invalid topic">>}
        end
    catch _:_ -> {error, -32602, <<"Invalid topic">>}
    end;
decode_topic(_) ->
    {error, -32602, <<"Invalid topic">>}.

%% ===================================================================
%% Index path
%% ===================================================================

%% Adapter from `aerpc_log_store' index entries back to the eth-shape
%% log map. Reuses the existing match/2 and shape_log/6 logic; the only
%% twist is that the entry already knows its block hash + tx idx, so we
%% don't re-derive them.
lookup_from_index(Criteria, From, To) ->
    Addresses = maps:get(addresses, Criteria),
    Raw       = aerpc_log_store:select_range(Addresses, From, To),
    Topics    = maps:get(topics, Criteria),
    [shape_index_entry(E) || E <- Raw, topics_match(E#log_entry.topics, Topics)].

shape_index_entry(#log_entry{} = E) ->
    #{
        <<"removed">>          => false,
        <<"logIndex">>         => aerpc_encoding:to_quantity(E#log_entry.log_idx),
        <<"transactionIndex">> => aerpc_encoding:to_quantity(E#log_entry.tx_idx),
        <<"transactionHash">>  =>
            aerpc_encoding:format_tx_hash(E#log_entry.tx_hash),
        <<"blockHash">>        =>
            aerpc_encoding:format_key_block_hash(E#log_entry.block_hash),
        <<"blockNumber">>      => aerpc_encoding:to_quantity(E#log_entry.height),
        <<"address">>          =>
            aerpc_encoding:format_contract(E#log_entry.address),
        <<"data">>             => aerpc_encoding:to_hex_data(E#log_entry.data),
        <<"topics">>           =>
            [aerpc_encoding:to_hex_data(T) || T <- E#log_entry.topics]
    }.

%% ===================================================================
%% Walking the chain
%% ===================================================================

collect(#{block_scope := {single, KeyBlockHash}} = Criteria) ->
    {ok, collect_at_keyblock(KeyBlockHash, Criteria)};
collect(#{block_scope := {range, From, To}} = Criteria) ->
    {ok, collect_range(From, To, Criteria, [])}.

collect_range(H, To, _Crit, Acc) when H > To ->
    lists:reverse(Acc);
collect_range(H, To, Crit, Acc) ->
    case aec_chain:get_key_block_by_height(H) of
        {ok, KB} ->
            {ok, KBHash} = aec_blocks:hash_internal_representation(KB),
            Logs = collect_at_keyblock(KBHash, Crit),
            collect_range(H + 1, To, Crit, lists:reverse(Logs, Acc));
        {error, _Reason} ->
            collect_range(H + 1, To, Crit, Acc)
    end.

collect_at_keyblock(KeyBlockHash, Crit) ->
    case aec_chain:get_generation_by_hash(KeyBlockHash, forward) of
        {ok, #{key_block := KB, micro_blocks := MBs}} ->
            Header = aec_blocks:to_key_header(KB),
            BlockNum = aec_headers:height(Header),
            collect_micros(MBs, KeyBlockHash, BlockNum, Crit, 0, 0, []);
        error ->
            []
    end.

collect_micros([], _KBH, _Num, _C, _TxIdx, _LogIdx, Acc) ->
    lists:reverse(Acc);
collect_micros([MB | Rest], KBH, Num, Crit, TxIdx, LogIdx, Acc) ->
    {ok, MBHash} = aec_blocks:hash_internal_representation(MB),
    Txs = aec_blocks:txs(MB),
    {NewAcc, TxIdx1, LogIdx1} =
        walk_txs(Txs, MBHash, KBH, Num, Crit, TxIdx, LogIdx, Acc),
    collect_micros(Rest, KBH, Num, Crit, TxIdx1, LogIdx1, NewAcc).

walk_txs([], _MBH, _KBH, _Num, _Crit, TxIdx, LogIdx, Acc) ->
    {Acc, TxIdx, LogIdx};
walk_txs([STx | Rest], MBH, KBH, Num, Crit, TxIdx, LogIdx, Acc) ->
    case logs_for_tx(STx, MBH) of
        [] ->
            walk_txs(Rest, MBH, KBH, Num, Crit, TxIdx + 1, LogIdx, Acc);
        Logs ->
            {Acc1, LogIdx1} =
                fold_logs(Logs, KBH, Num, STx, TxIdx, LogIdx, Crit, Acc),
            walk_txs(Rest, MBH, KBH, Num, Crit, TxIdx + 1, LogIdx1, Acc1)
    end.

logs_for_tx(STx, BlockHash) ->
    try
        Tx = aetx_sign:tx(STx),
        {Type, _} = aetx:specialize_type(Tx),
        case is_contract_tx(Type) of
            false -> [];
            true ->
                {CB, CTx} = aetx:specialize_callback(Tx),
                {ContractId, CallId} =
                    case Type of
                        contract_call_tx ->
                            {CB:ct_call_id(CTx), CB:call_id(CTx)};
                        contract_create_tx ->
                            {CB:contract_pubkey(CTx), CB:call_id(CTx)}
                    end,
                case aec_chain:get_contract_call(ContractId, CallId, BlockHash) of
                    {ok, Call}      -> aect_call:log(Call);
                    {error, _Reas}  -> []
                end
        end
    catch _:_ -> []
    end.

is_contract_tx(contract_call_tx) -> true;
is_contract_tx(contract_create_tx) -> true;
is_contract_tx(_) -> false.

fold_logs([], _KBH, _Num, _STx, _TxIdx, LogIdx, _Crit, Acc) ->
    {Acc, LogIdx};
fold_logs([Entry | Rest], KBH, Num, STx, TxIdx, LogIdx, Crit, Acc) ->
    case match(Entry, Crit) of
        true ->
            Shape = shape_log(Entry, KBH, Num, STx, TxIdx, LogIdx),
            fold_logs(Rest, KBH, Num, STx, TxIdx, LogIdx + 1, Crit,
                      [Shape | Acc]);
        false ->
            fold_logs(Rest, KBH, Num, STx, TxIdx, LogIdx + 1, Crit, Acc)
    end.

match({Address, Topics, _Data},
      #{addresses := AllowedA, topics := AllowedT}) ->
    address_match(Address, AllowedA) andalso topics_match(Topics, AllowedT).

address_match(_Addr, any) -> true;
address_match(Addr, Allowed) when is_list(Allowed) ->
    lists:member(Addr, Allowed).

topics_match(_LogTopics, []) -> true;
topics_match([], [_ | _])   -> false;
topics_match([T | RestL], [Slot | RestS]) ->
    case slot_match(T, Slot) andalso topics_match(RestL, RestS) of
        true  -> true;
        false -> false
    end.

slot_match(_T, any)            -> true;
slot_match(T,  {eq, X})        -> T =:= X;
slot_match(T,  {any_of, Xs})   -> lists:member(T, Xs).

shape_log({Address, Topics, Data}, KBH, BlockNumber, STx, TxIdx, LogIdx) ->
    TxHash = aetx_sign:hash(STx),
    #{
        <<"removed">>          => false,
        <<"logIndex">>         => aerpc_encoding:to_quantity(LogIdx),
        <<"transactionIndex">> => aerpc_encoding:to_quantity(TxIdx),
        <<"transactionHash">>  => aerpc_encoding:format_tx_hash(TxHash),
        <<"blockHash">>        => aerpc_encoding:format_key_block_hash(KBH),
        <<"blockNumber">>      => aerpc_encoding:to_quantity(BlockNumber),
        <<"address">>          => aerpc_encoding:format_contract(Address),
        <<"data">>             => aerpc_encoding:to_hex_data(Data),
        <<"topics">>           => [aerpc_encoding:to_hex_data(T) || T <- Topics]
    }.
