-module(mtree).

-compile([export_all, nowarn_export_all]).

root_hash({Root, _DB}) -> aeu_hex:bin_to_hex(Root).

mk_tree(Datas) ->
    N = length(Datas),
    mk_tree_([mk_leaf(Ix, Data) || {Ix, Data} <- lists:zip(lists:seq(0, N-1), Datas)]).

mk_tree_([Node]) -> Node;
mk_tree_(Ns) ->
    mk_tree_(mk_level(Ns)).

mk_level([]) -> [];
mk_level([Node]) ->
    [mk_node(Node, Node)];
mk_level([Node1, Node2 | Ns]) ->
    [mk_node(Node1, Node2) | mk_level(Ns)].

mk_node({Hash1, DB1}, {Hash2, DB2}) ->
    Bin     = iolist_to_binary([string:to_upper(aeu_hex:bin_to_hex(Hash1)),
                                string:to_upper(aeu_hex:bin_to_hex(Hash2))]),
    NewHash = aec_hash:hash(evm, Bin),
    NewDB   = maps:merge(DB1, DB2),
    N1      = maps:get(Hash1, DB1),
    N2      = maps:get(Hash2, DB2),

    {NewHash, NewDB#{ NewHash => {node, ix(N2) div 2, nil, Hash1, Hash2},
                      Hash1   => set_parent(NewHash, N1),
                      Hash2   => set_parent(NewHash, N2) }}.

mk_leaf(Ix, {Addr, Tokens}) ->
    mk_leaf(Ix, data(Addr, Tokens));
mk_leaf(Ix, Binary) ->
    Hash = aec_hash:hash(evm, Binary),
    {Hash, #{ Hash => {leaf, Ix, nil, value} }}.

get_poi({_, DB}, Ix) ->
    [LeafParent] = [ P || {_, {leaf, Ix1, P, _}} <- maps:to_list(DB), Ix == Ix1 ],
    get_poi(LeafParent, DB, Ix).

get_poi(nil, _DB, _Ix) -> [];
get_poi(NodeH, DB, Ix) ->
    case maps:get(NodeH, DB) of
        {node, _, P, _L, R} when Ix rem 2 == 0 ->
            [aeu_hex:bin_to_hex(R) | get_poi(P, DB, Ix div 2)];
        {node, _, P, L, _R} when Ix rem 2 == 1 ->
            [aeu_hex:bin_to_hex(L) | get_poi(P, DB, Ix div 2)]
    end.

tree_from_json(File) ->
    tree_from_json(File, []).
tree_from_json(File, Opts) ->
    {ok, Bin} = file:read_file(File),
    JSONData = jsx:decode(Bin),
    Data0 = [{binary_to_list(Addr), binary_to_integer(Tokens, 10)} || {Addr, Tokens} <- JSONData ],
    Data =
        %% Some test data depends on entries not being sorted...
        case lists:member(no_sort, Opts) of
            true  -> Data0;
            false -> lists:sort(Data0)
        end,
    mk_tree(Data).

total_sum_from_json(File) ->
    {ok, Bin} = file:read_file(File),
    JSONData = jsx:decode(Bin),
    lists:sum([ binary_to_integer(Tokens, 10) || {_Addr, Tokens} <- JSONData ]).

%% Helper

ix({leaf, Ix, _, _}) -> Ix;
ix({node, Ix, _, _, _}) -> Ix.

parent({leaf, _, Parent, _})    -> Parent;
parent({node, _, Parent, _, _}) -> Parent.

set_parent(Parent, {leaf, Ix, _, Val})  -> {leaf, Ix, Parent, Val};
set_parent(Parent, {node, Ix, _, L, R}) -> {node, Ix, Parent, L, R}.

pp_tree(Tree) -> io:format("~s", [pp_tree(0, Tree)]).

pp_tree(Indent, {Root, DB}) ->
    case maps:get(Root, DB) of
        {leaf, Ix, _, _} -> pp_node(Indent, Ix, Root);
        {node, Ix, _, L, L} ->
            [pp_tree(Indent + 2, {L, DB}), pp_node(Indent, Ix, Root)];
        {node, Ix, _, L, R} ->
            [pp_tree(Indent + 2, {L, DB}),
             pp_node(Indent, Ix, Root),
             pp_tree(Indent + 2, {R, DB})]
    end.

pp_node(Indent, Ix, Hash) ->
    io_lib:format("~*s~p : ~7s\n", [Indent, "", Ix, aeu_hex:bin_to_hex(Hash)]).

data(Addr0, Tokens0) ->
    Addr   = string:to_upper(Addr0),
    Tokens = io_lib:format("~p", [Tokens0]),
    iolist_to_binary([Addr, ":", Tokens]).

