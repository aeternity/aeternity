%%%-------------------------------------------------------------------
%% @doc aecli command tree.
%% @end
%%%-------------------------------------------------------------------
-module(aecli).

-export([init/0,
         banner/1,
         prompt/1,
         expand/2,
         execute/2
        ]).

-export([parse_value/2]).

-record(aecli, {mode = operational}).

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #aecli{}}.

banner(#aecli{}) ->
    {ok, "\r\n
    / \\     ___  | |_    ___   _ __   _ __   (_) | |_   _   _ \r
   / _ \\   / _ \\ | __|  / _ \\ | '__| | '_ \\  | | | __| | | | |\r
  / ___ \\ |  __/ | |_  |  __/ | |    | | | | | | | |_  | |_| |\r
 /_/   \\_\\ \\___|  \\__|  \\___| |_|    |_| |_| |_|  \\__|  \\__, |\r
                                                        |___/ \r
\r\nWelcome to the Aeternity system CLI\r\n
Hit TAB at any time to see available options\r\n\r\n"}.

prompt(#aecli{mode = Mode}) ->
    Suffix = case Mode of
                 operational ->
                     "> ";
                 configuration ->
                     "# "
             end,
    {ok, Hostname} = inet:gethostname(),
    {ok, Hostname ++  Suffix}.

expand([], #aecli{mode = operational} = J) ->
    {no, [], ecli:format_menu(operational_menu()), J};
expand(Chars, #aecli{mode = operational} = J) ->
    %% ?DBG("expand ~p~n",[Chars]),
    expand_cmd(Chars, operational_menu(), J).

execute(CmdStr, #aecli{mode = operational} = J) ->
    execute_cmd(CmdStr, operational_menu(), J).

%% Types in the cli schema can be declared as {Mod, Type}
%% This parses values with type {aecli, Type}
parse_value(peer_pubkey, Value) ->
    case aeser_api_encoder:safe_decode(peer_pubkey, list_to_binary(Value)) of
        {ok, _PeerKey} = PeerKey ->
            PeerKey;
        _ ->
            {error, "Invalid peer public key"}
    end;
parse_value(tx_hash, Value) ->
    case aeser_api_encoder:safe_decode(tx_hash, list_to_binary(Value)) of
        {ok, _} = TxHash ->
            TxHash;
        _ ->
            {error, "Invalid Tx Hash"}
    end;
parse_value(Other, Value) ->
    lager:info("CLI attempting to show value of undecoded type ~p ~p", [Other, Value]),
    {error, "Invalid command"}.

operational_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "show",
       desc => "Show commands",
       action => fun(J, Item, _Value) ->
                        show_status(J, Item)
                end,
       children => fun() -> operational_show_menu() end
      },
     #{role => cmd,
       node_type => container,
       name => "admin",
       desc => "Administer operational state",
       children => fun() -> operational_admin_menu() end
      },
     #{role => cmd,
       node_type => leaf,
       name => "exit",
       desc => "Close session",
       action => fun(_J1, _, _) -> stop end
      }
    ].

operational_show_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "status",
       desc => "Status summary",
       action => fun(J1, Item, _) -> show_status(J1, Item) end
      },
     #{role => cmd,
       node_type => container,
       name => "peers",
       desc => "Show peers",
       action => fun(J, _Item, _) -> show_connected_peers(J, all) end,
       children => fun() -> operational_peers_menu() end
      },
     #{role => cmd,
       node_type => container,
       name => "tx_pool",
       desc => "Transaction pool commands",
       action => fun(J, _Item, _) -> show_tx_pool_summary(J) end,
       children => fun() -> operational_tx_pool_menu() end
      }
    ].

operational_tx_pool_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "size",
       desc => "Show size of TX Pool",
       action => fun(J, _, _) -> show_tx_pool_size(J, all) end
      },
     #{role => cmd,
       node_type => container,
       name => "miner_gas_price",
       desc => "Show the current miner_gas_price",
       action => fun(J, _Item, _) -> show_tx_pool_miner_gas_price(J) end
      },
     #{role => cmd,
       node_type => container,
       name => "transaction",
       desc => "Inspect a transaction in the pool",
       children => fun() -> tx_hash_schema() end,
       action => fun(J, Item, _Value) -> show_tx_summary(J, Item) end
      }].

operational_peers_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "connected",
       desc => "Show connected peers",
       action => fun(J, _, _) -> show_connected_peers(J, all) end,
       children => fun() -> operational_connected_peers_menu() end
      },
     #{role => cmd,
       node_type => container,
       name => "verified",
       desc => "Show verified peers",
       action => fun(J, _Item, _) -> show_verified_peers(J) end
      },
     #{role => cmd,
       node_type => container,
       name => "unverified",
       desc => "Show unverified peers",
       action => fun(J, _Item, _) -> show_unverified_peers(J) end
      },
     #{role => cmd,
       node_type => container,
       name => "blocked",
       desc => "Show blocked peers",
       action => fun(J, _Item, _) -> show_blocked_peers(J) end
      }
    ].

operational_connected_peers_menu() ->
    [#{role => cmd,
       node_type => leaf,
       name => "all",
       desc => "Show all connected peers",
       action => fun(J, _, _) -> show_connected_peers(J, all) end
      },
     #{role => cmd,
       node_type => leaf,
       name => "outbound",
       desc => "Show outbound connected peers",
       action => fun(J, _Item, _) -> show_connected_peers(J, outbound) end
      },
     #{role => cmd,
       node_type => leaf,
       name => "inbound",
       desc => "Show inbound connected peers",
       action => fun(J, _Item, _) -> show_connected_peers(J, inbound) end
      }
    ].

operational_admin_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "peers",
       desc => "Administer peers",
       children => fun() -> operational_admin_peers_menu() end
      },
      #{role => cmd,
       node_type => container,
       name => "tx_pool",
       desc => "Administer TX pool",
       children => fun() -> operational_admin_tx_pool_menu() end
      }
    ].

operational_admin_peers_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "add",
       desc => "Add a peer",
       children => fun() -> peer_schema() end,
       action => fun(J, Item, Value) -> add_peer(J, Item, Value) end
      },
     #{role => cmd,
       node_type => container,
       name => "remove",
       desc => "Remove a peer",
       children => fun() -> peer_pubkey_schema() end,
        action => fun(J, Item, Value) -> remove_peer(J, Item, Value) end
      },
     #{role => cmd,
       node_type => container,
       name => "block",
       desc => "Block a peer",
       children => fun() -> [#{role => schema,
                                node_type => leaf,
                                name => "pubkey",
                                desc => "Remote node public key starting with pp_",
                                type => {aecli, peer_pubkey}
                                }] end,
        action => fun(J, Item, Value) -> block_peer(J, Item, Value) end
      },
     #{role => cmd,
       node_type => container,
       name => "unblock",
       desc => "Unblock a peer",
       children => fun() -> [#{role => schema,
                                node_type => leaf,
                                name => "pubkey",
                                desc => "Remote node public key starting with pp_",
                                type => {aecli, peer_pubkey}
                                }] end,
        action => fun(J, Item, Value) -> unblock_peer(J, Item, Value) end
      }
    ].

peer_schema() ->
    [#{role => schema,
       node_type => leaf,
       name => "host",
       desc => "Hostname",
       type => string
     },
     #{role => schema,
       node_type => leaf,
       name => "port",
       desc => "Port",
       type => uint32
     },
     #{role => schema,
       node_type => leaf,
       name => "pubkey",
       desc => "Remote node public key starting with pp_",
       type => {aecli, peer_pubkey}
     },
     #{role => schema,
       node_type => leaf,
       name => "trusted",
       desc => "If the peer is trusted",
       type => boolean
     }].

peer_pubkey_schema() ->
    [#{role => schema,
       node_type => leaf,
       name => "pubkey",
       desc => "Remote node public key starting with pp_",
       type => {aecli, peer_pubkey}
      }].

operational_admin_tx_pool_menu() ->
     [#{role => cmd,
       node_type => container,
       name => "set",
       desc => "Set TX pool parameters",
       children => fun() -> [#{role => schema,
                                node_type => leaf,
                                name => "minimum_gas_price",
                                desc => "Set the minumum miner_gas_price. This does not impact transactions currently in the pool",
                                type => uint64,
                                range => [{min, 1000000}]
                                }] end,
       action => fun(J, Item, Value) -> set_gas_price(J, Item, Value) end
      },
     #{role => cmd,
       node_type => container,
       name => "delete",
       desc => "Delete a transaction from the TX pool",
       children => fun() -> tx_hash_schema() end,
       action => fun(J, Item, Value) -> delete_tx(J, Item, Value) end
      }].

tx_hash_schema() ->
    [#{role => schema,
       node_type => leaf,
       name => "hash",
       desc => "Hash of transaction starting with th_",
       type => {aecli, tx_hash}
      }].

add_peer(_J, Items, _Value) ->
    {PeerInfo, Trusted} = parse_peer_cmd(Items),
    add_peer(PeerInfo, Trusted).

remove_peer(_J, Items, _Value) ->
    remove_peer(Items).

block_peer(_J, Items, _Value) ->
    block_peer(Items).

unblock_peer(_J, Items, _Value) ->
    unblock_peer(Items).

show_tx_pool_summary(J) ->
    Summary =
     #{<<"All">>         => aec_tx_pool:size(),
       <<"Visited">>     => aec_tx_pool:size(visited),
       <<"Not visited">> => aec_tx_pool:size(not_visited)},
    {ok, ecli:format(Summary), J}.

show_tx_pool_size(J, all) ->
    Summary =
     #{<<"All">> => aec_tx_pool:size()},
     {ok, ecli:format(Summary), J}.

show_tx_pool_miner_gas_price(J) ->
    Summary =
     #{<<"Expected minumum gas_price">> => aec_tx_pool:minimum_miner_gas_price()},
    {ok, ecli:format(Summary), J}.

show_tx_summary(J, [#{name := "hash", value := TxHash}]) ->
    case aec_tx_pool:inspect(TxHash) of
        {ok, Failures, Visited, TTL} ->
            Summary =
                #{<<"Failures">> => Failures,
                  <<"Visited">>  => Visited,
                  <<"TTL">>      => TTL},
            {ok, ecli:format(Summary), J};
        {error, not_found} ->
            {ok, "Transaction not present in the TX Pool", J};
         {error, already_accepted} ->
            {ok, "Transaction already accepted, no longer in TX Pool", J}
    end;
show_tx_summary(J, _Item) ->
    {ok, "Command not understood", J}.

set_gas_price(J, [#{name := "minimum_gas_price", value := NewGasPrice}], _Value) ->
    Config = #{<<"mining">> => #{ <<"min_miner_gas_price">> => NewGasPrice}},
    aeu_env:update_config(Config, _Notify = true, _InfoReport = silent),
    {ok, "Gas price updated", J};
set_gas_price(J, _, _Value) ->
    {ok, "Command not understood", J}.

delete_tx(J,  [#{name := "hash", value := TxHash}], _Value) ->
    case aec_tx_pool:delete(TxHash) of
        ok ->
            {ok, "Tx deleted from the pool", J};
        {error, not_found} ->
            {ok, "Transaction not present in the TX Pool", J};
         {error, already_accepted} ->
            {ok, "Transaction already accepted, no longer in TX Pool", J}
    end;
delete_tx(J, _Item, _Value) ->
    {ok, "Command not understood", J}.

show_status(#aecli{} = J, _Item) ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    GenesisBlockHash = aec_consensus:get_genesis_hash(),
    Difficulty = aec_blocks:difficulty(TopKeyBlock),
    {Syncing, SyncProgress} = aec_sync:sync_progress(),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = aec_peers:count(peers),
    PendingTxsCount = aec_tx_pool:size(),
    {ok, PeerPubkey} = aec_keys:peer_pubkey(),
    TopBlock = aec_chain:top_block(),
    TopBlockHeight = aec_blocks:height(TopBlock),
    TopBlockHash = aec_chain:top_key_block_hash(),
    Status =
     #{<<"Genesis block Hash">>     => aeser_api_encoder:encode(key_block_hash, GenesisBlockHash),
       <<"Difficulty">>                 => Difficulty,
       <<"Syncing">>                    => Syncing,
       <<"Sync progress">>              => SyncProgress,
       <<"Node version">>               => NodeVersion,
       <<"Node revision">>              => NodeRevision,
       <<"Peer count">>                 => PeerCount,
       <<"Peer connections (inbound)">> => aec_peers:count(inbound),
       <<"Peer connections (outbound)">> => aec_peers:count(outbound),
       <<"Pending transactions count">> => PendingTxsCount,
       <<"Network id">>                 => aec_governance:get_network_id(),
       <<"Peer pubkey">>                => aeser_api_encoder:encode(peer_pubkey, PeerPubkey),
       <<"Top key block hash">>         => aeser_api_encoder:encode(key_block_hash, TopBlockHash),
       <<"Top block height">>           => TopBlockHeight},
    {ok, ecli:format(Status), J}.

show_connected_peers(J, Which) when Which == all; Which == inbound; Which == outbound ->
    show_peers(aec_peers:connected_peers(Which), J).

show_verified_peers(J) ->
    show_peers(aec_peers:available_peers(verified), J).

show_unverified_peers(J) ->
    show_peers(aec_peers:available_peers(unverified), J).

show_blocked_peers(J) ->
    show_peers(aec_peers:blocked_peers(), J).

show_peers([], J) ->
    {ok, "No peers found\r\n", J};
show_peers(Peers, J) ->
    Peers1 = lists:map(
                fun(#{pubkey := Pubkey} = P) ->
                    maps:put(pubkey, aeser_api_encoder:encode(peer_pubkey, Pubkey), P)
                end, Peers),
    {ok, ecli:format_table(Peers1), J}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Given a string from the user and a tree of menu items match the
%% command against the tree. Several outcomes:
%%
%% 1. The string matches the prefix of a single node - Fill the
%%    remaining part of the menu item. With a space at the end if the
%%    node is a container, not if it is a leaf
%%
%% 2. The string fully matches a single container - Prompt with the
%%    next level of menu items
%%
%% 3. The String fully matches a single leaf - nothing to do
%%
%% 4. The string matches nothing - do nothing
%%
%% 5. The string matches several possible items - complete as far as
%%    we can and prompt the user with the possible matches

expand_cmd(Str, Menu, J) ->
    %% ?DBG("match_cmd ~p~n",[Str]),
    %% Use the library function provided in cli.erl to take care of
    %% the expansion.
    case ecli:expand(Str, Menu) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, MenuItems, J}
    end.

execute_cmd(CmdStr, Menu, #aecli{} = J) ->
    case ecli:lookup(CmdStr, Menu, txn) of
        {error, Reason} ->
            {ok, Reason, J};
        {ok, Cmd, Leaf} ->
            %% Cmd here is the list of all items along the path that are part of
            %% the command. We execute the action associated with the last one
            %% i.e. for "show configuration config path" we would execute
            %% the action for the "configuration" item with role == cmd.
            #{action := Action} = lists:last(Cmd),
            case catch Action(J, Leaf, "Value") of
                {'EXIT', _Reason} ->
                    {ok, "Error executing command", J};
                {ok, Result} ->
                    {ok, Result, J};
                {ok, Result, J1} ->
                    {ok, Result, J1};
                stop ->
                    stop
            end
    end.

parse_peer_cmd(Items) ->
    lists:foldl(fun parse_peer_cmd/2, {#{}, false}, Items).

parse_peer_cmd(#{name := "host", value := Host}, {PI, Trusted}) ->
    {PI#{host => Host}, Trusted};
parse_peer_cmd(#{name := "port", value := Port}, {PI, Trusted}) ->
    {PI#{port => Port}, Trusted};
parse_peer_cmd(#{name := "pubkey", value := Pubkey}, {PI, Trusted}) ->
    {PI#{pubkey => Pubkey}, Trusted};
parse_peer_cmd(#{name := "trusted", value := Trusted}, {PI, _}) ->
    {PI, Trusted}.

add_peer(#{host := _Host, port := _Port, pubkey := _PK} = PeerInfo, Trusted) when is_boolean(Trusted) ->
    case Trusted of
        false ->
            aec_peers:add_peers(undefined, PeerInfo);
        true ->
            aec_peers:add_trusted(PeerInfo)
    end,
    {ok, "Peer added"};
add_peer(_PeerInfo, _Trusted) ->
    {ok, "Missing parameter, peer not added"}.

remove_peer([#{name := "pubkey", value := PeerPubKey}]) ->
    ok = aec_peers:del_peer(PeerPubKey),
    {ok, "Peer removed"};
remove_peer(_) ->
    {ok, "Command not understood"}.

block_peer([#{name := "pubkey", value := PeerPubKey}]) ->
    ok = aec_peers:block_peer(PeerPubKey),
    {ok, "Peer blocked"};
block_peer(_) ->
    {ok, "Command not understood"}.

unblock_peer([#{name := "pubkey", value := PeerPubKey}]) ->
    ok = aec_peers:unblock_peer(PeerPubKey),
    {ok, "Peer unblocked"};
unblock_peer(_) ->
    {ok, "Command not understood"}.