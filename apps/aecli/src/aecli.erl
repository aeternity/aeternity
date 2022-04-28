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

-record(aecli, {mode = operational}).

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #aecli{}}.

banner(#aecli{}) ->
    {ok, "\r\n
    / \\      ___  | |_    ___   _ __   _ __   (_) | |_   _   _ \r
   / _ \\    / _ \\ | __|  / _ \\ | '__| | '_ \\  | | | __| | | | |\r
  / ___ \\  |  __/ | |_  |  __/ | |    | | | | | | | |_  | |_| |\r
 /_/   \\_\\  \\___|  \\__|  \\___| |_|    |_| |_| |_|  \\__|  \\__, |\r
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
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++  Suffix};
        _ ->
            {ok, Suffix}
    end.


expand([], #aecli{mode = operational} = J) ->
    {no, [], ecli:format_menu(operational_menu()), J};
expand(Chars, #aecli{mode = operational} = J) ->
    %% ?DBG("expand ~p~n",[Chars]),
    expand_cmd(Chars, operational_menu(), J).

execute(CmdStr, #aecli{mode = operational} = J) ->
    execute_cmd(CmdStr, operational_menu(), J).


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
       node_type => leaf,
       name => "set",
       desc => "Set operational status",
       action => fun(_J1, _, _) -> [] end
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
      }
    ].

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

show_status(#aecli{} = J, _Item) ->
    {ok, TopKeyBlock} = aec_chain:top_key_block(),
    GenesisBlockHash = aec_consensus:get_genesis_hash(),
    Difficulty = aec_blocks:difficulty(TopKeyBlock),
    {Syncing, SyncProgress} = aec_sync:sync_progress(),
    NodeVersion = aeu_info:get_version(),
    NodeRevision = aeu_info:get_revision(),
    PeerCount = aec_peers:count(peers),
    PeerConns = #{<<"inbound">> => aec_peers:count(inbound),
                  <<"outbound">> => aec_peers:count(outbound)},
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
       <<"Peer connections">>           => PeerConns,
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
    case ecli:expand(Str, Menu, state) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, MenuItems, J}
    end.

execute_cmd(CmdStr, Menu, #aecli{} = J) ->
    case ecli:lookup(CmdStr, Menu, txn) of
        {error, Reason} ->
            {ok, Reason, J};
        {ok, Cmd, Leaf, Value} ->
            %% Cmd here is the list of all items along the path that are part of
            %% the command. We execute the action associated with the last one
            %% i.e. for "show configuration config path" we would execute
            %% the action for the "configuration" item with role == cmd.
            #{action := Action} = lists:last(Cmd),
            case catch Action(J, Leaf, Value) of
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
