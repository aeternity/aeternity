-module(aehttp_dispatch_rosetta).

-export([forbidden/2]).
-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ process_request/2
                        , read_required_params/1
                        , read_optional_params/1
                        , parse_map_to_atom_keys/0
                        , api_decode/1
                        , nameservice_pointers_decode/1
                        , get_nonce/1
                        , get_nonce_from_account_id/1
                        , print_state/0
                        , get_contract_code/2
                        , get_info_object_from_tx/3
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , verify_name/1
                        , ttl_decode/1
                        , poi_decode/1
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/2
                        , when_stable/1
                        , ok_response/1
                        , read_optional_param/3
                        , get_poi/3
                        , get_block_hash_optionally_by_hash_or_height/1
                        , do_dry_run/0
                        ]).

-compile({parse_transform, lager_transform}).

-define(READ_Q, http_read).
-define(WRITE_Q, http_update).
-define(NO_Q, no_queue).

%% dry run limits
-define(DEFAULT_GAS_LIMIT, 6000000).
-define(DEFAULT_CALL_REQ_GAS_LIMIT, 1000000).

-define(TC(Expr, Msg), begin {Time, Res} = timer:tc(fun() -> Expr end), lager:debug("[~p] Msg = ~p", [Time, Msg]), Res end).

-spec forbidden( Mod :: module(), OperationID :: atom() ) -> boolean().
forbidden(_Mod, _OpId) -> false.

-spec handle_request(
        OperationID :: atom(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.
handle_request(OperationID, Req, Context) ->
    run(queue(OperationID),
        fun() ->
                ?TC(handle_request_(OperationID, Req, Context), Req)
        end).

%% run(no_queue, F) -> F();
run(Queue, F) ->
    try when_stable(
          fun() ->
                  aec_jobs_queues:run(Queue, F)
          end)
    catch
        error:{rejected, _} ->
            {503, [], #{reason => <<"Temporary overload">>}};
        error:timeout ->
            {503, [], #{reason => <<"Not yet started">>}};
        Class:Reason:Stacktrace ->
            lager:error("CRASH ~p ~p, ~p", [Class, Reason, Stacktrace]),
            {500, [], #{reason => <<"Internal server error">>}}
    end.

%% read transactions
queue('accountBalance')         -> ?READ_Q;
queue('accountCoins')           -> ?READ_Q;
queue('block')                  -> ?READ_Q;
queue('blockTransaction')       -> ?READ_Q;
queue('call')                   -> ?READ_Q;
queue('eventsBlocks')           -> ?READ_Q;
queue('mempool')                -> ?READ_Q;
queue('mempoolTransaction')     -> ?READ_Q;
queue('networkList')            -> ?READ_Q;
queue('networkOptions')         -> ?READ_Q;
queue('networkStatus')          -> ?READ_Q;
queue('searchTransactions')     -> ?READ_Q;
queue('constructionCombine')    -> ?WRITE_Q;
queue('constructionDerive')     -> ?WRITE_Q;
queue('constructionHash')       -> ?WRITE_Q;
queue('constructionMetadata')   -> ?WRITE_Q;
queue('constructionParse')      -> ?WRITE_Q;
queue('constructionPayloads')   -> ?WRITE_Q;
queue('constructionPreprocess') -> ?WRITE_Q;
queue('constructionSubmit')     -> ?WRITE_Q;
queue(_)                        -> ?WRITE_Q.

%% handle_request_('GetTopBlock', _, _Context) ->
%%     case aec_chain:top_block() of
%%         Block when Block =/= undefined ->
%%             case aec_blocks:height(Block) of
%%                 0 ->
%%                     Header = aec_blocks:to_header(Block),
%%                     {200, [], #{key_block => aec_headers:serialize_for_client(Header, key)}};
%%                 _ ->
%%                     PrevBlockHash = aec_blocks:prev_hash(Block),
%%                     case aec_chain:get_block(PrevBlockHash) of
%%                         {ok, PrevBlock} ->
%%                             PrevBlockType = aec_blocks:type(PrevBlock),
%%                             Header = aec_blocks:to_header(Block),
%%                             Type =
%%                                 case aec_headers:type(Header) of
%%                                     key -> key_block;
%%                                     micro -> micro_block
%%                                 end,
%%                             SerializedHeader = aec_headers:serialize_for_client(Header, PrevBlockType),
%%                             {200, [], #{Type => SerializedHeader}};
%%                         error ->
%%                             {404, [], #{reason => <<"Block not found">>}}
%%                     end
%%             end;
%%         undefined ->
%%             {404, [], #{reason => <<"Block not found">>}}
%%     end;

handle_request_('accountBalance', _, _Context) ->
    {501, [], #{}};
handle_request_('accountCoins', _, _Context) ->
    {501, [], #{}};
handle_request_('block', _, _Context) ->
    {501, [], #{}};
handle_request_('blockTransaction', _, _Context) ->
    {501, [], #{}};
handle_request_('call', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionCombine', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionDerive', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionHash', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionMetadata', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionParse', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionPayloads', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionPreprocess', _, _Context) ->
    {501, [], #{}};
handle_request_('constructionSubmit', _, _Context) ->
    {501, [], #{}};
handle_request_('eventsBlocks', _, _Context) ->
    {501, [], #{}};
handle_request_('mempool', _, _Context) ->
    {501, [], #{}};
handle_request_('mempoolTransaction', _, _Context) ->
    {501, [], #{}};
handle_request_('networkList', _, _Context) ->
    {200, [], #{<<"network_identifiers">> => [#{<<"blockchain">> => <<"aeternity">>, %% TODO: check hardcoding
                                                <<"network">> => <<"mainnet">> %% TODO: check hardcoding
                                               }
                                             ]
               }};
handle_request_('networkOptions', _, _Context) ->
    {501, [], #{}};
handle_request_('networkStatus', _, _Context) ->
    {501, [], #{}};
handle_request_('searchTransactions', _, _Context) ->
    {501, [], #{}};

handle_request_(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.
