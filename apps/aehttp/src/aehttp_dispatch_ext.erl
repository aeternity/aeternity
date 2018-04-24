-module(aehttp_dispatch_ext).

-export([handle_request/3]).

-import(aeu_debug, [pp/1]).
-import(aehttp_helpers, [ process_request/2
                        , read_required_params/1
                        , read_optional_params/1
                        , parse_map_to_atom_keys/0
                        , base58_decode/1
                        , hexstrings_decode/1
                        , nameservice_pointers_decode/1
                        , get_nonce/1
                        , print_state/0
                        , get_contract_code/2
                        , verify_oracle_existence/1
                        , verify_oracle_query_existence/2
                        , verify_name/1
                        , ttl_decode/1
                        , parse_tx_encoding/1
                        , compute_contract_call_data/0
                        , relative_ttl_decode/1
                        , unsigned_tx_response/1
                        , get_transaction/2
                        , encode_transaction/3
                        , ok_response/1
                        , read_tx_encoding_param/1
                        , parse_filter_param/2
                        , read_optional_param/3
                        , get_block/3
                        , get_block/4
                        ]).

-compile({parse_transform, lager_transform}).
-include_lib("aecore/include/common.hrl").

-spec handle_request(
        OperationID :: atom(),
        Req :: cowboy_req:req(),
        Context :: #{}
       ) -> {Status :: cowboy:http_status(), Headers :: list(), Body :: map()}.

handle_request('GetTop', _, _Context) ->
    {ok, TopHeader} = aehttp_logic:get_top(),
    {ok, Hash} = aec_headers:hash_header(TopHeader),
    EncodedHash = aec_base58c:encode(block_hash, Hash),
    EncodedHeader = aehttp_api_parser:encode(header, TopHeader),
    {200, [], maps:put(<<"hash">>, EncodedHash, EncodedHeader)};

handle_request('GetBlockGenesis', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_genesis/0, Req, json);

handle_request('GetBlockLatest', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_latest/0, Req, json);

handle_request('GetBlockPending', Req, _Context) ->
    get_block(fun aehttp_logic:get_block_pending/0, Req, json, false);

handle_request('GetBlockByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    get_block(fun() -> aehttp_logic:get_block_by_height(Height) end, Req, json);

handle_request('GetBlockByHeightDeprecated', Req, _Context) ->
    Height = maps:get('height', Req),
    case aehttp_logic:get_block_by_height(Height) of
        {ok, Block} ->
            {200, [], aehttp_api_parser:encode(block, Block)};
        {error, block_not_found} ->
            {404, [], #{reason => <<"Block not found">>}};
        {error, chain_too_short} ->
            {404, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetBlockByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            get_block(fun() -> aehttp_logic:get_block_by_hash(Hash) end, Req, json)
    end;

handle_request('GetBlockByHashDeprecated' = _Method, Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aehttp_logic:get_block_by_hash(Hash) of
                {ok, Block} ->
                    {200, [], aehttp_api_parser:encode(block, Block)};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end;

handle_request('GetHeaderByHash', Req, _Context) ->
    case aec_base58c:safe_decode(block_hash, maps:get('hash', Req)) of
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            case aehttp_logic:get_header_by_hash(Hash) of
                {ok, Header} ->
                    {200, [], aehttp_api_parser:encode(header, Header)};
                {error, header_not_found} ->
                    {404, [], #{reason => <<"Header not found">>}}
            end
    end;

handle_request('GetHeaderByHeight', Req, _Context) ->
    Height = maps:get('height', Req),
    case aehttp_logic:get_header_by_height(Height) of
        {ok, H} ->
            Resp = aehttp_api_parser:encode(header, H),
            lager:debug("Resp = ~p", [pp(Resp)]),
            {200, [], Resp};
        {error, chain_too_short} ->
            {400, [], #{reason => <<"Chain too short">>}}
    end;

handle_request('GetTxs', _Req, _Context) ->
    {ok, Txs0} = aec_tx_pool:peek(infinity),
    lager:debug("GetTxs : ~p", [pp(Txs0)]),
    Txs = [aehttp_api_parser:encode(tx, T) || T <- Txs0],
    {200, [], Txs};

handle_request('PostBlock', Req, _Context) ->
    %TODO: possibly return an error on a broken block intead of crashing?
    {ok, Block} = aehttp_api_parser:decode(block, maps:get('Block', Req)),

    %% Only for logging
    Header = aec_blocks:to_header(Block),
    {ok, HH} = aec_headers:hash_header(Header),
    lager:debug("'PostBlock'; header hash: ~p", [HH]),
    case aec_conductor:post_block(Block) of
        ok -> {200, [], #{}};
        {error, Reason} ->
            lager:error("Post block failed: ~p", [Reason]),
            {400, [], #{reason => <<"Block rejected">>}}
    end;

handle_request('PostTx', #{'Tx' := Tx} = Req, _Context) ->
    lager:debug("Got PostTx; Req = ~p", [pp(Req)]),
    case aehttp_api_parser:decode(tx, maps:get(<<"tx">>, Tx)) of
        {error, #{<<"tx">> := broken_tx}} ->
            {400, [], #{reason => <<"Invalid tx">>}};
        {error, _} ->
            {400, [], #{reason => <<"Invalid base58Check encoding">>}};
        {ok, SignedTx} ->
            lager:debug("deserialized: ~p", [pp(SignedTx)]),
            PushRes = aec_tx_pool:push(SignedTx, tx_received),
            lager:debug("PushRes = ~p", [pp(PushRes)]),
            {200, [], #{}}
    end;

handle_request('PostContractCreate', #{'ContractCreateData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([owner, code, vm_version, deposit,
                                       amount, gas, gas_price, fee,
                                       call_data]),
                 base58_decode([{owner, owner, account_pubkey}]),
                 get_nonce(owner),
                 hexstrings_decode([code, call_data]),
                 ok_response(
                    fun(Data) ->
                        {ok, Tx} = aect_create_tx:new(Data),
                        #{owner := Owner, nonce := Nonce} = Data,
                        ContractPubKey =
                            aect_contracts:compute_contract_pubkey(Owner, Nonce),
                        #{tx => aec_base58c:encode(transaction,
                                                  aetx:serialize_to_binary(Tx)),
                          contract_address => ContractPubKey,
                          tx_hash => aec_base58c:encode(tx_hash, aetx:hash(Tx))}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCall', #{'ContractCallData' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller, contract, vm_version,
                                       amount, gas, gas_price, fee,
                                       call_data]),
                 base58_decode([{caller, caller, account_pubkey}]),
                 get_nonce(caller),
                 get_contract_code(contract, contract_code),
                 hexstrings_decode([call_data]),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostContractCallCompute', #{'ContractCallCompute' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([caller, contract, vm_version,
                                       amount, gas, gas_price, fee,
                                       function, arguments]),
                 base58_decode([{caller, caller, account_pubkey}]),
                 get_nonce(caller),
                 get_contract_code(contract, contract_code),
                 compute_contract_call_data(),
                 unsigned_tx_response(fun aect_call_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleRegister', #{'OracleRegisterTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, {query_format, query_spec},
                                       {response_format, response_spec},
                                       query_fee, fee, ttl]),
                 base58_decode([{account, account, account_pubkey}]),
                 get_nonce(account),
                 ttl_decode(ttl),
                 unsigned_tx_response(fun aeo_register_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleExtend', #{'OracleExtendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle, fee, ttl]),
                 base58_decode([{oracle, oracle, oracle_pubkey}]),
                 get_nonce(oracle),
                 ttl_decode(ttl),
                 unsigned_tx_response(fun aeo_extend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleQuery', #{'OracleQueryTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender, oracle_pubkey, query,
                                       query_fee, fee, query_ttl, response_ttl]),
                 base58_decode([{sender, sender, account_pubkey},
                               {oracle_pubkey, oracle, oracle_pubkey}]),
                 get_nonce(sender),
                 ttl_decode(query_ttl),
                 relative_ttl_decode(response_ttl),
                 verify_oracle_existence(oracle),
                 unsigned_tx_response(fun aeo_query_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostOracleResponse', #{'OracleResponseTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([oracle, query_id,
                                       response, fee]),
                 base58_decode([{oracle, oracle, account_pubkey},
                               {query_id, query_id, oracle_query_id}]),
                 get_nonce(oracle),
                 verify_oracle_query_existence(oracle, query_id),
                 unsigned_tx_response(fun aeo_response_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNamePreclaim', #{'NamePreclaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, commitment, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {commitment, commitment, commitment}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_preclaim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameClaim', #{'NameClaimTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name, name_salt, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name, name, name}]),
                 get_nonce(account),
                 verify_name(name),
                 unsigned_tx_response(fun aens_claim_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameUpdate', #{'NameUpdateTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, name_ttl,
                                       pointers, ttl, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 nameservice_pointers_decode(pointers),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_update_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameTransfer', #{'NameTransferTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, recipient_pubkey,
                                       fee]),
                 base58_decode([{account, account, account_pubkey},
                                {recipient_pubkey, recipient_account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_transfer_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostNameRevoke', #{'NameRevokeTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([account, name_hash, fee]),
                 base58_decode([{account, account, account_pubkey},
                                {name_hash, name_hash, name}]),
                 get_nonce(account),
                 unsigned_tx_response(fun aens_revoke_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('PostSpend', #{'SpendTx' := Req}, _Context) ->
    ParseFuns = [parse_map_to_atom_keys(),
                 read_required_params([sender,
                                       {recipient_pubkey, recipient},
                                       amount, fee]),
                 read_optional_params([{payload, payload, <<>>}]),
                 base58_decode([{sender, sender, account_pubkey},
                                {recipient, recipient, account_pubkey}]),
                 get_nonce(sender),
                 unsigned_tx_response(fun aec_spend_tx:new/1)
                ],
    process_request(ParseFuns, Req);

handle_request('GetAccountBalance', Req, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get('account_pubkey', Req)) of
        {ok, AccountPubkey} ->
            case get_block_hash_optionally_by_hash_or_height(Req) of
                {error, not_found} ->
                    {404, [], #{reason => <<"Block not found">>}};
                {error, invalid_hash} ->
                    {400, [], #{reason => <<"Invalid block hash">>}};
                {error, blocks_mismatch} ->
                    {400, [], #{reason => <<"Invalid height and hash combination">>}};
                {ok, Hash} ->
                      case aehttp_logic:get_account_balance_at_hash(AccountPubkey, Hash) of
                          {error, account_not_found} ->
                              {404, [], #{reason => <<"Account not found">>}};
                          {error, not_on_main_chain} ->
                              {400, [], #{reason => <<"Block not on the main chain">>}};
                          {ok, Balance} ->
                              {200, [], #{balance => Balance}}
                      end
            end;
        _ ->
            {400, [], #{reason => <<"Invalid account hash">>}}
    end;

handle_request('GetCommitmentHash', Req, _Context) ->
    Name         = maps:get('name', Req),
    Salt         = maps:get('salt', Req),
    case aens:get_commitment_hash(Name, Salt) of
        {ok, CHash} ->
            EncodedCHash = aec_base58c:encode(commitment, CHash),
            {200, [], #{commitment => EncodedCHash}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('GetName', Req, _Context) ->
    Name = maps:get('name', Req),
    case aec_chain:name_entry(Name) of
        {ok, NameEntry} ->
            #{<<"name">>     := Name,
              <<"hash">>     := Hash,
              <<"name_ttl">> := NameTTL,
              <<"pointers">> := Pointers} = NameEntry,
            {200, [], #{name      => Name,
                        name_hash => aec_base58c:encode(name, Hash),
                        name_ttl  => NameTTL,
                        pointers  => Pointers}};
        {error, name_not_found} ->
            {404, [], #{reason => <<"Name not found">>}};
        {error, name_revoked} ->
            {404, [], #{reason => <<"Name revoked">>}};
        {error, Reason} ->
            ReasonBin = atom_to_binary(Reason, utf8),
            {400, [], #{reason => <<"Name validation failed with a reason: ", ReasonBin/binary>>}}
    end;

handle_request('GetAccountsBalances', _Req, _Context) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"debug">>],
                                    aehttp, enable_debug_endpoints, false) of
        true ->
            {ok, AccountsBalances} = aehttp_logic:get_all_accounts_balances(),
            {200, [], #{accounts_balances =>
                        aehttp_api_parser:encode(account_balances,
                                                 AccountsBalances)}};
        false ->
            {403, [], #{reason => <<"Balances not enabled">>}}
    end;

handle_request('GetAccountTransactions', Req, _Context) ->
    case aec_base58c:safe_decode(account_pubkey, maps:get('account_pubkey', Req)) of
        {ok, AccountPubkey} ->
            {ok, TopBlockHash} = aehttp_logic:get_top_hash(),
            case aehttp_logic:get_account_balance_at_hash(AccountPubkey, TopBlockHash) of
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}};
                {ok, _} ->
                    case get_account_transactions(AccountPubkey, Req) of
                        {error, unknown_type} ->
                            {400, [], #{reason => <<"Unknown transaction type">>}};
                        {ok, HeaderTxs} ->
                            case encode_txs(HeaderTxs, Req) of
                                {error, Err} ->
                                    Err;
                                {ok, EncodedTxs, DataSchema} ->
                                    {200, [], #{transactions => EncodedTxs,
                                                data_schema => DataSchema}}
                            end
                    end
            end;
        _ ->
            {400, [], #{reason => <<"Invalid account hash">>}}
    end;

handle_request('GetVersion', _Req, _Context) ->
    {ok, Version} = aehttp_logic:version(),
    {ok, Revision} = aehttp_logic:revision(),
    {ok, GenHash} = aehttp_logic:get_genesis_hash(),
    Resp = #{<<"version">> => Version,
             <<"revision">> => Revision,
             <<"genesis_hash">> => GenHash},
    {200, [], aehttp_api_parser:encode(node_version, Resp)};

handle_request('GetInfo', _Req, _Context) ->
    case aeu_env:user_config_or_env([<<"http">>, <<"debug">>],
                                    aehttp, enable_debug_endpoints, false) of
        true ->
            {ok, TimeSummary} = aehttp_logic:get_top_blocks_time_summary(30),
            {200, [], #{last_30_blocks_time => TimeSummary}};
        false ->
            {403, [], #{reason => <<"Info not enabled">>}}
    end;

handle_request('CompileContract', Req, _Context) ->
    case Req of
        #{'Contract' :=
              #{ <<"code">> := Code
               , <<"options">> := Options }} ->
            %% TODO: Handle other languages
            case aehttp_logic:contract_compile(Code, Options) of
                 {ok, ByteCode} ->
                     {200, [], #{ bytecode => ByteCode}};
                 {error, ErrorMsg} ->
                     {403, [], #{reason => ErrorMsg}}
             end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('CallContract', Req, _Context) ->
    case Req of
        #{'ContractCallInput' :=
              #{ <<"abi">> := ABI
               , <<"code">> := Code
               , <<"function">> := Function
               , <<"arg">> := Argument }}  ->
            case aehttp_logic:contract_call(ABI, Code, Function, Argument) of
                {ok, Result} ->
                    {200, [], #{ out => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('EncodeCalldata', Req, _Context) ->
    case Req of
        #{'ContractCallInput' :=
              #{ <<"abi">>  := ABI
	       , <<"code">> := Code
               , <<"function">> := Function
               , <<"arg">> := Argument }} ->
            %% TODO: Handle other languages
            case aehttp_logic:contract_encode_call_data(ABI, Code, Function, Argument) of
                {ok, Result} ->
                    {200, [], #{ calldata => Result}};
                {error, ErrorMsg} ->
                    {403, [], #{reason => ErrorMsg}}
            end;
        _ -> {403, [], #{reason => <<"Bad request">>}}
    end;

handle_request('GetTx', Req, _Context) ->
    ParseFuns = [read_required_params([tx_hash]),
                 read_optional_params([{tx_encoding, tx_encoding, message_pack}]),
                 parse_tx_encoding(tx_encoding),
                 base58_decode([{tx_hash, tx_hash, tx_hash}]),
                 get_transaction(tx_hash, tx),
                 encode_transaction(tx, tx_encoding, encoded_tx),
                 ok_response(
                    fun(#{encoded_tx := #{tx := Tx, schema := Schema}}) ->
                        #{transaction => Tx,
                          data_schema => Schema}
                    end)
                ],
    process_request(ParseFuns, Req);

handle_request('GetPeerKey', _Req, _Context) ->
    case aehttp_logic:peer_pubkey() of
        {ok, PeerKey} ->
            {200, [], #{pub_key => aec_base58c:encode(peer_pubkey, PeerKey)}};
        {error, key_not_found} ->
            {404, [], #{reason => <<"Keys not configured">>}}
    end;

handle_request(OperationID, Req, Context) ->
    error_logger:error_msg(
      ">>> Got not implemented request to process: ~p~n",
      [{OperationID, Req, Context}]
     ),
    {501, [], #{}}.

encode_txs(HeaderTxs, Req) ->
    case read_tx_encoding_param(Req) of
        {error, _} = Err ->
            Err;
        {ok, TxEncoding} ->
            DataSchema =
                case TxEncoding of
                    json ->
                        <<"JSONTxs">>;
                    message_pack ->
                        <<"MsgPackTxs">>
                end,
            EncodedTxs =
                lists:map(
                    fun({mempool, Tx}) ->
                        aetx_sign:serialize_for_client_pending(TxEncoding, Tx);
                    ({BlockHeader, Tx}) ->
                        aetx_sign:serialize_for_client(TxEncoding,
                                                       BlockHeader, Tx)
                    end,
                    HeaderTxs),
            {ok, EncodedTxs, DataSchema}
    end.

get_account_transactions(Account, Req) ->
    case {parse_filter_param(tx_types, Req),
          parse_filter_param(exclude_tx_types, Req)} of
        {{error, unknown_type} = Err, _} ->
            Err;
        {_, {error, unknown_type} = Err} ->
            Err;
        {{ok, KeepTxTypes}, {ok, DropTxTypes}} ->
            ShowPending = read_optional_param(pending, Req, true),
            FilteredTxs = get_txs_and_headers(KeepTxTypes,
                                              DropTxTypes,
                                              ShowPending,
                                              Account),
            Res =
              lists:sort(
                  fun({mempool, SignedTxA}, {mempool, SignedTxB}) ->
                      TxA = aetx_sign:tx(SignedTxA),
                      TxB = aetx_sign:tx(SignedTxB),
                      {aetx:origin(TxA), aetx:nonce(TxA), TxA} >=
                      {aetx:origin(TxB), aetx:nonce(TxB), TxB};
                     ({mempool, _}, {_, _}) -> true;
                     ({_, _}, {mempool, _}) -> false;
                     ({HeaderA, SignedTxA}, {HeaderB, SignedTxB}) ->
                      HeightA = aec_headers:height(HeaderA),
                      HeightB = aec_headers:height(HeaderB),
                      TxA = aetx_sign:tx(SignedTxA),
                      TxB = aetx_sign:tx(SignedTxB),
                      {HeightA, aetx:origin(TxA), aetx:nonce(TxA), TxA} >=
                      {HeightB, aetx:origin(TxB), aetx:nonce(TxB), TxB}
                  end,
                  FilteredTxs),
            {ok, offset_and_limit(Req, Res)}
      end.

get_txs_and_headers(KeepTxTypes, DropTxTypes, ShowPending, Account) ->
    Filter =
        fun(SignedTx) ->
              Tx = aetx_sign:tx(SignedTx),
              TxType = aetx:tx_type(Tx),
              Drop = lists:member(TxType, DropTxTypes),
              Keep = KeepTxTypes =:= []
                  orelse lists:member(TxType, KeepTxTypes),
              Keep andalso not Drop
        end,
    Fun =
        fun() ->
                Txs = aec_db:transactions_by_account(Account, Filter,
                                                     ShowPending),
                TxHashes = lists:usort([aetx:hash(aetx_sign:tx(SignedTx))
                                        || SignedTx <- Txs]),
                [case aec_chain:find_transaction_in_main_chain_or_mempool(TxHash) of
                     {mempool, SignedTx} -> {mempool, SignedTx};
                     {BlockHash, SignedTx} ->
                         {ok, H} = aec_chain:get_header(BlockHash),
                         {H, SignedTx}
                 end
                 || TxHash <- TxHashes]
        end,
    %% Put this in a transaction to avoid multiple transaction and to
    %% get a snapshot of the chain state.
    aec_db:ensure_transaction(Fun).

offset_and_limit(Req, ResultList) ->
    Limit = read_optional_param(limit, Req, 20),
    Offset = read_optional_param(offset, Req, 0) + 1, % lists are 1-indexed
    Sublist =
        fun Sub([], _, _, _, Accum) -> Accum;
            Sub(_, _, _, LeftToTake, Accum) when LeftToTake =< 0 -> Accum;
            Sub([H | T], Idx, StartIdx, LeftToTake, Accum) ->
                case Idx >= StartIdx of
                    true -> Sub(T, Idx + 1, StartIdx, LeftToTake - 1, [H | Accum]);
                    false -> Sub(T, Idx + 1, StartIdx, LeftToTake, Accum)
                end
        end,
    lists:reverse(Sublist(ResultList, 1, Offset, Limit, [])).

-spec get_block_hash_optionally_by_hash_or_height(map()) ->
    {ok, binary()} | {error, not_found | invalid_hash | blocks_mismatch}.
get_block_hash_optionally_by_hash_or_height(Req) ->
    GetHashByHeight =
        fun(Height) ->
            case aehttp_logic:get_header_by_height(Height) of
                {error, chain_too_short} ->
                    {error, not_found};
                {ok, Header} ->
                    {ok, _Hash} = aec_headers:hash_header(Header)
            end
        end,
    case {maps:get('height', Req), maps:get('hash', Req)} of
        {undefined, undefined} ->
            {ok, _} = aehttp_logic:get_top_hash();
        {undefined, EncodedHash} ->
            case aec_base58c:safe_decode(block_hash, EncodedHash) of
                {error, _} ->
                    {error, invalid_hash};
                {ok, Hash} ->
                    case aec_chain:has_block(Hash) of
                        false ->
                            {error, not_found};
                        true ->
                            {ok, Hash}
                    end
            end;
        {Height, undefined} ->
            GetHashByHeight(Height);
        {Height, EncodedHash} ->
            case GetHashByHeight(Height) of
                {error, _} = Err ->
                    Err;
                {ok, Hash} -> % ensure it is the same hash
                    case aec_base58c:safe_decode(block_hash, EncodedHash) of
                        {error, _} ->
                            {error, invalid_hash};
                        {ok, Hash} -> % same hash
                            {ok, Hash};
                        {ok, _OtherHash} ->
                            {error, blocks_mismatch}
                    end
            end
    end.
