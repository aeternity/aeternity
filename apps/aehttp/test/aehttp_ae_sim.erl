%%%-----------------------------------------------------------------------------
%%% @title /Users/sean/Aeternity/aeternity-hyperchains/apps/aehttp/test/aehttp_btc_sim
%%% @doc ae node simulator embedded in minimum http api
%%%
%%% @author sean
%%% @copyright (C) 2022, Aeternity
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(aehttp_ae_sim).

-behaviour(gen_server).

%% External API
-export([start_link/3, stop/1]).

%% Cowboy callbacks called by aehttp_api_handler
-export([forbidden/2, handle_request/3]).


%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

-include_lib("aehttp/include/aehttp_spec.hrl").

%%%=============================================================================
%%% Start cowboy web server pointing to standard ae http routes
%%%=============================================================================
start_cowboy(Name, Port) ->
    Paths = aehttp_api_router:get_paths(external, ?MODULE, #{sim_name => Name}),
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    _ = aehttp_api_validate:validator(?SWAGGER2),
    _ = aehttp_api_validate:validator(?OAS3),
    Opts = [{port, Port},
            {ip, {127,0,0,1}},
            {num_acceptors, 3}],
    Env = #{env => #{dispatch => Dispatch},
            middlewares => [aehttp_cors_middleware,
                            cowboy_router,
                            cowboy_handler]},
    lager:debug("AE Sim Opts = ~p", [Opts]),
    {ok, _} = cowboy:start_clear(Name, Opts, Env).

forbidden(_Mod, _OperationId) ->
    false.

%% Simulator compatible callbacks for a subset of the API required
handle_request('GetCurrentKeyBlockHash',_,#{sim_name := SimName}) ->
    Hash = aec_chain_sim:top_key_block_hash(SimName),
    EncodedHash = aeser_api_encoder:encode(key_block_hash, Hash),
    {200, [], #{hash => EncodedHash}};
handle_request('GetCurrentGeneration', _, #{sim_name := SimName}) ->
    generation_rsp(SimName, aec_chain_sim:get_current_generation());
handle_request('GetGenerationByHash',#{hash := EncHash},#{sim_name := SimName}) ->
    case aeser_api_encoder:safe_decode(key_block_hash, EncHash) of
        {error, _} -> {400, [], #{reason => <<"Invalid hash">>}};
        {ok, Hash} ->
            %% Forward gives us all transactions at the same height as the keyblock
            case aec_chain_sim:get_generation_by_hash(SimName, Hash, forward) of
                Ok = {ok, _G} -> generation_rsp(SimName, Ok);
                error         -> {400, [], #{reason => <<"Hash not on main chain">>}}
            end
    end;
handle_request('GetGenerationByHeight',#{height := HeightStr},#{sim_name := SimName}) ->
    Height = aehttp_helpers:to_int(HeightStr),
    case aec_chain_sim:get_key_block_hash_at_height(SimName, Height) of
        error -> {404, [], #{reason => <<"Chain too short">>}};
        {ok, Hash} -> generation_rsp(SimName, aec_chain_sim:get_generation_by_hash(SimName, Hash, forward))
    end;
handle_request('GetMicroBlockTransactionsByHash', #{hash := EncHash}, #{sim_name := SimName}) ->
    case aeser_api_encoder:safe_decode(micro_block_hash, EncHash) of
        {ok, Hash} ->
            case aec_chain_sim:block_by_hash(SimName, Hash) of
                {ok, Block} ->
                    Header = aec_blocks:to_header(Block),
                    Txs = [ aetx_sign:serialize_for_client(Header, Tx)
                            || Tx <- aec_blocks:txs(Block)],
                    {200, [], #{transactions => Txs}};
                {error, block_not_found} ->
                    {404, [], #{reason => <<"Block not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid hash">>}}
    end;
handle_request('GetAccountNextNonce',#{pubkey := EncPubkey}, #{sim_name := SimName}) ->
    case aeser_api_encoder:safe_decode(account_pubkey, EncPubkey) of
        {ok, Pubkey} ->
            case aec_chain_sim:get_next_nonce(SimName, Pubkey) of
                {ok, NextNonce} ->
                    {200, [], #{next_nonce => NextNonce}};
                {error, account_not_found} ->
                    {404, [], #{reason => <<"Account not found">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid public key">>}}
    end;
handle_request('PostTransaction', #{'Tx' := Tx}, Context) -> %% swagger2
    handle_request('PostTransaction', Tx, Context);
handle_request('PostTransaction', #{'EncodedTx' := Tx}, Context) -> %% oas3
    handle_request('PostTransaction', Tx, Context);
handle_request('PostTransaction', #{<<"tx">> := Tx}, #{sim_name := SimName}) ->
    case aeser_api_encoder:safe_decode(transaction, Tx) of
        {ok, TxDec} ->
            SignedTx = aetx_sign:deserialize_from_binary(TxDec),
            case aec_chain_sim:push(SimName, SignedTx) of
                ok ->
                    Hash = aetx_sign:hash(SignedTx),
                    {200, [], #{<<"tx_hash">> => aeser_api_encoder:encode(tx_hash, Hash)}};
                {error, E} ->
                    lager:debug("Transaciton ~p failed to be pushed to pool because: ~p", [SignedTx, E]),
                    {400, [], #{reason => <<"Invalid tx">>}}
            end;
        {error, _} ->
            {400, [], #{reason => <<"Invalid api encoding">>}}
    end;
handle_request(OperationId, Params, Context) ->
    lager:debug("Unsupported request = ~p~n", [{OperationId, Params, Context}]),
    {404, [], #{reason => <<"Unsupported operation in ae_sim">>}}.

generation_rsp(_, error) ->
    {404, [], #{reason => <<"Block not found">>}};
generation_rsp(SimName, {ok, #{ key_block := KeyBlock, micro_blocks := MicroBlocks }}) ->
    case aec_blocks:height(KeyBlock) of
        0 ->
            {200, [], aehttp_helpers:encode_generation(KeyBlock, MicroBlocks, key)};
        _ ->
            PrevBlockHash = aec_blocks:prev_hash(KeyBlock),
            case aec_chain_sim:block_by_hash(SimName, PrevBlockHash) of
                {ok, PrevBlock} ->
                    PrevBlockType = aec_blocks:type(PrevBlock),
                    {200, [], aehttp_helpers:encode_generation(KeyBlock, MicroBlocks, PrevBlockType)};
                error ->
                    {404, [], #{reason => <<"Block not found">>}}
            end
    end.

%%%=============================================================================
%%% Gen Server
%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% Loop state
-record(state,
    {
        chain,
        accounts = #{}
    }).
-type state() :: state.

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Start a AE node simulator
%% The node will start at the first of the provided generations with
%% the specified accounts and balances 
%% #{generations => ets storing [{{Height, Hash}, Fork, Txs}]
%%   accounts => [{<<"3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX">> -> 10000}]}.
-spec start_link(atom(), integer(), map()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Name, Port, _InitialState) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Port], []).

stop(Pid) ->
    gen_server:stop(Pid).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================
-spec init(state()) -> {ok, state()}.
init([Name, Port]) ->
    %% First start simulated chain then open up http api to it
    {ok, _ChainP} = aec_chain_sim:start(#{name => Name}),
    Enabled = [<<"chain">>, <<"account">>, <<"transaction">>],
    application:set_env(aehttp, enabled_endpoint_groups, Enabled),
    {ok, _} = start_cowboy(Name, Port),
    {ok, #state{chain = Name, accounts = []}}.

-spec handle_call(any(), pid(), state()) -> {ok, any(), state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {ok, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================