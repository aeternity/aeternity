%%%-----------------------------------------------------------------------------
%%% @title /Users/sean/Aeternity/aeternity-hyperchains/apps/aehttp/test/aehttp_btc_sim
%%% @doc Extremely basic web server simulating a bitcoin node
%%% supporting a small number of hard coded bitcoin operations.
%%% for testing hyperchains parent chain connector
%%%
%%% Simulates an embedded wallet as well as the bitcoin chain.
%%%
%%% When a UTXO is sent to the mempool it is marked in the wallet as unavailable.
%%% When it is placed in a block it disappears from the Wallet and any new UTXOs
%%% put in the wallet
%%%
%%% @author sean
%%% @copyright (C) 2022, Aeternity```
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(aehttp_btc_sim).

-behaviour(gen_server).

%% External API
-export([start_link/3, stop/1]).
-export([mine_on_fork/2, get_height/2]).

%% Handy functions to generate various test scenarios as initial state
%% for the simulator
-export([btc_accounts/0, scenario/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
            terminate/2, code_change/3]).

%%%=============================================================================
%%% Standalone cowboy web server
%%%=============================================================================
-export([init/2]).
start_cowboy(Name, Port, Pid) ->
    Paths = [{"/wallet/:wallet", ?MODULE, Pid},
            {"/", ?MODULE, Pid}],
    Dispatch = cowboy_router:compile([{'_', Paths}]),

    Opts = [{port, Port},
            {ip, {127,0,0,1}},
            {num_acceptors, 3}],
    Env = #{env => #{dispatch => Dispatch},
            max_skip_body_length => 1000000,
            middlewares => [aehttp_cors_middleware,
                            cowboy_router,
                            cowboy_handler]},
    lager:debug("BTC Sim Opts = ~p", [Opts]),
    {ok, _} = cowboy:start_clear(Name, Opts, Env).

init(Req0=#{method := <<"POST">>}, Pid) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    %% io:format(user, "Data = ~p~n", [jsx:decode(Data, [return_maps])]),
    #{
      <<"jsonrpc">> := <<"2.0">>,
      <<"method">> := Method,
      <<"params">> := Params,
      <<"id">> := Id
    } = jsx:decode(Data, [return_maps]),
    Bindings = cowboy_req:bindings(Req),
    case post_req(Pid, Method, Params, Bindings) of
        {ok, Resp} ->
            RespBody = #{<<"result">> => Resp,
                         <<"error">> => null,
                         <<"id">> => Id},
            %% io:format(user, "Resp = ~p~n", [RespBody]),
            Body = jsx:encode(RespBody),
            Headers = #{ <<"content-type">> => <<"application/json">> },
            Req1 = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Req1, Pid};
        {error, _Reason} ->
            Req1 = cowboy_req:reply(500, Req),
            {ok, Req1, Pid}
    end.

%%%=============================================================================
%%% Gen Server
%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% Loop state
-record(state,
    {
        cowboy_listener,
        chain,
        wallets = #{},
        mempool = []
    }).
-type state() :: state.

%% BTC transaction - the parts we care about for HC
-record(tx,
    {
        txid,
        vin = [],
        vout = []
    }).

-record(vin,
    {
        txid,
        vout,
        scriptSig,
        sequence
    }).

-record(vout,
    {
        value,
        n,
        scriptPubKey
    }).

-record(utxo,
    {
        txid,
        vout,
        address,
        scriptPubKey,
        amount,
        confirmations = 0
    }).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Start a BTC node simulator
%% The node will start at the first of the provided generations with
%% the specified accounts and balances
%% #{generations => ets storing [{{Height, Hash}, Fork, Txs}]
%%   accounts => [{<<"3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX">> -> 10000}]}.
-spec start_link(atom(), integer(), map()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::any()}.
start_link(Name, Port, InitialState) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Port, InitialState], []).

stop(Pid) ->
    gen_server:stop(Pid).

%% Web server received POST request
post_req(Pid, Method, Params, Bindings) ->
    gen_server:call(Pid, {post_req, Method, Params, Bindings}).

%% @doc mine a block on a simulated BTC node.
%% Afterwards this block will hold the current contents of the mempool
mine_on_fork(Name, ForkName) when is_atom(Name), is_atom(ForkName) ->
    gen_server:call(Name, {mine_on_fork, ForkName}).

get_height(Name, ForkName) when is_atom(Name), is_atom(ForkName) ->
    gen_server:call(Name, {get_height, ForkName}).

%% @doc Generate an initial state for a BTC chain with a genesis block
%% and set of accounts.
%% generations => [{Hash, Height, ForkName, Transactions}]
scenario(BTCAccounts) ->
    Accounts = lists:map(fun({Pub, _Priv}) -> {Pub, 10000} end, BTCAccounts),
    #{accounts => maps:from_list(Accounts)}.

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================
-spec init(state()) -> {ok, state()}.
init([Name, Port, #{accounts := Accounts}]) ->
    {ok, _} = start_cowboy(Name, Port, self()),
    Chain = chain_new(),
    Wallets = maps:map(fun(Acct, Balance) -> wallet_new(Acct, Balance) end, Accounts),
    {ok, #state{chain = Chain, wallets = Wallets, cowboy_listener = Name}}.

-spec handle_call(any(), pid(), state()) -> {ok, any(), state()}.
handle_call({post_req, Method, Params, Bindings}, _From, State) ->
    %% io:format(user, "~p - postReq ~p~n", [self(), {post_req, Method, Params, Bindings}]),
    {Reply, State1} = handle_post(Method, Params, Bindings, State),
    {reply, Reply, State1};
handle_call({mine_on_fork, Fork}, _From, State) ->
    #state{chain = Chain, mempool = Mempool, wallets = Wallets} = State,
    {ok, Hash, Height, PrevHash} = chain_post_block(Chain, Fork, Mempool),
    Wallets1 = wallet_apply_txs(Wallets, Mempool),
    Reply = {ok, {Hash, Height, PrevHash}},
    {reply,  Reply, State#state{mempool = [], wallets = Wallets1}};
handle_call({get_height, Fork}, _From, #state{chain = Chain} = State) ->
    {{Height, _PrevHash0}, _OtherFork, _, _Txs} = chain_top_block(Chain, Fork),
    {reply, Height, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{cowboy_listener = Cowboy}) ->
    ok = cowboy:stop_listener(Cowboy),
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_post(<<"getbestblockhash">>, Params, _Bindings, State) ->
    handle_getbestblockhash(Params, State);
handle_post(<<"getblockhash">>, [Height], _Bindings, State) ->
    handle_getblockhash(Height, State);
handle_post(<<"getblock">>, [BlockHash, Verbosity], _Bindings, State) ->
    handle_getblock(BlockHash, Verbosity, State);
handle_post(<<"listunspent">>, [Minconf, Maxconf], _Bindings, State) ->
    Wallets = State#state.wallets,
    handle_listunspent(Wallets, Minconf, Maxconf, State);
handle_post(<<"createrawtransaction">>, [Inputs, Outputs], _Bindings, State) ->
    handle_createrawtransaction(Inputs, Outputs, State);
handle_post(<<"signrawtransactionwithwallet">>, [RawTx], _Bindings, State) ->
    handle_signrawtransactionwithwallet(RawTx, State);
handle_post(<<"sendrawtransaction">>, [Hex], _Bindings, State) ->
    handle_sendrawtransaction(Hex, State).

handle_getbestblockhash(_, #state{chain = Chain} = State) ->
    {ok, Hash} = chain_top_hash(Chain),
    {{ok, Hash}, State}.

handle_getblock(Hash, Verbosity, #state{chain = Chain} = State) ->
    case chain_get_block(Chain, Hash) of
        false ->
            {{error, not_found}, State};
        {{Height, _}, _Fork, Prev, Txs} ->
            Txs1 = if Verbosity == 2 -> Txs;
                      true -> []
                   end,
            Res = getblock_result(Hash, Height, Prev, Txs1),
            {{ok, Res}, State}
    end.

handle_getblockhash(Height, #state{chain = Chain} = State) ->
    case chain_get_blockhash(Chain, Height) of
        false ->
            {{error, not_found}, State};
        Hash ->
            {{ok, Hash}, State}
    end.

getblock_result(Hash, Height, Prev, Txs) ->
    #{
    <<"hash">> => Hash,                % (string) the block hash (same as provided)
    <<"confirmations">> => 2,          % (numeric) The number of confirmations, or -1 if the block is not on the main chain
    <<"size">> => n,                   % (numeric) The block size
    <<"strippedsize">> => n,           % (numeric) The block size excluding witness data
    <<"weight">> => n,                 % (numeric) The block weight as defined in BIP 141
    <<"height">> => Height,            % (numeric) The block height or index
    <<"version">> => 23,               % (numeric) The block version
    <<"versionHex">> => "4554",        % (string) The block version formatted in hexadecimal
    <<"merkleroot">> => "5454",        % (string) The merkle root
    <<"tx">> => txs_result(Txs, Hash), % (json array) The transactions
    <<"time">> => 123,                 % (numeric) The block time expressed in UNIX epoch time
    <<"mediantime">> => 456,           % (numeric) The median block time expressed in UNIX epoch time
    <<"nonce">> => Height + 54,        % (numeric) The nonce
    <<"bits">> => "hex",               % (string) The bits
    <<"difficulty">> => 21,            % (numeric) The difficulty
    <<"chainwork">> => "hex",          % (string) Expected number of hashes required to produce the chain up to this block (in hex)
    <<"nTx">> => length(Txs),          % (numeric) The number of transactions in the block
    <<"previousblockhash">> => Prev,   % (string) The hash of the previous block
    <<"nextblockhash">> => "hex"       % (string) The hash of the next block
}.

txs_result(Txs, BlockHash) ->
    lists:map(fun(Tx) -> tx_result(Tx, BlockHash) end, Txs).

tx_result(Tx, BlockHash) ->
    #{
     <<"in_active_chain">> => true,  % (boolean) Whether specified block is in the active chain or not (only present with explicit "blockhash" argument)
     <<"hex">> => "hex",             % (string) The serialized, hex-encoded data for 'txid'
     <<"txid">> => Tx#tx.txid,       % (string) The transaction id (same as provided)
     <<"hash">> => Tx#tx.txid,       % (string) The transaction hash (differs from txid for witness transactions)
     <<"size">> => 21,               % (numeric) The serialized transaction size
     <<"vsize">> => 21,              % (numeric) The virtual transaction size (differs from size for witness transactions)
     <<"weight">> => 21,             % (numeric) The transaction's weight (between vsize*4-3 and vsize*4)
     <<"version">> => 21,            % (numeric) The version
     <<"locktime">> => 21,           % (numeric) The lock time
     <<"vin">> =>
        lists:map(
            fun(Vin) ->
                #{
                    <<"txid">> => Vin#vin.txid,  % (string) The transaction id
                    <<"vout">> => Vin#vin.vout, % (numeric) The output number
                    <<"scriptSig">> => Vin#vin.scriptSig,
                    <<"sequence">> => Vin#vin.sequence,      % (numeric) The script sequence number
                    <<"txinwitness">> => []
                }
            end, Tx#tx.vin),
     <<"vout">> =>
            lists:map(
                fun(Vout) ->
                    #{
                        <<"value">> => Vout#vout.value,        % (numeric) The value in BTC
                        <<"n">> => Vout#vout.n,                % (numeric) index
                        <<"scriptPubKey">> => Vout#vout.scriptPubKey
                    }
                end, Tx#tx.vout),
     <<"blockhash">> => BlockHash,             % (string) the block hash
     <<"confirmations">> => 1,                 % (numeric) The confirmations
     <<"blocktime">> => 21,                    % (numeric) The block time expressed in UNIX epoch time
     <<"time">> => 21                          % (numeric) Same as "blocktime"
    }.

handle_listunspent(Wallets, Minconf, Maxconf, State) ->
    AllUnspents = lists:flatten(maps:values(Wallets)),
    InRange = lists:filter(
                        fun(#utxo{confirmations = C}) ->
                            C >= Minconf andalso C =< Maxconf
                        end, AllUnspents),
    Result = listunspent_result(InRange),
    {{ok, Result}, State}.

listunspent_result(UTXOs) ->
    lists:map(fun(U) ->
     #{
         <<"txid">> => U#utxo.txid,       % (string) the transaction id
         <<"vout">> => U#utxo.vout,       % (numeric) the vout value
         <<"address">> => U#utxo.address, % (string) the bitcoin address
         <<"label">> => "str",            % (string) The associated label, or "" for the default label
         <<"scriptPubKey">> => "str",     % (string) the script key
         <<"amount">> => U#utxo.amount,   % (numeric) the transaction output amount in BTC
         <<"confirmations">> => U#utxo.confirmations,  % (numeric) The number of confirmations
         <<"redeemScript">> => "hex",     % (string) The redeemScript if scriptPubKey is P2SH
         <<"witnessScript">> => "str",    % (string) witnessScript if the scriptPubKey is P2WSH or P2SH-P2WSH
         <<"spendable">> => true,         % (boolean) Whether we have the private keys to spend this output
         <<"solvable">> => true,          % (boolean) Whether we know how to spend this output, ignoring the lack of keys
         <<"reused">> => false,           % (boolean) (only present if avoid_reuse is set) Whether this output is reused/dirty (sent to an address that was previously spent from)
         <<"desc">> => "str",             % (string) (only when solvable) A descriptor for spending this output
         <<"safe">> => true               % (boolean) Whether this output is considered safe to spend. Unconfirmed transactions
     }
    end, UTXOs).

handle_createrawtransaction([Input], Outputs, State) ->
    #{
      <<"txid">> := TxId,           % (string, required) The transaction id
      <<"vout">> := Vout            % (numeric, required) The output number
    } = Input,
    [Acct, HCAcct, #{<<"data">> := HexPayload}] = Outputs,
    Payload = aeu_hex:hex_to_bin(HexPayload),
    65 = size(Payload),
    OpRet = list_to_binary(aeu_hex:bin_to_hex(<<16#6a, 65, Payload/binary>>)),
    [{Account, Amount}] = maps:to_list(Acct),
    [{HCAccount, HCAmount}] = maps:to_list(HCAcct),
    Tx = raw_tx(TxId, Vout, Account, Amount, HCAccount, HCAmount, OpRet),
    %% Any encoding will do for our purposes, so long as we know
    %% how to decode it when it is submitted to the simulated chain.
    TxStr = jsx:encode(Tx),
    {{ok, to_hex(TxStr)}, State}.

raw_tx(TxId, Vout, Account, Amount, HCAccount, HCAmount, HexPayload) ->
    Tx = #{
            <<"txid">> => <<>>,
            <<"version">> => 2,
            <<"locktime">> => 0,
            <<"vin">> => [#{
                            <<"txid">> => TxId,
                            <<"vout">> => Vout,
                            <<"scriptSig">> =>
                                #{
                                    <<"asm">> => <<"">>,
                                    <<"hex">> => <<"">>
                                }
                            }],
            <<"vout">> => [#{
                            <<"value">> => Amount,
                            <<"n">> => 0,
                            <<"scriptPubKey">> =>
                                #{<<"address">> => Account}
                            },
                            #{
                            <<"value">> => HCAmount,
                            <<"n">> => 1,
                            <<"scriptPubKey">> =>
                                #{<<"address">> => HCAccount}
                            },
                            #{
                            <<"value">> => Amount,
                            <<"n">> => 2,
                            <<"scriptPubKey">> =>
                                #{<<"hex">> => HexPayload,
                                  <<"type">> => <<"nulldata">>
                                }
                            }]
        },
        TxHash = to_hex(aec_hash:sha256_hash(jsx:encode(Tx))),
        maps:put(<<"txid">>, TxHash, Tx).

handle_signrawtransactionwithwallet(RawTx, State) ->
    Result = #{
        <<"hex">> => RawTx,
        <<"complete">> => true
    },
    {{ok, Result}, State}.

handle_sendrawtransaction(Hex, #state{mempool = Mempool} = State) ->
    Tx = jsx:decode(from_hex(Hex), [return_maps]),
    #{<<"txid">> := TxId} = Tx,
    MempoolEntry = parse_tx(Tx),
    %% Place the Tx in the mempool
    Mempool1 = Mempool ++ [MempoolEntry],
    {{ok, #{<<"tx_hash">> => TxId }}, State#state{mempool = Mempool1}}.

parse_tx(#{<<"txid">> := TxId, <<"vin">> := Vin, <<"vout">> := Vout}) ->
    #tx{txid = TxId,
        vin = parse_vin(Vin),
        vout = parse_vout(Vout)
        }.

parse_vin(Vin) ->
    lists:map(
        fun(#{<<"txid">> := TxId, <<"vout">> := Vout, <<"scriptSig">> := ScriptSig}) ->
            #vin{txid = TxId,
                 vout = Vout,
                 scriptSig = ScriptSig
                }
        end, Vin).

parse_vout(Vout) ->
    lists:map(
        fun(#{<<"value">> := Value, <<"n">> := N, <<"scriptPubKey">> := ScriptPubKey}) ->
            #vout{value = Value,
                  n = N,
                  scriptPubKey = ScriptPubKey}
        end, Vout).

%% @doc a few randomly generated bitcoin pub/priv keypairs
%% Created by the clearly super safe and secure website https://biteaddress.org
btc_accounts() ->
    [
        {<<"1LkywyS3db5bERKzLnEKeHwcNXUNbJtEKe">>, <<"KypCwh22qucwCawGZezgQDHaf2w8rXEXJ9DA5WbMtuaTbZK5E6Xt">>},
        {<<"1KfjrVYYjSQS1FWsEgmppw5DKU9z8Wooyu">>, <<"KyJELTa2uPhXnPRCXZzoNsjYPuhnyfSf9NCX7kRPSVPrrdFzntRZ">>},
        {<<"179EqbuWwNnjpYkZu7yT9bZBQCZJSeAFgr">>, <<"KxarYvhmqALBQtRaJCiEbjrB3Wgq47faamcLvUnhPPS6vKo8sMpp">>},
        {<<"1LfX3EayogHy3xkB2VENCfqy6xxjPfkwB5">>, <<"L2JVLqswyoWTfuwaBgzooXMNknV9DWfvkXKiRyapNGKZvRRnVsvY">>},
        {<<"1FX1XPRgJiX5FpFP1SuRQyrqqNuCXUhj8P">>, <<"KxrrM9JvM4Fr9fP2gH7nFd6KfqU5j8nMptR7PxEX9Y6mPXN9vThE">>},
        {<<"1J8nf2iLzTvTsFQHWdGWkR4Qu1VZc5etGj">>, <<"L1GLav43RALrrXq9gffcUSv7NtrZRqsgqjRGRouWMeYDUcSLBm67">>}
    ].


height_to_hash(Height, Fork) ->
    BinFork = to_binary(Fork),
    BinHeight = integer_to_binary(Height),
    list_to_binary(aeu_hex:bin_to_hex(aec_hash:sha256_hash(<<BinFork/binary, BinHeight/binary>>))).

to_binary(Fork) when is_atom(Fork) ->
    list_to_binary(atom_to_list(Fork)).

-spec to_hex(binary()) -> binary().
to_hex(Payload) ->
  ToHex = fun (X) -> integer_to_binary(X,16) end,
  _HexData = << <<(ToHex(X))/binary>> || <<X:4>> <= Payload >>.

-spec from_hex(binary()) -> binary().
from_hex(HexData) ->
  ToInt = fun (H, L) -> binary_to_integer(<<H, L>>,16) end,
  _Payload = << <<(ToInt(H, L))>> || <<H:8, L:8>> <= HexData >>.

%%%===================================================================
%%% Chain DB
%%% Ordered Ets stable storing {{Height, Hash}, ForkName, [Tx]}
%%%===================================================================

chain_new() ->
    Ets = ets:new(?MODULE, [ordered_set]),
    %% Insert genesis block
    ets:insert(Ets, {{0, <<"BTCGENESIS">>}, main, <<>>, []}),
    Ets.

chain_top_hash(Chain) ->
    {Height, Hash} = ets:last(Chain),
    case ets:lookup(Chain, {Height, Hash}) of
        [{{Height, Hash}, main, _Prev, _Txs}] ->
            {ok, Hash};
        [{{Height, Hash}, _OtherFork, _Txs}] ->
            case ets:match_object(Chain, {{Height, '_'}, '_', '_', '_'}) of
                [_Single] ->
                    {ok, Hash};
                [_, {{Height, OtherHash}, _Fork, _Prev, _OtherTxs} |_] ->
                    {ok, OtherHash}
            end
    end.

chain_get_block(Chain, BlockHash) ->
    case ets:match_object(Chain, {{'_', BlockHash}, '_', '_',  '_'}, 1) of
        '$end_of_table' ->
            false;
        {[{{_Height, _}, _Fork, _Prev, _Txs} = Block], _} ->
            Block
    end.

chain_get_blockhash(Chain, Height) ->
    case ets:match_object(Chain, {{Height, '_'}, '_', '_',  '_'}, 1) of
        '$end_of_table' ->
            false;
        {[{{_Height, Hash}, _Fork, _Prev, _Txs}], _} ->
            Hash
    end.

chain_post_block(Chain, Fork, Txs) ->
    %% io:format(user, "~p - chain_post_block ~p~n", [self(), {Fork, Txs}]),
    {Height, PrevHash} = case chain_top_block(Chain, Fork) of
                            not_found ->
                                %% New fork, start at current main fork height for now
                                {{Height0, PrevHash0}, main, _, _Txs} = chain_top_block(Chain, main),
                                {Height0 + 1, PrevHash0};
                            {{Height0, PrevHash0}, _OtherFork, _, _Txs} ->
                                {Height0 + 1, PrevHash0}
                        end,
    Hash = height_to_hash(Height, Fork),
    ets:insert(Chain, {{Height, Hash}, Fork, PrevHash, Txs}),
    {ok, Hash, Height, PrevHash}.

chain_top_block(Chain, Fork) ->
    %% Traverse down from top until found entry for the named fork
    chain_top_block(Chain, ets:last(Chain), Fork).

chain_top_block(_Chain, '$end_of_table', _Fork) ->
    not_found;
chain_top_block(Chain, Key, Fork) ->
    case ets:lookup(Chain, Key) of
        [{{_Height, _Hash}, Fork, _PrevHash, _Txs} = Block] ->
            Block;
        [_] ->
            %% Wrong fork, keep searching
            chain_top_block(Chain, ets:prev(Chain, Key), Fork)
    end.

find_tx_vin(TxId, Vout, [#tx{vin = Vin} = Tx|Txs]) ->
    case lists:filter(fun(#vin{txid = TxTxid, vout = TxVout}) ->
                            TxId == TxTxid andalso Vout == TxVout
                      end, Vin) of
            [] -> find_tx_vin(TxId, Vout, Txs);
            [_] -> Tx
    end;
find_tx_vin(_TxId, _Vout, []) ->
    false.

find_tx_address(Address, Vout) ->
    case lists:filter(
        fun(#vout{scriptPubKey = [ScriptPubKey]}) when Address == ScriptPubKey -> true;
            (_) -> false
        end, Vout) of
            [] -> false;
            [Found] -> Found
    end.

%%%===================================================================
%%% Wallet store
%%% Map of PubKey -> [UTXO]
%%%===================================================================
wallet_new(Account, Balance) ->
    [#utxo{txid = <<"init", Account/binary>>,
            vout = 0,
            address = Account,
            scriptPubKey = <<>>,
            amount = Balance,
            confirmations = 0
    }].

%% Pull out transactions from the mempool that happen to be for one of our wallets
%% Real BTC nodes incorporating a wallet also have to do this
wallet_apply_txs(Wallets, Txs) ->
    %% Go through each wallet, adding/removing UTXOs in line with the
    %% incoming Txs and increasing all confirmation counts by 1.
    maps:map(
        fun(W, Utxos) ->
            Utxos1 = wallet_drop_vins(Txs, Utxos),
            Utxos2 = wallet_inc_confirmations(Utxos1),
            wallet_add_vouts(W, Txs, Utxos2)
             end, Wallets).

%% Remove spent UTXOs from the wallet. These are identified as vin entries
%% in the incoming transaction feed.
wallet_drop_vins(Txs, Utxos) ->
    lists:filter(
        fun(#utxo{txid = TxId, vout = Vout}) ->
            false == find_tx_vin(TxId, Vout, Txs)
        end, Utxos).

%% Add incoming Txs where their vout is aimed towards one of our accounts
%% as new UTXOs in our wallet.
%% In a real bitcoin node we would have to run the scriptPubKey stack machines,
%% to extract the addresses, but we cheat by just including the addresses instead
%% of a script.
wallet_add_vouts(Address, Txs, Utxos) ->
    lists:foldl(
        fun(#tx{vout = Vout, txid = Txid}, UtxoAcc) ->
            case find_tx_address(Address, Vout) of
                false -> UtxoAcc;
                #vout{value = Value} ->
                    Utxo = #utxo{txid = Txid,
                                 address = Address,
                                 amount = Value,
                                 confirmations = 0
                                },
                    UtxoAcc ++ [Utxo]
            end
        end, Utxos, Txs).

wallet_inc_confirmations(Utxos) ->
    lists:map(fun(#utxo{confirmations = C} = U) ->
                U#utxo{confirmations = C + 1}
            end, Utxos).