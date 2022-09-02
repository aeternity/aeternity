%%%-----------------------------------------------------------------------------
%%% @title /Users/sean/Aeternity/aeternity-hyperchains/apps/aehttp/test/aehttp_btc_sim
%%% @doc Extremely basic web server simulating a bitcoin node
%%% supporting a small number of hard coded bitcoin operations.
%%% for testing hyperchains parent chain connector
%%%
%%% @author sean
%%% @copyright (C) 2022, Aeternity```
%%% @version 0.0.1
%%% @end
%%%-----------------------------------------------------------------------------

-module(aehttp_btc_sim).

-behaviour(gen_server).

%% External API
-export([start_link/3]).
-export([mine_on_fork/2]).

%% Handy functions to generate various test scenarios as initial state
%% for the simulator
-export([scenario/0]).

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
            middlewares => [aehttp_cors_middleware,
                            cowboy_router,
                            cowboy_handler]},
    lager:debug("BTC Sim Opts = ~p", [Opts]),
    {ok, _} = cowboy:start_clear(Name, Opts, Env).

init(Req0=#{method := <<"POST">>}, Pid) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    #{
      <<"jsonrpc">> := <<"2.0">>,
      <<"method">> := Method,
      <<"params">> := Params,
      <<"id">> := _Id
    } = jsx:decode(Data),
    Bindings = cowboy_req:bindings(Req),
    case post_req(Pid, Method, Params, Bindings) of
        {ok, Resp} ->
            Body = jsx:encode(Resp),
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
        chain,
        accounts = #{},
        mempool = []
    }).
-type state() :: state.

%% BTC transaction - the parts we care about for HC
-record(tx,
    {
        hash,
        amount,
        from,
        to,
        commitment
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

%% Web server received POST request
post_req(Pid, Method, Params, Bindings) ->
    gen_server:call(Pid, {post_req, Method, Params, Bindings}).

%% @doc mine a block on a simulated BTC node.
%% Afterwards this block will hold the current contents of the mempool
mine_on_fork(Name, ForkName) when is_atom(Name), is_atom(ForkName) ->
    gen_server:call(Name, {mine_on_fork, ForkName}).

%% @doc Generate an initial state for a BTC chain with a genesis block
%% and set of accounts.
%% generations => [{Hash, Height, ForkName, Transactions}]
scenario() ->
    Accounts = lists:map(fun({Pub, _Priv}) -> {Pub, 10000} end, btc_accounts()),
    #{accounts => maps:from_list(Accounts)}.

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================
-spec init(state()) -> {ok, state()}.
init([Name, Port, #{accounts := Accounts}]) ->
    {ok, _} = start_cowboy(Name, Port, self()),
    Chain = chain_new(),
    chain_post_block(Chain, main, []),
    {ok, #state{chain = Chain, accounts = Accounts}}.

-spec handle_call(any(), pid(), state()) -> {ok, any(), state()}.
handle_call({post_req, Method, Params, Bindings}, _From, State) ->
    {Reply, State1} = handle_post(Method, Params, Bindings, State),
    {ok, Reply, State1};
handle_call({mine_on_fork, Fork}, _From, State) ->
    #state{chain = Chain, mempool = Mempool} = State,
    chain_post_block(Chain, Fork, Mempool),
    {ok, ok, State#state{mempool = []}};
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
handle_post(<<"getbestblockhash">>, Params, _Bindings, State) ->
    handle_getbestblockhash(Params, State);
handle_post(<<"getblock">>, [BlockHash, 2 = _Verbosity], _Bindings, State) ->
    handle_getblock(BlockHash, State);
handle_post(<<"listunspent">>, [Minconf, Maxconf, Addresses, Unsafe, Query], Bindings, State) ->
    Wallet = proplists:get_value(wallet, Bindings, no_wallet),
    handle_listunspent(Wallet, Minconf, Maxconf, Addresses, Unsafe, Query, State);
handle_post(<<"createrawtransaction">>, [Inputs, Outputs], _Bindings, State) ->
    handle_createrawtransaction(Inputs, Outputs, State);
handle_post(<<"signrawtransactionwithkey">>, [RawTx, [PrivKey]], Bindings, State) ->
    Wallet = proplists:get_value(wallet, Bindings, no_wallet),
    handle_signrawtransactionwithkey([RawTx, [PrivKey]], Wallet, State);
handle_post(<<"sendrawtransaction">>, [Hex], _Bindings, State) ->
    handle_sendrawtransaction(Hex, State).

handle_getbestblockhash(_, #state{chain = Chain} = State) ->
    {ok, Hash} = chain_top_hash(Chain),
    {{ok, #{<<"result">> => Hash}}, State}.

handle_getblock(Hash, #state{chain = Chain} = State) ->
    case chain_get_block(Chain, Hash) of
        false ->
            {{error, not_found}, State};
        {{Height, _}, _Fork, Txs} ->
            Res = getblock_result(Hash, Height, Txs),
            {{ok, Res}, State}
    end.

getblock_result(Hash, Height, Txs) ->
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
    <<"previousblockhash">> => "hex",  % (string) The hash of the previous block
    <<"nextblockhash">> => "hex"       % (string) The hash of the next block
}.

txs_result(Txs, BlockHash) ->
    lists:map(fun(Tx) -> tx_result(Tx, BlockHash) end, Txs).

tx_result(Tx, BlockHash) ->
    #{
     <<"in_active_chain">> => true,  % (boolean) Whether specified block is in the active chain or not (only present with explicit "blockhash" argument)
     <<"hex">> => "hex",             % (string) The serialized, hex-encoded data for 'txid'
     <<"txid">> => Tx#tx.hash,       % (string) The transaction id (same as provided)
     <<"hash">> => Tx#tx.hash,       % (string) The transaction hash (differs from txid for witness transactions)
     <<"size">> => 21,               % (numeric) The serialized transaction size
     <<"vsize">> => 21,              % (numeric) The virtual transaction size (differs from size for witness transactions)
     <<"weight">> => 21,             % (numeric) The transaction's weight (between vsize*4-3 and vsize*4)
     <<"version">> => 21,            % (numeric) The version
     <<"locktime">> => 21,           % (numeric) The lock time
     <<"vin">> => [
          #{
          <<"txid">> => Tx#tx.hash,  % (string) The transaction id
          <<"vout">> => 0,           % (numeric) The output number
          <<"scriptSig">> => #{      % (json object) The script
              <<"asm">> => "str",    % (string) asm
              <<"hex">> => "hex"     % (string) hex
          },
          <<"sequence">> => 23,      % (numeric) The script sequence number
          <<"txinwitness">> => []
         }
     ],
     <<"vout">> => [
         #{
           <<"value">> => Tx#tx.amount,        % (numeric) The value in BTC
           <<"n">> => 0,                       % (numeric) index
           <<"scriptPubKey">> => #{
               <<"asm">> => "str",             % (string) the asm
               <<"hex">> => "str",             % (string) the hex
               <<"reqSigs">> => 1,             % (numeric) The required sigs
               <<"type">> => "pubkeyhash",     % (string) The type, eg 'pubkeyhash'
               <<"addresses">> => [Tx#tx.to]   % [(string)] bitcoin addresses
            }
         }
     ],
     <<"blockhash">> => BlockHash,             % (string) the block hash
     <<"confirmations">> => 1,                 % (numeric) The confirmations
     <<"blocktime">> => 21,                    % (numeric) The block time expressed in UNIX epoch time
     <<"time">> => 21                          % (numeric) Same as "blocktime"
    }.

handle_listunspent(Wallet, _Minconf, _Maxconf, [_Address], _Unsafe,
                    #{ <<"minimumAmount">> := MinAmount }, State) ->
    case lookup_account(Wallet, State) of
        false ->
            {{error, account_not_found}, State};
        {Wallet, Balance} when Balance >= MinAmount ->
            Result = listunspent_result(Wallet, Balance),
            {{ok, #{<<"result">> => Result}}, State};
        _ ->
            {{error, insufficient_funds}, State}
    end.

listunspent_result(Wallet, Balance) ->
    [
     #{
         <<"txid">> => "hex",             % (string) the transaction id
         <<"vout">> => n,                 % (numeric) the vout value
         <<"address">> => Wallet,         % (string) the bitcoin address
         <<"label">> => "str",            % (string) The associated label, or "" for the default label
         <<"scriptPubKey">> => "str",     % (string) the script key
         <<"amount">> => Balance,         % (numeric) the transaction output amount in BTC
         <<"confirmations">> => 100,      % (numeric) The number of confirmations
         <<"redeemScript">> => "hex",     % (string) The redeemScript if scriptPubKey is P2SH
         <<"witnessScript">> => "str",    % (string) witnessScript if the scriptPubKey is P2WSH or P2SH-P2WSH
         <<"spendable">> => true,         % (boolean) Whether we have the private keys to spend this output
         <<"solvable">> => true,          % (boolean) Whether we know how to spend this output, ignoring the lack of keys
         <<"reused">> => false,           % (boolean) (only present if avoid_reuse is set) Whether this output is reused/dirty (sent to an address that was previously spent from)
         <<"desc">> => "str",             % (string) (only when solvable) A descriptor for spending this output
         <<"safe">> => true               % (boolean) Whether this output is considered safe to spend. Unconfirmed transactions
     }
    ].

handle_createrawtransaction([Input], Outputs, State) ->
    #{
      <<"txid">> := TxId,           % (string, required) The transaction id
      <<"vout">> := Vout            % (numeric, required) The output number
    } = Input,
    [Acct, #{<<"data">> := _HexPayload}] = Outputs,
    [{Account, Amount}] = maps:to_list(Acct),
    Tx = raw_tx(TxId, Vout, Account, Amount),
    TxStr = jsx:encode(Tx),
    {{ok, #{<<"result">> => to_hex(TxStr)}}, State}.

raw_tx(TxId, Vout, _Account, Amount) ->
    #{
        <<"txid">> => <<"TxHash">>,
        <<"version">> => 2,
        <<"locktime">> => 21,
        <<"vin">> => [#{
                        <<"txid">> => TxId,
                        <<"vout">> => Vout,
                        <<"scriptSig">> =>
                            #{
                                <<"asm">> => <<"ASM">>,
                                <<"hex">> => <<"hex">>
                            }
                        }],
        <<"vout">> => [#{
                        <<"value">> => Amount,
                        <<"n">> => 0,
                        <<"scriptPubKey">> => #{}
                        }],
        <<"blockhash">> =>  <<"blockhash">>
    }.

handle_signrawtransactionwithkey([RawTx, [_PrivKey]], _Wallet, State) ->
    Result = #{
        <<"hex">> => RawTx,
        <<"complete">> => true
    },
    {{ok, #{<<"result">> => Result}}, State}.

handle_sendrawtransaction(_Hex, State) ->
%%     Tx = jsx:decode(from_hex(Hex)),
    %% Place the Tx in the mempool
    %% We need to record commitments against each
    {{ok, #{<<"result">> =>  <<"">>}}, State}.

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
    aeu_hex:bin_to_hex(aec_hash:sha256_hash(<<BinFork/binary, BinHeight/binary>>)).

to_binary(Fork) when is_atom(Fork) ->
    list_to_binary(atom_to_list(Fork)).

lookup_account(Address, #state{accounts = Accounts}) ->
    case maps:find(Address, Accounts) of
        {ok, Balance} ->
            {Address, Balance};
        _ ->
            false
    end.

-spec to_hex(binary()) -> binary().
to_hex(Payload) ->
  ToHex = fun (X) -> integer_to_binary(X,16) end,
  _HexData = << <<(ToHex(X))/binary>> || <<X:4>> <= Payload >>.

%% -spec from_hex(binary()) -> binary().
%% from_hex(HexData) ->
%%   ToInt = fun (H, L) -> binary_to_integer(<<H, L>>,16) end,
%%   _Payload = << <<(ToInt(H, L))>> || <<H:8, L:8>> <= HexData >>.

%%%===================================================================
%%% Chain DB
%%% Ordered Ets stable storing {{Height, Hash}, ForkName, [Tx]}
%%%===================================================================

chain_new() ->
    ets:new([ordered_set]).

chain_top_hash(Chain) ->
    case ets:last(Chain) of
        {{_Height, Hash}, main, _Txs} ->
            {ok, Hash};
        {{Height, Hash}, _OtherFork, _Txs} ->
            case ets:match_object(Chain, {{Height, '_'}, '_', '_'}) of
                [_Single] ->
                    {ok, Hash};
                [_, {{Height, OtherHash}, _Fork, _OtherTxs} |_] ->
                    {ok, OtherHash}
            end
    end.

chain_get_block(Chain, BlockHash) ->
    case ets:match_object(Chain, {{'_', BlockHash}, '_', '_'}, 1) of
        [] ->
            {error, not_found};
        [{{_Height, _}, _Fork, _Txs} = Block] ->
            {ok, Block}
    end.

chain_post_block(Chain, Fork, Txs) ->
    Height = case chain_top_block(Chain, Fork) of
        not_found ->
            %% New fork, start at current main fork height for now
             {{Height0, _}, main, _Txs} = chain_top_block(Chain, main),
             Height0 + 1;
        {{Height0, _}, _OtherFork, Txs} ->
            Height0 + 1
    end,
    Hash = height_to_hash(Height, Fork),
    ets:insert(Chain, {{{Height, Hash}, Fork, Txs}}).

chain_top_block(Chain, Fork) ->
    case ets:match_object(Chain, {{'_', '_'}, Fork, '_'}, 1) of
        [{{_Height, _Hash}, _OtherFork, _Txs} = Block] ->
            Block;
        [] ->
            not_found
    end.
