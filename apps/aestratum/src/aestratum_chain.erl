%%%-------------------------------------------------------------------
%%% @copyright (C) 2019,
%%% @doc
%%%
%%% Interface to AE node
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aestratum_chain).

-behaviour(gen_server).

%% TODO: eunit
%% TODO: add new event to the node
%% TODO: type specs

%% API.
-export([start_link/0,
         payout_rewards/3,
         get_reward_key_header/1,
         hash_header/1,
         header_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(CHAIN_TOP_CHECK_INTERVAL, 1000). % 1 second

%%%===================================================================
%%% STATE
%%%===================================================================

-record(state,
        {last_keyblock}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

payout_rewards(Height, PoolRewards, MinersRewards) ->
    gen_server:cast(?MODULE, {payout_rewards, Height, PoolRewards, MinersRewards}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, _} = timer:send_interval(?CHAIN_TOP_CHECK_INTERVAL, chain_top_check),
    {ok, #state{last_keyblock = aec_chain:top_key_block_hash()}}.


handle_call(_Req, _From, State) ->
    {reply, ok, State}.


handle_cast({payout_rewards, Height, _PoolRewards, _MinersRewards}, State) ->
    %% TODO: actually pay out the rewards
    spawn_link(fun () ->
                       timer:sleep(10000),
                       aestratum_reward:confirm_payout(Height)
               end),
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(chain_top_check, #state{} = State) ->
    NewKB = aec_chain:top_key_block_hash(),
    NewKB == State#state.last_keyblock orelse aestratum_reward:keyblock(),
    {noreply, State#state{last_keyblock = NewKB}};

handle_info(_Req, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_reward_key_header(RoundsDelay) ->
    case aec_chain:top_key_block() of
        {ok, {key_block, TopKeyHeader}} ->
            RewardHeight = aec_headers:height(TopKeyHeader) - RoundsDelay,
            aec_chain:get_key_header_by_height(RewardHeight);
        error ->
            {error, cant_get_top_key_block}
    end.


hash_header(KeyHeader) ->
    aec_headers:hash_header(KeyHeader).


header_info({KeyHeader, Hash}) ->
    {value, Fees} = aec_db:find_block_fees(Hash),
    Height = aec_headers:height(KeyHeader),
    Target = aec_headers:target(KeyHeader),
    Tokens = aec_coinbase:coinbase_at_height(Height) + Fees,
    {ok, Height, Target, Tokens}.
