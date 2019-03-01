-module(aestratum_chain).

-behaviour(gen_server).

%% TODO: eunit
%% TODO: add new event to the node
%% TODO: type specs

%% API.
-export([start_link/0,
         subscribe/0,
         payout_rewards/3,
         get_reward_key_header/1,
         hash_header/1,
         header_info/1]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
          header_hash,
          target
         }).

%% API.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe() ->
    aestratum_pubsub:subscribe(new_block).

unsubsribe() ->
    aestratum_pubsub:unsubscribe(new_block).

payout_rewards(Height, PoolRewards, MinersRewards) ->
    gen_server:cast(?MODULE, {payout_rewards, Height, PoolRewards, MinersRewards}).

%% Callbacks.

init([]) ->
    %% TODO: create an event in the node
    aec_events:subscribe(new_pending_key_block),
    {ok, undefined}.

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

%% micro block candidate
handle_info({gproc_ps_event, new_pending_key_block, #{info := Info}}, State) ->
    {noreply, handle_new_pending_key_block(Info, State)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_new_pending_key_block({HdrHash, Target}, State) ->
    Info = #{header_hash => HdrHash, target => Target},
    aestratum_pubsub:publish(new_block, Info),
    State#state{header_hash = HdrHash, target = Target}.


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
