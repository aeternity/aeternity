%%%-----------------------------------------------------------------------------
%%% @doc
%%% Gen Server built from template.
%%% @author mans
%%% @end
%%%-----------------------------------------------------------------------------

-module(aec_penalty_agent).
-author("mans@happihacking.se").
-behaviour(gen_server).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([
    start_link/1,
    stop/0
]).

%% Callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%% Loop state
-record(loop_state, {
    contract
}).
-type loop_state() :: loop_state.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(term()) -> {ok, pid()} | {error, {already_started, pid()}} | ignore | {error, Reason::any()}.
start_link(Contract) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Contract], []).


stop() ->
    gen_server:stop(?SERVER).

%%%=============================================================================
%%% Gen Server Callbacks
%%%=============================================================================

init([Contract]) ->
    State = #loop_state{contract = Contract},
    lager:debug("started penalty agent"),
    %aec_events:subscribe(new_epoch),
    aec_events:subscribe(top_changed),
    {ok, State}.

handle_call(_Request, _From, LoopState) ->
    Reply = ok,
    {reply, Reply, LoopState}.

handle_cast(_Msg, LoopState) ->
    {noreply, LoopState}.
%% Everuytime a block is added to the chain, check if there are penalties on the stack.
%% If you are the leader, post them, always remove them from the stack.
handle_info({gproc_ps_event, top_changed, #{info := #{height := Height}}},
    #loop_state{contract = Contract} = State) ->
    PostedPens = maybe_post_penalties(Contract, Height, aec_hc_penalty_service:get_penalties()),
    pop_penalties(PostedPens),
    {noreply, State};
handle_info(_Info, LoopState) ->
    {noreply, LoopState}.

terminate(_Reason, _LoopState) ->
    ok.

code_change(_OldVsn, LoopState, _Extra) ->
    {ok, LoopState}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

maybe_post_penalties(_Contract, _Height, []) -> lager:debug("no penalties to post");
maybe_post_penalties(Contract, Height, Penalties) ->
    %% Check if we are the leader
    {ok, Leader} = aec_consensus_hc:leader_for_height(Height),
    Local = aec_preset_keys:is_key_present(Leader),
    case Local of     % If so, post penalties
        true ->
            lager:debug("posting penalties at height ~p , validator: ~p", [Height, Leader]);
        false ->
            lager:debug("not posting penalties at ~p, not local validator: ~p", [Height, Leader])
    end,
    check_penalties(Contract, Leader, Local, Penalties).


check_penalties(Contract, Leader, Local, Penalties) -> check_penalties(Contract, Leader, Local, Penalties, []).
check_penalties(_Contract, _Leader, _Local, [], PostedPens) ->
    lager:debug("no more penalties to post"),
    PostedPens;
check_penalties(Contract, Leader, Local, [{invalid_producer, BlockHeader} | Penalties ], PostedPens) -> check_penalties(Contract, Leader, Local, Penalties, PostedPens); % TODO we ignore this one for now
check_penalties(Contract, Leader, Local, [{Type, BlockHeader, BlockHeader2} = Penalty | Penalties ], PostedPens) -> % Type = dbl_microblock | dbl_keyblock
   case aec_headers:type(BlockHeader) of
        key ->
            case aec_headers:miner(BlockHeader) =/= Leader of
                true ->
                    maybe_post_penalty(Penalty, Leader, Local),
                    check_penalties(Contract, Leader, Local, Penalties, [Penalty | PostedPens]);
                false ->
                    lager:debug("can't post penalty on myself ~p, ~p:~p", [Leader, Type, BlockHeader]),
                    check_penalties(Contract, Leader, Local, Penalties, PostedPens)
            end;
        micro ->
            case aeu_sig:verify(BlockHeader, Leader) of
                ok ->
                    lager:debug("can't post penalty on myself ~p, ~p:~p", [Leader, Type, BlockHeader]),
                    check_penalties(Contract, Leader, Local, Penalties, PostedPens);
                {error, _} ->
                    maybe_post_penalty(Penalty, Leader, Local),
                    check_penalties(Contract, Leader, Local, Penalties, [Penalty | PostedPens])
            end
    end.

pop_penalties(Pens) ->
    aec_hc_penalty_service:pop_penalties(Pens).

maybe_post_penalty(_Penalty, _Leader, false) -> ok;
maybe_post_penalty({Type, BlockHeader, BlockHeader2}, Leader, true) ->
    %% Post penalty to contract
    lager:debug("posting penalty ~p:~p as ~p", [BlockHeader, Leader]).


%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.
