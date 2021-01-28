-module(aecore_suite_ttb).
-behavior(tr_ttb).


-export([ on_nodes/2      % (Nodes, File)
        , on_nodes/3      % -> tr_ttb:on_nodes(Nodes, File, Module)
        , stop/0          % -> tr_ttb:stop()
        , stop_nofetch/0  % -> tr_ttb:stop_nofetch/0
        , format/2        % -> tr_ttb:format(Dir, Out)
        , format/3 ]).    % -> tr_ttb:format(Dir, Out, Opts)

-export([ with_trace/3
        , with_trace/4 ]).

-export([ patterns/0
        , flags/0 ]).


-export([event/1]).


-include_lib("trace_runner/include/trace_runner.hrl").


%% Traced function. Can be used to insert markers in the trace log
event(E) ->
    event(?LINE, E, none).

event(_, _, _) ->
    ok.


on_nodes(Ns, File) ->
    on_nodes(Ns, File, ?MODULE).

on_nodes(Ns, File, Module) ->
    tr_ttb:on_nodes(Ns, File, Module).

patterns() ->
    [ {aec_sync, '_', '_', []}
    , {aec_resilience, '_', '_', []}
    | [ {aec_chain_state, F, A, []} || {F, A} <- aec_chain_state:module_info(exports) ]]
    ++ [{aec_chain, F, A, []} || {F, A} <- aec_chain:module_info(exports) ].

flags() ->
    {all, call}.

stop()           -> tr_ttb:stop().
stop_nofetch()   -> tr_ttb:stop_nofetch().
format(Dir, Out) -> tr_ttb:format(Dir, Out).
format(Dir, Out, Opts) -> tr_ttb:format(Dir, Out, Opts).

with_trace(F, Config, File) ->
    with_trace(F, Config, File, on_error).

with_trace(F, Config, File, When) ->
    ct:log("with_trace ...", []),
    TTBRes = on_nodes([node()|get_nodes(Config)], File),
    ct:log("Trace set up: ~p", [TTBRes]),
    try F(Config)
    catch E:R:Stack ->
        case E of
            error ->
                ct:pal("Error ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                erlang:error(R);
            exit ->
                ct:pal("Exit ~p~nStack = ~p", [R, Stack]),
                ttb_stop(),
                exit(R);
            throw ->
                ct:pal("Caught throw:~p", [R]),
                throw(R)
        end
    end,
    case When of
        on_error ->
            ct:log("Discarding trace", []),
            aesc_ttb:stop_nofetch();
        always ->
            ttb_stop()
    end,
    ok.

ttb_stop() ->
    Dir = stop(),
    Out = filename:join(filename:dirname(Dir),
                        filename:basename(Dir) ++ ".txt"),
    case format(Dir, Out, #{limit => 30000}) of
        {error, Reason} ->
            ct:pal("TTB formatting error: ~p", [Reason]);
        _ ->
            ok
    end,
    ct:pal("Formatted trace log in ~s~n", [Out]).

get_nodes(Config) ->
    [N || {_, N} <- proplists:get_value(nodes, Config, [])].
