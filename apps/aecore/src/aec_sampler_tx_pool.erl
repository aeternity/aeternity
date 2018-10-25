-module(aec_sampler_tx_pool).

-behaviour(jobs_sampler).

-export([
          init/2
        , sample/2
        , handle_msg/3
        , calc/2
        ]).

-record(st, {levels = []}).

default_levels() ->
    [{N*1000, N} || N <- lists:seq(1, 10)].

parse_levels([{_,_}|_] = Ls) ->
    true = lists:all(fun valid_level_pair/1, Ls),
    Ls;
parse_levels(Bin) when is_binary(Bin) ->
    %% Parse: expect (binary) string of type "S1:L1 [, ...] Sn:Ln", where
    %% Sx and Lx are non-negative integers; allow for whitespace."
    Ls = [{binary_to_integer(A), binary_to_integer(B)}
          || [A, B] <-
                 [re:split(B, <<"\\h*:\\h*">>, [{return,binary}])
                  || B <- re:split(Bin, <<"\\h*,\\h*">>, [{return, binary}])]],
    true = lists:all(fun valid_level_pair/1, Ls),
    Ls.

valid_level_pair({A,B}) ->
    is_integer(A)
        andalso is_integer(B)
        andalso A >= 0
        andalso B >= 0.

init(_Name, _Opts) ->
    {ok, Levels} = aeu_env:find_config(
                     [<<"load_levels">>, <<"mempool">>, <<"size">>],
                     [ user_config
                     , schema_default
                     , {value, default_levels()} ]),
    {ok, #st{levels = parse_levels(Levels)}}.

sample(_Timestamp, #st{} = S) ->
    {aec_tx_pool:size(), S}.

handle_msg(_Msg, _Timestamp, S) ->
    {ignore, S}.

calc(History, #st{levels = Levels} = S) ->
    L = jobs_sampler:calc(value, Levels, History),
    {[{<<"mempool.size">>, L}], S}.
