%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% EUnit tests for aec_coinbase
%%% @end
%%%-------------------------------------------------------------------

-module(aec_coinbase_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BLOCKS_PER_YEAR, 175200).  %% 365 * 24 * 20
-define(SLOW_START_BLOCKS, 960). %% 2 * 24 * 20 (2 days)

coinbase_test_() ->
     [ {"Coinbase follows predictable shape", {timeout, 120, fun test_coinbase_shape/0}}
     ].

%%%=============================================================================
%%% Test the shape of the coinbase curve

test_coinbase_shape() ->
    test_coinbase_shape(0, 0).

test_coinbase_shape(Height, Last) when Height < ?SLOW_START_BLOCKS ->
    case aec_coinbase:coinbase_at_height(Height) of
        CB when CB =:= Last -> test_coinbase_shape(Height + 1, CB);
        CB when CB > Last ->
            %% Test that proof of fraud can be computed from all coinbases
            ?assert(is_integer(aec_governance:fraud_report_reward(Height))),
            test_coinbase_shape(Height + 1, CB);
        Other -> error({illegal_coinbase, Height, Other, Last})
    end;
test_coinbase_shape(Height, Last) ->
    case aec_coinbase:coinbase_at_height(Height) of
        0 -> ?assert(Height > ?BLOCKS_PER_YEAR * 100); %% More than 100 years
        CB when CB =:= Last -> test_coinbase_shape(Height + 1, CB);
        CB when CB < Last ->
            %%% Test that proof of fraud can be computed from all coinbases
            ?assert(is_integer(aec_governance:fraud_report_reward(Height))),
            test_coinbase_shape(Height + 1, CB);
        Other -> error({illegal_coinbase, Height, Other, Last})
    end.

