-module(aec_headers).

%% API
-export([prev_hash/1,
         time_in_secs/1,
         get_by_height/1,
         get_by_hash/1]).

-include("common.hrl").
-include("blocks.hrl").

prev_hash(Header) ->
    Header#header.prev_hash.

time_in_secs(Header) ->
    Time = Header#header.time,
    aeu_time:msecs_to_secs(Time).

get_by_height(_Height) ->
    %% TODO: Return block header by height
    %% This may go to aec_blocks
    {ok, #header{}}.

get_by_hash(_Hash) ->
    %% TODO: Return block header by hash
    %% This may go to aec_blocks
    {ok, #header{}}.
