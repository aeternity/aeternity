-module(aestratum_submission).

%% TODO: type spec
%% TODO: eunit

-export([new/3,
         worker/1,
         miner_nonce/1,
         pow/1,
         timestamp/1,
         set_is_valid/2
        ]).

-record(submission, {
          worker,
          miner_nonce,
          pow,
          is_valid,
          timestamp
         }).

new(Worker, MinerNonce, Pow) ->
    #submission{worker = Worker, miner_nonce = MinerNonce, pow = Pow,
                timestamp = aestratum_utils:timestamp()}.

worker(#submission{worker = Worker}) ->
    Worker.

miner_nonce(#submission{miner_nonce = MinerNonce}) ->
    MinerNonce.

pow(#submission{pow = Pow}) ->
    Pow.

timestamp(#submission{timestamp = Timestamp}) ->
    Timestamp.

set_is_valid(IsValid, Submission) when is_boolean(IsValid) ->
    Submission#submission{is_valid = IsValid}.

