-module(aestratum_job).

%% TODO: type spec
%% TODO: eunit

-export([new/7,
         id/1,
         block_hash/1,
         block_target/1,
         block_version/1,
         share_target/1,
         desired_solve_time/1,
         max_solve_time/1,
         timestamp/1,
         submission/1,
         is_submitted/1,
         solve_time/1,
         make_id/3
        ]).

-export([set_submission/2,
         validate_submission/1
        ]).

-export_type([job/0]).

-record(job, {
          id,
          block_hash,
          block_target,
          block_version,
          share_target,
          desired_solve_time,
          max_solve_time,
          timestamp,
          submission
         }).

-opaque job() :: #job{}.

%% API.

new(Id, BlockHash, BlockTarget, BlockVersion, ShareTarget, DesiredSolveTime, MaxSolveTime) ->
    #job{id                 = Id,
         block_hash         = BlockHash,
         block_target       = BlockTarget,
         block_version      = BlockVersion,
         share_target       = ShareTarget,
         desired_solve_time = DesiredSolveTime,
         max_solve_time     = MaxSolveTime,
         timestamp          = aestratum_utils:timestamp()}.

id(#job{id = Id}) ->
    Id.

block_hash(#job{block_hash = BlockHash}) ->
    BlockHash.

block_target(#job{block_target = BlockTarget}) ->
    BlockTarget.

block_version(#job{block_version = BlockVersion}) ->
    BlockVersion.

share_target(#job{share_target = ShareTarget}) ->
    ShareTarget.

desired_solve_time(#job{desired_solve_time = DesiredSolveTime}) ->
    DesiredSolveTime.

max_solve_time(#job{max_solve_time = MaxSolveTime}) ->
    MaxSolveTime.

timestamp(#job{timestamp = StartTime}) ->
    StartTime.

submission(#job{submission = Submission}) ->
    Submission.

is_submitted(#job{submission = Submission}) when Submission =/= undefined ->
    true;
is_submitted(#job{submission = undefined}) ->
    false.

solve_time(#job{timestamp = Timestamp, submission = Submission}) when
      Submission =/= undefined ->
    aestratum_submission:timestamp(Submission) - Timestamp;
solve_time(#job{max_solve_time = MaxSolveTime, submission = undefined}) ->
    MaxSolveTime.

make_id(BlockHash, BlockTarget, BlockVersion) ->
    make_id1(BlockHash, BlockTarget, BlockVersion).

set_submission(Submission, Job) ->
    Job#job{submission = Submission}.

validate_submission(_Job) ->
    ok.

%% Internal functions.

make_id1(BlockHash, BlockTarget, BlockVersion) ->
    BlockTarget1 = aestratum_target:to_hex(BlockTarget),
    <<Id:8/binary, _Rest/binary>> =
        crypto:hash(sha256, [BlockHash, BlockTarget1, BlockVersion]),
    to_hex(Id).

to_hex(Bin) ->
    <<begin
            if N < 10 -> <<($0 + N)>>;
               true   -> <<(87 + N)>>   %% 87 = ($a - 10)
            end
        end || <<N:4>> <= Bin
    >>.
