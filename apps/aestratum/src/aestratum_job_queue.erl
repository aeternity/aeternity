-module(aestratum_job_queue).

-export([new/0,
         add/2,
         member/2,
         get_front/1,
         get_rear/1,
         share_target/3
        ]).

-define(QUEUE_LEN_THRESHOLD, 3).
-define(MAX_JOBS, application:get_env(aestratum, max_jobs, 20)).

new() ->
    aestratum_lqueue:new(?MAX_JOBS).

add(Job, Queue) ->
    aestratum_lqueue:in({aestratum_job:id(Job), Job}, Queue).

member(JobId, Queue) ->
    aestratum_lqueue:keymember(JobId, Queue).

%% Front - the oldest element of the queue.
get_front(Queue) ->
    aestratum_lqueue:get(Queue).

%% Rear - the newest element of the queue.
get_rear(Queue) ->
    aestratum_lqueue:get_r(Queue).

%% The job queue must have at least ?QUEUE_LEN_THRESHOLD items in order to
%% compute the new share target from the previous share targets.
share_target(DesiredSolveTime, MaxTarget, Queue) ->
    case aestratum_lqueue:len(Queue) of
        N when N >= ?QUEUE_LEN_THRESHOLD ->
            TargetsAndSolveTimes =
                [{aestratum_job:share_target(Job), aestratum_job:solve_time(Job)}
                 || {_JobId, Job} <- aestratum_lqueue:to_list(Queue)],
            {ok, aestratum_target:recalculate(TargetsAndSolveTimes, DesiredSolveTime, MaxTarget)};
        _Other ->
            {error, not_enough_jobs}
    end.

