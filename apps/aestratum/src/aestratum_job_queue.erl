-module(aestratum_job_queue).

-export([new/0,
         add/2,
         member/2,
         find/2,
         get_front/1,
         get_rear/1,
         replace/3,
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

find(JobId, Queue) ->
    case aestratum_lqueue:keyfind(JobId, Queue) of
        {JobId, Job} -> {ok, Job};
        false        -> {error, not_found}
    end.

%% Front - the oldest element of the queue.
get_front(Queue) ->
    case aestratum_lqueue:get(Queue) of
        {ok, {_JobId, Job}}  -> {ok, Job};
        {error, empty} = Err -> Err
    end.

%% Rear - the newest element of the queue.
get_rear(Queue) ->
    case aestratum_lqueue:get_r(Queue) of
        {ok, {_JobId, Job}}  -> {ok, Job};
        {error, empty} = Err -> Err
    end.

replace(JobId, Job, Queue) ->
    aestratum_lqueue:keyreplace(JobId, Job, Queue).

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

