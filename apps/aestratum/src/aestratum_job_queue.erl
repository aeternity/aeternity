-module(aestratum_job_queue).

-export([new/0,
         add/2,
         member/2,
         find/2,
         get_front/1,
         get_rear/1,
         replace/3,
         share_target/3,
         to_list/1
        ]).

-export_type([job_queue/0]).

-define(QUEUE_LEN_THRESHOLD, 3).
-define(MAX_JOBS, application:get_env(aestratum, max_jobs, 20)).

-type job_queue()          :: aestratum_lqueue:lqueue().

-type job()                :: aestratum_job:job().

-type job_id()             :: aestratum_job:id().

-type desired_solve_time() :: aestratum_job:desired_solve_time().

-type share_target()       :: aestratum_target:int_target().

-type max_share_target()   :: share_target().

-spec new() -> job_queue().
new() ->
    aestratum_lqueue:new(?MAX_JOBS).

-spec add(job(), job_queue()) -> job_queue().
add(Job, Queue) ->
    aestratum_lqueue:in({aestratum_job:id(Job), Job}, Queue).

-spec member(job_id(), job_queue()) -> boolean().
member(JobId, Queue) ->
    aestratum_lqueue:keymember(JobId, Queue).

-spec find(job_id(), job_queue()) -> {ok, job()} | {error, term()}.
find(JobId, Queue) ->
    case aestratum_lqueue:keyfind(JobId, Queue) of
        {JobId, Job} -> {ok, Job};
        false        -> {error, not_found}
    end.

%% Front - the oldest element of the queue.
-spec get_front(job_queue()) -> {ok, job()} | {error, term()}.
get_front(Queue) ->
    case aestratum_lqueue:get(Queue) of
        {ok, {_JobId, Job}}  -> {ok, Job};
        {error, empty} = Err -> Err
    end.

%% Rear - the newest element of the queue.
-spec get_rear(job_queue()) -> {ok, job()} | {error, term()}.
get_rear(Queue) ->
    case aestratum_lqueue:get_r(Queue) of
        {ok, {_JobId, Job}}  -> {ok, Job};
        {error, empty} = Err -> Err
    end.

-spec replace(job_id(), job(), job_queue()) -> job_queue().
replace(JobId, Job, Queue) ->
    aestratum_lqueue:keyreplace(JobId, Job, Queue).

%% The job queue must have at least ?QUEUE_LEN_THRESHOLD items in order to
%% compute the new share target from the previous share targets.
-spec share_target(desired_solve_time(), max_share_target(), job_queue()) ->
    {ok, share_target()} | {error, term()}.
share_target(DesiredSolveTime, MaxShareTarget, Queue) ->
    case aestratum_lqueue:len(Queue) of
        N when N >= ?QUEUE_LEN_THRESHOLD ->
            TargetsAndSolveTimes =
                [{aestratum_job:share_target(Job), aestratum_job:solve_time(Job)}
                 || {_JobId, Job} <- aestratum_lqueue:to_list(Queue)],
            {ok, aestratum_target:recalculate(TargetsAndSolveTimes, DesiredSolveTime, MaxShareTarget)};
        _Other ->
            {error, not_enough_jobs}
    end.

-spec to_list(job_queue()) -> [job()].
to_list(Queue) ->
    [Job || {_JobId, Job} <- aestratum_lqueue:to_list(Queue)].
