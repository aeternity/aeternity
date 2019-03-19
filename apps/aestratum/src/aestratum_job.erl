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
         created/1,
         shares/1,
         is_submitted/1,
         solve_time/1,
         make_id/3
        ]).

-export([add_share/2,
         is_share_present/3
        ]).

-export_type([job/0]).

-record(job, {
          id,
          block_hash,
          block_version,
          block_target,
          share_target,
          desired_solve_time,
          max_solve_time,
          shares,
          created
        }).

-opaque job() :: #job{}.

%% API.

new(Id, BlockHash, BlockVersion, BlockTarget, ShareTarget,
    DesiredSolveTime, MaxSolveTime) ->
    #job{id = Id,
         block_hash = BlockHash,
         block_version = BlockVersion,
         block_target = BlockTarget,
         share_target = ShareTarget,
         desired_solve_time = DesiredSolveTime,
         max_solve_time = MaxSolveTime,
         shares = [],
         created = aestratum_utils:timestamp()}.

id(#job{id = Id}) ->
    Id.

block_hash(#job{block_hash = BlockHash}) ->
    BlockHash.

block_version(#job{block_version = BlockVersion}) ->
    BlockVersion.

block_target(#job{block_target = BlockTarget}) ->
    BlockTarget.

share_target(#job{share_target = ShareTarget}) ->
    ShareTarget.

desired_solve_time(#job{desired_solve_time = DesiredSolveTime}) ->
    DesiredSolveTime.

max_solve_time(#job{max_solve_time = MaxSolveTime}) ->
    MaxSolveTime.

created(#job{created = Created}) ->
    Created.

shares(#job{shares = Shares}) ->
    Shares.

is_submitted(#job{shares = Shares}) when Shares =/= [] ->
    true;
is_submitted(#job{shares = []}) ->
    false.

solve_time(#job{created = Created, shares = Shares}) when Shares =/= [] ->
    %% The shortest time is considered to be the solve time. The first
    %% submitted share, which has the shortest solve time, is the last in
    %% the list.
    Share = lists:last(Shares),
    aestratum_share:created(Share) - Created;
solve_time(#job{max_solve_time = MaxSolveTime, shares = []}) ->
    MaxSolveTime.

make_id(BlockHash, BlockVersion, BlockTarget) ->
    make_id1(BlockHash, BlockVersion, BlockTarget).

add_share(Share, #job{shares = Shares} = Job) ->
    %% TODO: add limit on how many shares can be submitted per job.
    Job#job{shares = [Share | Shares]}.

is_share_present(MinerNonce, Pow, #job{shares = Shares}) ->
    lists:any(
      fun(Share) ->
              aestratum_share:is_duplicate(MinerNonce, Pow, Share)
      end, Shares).

%% Internal functions.

make_id1(BlockHash, BlockVersion, BlockTarget) ->
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

