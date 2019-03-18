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

new(Id, BlockHash, BlockVersion, BlockTarget, ShareTarget, DesiredSolveTime, MaxSolveTime) ->
    #job{id                 = Id,
         block_hash         = BlockHash,
         block_version      = BlockVersion,
         block_target       = BlockTarget,
         share_target       = ShareTarget,
         desired_solve_time = DesiredSolveTime,
         max_solve_time     = MaxSolveTime,
         created            = aestratum_utils:timestamp()}.

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
    %% Single job can have multiple shares (a client can have multiple
    %% miner instances running and more of them can find a valid solution for
    %% the same job).
    Shares.

is_submitted(#job{shares = Shares}) when Shares =/= undefined ->
    true;
is_submitted(#job{shares = undefined}) ->
    false.

solve_time(#job{created = Created, shares = [Share | _Shares]}) ->
    %% The shortest time is considered to be the solve time. Since the shares
    %% are sorted based on the created time, the first share is the one with
    %% the smallest solve time.
    aestratum_share:created(Share) - Created;
solve_time(#job{max_solve_time = MaxSolveTime, shares = undefined}) ->
    MaxSolveTime.

make_id(BlockHash, BlockVersion, BlockTarget) ->
    make_id1(BlockHash, BlockVersion, BlockTarget).

add_share(Share, #job{shares = undefined} = Job) ->
    Job#job{shares = [Share]};
add_share(Share, #job{shares = Shares} = Job) ->
    %% When a new share is added, the list of shares is sorted so the
    %% one with the shortest time is at the front of the list. Check the
    %% solve_time/1 function for more explanation.
    Shares1 =
        lists:sort(
          fun(S1, S2) ->
                  aestratum_share:created(S1) =< aestratum_share:created(S2)
          end, [Share | Shares]),
    Job#job{shares = Shares1}.

is_share_present(MinerNonce, Pow, #job{shares = Shares}) ->
    lists:any(
      fun(S) ->
              aestratum_share:is_duplicate(MinerNonce, Pow, S)
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

