-module(aestratum_job).

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
         error_shares/1,
         is_submitted/1,
         solve_time/1,
         make_id/3,
         status/1
        ]).

-export([add_share/2,
         add_error_share/2,
         is_share_present/3
        ]).

-export_type([job/0,
              id/0,
              desired_solve_time/0,
              max_solve_time/0
             ]).

-type id()                   :: binary().

-type block_hash()           :: aestratum_miner:block_hash().

-type block_version()        :: aestratum_miner:block_version().

-type block_target()         :: aestratum_target:int_target().

-type share_target()         :: aestratum_target:int_target().

-type solve_time()           :: aestratum_target:solve_time().

-type desired_solve_time()   :: solve_time().

-type max_solve_time()       :: solve_time().

-type miner_nonce()          :: aestratum_nonce:part_nonce().

-type pow()                  :: aestratum_miner:pow().

-type share()                :: aestratum_share:share().

-type error_share()          :: share().

-type created()              :: aestratum_utils:timestamp().

-record(job, {
          id                 :: id(),
          block_hash         :: block_hash(),
          block_version      :: block_version(),
          block_target       :: block_target(),
          share_target       :: share_target(),
          desired_solve_time :: desired_solve_time(),
          max_solve_time     :: max_solve_time(),
          shares             :: [share()],
          error_shares       :: [error_share()],
          created            :: created()
        }).

-opaque job()                :: #job{}.

%% API.

-spec new(id(), block_hash(), block_version(), block_target(), share_target(),
          desired_solve_time(), max_solve_time()) -> job().
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
         error_shares = [],
         created = aestratum_utils:timestamp()}.

-spec id(job()) -> id().
id(#job{id = Id}) ->
    Id.

-spec block_hash(job()) -> block_hash().
block_hash(#job{block_hash = BlockHash}) ->
    BlockHash.

-spec block_version(job()) -> block_version().
block_version(#job{block_version = BlockVersion}) ->
    BlockVersion.

-spec block_target(job()) -> block_target().
block_target(#job{block_target = BlockTarget}) ->
    BlockTarget.

-spec share_target(job()) -> share_target().
share_target(#job{share_target = ShareTarget}) ->
    ShareTarget.

-spec desired_solve_time(job()) -> desired_solve_time().
desired_solve_time(#job{desired_solve_time = DesiredSolveTime}) ->
    DesiredSolveTime.

-spec max_solve_time(job()) -> max_solve_time().
max_solve_time(#job{max_solve_time = MaxSolveTime}) ->
    MaxSolveTime.

-spec created(job()) -> created().
created(#job{created = Created}) ->
    Created.

-spec shares(job()) -> [share()].
shares(#job{shares = Shares}) ->
    Shares.

-spec error_shares(job()) -> [error_share()].
error_shares(#job{error_shares = ErrorShares}) ->
    ErrorShares.

-spec is_submitted(job()) -> boolean().
is_submitted(#job{shares = Shares}) when Shares =/= [] ->
    true;
is_submitted(#job{shares = []}) ->
    false.

-spec solve_time(job()) -> solve_time().
solve_time(#job{created = Created, shares = Shares}) when Shares =/= [] ->
    %% The shortest time is considered to be the solve time. The first
    %% submitted share, which has the shortest solve time, is the last in
    %% the list.
    Share = lists:last(Shares),
    aestratum_share:created(Share) - Created;
solve_time(#job{max_solve_time = MaxSolveTime, shares = []}) ->
    MaxSolveTime.

-spec make_id(block_hash(), block_version(), block_target()) -> binary().
make_id(BlockHash, BlockVersion, BlockTarget) ->
    make_id1(BlockHash, BlockVersion, BlockTarget).

-spec status(job()) -> map().
status(#job{id = Id,
            block_hash = BlockHash,
            block_version = BlockVersion,
            block_target = BlockTarget,
            share_target = ShareTarget,
            desired_solve_time = DesiredSolveTime,
            max_solve_time = MaxSolveTime,
            shares = Shares,
            error_shares = ErrorShares,
            created = Created}) ->
    #{id => Id,
      block_hash => BlockHash,
      block_version => BlockVersion,
      block_target => aestratum_target:to_hex(BlockTarget),
      share_target => aestratum_target:to_hex(ShareTarget),
      desired_solve_time => DesiredSolveTime,
      max_solve_time => MaxSolveTime,
      shares => [aestratum_share:status(S) || S <- Shares],
      error_shares => [aestratum_share:status(S) || S <- ErrorShares],
      created => Created}.

-spec add_share(share(), job()) -> job().
add_share(Share, #job{shares = Shares} = Job) ->
    %% TODO: add limit on how many shares can be submitted per job.
    Job#job{shares = [Share | Shares]}.

-spec add_error_share(error_share(), job()) -> job().
add_error_share(ErrorShare, #job{error_shares = ErrorShares} = Job) ->
    %% TODO: add limit on how many error shares can be submitted per job.
    Job#job{error_shares = [ErrorShare | ErrorShares]}.

-spec is_share_present(miner_nonce(), pow(), job()) -> boolean().
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
