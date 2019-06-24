-module(aestratum_session).

-export([new/0,
         handle_event/2,
         close/1
        ]).

-export([set/3,
         status/1
        ]).

-ifdef(TEST).
-export([state/1]).
-endif.

-include("aestratum_log.hrl").

-export_type([key/0,
              value/0,
              session/0]).

-type key()                           :: atom().

-type value()                         :: term().

-type phase()                         :: connected
                                       | configured
                                       | subscribed
                                       | authorized
                                       | disconnected.

-type timer()                         :: {reference(), phase()}.

-type account()                       :: aestratum_user_register:account().

-type extra_nonce()                   :: aestratum_nonce:part_nonce().

-type accept_blocks()                 :: boolean().

-type block_count()                   :: non_neg_integer().

-type share_target()                  :: aestratum_target:int_target().

-type initial_share_target()          :: aestratum_target:int_target().

-type max_share_target()              :: aestratum_target:int_target().

-type share_target_diff_threshold()   :: float().

-type desired_solve_time()            :: aestratum_target:solve_time().

-type max_solve_time()                :: aestratum_target:solve_time().

-type job_queue()                     :: aestratum_job_queue:job_queue().

-type raw_msg()                       :: aestratum_jsonrpc:raw_msg().

-type event()                         :: {conn, conn_event()}
                                       | {chain, chain_event()}.

-type conn_event()                    :: conn_init_event()
                                       | conn_recv_data_event()
                                       | conn_timeout_event()
                                       | conn_close_event().

-type conn_init_event()               :: #{event              => init}.

-type conn_recv_data_event()          :: #{event              => recv_data,
                                           data               => raw_msg()}.

-type conn_timeout_event()            :: #{event              => timeout}.

-type conn_close_event()              :: #{event              => close}.

-type chain_event()                   :: chain_recv_block_event()
                                       | chain_set_target_event()
                                       | chain_notify_event().

-type chain_recv_block_event()        :: #{event              => recv_block,
                                           block              => block()}.

-type chain_set_target_event()        :: #{event              => set_target}.

-type chain_notify_event()            :: #{event              => notify,
                                           job_info           => job_info()}.

-type action()                        :: {send, raw_msg(), session()}
                                       | {no_send, session()}
                                       | {stop, session()}.

-type block()                         :: #{block_hash         => block_hash(),
                                           block_version      => block_version(),
                                           block_target       => block_target()}.

-type job_info()                      :: #{job_id             => job_id(),
                                           block_hash         => block_hash(),
                                           block_version      => block_version(),
                                           block_target       => block_target(),
                                           share_target       => share_target(),
                                           desired_solve_time => desired_solve_time(),
                                           max_solve_time     => max_solve_time()}.

-type job_id()                        :: aestratum_job:id().

-type block_hash()                    :: binary().

-type block_version()                 :: pos_integer().

-type block_target()                  :: aestratum_target:int_target().

-record(session, {
          phase                       :: phase(),
          timer                       :: timer() | undefined,
          account                     :: account() | undefined,
          extra_nonce                 :: extra_nonce() | undefined,
          accept_blocks = false       :: accept_blocks(),
          block_count = 0             :: block_count(),
          initial_share_target        :: initial_share_target() | undefined,
          max_share_target            :: max_share_target() | undefined,
          share_target_diff_threshold :: share_target_diff_threshold() | undefined,
          desired_solve_time          :: desired_solve_time() | undefined,
          max_solve_time              :: max_solve_time() | undefined,
          jobs                        :: job_queue() | undefined
         }).

-opaque session()                     :: #session{}.

%% API.

-spec new() -> session().
new() ->
    #session{phase = connected}.

-spec handle_event(event(), session()) -> action().
handle_event({conn, Event}, Session) ->
    handle_conn_event(Event, Session);
handle_event({chain, Event}, Session) ->
    handle_chain_event(Event, Session).

-spec close(session()) -> ok.
close(#session{} = Session) ->
    close_session(Session),
    ok.

-spec set(key(), value(), session()) -> session().
set(max_share_target, Val, Session) when is_integer(Val) ->
    Session#session{max_share_target = Val};
set(share_target_diff_threshold, Val, Session) when is_float(Val) ->
    Session#session{share_target_diff_threshold = Val};
set(desired_solve_time, Val, Session) when is_integer(Val) ->
    Session#session{desired_solve_time = Val};
set(max_solve_time, Val, Session) when is_integer(Val) ->
    Session#session{max_solve_time = Val}.

-spec status(session()) -> map().
status(#session{phase = Phase,
                account = Account,
                extra_nonce = ExtraNonce,
                initial_share_target = InitialShareTarget,
                max_share_target = MaxShareTarget,
                share_target_diff_threshold = ShareTargetDiffThreshold,
                desired_solve_time = DesiredSolveTime,
                max_solve_time = MaxSolveTime,
                jobs = Jobs}) ->
    ExtraNonce1 =
        case ExtraNonce of
            N when N =/= undefined -> aestratum_nonce:to_hex(N);
            undefined              -> undefined
        end,
    InitialShareTarget1 =
        case InitialShareTarget of
            IT when IT =/= undefined -> aestratum_target:to_hex(IT);
            undefined                -> undefined
        end,
    MaxShareTarget1 =
        case MaxShareTarget of
            MT when MT =/= undefined -> aestratum_target:to_hex(MT);
            undefined                -> undefined
        end,
    #{phase => Phase,
      account => Account,
      extra_nonce => ExtraNonce1,
      initial_share_target => InitialShareTarget1,
      max_share_target => MaxShareTarget1,
      share_target_diff_threshold => ShareTargetDiffThreshold,
      desired_solve_time => DesiredSolveTime,
      max_solve_time => MaxSolveTime,
      jobs => [aestratum_job:status(J) || J <- aestratum_job_queue:to_list(Jobs)]}.

%% Internal functions.

handle_conn_event(#{event := init}, #session{phase = connected} = Session) ->
    {no_send, Session#session{timer = set_timer(connected)}};
handle_conn_event(#{event := recv_data, data := RawMsg}, Session) ->
    case aestratum_jsonrpc:decode(RawMsg) of
        {ok, Msg}    -> recv_msg(Msg, Session);
        {error, Rsn} -> recv_msg_error(Rsn, Session)
    end;
%% TODO: {reconnect, Host, Port, WaitTime},...
handle_conn_event(#{event := timeout}, Session) ->
    handle_conn_timeout(Session);
handle_conn_event(#{event := close}, Session) ->
    handle_conn_close(Session).

handle_chain_event(#{event := recv_block, block := Block}, Session) ->
    handle_chain_recv_block(Block, Session);
handle_chain_event(#{event := set_target}, Session) ->
    handle_chain_set_target(Session);
handle_chain_event(#{event := notify, job_info := JobInfo}, Session) ->
    handle_chain_notify(JobInfo, Session).

%% Handle received messages from client.

recv_msg(#{type := req, method := configure} = Req,
         #session{phase = connected} = Session) ->
    ?INFO("recv_configure_req, req: ~p", [Req]),
    send_configure_rsp(Req, Session);
recv_msg(#{type := req, method := subscribe} = Req,
         #session{phase = connected} = Session) ->
    ?INFO("recv_subscribe_req, req: ~p", [Req]),
    send_subscribe_rsp(Req, Session);
recv_msg(#{type := req, method := subscribe} = Req,
         #session{phase = configured} = Session) ->
    ?INFO("recv_subscribe_req, req: ~p", [Req]),
    send_subscribe_rsp(Req, Session);
recv_msg(#{type := req, method := authorize} = Req,
         #session{phase = Phase} = Session) when
      (Phase =:= subscribed) or (Phase =:= authorized) ->
    ?INFO("recv_authorize_req, req: ~p", [Req]),
    send_authorize_rsp(Req, Session);
%% Submit request is accepted only when the connection is in authorized phase
%% and the share target is set (set_target notification was sent to the client).
recv_msg(#{type := req, method := submit} = Req,
         #session{phase = authorized,
                  initial_share_target = InitialShareTarget} = Session) when
      InitialShareTarget =/= undefined ->
    ?INFO("recv_submit_req, req: ~p", [Req]),
    send_submit_rsp(Req, Session);
recv_msg(#{type := req, method := submit} = Req,
         #session{phase = authorized, initial_share_target = undefined} = Session) ->
    ?ERROR("recv_submit_req, req: ~p", [Req]),
    send_unknown_error_rsp(Req, target_not_set, Session);
recv_msg(#{type := req, method := submit} = Req,
         #session{phase = subscribed} = Session) ->
    ?ERROR("recv_submit_req, reason: ~p, req: ~p", [unauthorized, Req]),
    send_unauthorized_worker_rsp(Req, null, Session);
recv_msg(#{type := req, method := Method} = Req,
         #session{phase = Phase} = Session) when
      ((Method =:= authorize) or (Method =:= submit)) and
      ((Phase =:= connected) or (Phase =:= configured)) ->
    ?ERROR("recv_req, reason: ~p, req: ~p", [not_subscribed, Req]),
    send_not_subscribed_rsp(Req, null, Session);
recv_msg(Msg, Session) ->
    ?ERROR("recv_msg, reason: ~p, msg: ~p", [unexpected_msg, Msg]),
    send_unknown_error_rsp(Msg, unexpected_msg, Session).

%% JSON-RPC error responses.

recv_msg_error(parse_error = Rsn, Session) ->
    ?ERROR("recv_msg_error, reason: ~p", [Rsn]),
    RspMap = #{type => rsp, method => undefined, id => null,
               reason => parse_error, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session};
recv_msg_error({invalid_msg = Rsn, MaybeId}, Session) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_msg, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session};
recv_msg_error({invalid_method = Rsn, MaybeId}, Session) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_method, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session};
recv_msg_error({invalid_param = Rsn, Param, MaybeId}, Session) ->
    ?ERROR("recv_msg_error, reason: ~p, param: ~p, id: ~p",
           [Rsn, Param, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => invalid_param, data => atom_to_binary(Param, utf8)},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session};
recv_msg_error({internal_error = Rsn, MaybeId}, Session) ->
    ?ERROR("recv_msg_error, reason: ~p, id: ~p", [Rsn, MaybeId]),
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_jsonrpc:to_id(MaybeId),
               reason => internal_error, data => null},
    ?INFO("send_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

%% Handle timeout.

handle_conn_timeout(#session{phase = Phase, timer = {_TRef, Phase}} = Session) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    %% The timer's phase is the same as the current phase, so the timeout
    %% applies and the connection to the client is closed.
    ?INFO("handle_conn_timeout, phase: ~p", [Phase]),
    {stop, close_session(Session)};
handle_conn_timeout(Session) ->
    {no_send, Session}.

handle_conn_close(Session) ->
    {stop, close_session(Session)}.

%% Server to client responses.

send_configure_rsp(Req, Session) ->
    send_configure_rsp1(validate_configure_req(Req, Session), Req, Session).

send_subscribe_rsp(Req, Session) ->
    send_subscribe_rsp1(validate_subscribe_req(Req, Session), Req, Session).

send_authorize_rsp(Req, Session) ->
    send_authorize_rsp1(validate_authorize_req(Req, Session), Req, Session).

send_submit_rsp(#{user := User, miner_nonce := MinerNonce, pow := Pow} = Req,
                Session) ->
    %% MinerNonce is guaranteed (by decoder) to be of valid size and hex
    %% encoded. What is not guaranteed here is that the miner nonce bytes +
    %% extra nonce bytes are not 8 together.
    %% TODO: move below to aestratum_nonce module
    MinerNonceNBytes = byte_size(MinerNonce) div 2,
    MinerNonceVal = aestratum_nonce:to_int(miner, MinerNonce, MinerNonceNBytes),
    MinerNonce1 = aestratum_nonce:new(miner, MinerNonceVal, MinerNonceNBytes),
    Share = aestratum_share:new(User, MinerNonce1, Pow),
    Extra = #{miner_nonce => MinerNonce1, share => Share},
    send_submit_rsp1(validate_submit_req(Req, Session, Extra), Req, Session).

send_configure_rsp1(ok, #{id := Id}, #session{timer = Timer} = Session) ->
    %% TODO: there are no configure params currently
    maybe_cancel_timer(Timer),
    RspMap = #{type => rsp, method => configure, id => Id, result => []},
    ?INFO("send_configure_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session#session{phase = configured,
                                           timer = set_timer(configured)}}.
%%send_configure_rsp1({error, Rsn}, ...

send_subscribe_rsp1(ok, #{id := Id} = Req, #session{timer = Timer} = Session) ->
    maybe_cancel_timer(Timer),
    %% TODO: Session resumption not supported (yet).
    ExtraNonceNBytes = aestratum_env:get(extra_nonce_bytes),
    case aestratum_extra_nonce_cache:get(ExtraNonceNBytes) of
        {ok, ExtraNonce} ->
            SessionId1 = null,
            ExtraNonce1 = aestratum_nonce:to_hex(ExtraNonce),
            RspMap = #{type => rsp, method => subscribe, id => Id,
                       result => [SessionId1, ExtraNonce1]},
            ?INFO("send_subscribe_rsp, rsp: ~p", [RspMap]),
            %% Set timer for authorize request.
            {send, encode(RspMap),
             Session#session{phase = subscribed, timer = set_timer(subscribed),
                             extra_nonce = ExtraNonce}};
        {error, Rsn} ->
            send_unknown_error_rsp(Req, Rsn, Session)
    end;
send_subscribe_rsp1({error, Rsn}, Req, Session) ->
    send_unknown_error_rsp(Req, Rsn, Session).

send_authorize_rsp1({ok, first_worker}, #{id := Id, user := {Account, Worker}},
                    #session{timer = Timer} = Session) ->
    maybe_cancel_timer(Timer),
    aestratum_user_register:add(Account, Worker, self()),
    RspMap = #{type => rsp, method => authorize, id => Id, result => true},
    %% After the authorization, the server is supposed to send an initial
    %% target.
    self() ! {chain, #{event => set_target}},
    %% No need to set timer after authorization, there are no further expected
    %% requests within a time period. Submit requests do not require timeout.
    %% Job queue is initialized to make it ready to accept client sumbissions.
    ?INFO("send_authorize_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap),
     Session#session{phase = authorized, timer = undefined, account = Account,
                     jobs = aestratum_job_queue:new()}};
send_authorize_rsp1({ok, other_worker}, #{id := Id, user := {Account, Worker}},
                    Session) ->
    RspMap =
        case aestratum_user_register:add_worker(Account, Worker) of
            ok ->
                #{type => rsp, method => authorize, id => Id, result => true};
            {error, Rsn} ->
                ?ERROR("add_worker, reason: ~p", [Rsn]),
                #{type => rsp, method => authorize, id => Id, result => false}
        end,
    ?INFO("send_authorize_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session};
send_authorize_rsp1({error, _Rsn}, #{id := Id}, Session) ->
    RspMap = #{type => rsp, method => authorize, id => Id, result => false},
    ?INFO("send_authorize_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_submit_rsp1({ok, Share, Job}, #{id := Id, pow := Pow},
                 #session{extra_nonce = ExtraNonce, jobs = Jobs} = Session) ->
    JobId = aestratum_job:id(Job),
    Job1 = aestratum_job:add_share(Share, Job),
    Jobs1 = aestratum_job_queue:replace(JobId, Job1, Jobs),
    {Account, _Worker} = aestratum_share:user(Share),
    ShareTarget = aestratum_job:share_target(Job),
    BlockHash = aestratum_utils:hex_to_bin(aestratum_job:block_hash(Job)),
    aestratum:submit_share(Account, ShareTarget, BlockHash),
    case aestratum_share:validity(Share) of
        valid_block ->
            MinerNonce = aestratum_share:miner_nonce(Share),
            Nonce = aestratum_nonce:merge(ExtraNonce, MinerNonce),
            aestratum:submit_solution(BlockHash, aestratum_nonce:value(Nonce), Pow);
        valid_share ->
            ok
    end,
    RspMap = #{type => rsp, method => submit, id => Id, result => true},
    ?INFO("send_submit_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session#session{jobs = Jobs1}};
send_submit_rsp1({error, Share, Job}, #{id := Id} = Req,
                 #session{jobs = Jobs} = Session) when Job =/= undefined ->
    JobId = aestratum_job:id(Job),
    Job1 = aestratum_job:add_error_share(Share, Job),
    Jobs1 = aestratum_job_queue:replace(JobId, Job1, Jobs),
    Session1 = Session#session{jobs = Jobs1},
    case aestratum_share:validity(Share) of
        user_not_found ->
            send_unauthorized_worker_rsp(Req, null, Session1);
        invalid_miner_nonce = Rsn ->
            send_unknown_error_rsp(Req, Rsn, Session1);
        duplicate_share ->
            send_duplicate_share_rsp(Req, null, Session1);
        invalid_solution = Rsn ->
            send_unknown_error_rsp(Req, Rsn, Session1);
        high_target_share ->
            send_low_difficulty_share_rsp(Req, null, Session1);
        max_solve_time_exceeded ->
            %% TODO: send unknow_error with data set instead?
            RspMap = #{type => rsp, method => submit, id => Id, result => false},
            ?INFO("send_submit_rsp, rsp: ~p", [RspMap]),
            {send, encode(RspMap), Session1}
    end;
send_submit_rsp1({error, Share, undefined}, Req, Session) ->
    %% The share is not saved here as there is no job associated with it. This
    %% can be a security issue, we need to check how many of these are
    %% submitted and possibly ban/disconnect the client.
    job_not_found = aestratum_share:validity(Share),
    send_job_not_found_rsp(Req, null, Session).

%% Stratum error responses.

send_not_subscribed_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => not_subscribed,
               data => error_data(Data)},
    ?INFO("send_not_subscribed_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_unauthorized_worker_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => unauthorized_worker,
               data => error_data(Data)},
    ?INFO("send_unauthorized_worker_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_low_difficulty_share_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => low_difficulty_share,
               data => error_data(Data)},
    ?INFO("send_low_difficulty_share_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_duplicate_share_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => duplicate_share,
               data => error_data(Data)},
    ?INFO("send_duplicate_share_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_job_not_found_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => job_not_found,
               data => error_data(Data)},
    ?INFO("send_job_not_found_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

send_unknown_error_rsp(#{id := Id}, Data, Session) ->
    RspMap = #{type => rsp, id => Id, reason => unknown_error,
               data => error_data(Data)},
    ?INFO("send_unknown_error_rsp, rsp: ~p", [RspMap]),
    {send, encode(RspMap), Session}.

error_data(null) ->
    null;
error_data(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8).

%% TODO: reconnect request.

%% Handle chain related messages.

handle_chain_recv_block(#{block_hash := BlockHash, block_version := BlockVersion,
                          block_target := BlockTarget} = NewBlock,
                       #session{phase = authorized, accept_blocks = true,
                                block_count = 0, jobs = Jobs} = Session) ->
    JobId = aestratum_job:make_id(BlockHash, BlockVersion, BlockTarget),
    BlockCount = set_block_count(0),
    case aestratum_job_queue:member(JobId, Jobs) of
        true  -> {no_send, Session#session{block_count = BlockCount}}; %% Unlikely to happen?
        false -> handle_chain_recv_block1(NewBlock#{job_id => JobId},
                                          Session#session{block_count = BlockCount})
    end;
%% TODO: shall be accept_blocks set to true?
handle_chain_recv_block(_NewBlock, #session{phase = authorized,
                                            block_count = BlockCount} = Session) ->
    {no_send, Session#session{block_count = set_block_count(BlockCount)}};
handle_chain_recv_block(_NewBlock, Session) ->
    %% Skip this new block. The session is not ready to handle it.
    %% The phase is not authorized or the accept_blocks is set to false.
    {no_send, Session}.

handle_chain_recv_block1(#{block_hash := BlockHash, block_version := BlockVersion,
                           block_target := BlockTarget, job_id := JobId} = NewJobInfo,
                        #session{initial_share_target = InitialShareTarget,
                                 max_share_target = MaxShareTarget,
                                 share_target_diff_threshold = ShareTargetDiffThreshold,
                                 desired_solve_time = DesiredSolveTime,
                                 max_solve_time = MaxSolveTime,
                                 jobs = Jobs} = Session) ->
    %% We take the newest queue job, and get its share target (or just take
    %% the initial share target if there are no jobs in the queue).
    OldJobShareTarget =
        case aestratum_job_queue:get_rear(Jobs) of
            {ok, OldJob}   -> aestratum_job:share_target(OldJob);
            {error, empty} -> InitialShareTarget
        end,
    %% If there are enough jobs in the queue, we calculate the new job's
    %% target from them, otherwise we use the initial share target.
    NewJobShareTarget =
        case aestratum_job_queue:share_target(DesiredSolveTime, MaxShareTarget, Jobs) of
            %% The share target can never be lower than the block target.
            %% This should never happen in the wild, just in the tests.
            {ok, NewShareTarget}     -> max(NewShareTarget, BlockTarget);
            {error, not_enough_jobs} -> InitialShareTarget
        end,
    case aestratum_target:diff(NewJobShareTarget, OldJobShareTarget) of
        {_Change, Percent} when Percent > ShareTargetDiffThreshold ->
            %% We stop accepting blocks until the {chain, {notify, ...}}
            %% event is processed. There might be some other new block chain
            %% events in the message queue of this process, these are skipped.
            %% We copy share target, desired solve time and max solve time
            %% as these values were used for the new target computation and can
            %% be changed in the state when processing {chain, {notify, ...}}
            %% event.
            NewJobInfo1 = NewJobInfo#{share_target => NewJobShareTarget,
                                      desired_solve_time => DesiredSolveTime,
                                      max_solve_time => MaxSolveTime},
            self() ! {chain, #{event => notify, job_info => NewJobInfo1}},
            Session1 = Session#session{accept_blocks = false},
            send_set_target_ntf(NewJobShareTarget, Session1);
        _Other ->
            Job = aestratum_job:new(JobId, BlockHash, BlockVersion, BlockTarget,
                                    OldJobShareTarget, DesiredSolveTime, MaxSolveTime),
            %% TODO: compute if the client's job queueu should be cleaned.
            Session1 = Session#session{jobs = aestratum_job_queue:add(Job, Jobs)},
            send_notify_ntf(JobId, BlockHash, BlockVersion, true, Session1)
    end.

handle_chain_set_target(Session) ->
    InitialShareTarget = aestratum_env:get(initial_share_target),
    MaxShareTarget = aestratum_env:get(max_share_target),
    ShareTargetDiffThreshold = aestratum_env:get(share_target_diff_threshold),
    DesiredSolveTime = aestratum_env:get(desired_solve_time),
    MaxSolveTime = aestratum_env:get(max_solve_time),
    Session1 =
        Session#session{accept_blocks = true,
                        initial_share_target = InitialShareTarget,
                        max_share_target = MaxShareTarget,
                        share_target_diff_threshold = ShareTargetDiffThreshold,
                        desired_solve_time = DesiredSolveTime,
                        max_solve_time = MaxSolveTime},
    send_set_target_ntf(InitialShareTarget, Session1).

handle_chain_notify(#{job_id := JobId, block_hash := BlockHash,
                      block_version := BlockVersion, block_target := BlockTarget,
                      share_target := ShareTarget,
                      desired_solve_time := DesiredSolveTime,
                      max_solve_time := MaxSolveTime},
                    #session{accept_blocks = false, jobs = Jobs} = Session) ->
    Job = aestratum_job:new(JobId, BlockHash, BlockVersion, BlockTarget,
                            ShareTarget, DesiredSolveTime, MaxSolveTime),
    Jobs1 = aestratum_job_queue:add(Job, Jobs),
    Session1 = Session#session{accept_blocks = true, jobs = Jobs1},
    send_notify_ntf(JobId, BlockHash, BlockVersion, true, Session1).

%% Notifications from server to client.

send_set_target_ntf(ShareTarget, Session) ->
    NtfMap = #{type => ntf, method => set_target,
               target => aestratum_target:to_hex(ShareTarget)},
    ?INFO("send_set_target_ntf, ntf: ~p", [NtfMap]),
    {send, encode(NtfMap), Session}.

send_notify_ntf(JobId, BlockHash, BlockVersion, EmptyQueue, #session{} = Session) ->
    NtfMap = #{type => ntf, method => notify, job_id => JobId,
               block_hash => BlockHash, block_version => BlockVersion,
               empty_queue => EmptyQueue},
    ?INFO("send_notify_ntf, ntf: ~p", [NtfMap]),
    {send, encode(NtfMap), Session}.

%% Helper functions.

close_session(#session{phase = Phase, extra_nonce = ExtraNonce,
                       timer = Timer} = Session) when Phase =/= disconnected ->
    maybe_free_extra_nonce(ExtraNonce),
    maybe_cancel_timer(Timer),
    case Phase of
        authorized -> aestratum_user_register:del(self());
        _Other     -> ok
    end,
    ?INFO("close_session", []),
    Session#session{phase = disconnected, extra_nonce = undefined,
                    timer = undefined};
close_session(Session) ->
    Session.

maybe_free_extra_nonce(ExtraNonce) when ExtraNonce =/= undefined ->
    aestratum_extra_nonce_cache:free(ExtraNonce),
    ok;
maybe_free_extra_nonce(undefined) ->
    ok.

set_timer(Phase) ->
    Timeout = aestratum_env:get(msg_timeout),
    TRef = erlang:send_after(Timeout, self(), {conn, #{event => timeout}}),
    {TRef, Phase}.

set_block_count(BlockCount) ->
    case BlockCount =:= aestratum_env:get(skip_num_blocks) of
        true  -> 0;
        false -> BlockCount + 1
    end.

maybe_cancel_timer({TRef, _Phase}) when is_reference(TRef) ->
    erlang:cancel_timer(TRef);
maybe_cancel_timer(undefined) ->
    ok.

validate_configure_req(#{params := []}, _Session) ->
    ok.

validate_subscribe_req(Req, Session) ->
    run([fun check_user_agent/2,
         fun check_session_id/2,
         %%fun check_host/2,  %% TODO: check disabled due to testing
         fun check_port/2], Req, Session).

validate_authorize_req(Req, Session) ->
    run([fun check_user_and_password/2], Req, Session).

validate_submit_req(Req, Session, Extra) ->
    run([fun check_job_id/3,
         fun check_user/3,
         fun check_miner_nonce/3,
         fun check_duplicate_share/3,
         fun check_solution/3,
         fun check_target/3,
         fun check_timestamp/3], Req, Session, Extra).

check_user_agent(#{user_agent := _UserAgent}, _Session) ->
    %% Some user agents may not by supported by the server
    continue.

check_session_id(#{session_id := _SessionId}, _Session) ->
    continue.

%% check_host(#{host := Host}, _Session) ->
%%     check_host1(Host, aestratum_env:get(host)).

%% check_host1(Host, Host) ->
%%     continue;
%% check_host1(_Host, _Host1) ->
%%     {done, {error, host_mismatch}}.

check_port(#{port := Port}, _Session) ->
    check_port1(Port, aestratum_env:get(port)).

check_port1(Port, Port) ->
    continue;
check_port1(_Port, _Port1) ->
    {done, {error, port_mismatch}}.

check_user_and_password(#{user := {Account, _Worker}, password := null},
                        #session{phase = Phase}) ->
    case {Phase, aestratum_user_register:member(Account, self())} of
        {subscribed, neither}      -> {done, {ok, first_worker}};
        {authorized, both}         -> {done, {ok, other_worker}};
        %% Connection is authorized, but it's a different client/process.
        {authorized, account_only} -> {done, {error, conn_pid_mismatch}};
        %% Connection is subscribed, but other connection uses the same account
        %% and maybe worker name.
        {subscribed, _}            -> {done, {error, account_authorized}};
        %% A worker tries to authorise using an account that doesn't match.
        {authorized, neither}      -> {done, {error, account_mismatch}}
    end.

check_job_id(#{job_id := JobId}, #session{jobs = Jobs}, #{share := Share}) ->
    case aestratum_job_queue:find(JobId, Jobs) of
         {ok, Job} ->
            {add_extra, #{job => Job}};
         {error, not_found} ->
            Share1 = aestratum_share:set_validity(job_not_found, Share),
            {done, {error, Share1, undefined}}
    end.

check_user(#{user := {Account, _Worker}}, _Session, #{share := Share, job := Job}) ->
    case aestratum_user_register:member(Account, self()) of
        both ->
            continue;
        _Other ->
            %% Here a user could send a share to a different account. It could
            %% be checked here and logged - to be considered.
            Share1 = aestratum_share:set_validity(user_not_found, Share),
            {done, {error, Share1, Job}}
    end.

check_miner_nonce(_Req, #session{extra_nonce = ExtraNonce},
                  #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    ComplementNBytes = aestratum_nonce:complement_nbytes(ExtraNonce),
    case aestratum_nonce:nbytes(MinerNonce) =:= ComplementNBytes of
        true ->
            continue;
        false ->
            Share1 = aestratum_share:set_validity(invalid_miner_nonce, Share),
            {done, {error, Share1, Job}}
    end.

check_duplicate_share(#{pow := Pow}, _Session,
                      #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    case aestratum_job:is_share_present(MinerNonce, Pow, Job) of
        false ->
            continue;
        true ->
            Share1 = aestratum_share:set_validity(duplicate_share, Share),
            {done, {error, Share1, Job}}
    end.

check_solution(#{pow := Pow}, #session{extra_nonce = ExtraNonce},
               #{miner_nonce := MinerNonce, share := Share, job := Job}) ->
    BlockHash = aestratum_job:block_hash(Job),
    BlockVersion = aestratum_job:block_version(Job),
    Nonce = aestratum_nonce:merge(ExtraNonce, MinerNonce),
    case aestratum_miner:verify_proof(BlockHash, BlockVersion, Nonce,
                                      Pow, aestratum_env:get(edge_bits)) of
        true ->
            continue;
        false ->
            Share1 = aestratum_share:set_validity(invalid_solution, Share),
            {done, {error, Share1, Job}}
    end.

check_target(#{pow := Pow}, _Session, #{share := Share, job := Job}) ->
    BlockTarget = aestratum_job:block_target(Job),
    ShareTarget = aestratum_job:share_target(Job),
    case aestratum_miner:get_target(Pow, aestratum_env:get(edge_bits)) of
        Target when Target =< BlockTarget ->  %% TODO =< or just < ?
            Share1 = aestratum_share:set_validity(valid_block, Share),
            {add_extra, #{share => Share1}};
        Target when Target =< ShareTarget ->  %% TODO: =< or just < ?
            Share1 = aestratum_share:set_validity(valid_share, Share),
            {add_extra, #{share => Share1}};
        _Other ->
            Share1 = aestratum_share:set_validity(high_target_share, Share),
            {done, {error, Share1, Job}}
    end.

check_timestamp(_Req, #session{max_solve_time = MaxSolveTime},
                #{share := Share, job := Job}) ->
    case aestratum_share:created(Share) - aestratum_job:created(Job) of
        SolveTime when SolveTime =< MaxSolveTime ->
            %% Validity already set in the check_target/3 check.
            {done, {ok, Share, Job}};
        _Other ->
            %% Solve time is greater than the max solve time.
            Share1 = aestratum_share:set_validity(max_solve_time_exceeded, Share),
            {done, {error, Share1, Job}}
    end.

run([Fun | Funs], Req, Session) ->
    case Fun(Req, Session) of
        continue ->
            run(Funs, Req, Session);
        {done, Res} ->
            Res
    end;
run([], _Req, _Session) ->
    ok.

run([Fun | Funs], Req, Session, Extra) ->
    case Fun(Req, Session, Extra) of
        {add_extra, M} when is_map(M) ->
            run(Funs, Req, Session, maps:merge(Extra, M));
        continue ->
            run(Funs, Req, Session, Extra);
        {done, Res} ->
            Res
    end.

encode(Map) ->
    {ok, RawMsg} = aestratum_jsonrpc:encode(Map),
    RawMsg.

%% Used for testing only.

-ifdef(TEST).
state(#session{phase = Phase, timer = Timer, extra_nonce = ExtraNonce,
               accept_blocks = AcceptBlocks, initial_share_target = InitialShareTarget,
               max_solve_time = MaxSolveTime}) ->
    #{phase => Phase,
      timer_phase => case Timer of
                         {_, TPhase} -> TPhase;
                         undefined -> undefined
                     end,
      extra_nonce => ExtraNonce,
      accept_blocks => AcceptBlocks,
      initial_share_target => InitialShareTarget,
      max_solve_time => MaxSolveTime
     }.
-endif.
