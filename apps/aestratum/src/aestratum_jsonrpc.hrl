
-define(ID_MIN, 0).
-define(ID_MAX, 16#ffffffff).

-define(HOST_MAX_SIZE, 16#ff).

-define(PORT_MIN, 1).
-define(PORT_MAX, 16#ffff).

-define(USER_MAX_SIZE, 64).

-define(USER_AGENT_MAX_SIZE, 64).

-define(SESSION_ID_SIZE, 16).

-define(PASSWORD_SIZE, 64).

-define(TARGET_SIZE, 64).

-define(JOB_ID_SIZE, 16).

%% Size when hex encoded string.
-define(NONCE_BYTES_1_SIZE, 2).
-define(NONCE_BYTES_2_SIZE, 4).
-define(NONCE_BYTES_3_SIZE, 6).
-define(NONCE_BYTES_4_SIZE, 8).
-define(NONCE_BYTES_5_SIZE, 10).
-define(NONCE_BYTES_6_SIZE, 12).
-define(NONCE_BYTES_7_SIZE, 14).

-define(BLOCK_VERSION_MIN, 1).
-define(BLOCK_VERSION_MAX, 16#ffffffff).

-define(BLOCK_HASH_SIZE, 64).

%% In seconds.
-define(WAIT_TIME_MIN, 0).
-define(WAIT_TIME_MAX, 24 * 60 * 60).

-define(POW_SIZE, 42).
-define(POW_NUMBER_MIN, 0).
-define(POW_NUMBER_MAX, 16#ffffffff).

-define(ERROR_MSG_MAX_SIZE, 16#ff).

-define(ERROR_DATA_MAX_SIZE, 16#1ff).

-define(IS_ID(Id), (Id >= ?ID_MIN) and (Id =< ?ID_MAX)).
-define(IS_HOST(Host), (Host =/= <<>>) and (byte_size(Host) =< ?HOST_MAX_SIZE)).
-define(IS_PORT(Port), (Port > ?PORT_MIN) and (Port =< ?PORT_MAX)).
-define(IS_USER(User), (User =/= <<>>) and (byte_size(User) =< ?USER_MAX_SIZE)).
-define(IS_USER_AGENT(Agent), (Agent =/= <<>>) and (byte_size(Agent) =< ?USER_AGENT_MAX_SIZE)).
-define(IS_SESSION_ID(Id), byte_size(Id) =:= ?SESSION_ID_SIZE).
-define(IS_PASSWORD(Password), byte_size(Password) =:= ?PASSWORD_SIZE).
-define(IS_TARGET(Target), byte_size(Target) =:= ?TARGET_SIZE).
-define(IS_JOB_ID(Id), byte_size(JobId) =:= ?JOB_ID_SIZE).
-define(IS_EXTRA_NONCE(Nonce), (byte_size(Nonce) >= ?NONCE_BYTES_1_SIZE) and (byte_size(Nonce) =< ?NONCE_BYTES_7_SIZE)).
-define(IS_MINER_NONCE(Nonce), (byte_size(Nonce) >= ?NONCE_BYTES_1_SIZE) and (byte_size(Nonce) =< ?NONCE_BYTES_7_SIZE)).
-define(IS_BLOCK_VERSION(Version), (Version >= ?BLOCK_VERSION_MIN) and (Version =< ?BLOCK_VERSION_MAX)).
-define(IS_BLOCK_HASH(Hash), (byte_size(Hash) =:= ?BLOCK_HASH_SIZE)).
-define(IS_EMPTY_QUEUE(EmptyQueue), is_boolean(EmptyQueue)).
-define(IS_WAIT_TIME(Time), (Time >= ?WAIT_TIME_MIN) and (Time =< ?WAIT_TIME_MAX)).
-define(IS_POW(Pow), is_list(Pow) and (length(Pow) =:= ?POW_SIZE)).
-define(IS_POW_NUMBER(N), (N >= ?POW_NUMBER_MIN) and (N =< ?POW_NUMBER_MAX)).
-define(IS_ERROR_MSG(Msg), (Msg =/= <<>>) and (byte_size(Msg) =< ?ERROR_MSG_MAX_SIZE)).
-define(IS_ERROR_DATA(Data), (Data =/= <<>>) and (byte_size(Data) =< ?ERROR_DATA_MAX_SIZE)).

