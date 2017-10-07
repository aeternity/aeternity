-record(peer, {
          uri = ""          :: http_uri:uri(),
          last_seen = 0     :: integer(), % Erlang system time (POSIX time)
          last_pings = []   :: [integer()], % Erlang system time
          ping_tref         :: reference() | undefined
         }).


-type peer() :: #peer{}.
