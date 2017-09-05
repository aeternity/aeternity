-record(peer, {
          uri = ""          :: http_uri:uri(),
          last_seen = 0     :: integer()}). % Erlang system time (POSIX time)


-type peer() :: #peer{}.
