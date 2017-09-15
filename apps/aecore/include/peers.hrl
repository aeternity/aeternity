-type uri() :: string().

-record(peer, {
          uri = ""          :: uri(),
          last_seen = 0     :: integer()}). % Erlang system time (POSIX time)


-type peer() :: #peer{}.
