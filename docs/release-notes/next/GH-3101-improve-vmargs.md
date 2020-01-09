* The default configuration for the Erlang runtime system has been adapted to
  use less CPU cores and threads. This should improve CPU contention on
  systems with fewer cores and improve responsiveness.
  For systems with 4+ CPU cores the settings can be increased if the node 
  is experiencing any form of CPU limitations.
