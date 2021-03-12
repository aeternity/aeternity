* An API method has been added for state channels that allows the client to tell the fsm
  to assume that minimum depth has been achieved for a certain tx hash. If used by both
  clients, it can make a state channel immediately available after creation (and after
  deposits/withdrawals). The minimum_depth confirmation message is reported as it arrives
  later.
