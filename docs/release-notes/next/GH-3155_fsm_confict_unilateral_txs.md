* State Channel FSM bugfix: when one party asks its FSM to create an unilateral transaction
  (ex. close solo) and while the FSM is waiting for it to be confirmed, the
  other party could create an off-chain update that would cause a conflict on
  both ends. This could deny the former party the possibility of a dispute.
  Fixed
