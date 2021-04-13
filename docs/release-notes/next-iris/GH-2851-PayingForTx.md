* Add a new transaction to the protocol. PayingForTx allows an account to pay
  for a transaction on behalf of someone else. This means paying for fees and
  gas cost, but it will **not** cover the amount spent by the transaction just
  the "the cost of the transaction" (and the extra size added by wrapping the
  original transaction).
