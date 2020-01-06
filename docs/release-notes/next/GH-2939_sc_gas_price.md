* State Channel FSM: fixes fee computation according to transaction size.
  Until now close mutual transactions were expecting a fee to be provided by
  the initiating party, or a default value of 20 000 gas would be used instead.
* State Channel FSM: adds support for an optional `gas_price` parameter for
  all on-chain transactions, including the channel create one. This
  `gas_price` is to be used in transaction fee computation: the fee is
  `gas_price * transaction_gas_cost`. The `transaction_gas_cost` is subject
  to a couple of parameters, one of which is the size, adding a bigger fee
  could lead to a bigger transaction and bigger fee expectations. This makes
  the fee computation not trivial and thus this functionality could be
  valuable for State Channel clients.
