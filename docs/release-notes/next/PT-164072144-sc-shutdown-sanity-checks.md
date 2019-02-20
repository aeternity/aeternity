* More consistent handling of defaults for state channel on-chain transactions
* If a fee is specified for an on-chain state channel tx, but is too low, an error will be raised. If no fee is specified, a default fee 10% above the minimum is chosen.
* If a channel shutdown is requested, but the balances are insufficient to cover the transaction fee, the shutdown request will be rejected right away.
