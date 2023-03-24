* Fixes a bug in `/transactions/{hash}/info` endpoint: when the transaction
  that has info (contract create/call/force progress) is wrapped around a meta
  transaction or a paying-for transaction: the info of the inner transaction
  was not provided in the response. This is now fixed.
