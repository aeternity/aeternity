* Adds an extra check to reject normal TX signed by GA in mempool. The check is
  cheap so let's avoid cluttering the mempool with bad transactions.
