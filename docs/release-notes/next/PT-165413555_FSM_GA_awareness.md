* Introduces State Channel FSM Generalized Accounts awareness. It now fully
  supports creation of meta transactions and verifies them.
* Changes WebSocket API regarding signing transactions: now sign requests
  provide a signed transaction to be signed. Old approach was sending unsigned
  transactions.
* Changes channel reestablish checks: for on-chain transactions authentication
  can not always be checked at current chain top. Check is done, if the last
  transaction is an on-chain one, it must be present in on-chain.
