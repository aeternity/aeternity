* Changes the encoding of private keys in the config. Till now keys were
  encoded as contract byte array (cb_...) but from now on they would be hex
  instead. This is for better integration with wallets.
* Enforces staker key pairs to be valid at start time: if any of the key pairs 
  is invalid, the node will not start.
