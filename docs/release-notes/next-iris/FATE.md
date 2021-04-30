* Changed `Chain.block_hash` - in `VM_FATE_SOPHIA_2` it will return
  `Some(<blockhash>)` for `Chain.block_height` (i.e. current generation)
  previously it returned `None`. With Bitcoin-NG we do have the block hash of
  the current generation, so no reason not to allow this.

* Generalized accounts, allow access to the signed transaction within the authentication context:
```
switch(Auth.tx)
  Spend(from, to, amount, payload) => ...
  AENSTransfer(from, to, name) => ...
  ...
```
  This enables more use-cases, for example in combination with PayingForTx.

* Added more crypto primitives (mainly pairing operations) for BLS12-381. This
  enables for example Zero-knowledge proofs and more multi-signature schemes.

* Added functions related to strings. It introduces `to_list` and `from_list`
  primitives that enables flexible string manipulation. `Strings.aes` standard
  library functions include many useful string functions.

* Revisited gas prices and gas charging mechanism. The main change is that gas
  will, in some cases, be charged earlier - i.e. contracts run out of gas
  before expensive operations rather than after. This should make the FATE VM
  more efficient. Gas prices have also been adjusted and new operations have
  been calibrated.
