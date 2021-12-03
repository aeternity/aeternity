# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v6.0.0-rc1) is the first Iris release candidate.

It:

* Fixed buggy serialization of contract information - this means the compiler version is actually
  stored on chain, and isn't replaced by "unknown".

* Added `AENS.update` to FATE VM

* Added `AENS.lookup` and `Oracle.expiry` lookup functions to FATE VM

* Fixed bug regarding TTL of preclaims in FATE VM - it was incorrectly always
  set to 0, from `VM_FATE_SOPHIA_2` it has the correct value.

* Fixed a bug in `AENS.resolve` in FATE VM - for invalid names `VM_FATE_SOPHIA_1`
  will crash. From `VM_FATE_SOPHIA_2` it will not crash, rather return `None`.

* Changed `Chain.block_hash` - in `VM_FATE_SOPHIA_2` it will return
  `Some(<blockhash>)` for `Chain.block_height` (i.e. current generation)
  previously it returned `None`. With Bitcoin-NG we do have the block hash of
  the current generation, so no reason not to allow this.

* Extended AENS name max expiration time from 50000 generations (~100 days) to
  180000 generations (~375 days).

* Changed how a meta transaction TTL's is being validated: so far it used to
  be the outermost transaction's ttl that was taken into account, now it is
  the innermost one instead. Meta transactions no longer have TTL.

* Fixed a protocol issue: a valid force progress call with invalid CallData or
  failing call would result in on-chain transaction but tokens from the caller
  would still be moved to the forced contract. This is fixed and failed calls
  in successful force progress transactions result in rollback of the
  off-chain balances.

* Improved the functionality of State Channel delegates: now they can provide
  `channel_solo_snapshot_tx` as well. This is really handy in cases one party
  is missing and the other is doing malicious force progess on-chain while the
  channel is still open.

* Revisited the State Channel delegates: so far they were a shared list for
  both participants. From Iris on, delegates are per peer: there is a list of
  delegates for the `initiator` and another one for the `responder`. Old
  channel objects can still be used but users are strongly recomended to reset
  their `delegates` list if they had any. Note that the HTTP representations
  are changed accordingly.

* Allowed delegates to force progress on behalf of the user that authorized
  them to do so.

* Improved garbage collector for all Fate contracts form Iris

* Fate contracts of different versions can now call each other (Fate1 can call Fate2 and vice-versa)

* Opcode availability and behaviour now depends on VM version of the contract
  (Fate2 opcodes are available both when Fate2 contract is called directly and
  when called by another (possibly Fate1) contract)

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

* Added the possibility to query a an oracle by name hash. A name pointer can
  map `oracle_pubkey` to a an oracle to enable query by name hash.

* Added a new transaction to the protocol. `PayingForTx` allows an account to pay
  for a transaction on behalf of someone else. This means paying for fees and
  gas cost, but it will **not** cover the amount spent by the transaction just
  the "the cost of the transaction" (and the extra size added by wrapping the
  original transaction).

* Fixed a bug in the contract store garbage collector causing maps to be
  more expensive than they should be.

* Added support for protected contract calls. Making a contract call with the named
  argument `protected` set to `true` wraps the result of the call in an
  `option` type, returning `Some(res)` if the call succeeds with result `res`
  and `None` if the call fails for any reason. If the call fails, any
  side-effects it performed are rolled back.

* AENS pointers are now limited, this is enforced when updating a name:
  - No duplicate pointer keys.
  - Pointer keys are not longer than 256 bytes.
  - A name can not have more than 32 pointers.
  When a name is updated, or looked up, inside a Sophia contract keys
  that are no longer valid are automatically removed.

* Added support for `CREATE` opcode

* Added support for `CLONE` opcode

* Added support for `CLONE_G` opcode

* Added support for `BYTECODE_HASH` opcode

* Included full FATE 2 code with init in state trees. This also applies to off-chain contracts in state channels

* Changed comparison and MAP\_TO\_LIST FATE opcodes to follow ordering as defined in `aebytecode`

* Added a new FEE opcode that returns call transaction fee

* Fixed `STR_REVERSE` to reverse on unicode codepoints instead of raw bytes

* Deprecated Ubuntu 16.04 support. EOL Apr 2021.

* Introduced a new HTTP API for asking the node to provide a correct
  `paying_for_tx`. It is marked as `debug` and it is intended to be used while
  developing tools that produce that transaction. This API must not be used in
  real-life scenarios. Since the inner transaction has a specific
  `network_id`, a proper check has been added to the API so attempts to create
  an erroneous `paying_for_tx` will fail.

* Revisited gas prices and gas charging mechanism. The main change is that gas
  will, in some cases, be charged earlier - i.e. contracts run out of gas
  before expensive operations rather than after. This should make the FATE VM
  more efficient. Gas prices have also been adjusted and new operations have
  been calibrated.


Please test the **iris** protocol by activating it in the configuration file as below:

```yaml
  chain:
    hard_forks:
      "1": 0
      "5": 1

 fork_management:
    network_id: "my_test_iris"
```

You can also join `testnet`, the hard fork will happen there at [height 425900](https://github.com/aeternity/aeternity/pull/3584/files#diff-d72f1cea18e8c6f4fee2ca52334645d163d663cf616259b3d8c53c5c9ca848f6R110).

Please let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
