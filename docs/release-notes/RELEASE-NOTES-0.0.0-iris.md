# About this release

[This](https://github.com/aeternity/aeternity/releases/tag/v0.0.0-iris) is a Iris test release.

It:

* Fix buggy serialization of contract information - this means compiler version is actually
  stored on chain, and isn't replaced by "unknown".

* Added `AENS.update` to FATE VM

* Added `AENS.lookup` and `Oracle.expiry` lookup functions to FATE VM

* Fixed bug regarding TTL of preclaims in FATE VM - it was incorrectly always
  set to 0, from `VM_FATE_SOPHIA_2` it has the correct value.

* Fixed bug in `AENS.resolve` in FATE VM - for invalid names `VM_FATE_SOPHIA_1`
  will crash. From `VM_FATE_SOPHIA_2` it will not crash, rather return `None`.

* Changed `Chain.block_hash` - in `VM_FATE_SOPHIA_2` it will return
  `Some(<blockhash>)` for `Chain.block_height` (i.e. current generation)
  previously it returned `None`. With Bitcoin-NG we do have the block hash of
  the current generation, so no reason not to allow this.

* Extend AENS name max expiration time from 50000 generations (~100 days) to
  180000 generations (~375 days).

* Changes how a meta transaction TTL's is being validated: so far it used to
  be the outermost transaction's ttl that was taken into account, now it is
  the innermost one instead. Meta transactions no longer have TTL.

* Fixes a protocol issue: a valid force progress call with invalid CallData or
  failing call will result in on-chain transaction but tokens from the caller
  would still be moved to the forced contract. This is fixed and failed calls
  in successful force progress transactions result in rollback of the
  off-chain balances.

* Improves the functionality of State Channel delegates: now they can provide
  `channel_solo_snapshot_tx` as well. This is really handy in cases one party
  is missing and the other is doing malicious force progess on-chain while the
  channel is still open.

* Revisits the State Channel delegates: so far they were a shared list for
  both participants. From Iris on, delegates are per peer: there is a list of
  delegates for the `initiator` and another one for the `responder`. Old
  channel objects can still be used but users are strongly recomended to reset
  their `delegates` list if they had any. Note that the HTTP representations
  are changed accordingly.

* Allows delegates to force progress on behalf of the user that authorized
  them to do so.
* Improved garbage collector for all Fate contracts form Iris

* Fate contracts of different versions can call eachother (Fate1 can call Fate2 and vice-versa)

* Opcode availability and behaviour depends on VM version of the contract (Fate2 opcodes are available both when Fate2 contract is called directly and when called by another (possibly Fate1) contract)

Please test the **iris** protocol by activating it in the configuration file as below:

```yaml
  chain:
    hard_forks:
      "1": 0
      "2": 1
      "3": 2
      "4": 3

 fork_management:
    network_id: "my_test_iris"
```

Please let us know if you have any problems by [opening a ticket](https://github.com/aeternity/aeternity/issues).
Troubleshooting of common issues is documented [in the wiki](https://github.com/aeternity/aeternity/wiki/Troubleshooting).

## Documentation

For an overview of the installation process for different platforms,
building the package from source, configuration and operation of the Aeternity
node please refer to [Aeternity node documentation](https://docs.aeternity.io/).
